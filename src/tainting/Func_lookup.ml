(* An index type built from a [Hashtbl.t] keeps that table with no copy, so
   the caller must not mutate the table after building the index. *)

(* [Layered (front, back)] returns [front]'s functions for the bare name
   followed by [back]'s. [Override (front, back)] returns [front]'s functions
   when [front] holds the bare name, and [back]'s otherwise. Both forms read
   [back], the project table shared by every file, and never write to it;
   [front] is a small table built for one file. *)
type bare_name_index =
  | Table of (string, Func_info.t list) Hashtbl.t
  | Layered of bare_name_index * bare_name_index
  | Override of bare_name_index * bare_name_index
type module_index = (Names.Module_qn.t, Func_info.t list) Hashtbl.t
type alias_index = (string, Names.Module_qn.t) Hashtbl.t
type file_module_index = (string, Names.Module_qn.t) Hashtbl.t
type name_set = (string, unit) Hashtbl.t

(* Local import name -> the exported name it binds and the files that
   export it.  [import { C as Alias }] names its origin exactly, which is
   what tells two same-named imported classes apart. *)
type class_alias_index = (string, string * name_set) Hashtbl.t

type constructor_index = Func_info.t list Common.SMap.t

let bare_name_index_of_hashtbl tbl = Table tbl
let bare_name_index_layered ~front ~back = Layered (front, back)
let bare_name_index_override ~front ~back = Override (front, back)

let rec find_in_index (idx : bare_name_index) (key : string) : Func_info.t list =
  match idx with
  | Table tbl -> Option.value (Hashtbl.find_opt tbl key) ~default:[]
  | Layered (front, back) -> (
      match (find_in_index front key, find_in_index back key) with
      | xs, [] -> xs
      | [], ys -> ys
      | xs, ys -> xs @ ys)
  | Override (front, back) -> (
      match find_in_index front key with
      | [] -> find_in_index back key
      | xs -> xs)

let module_index_of_hashtbl tbl = tbl
let alias_index_of_hashtbl tbl = tbl
let file_module_index_of_hashtbl tbl = tbl
let name_set_of_hashtbl tbl = tbl
let class_alias_index_of_hashtbl tbl = tbl
let name_set_mem set name = Hashtbl.mem set name

let constructor_index_of_funcs ~(lang : Lang.t)
    (funcs : Func_info.t list) : constructor_index =
  List.fold_left
    (fun (constructors_by_class : constructor_index) (func : Func_info.t) ->
      match Func_info.as_method func.fn_id with
      | None -> constructors_by_class
      | Some ((cls : IL.name), (meth : IL.name)) ->
        let class_name = fst cls.IL.ident in
        if
          not
            (Object_initialization.is_constructor lang (fst meth.IL.ident)
               (Some class_name))
        then constructors_by_class
        else
          Common.SMap.update class_name
            (function
              | None -> Some [ func ]
              | Some (constructors : Func_info.t list) ->
                Some (func :: constructors))
            constructors_by_class)
    Common.SMap.empty funcs
  |> Common.SMap.map List.rev

let constructor_index_of_hashtbl ~(lang : Lang.t)
    (tbl : (string, Func_info.t list) Hashtbl.t) : constructor_index option =
  match
    List.concat_map
      (fun (name : string) ->
        Option.value (Hashtbl.find_opt tbl name) ~default:[])
      (Object_initialization.get_constructor_names lang)
  with
  | [] -> None
  | _ :: _ as funcs_under_constructor_names ->
    Some (constructor_index_of_funcs ~lang funcs_under_constructor_names)

type t = {
  funcs_by_name : bare_name_index option;
  project_funcs_by_name : bare_name_index option;
  funcs_by_module_qn : module_index option;
  alias_to_module_qn : alias_index option;
  same_file_funcs_by_name : bare_name_index option;
  funcs_by_package : bare_name_index option;
  (* Disambiguates method homonyms across same-basename packages by exact import path. *)
  file_module_qn : file_module_index option;
  local_imports : name_set option;
  class_aliases : class_alias_index option;
  constructors : constructor_index option;
  project_constructors : constructor_index option;
  (* The project index widens an overload group's representative to the
     union of the group (see [Structural_dispatch.emit_overload_edges]),
     so a same-arity tie resolves to it; a single-file graph has no such
     union and gives up on the tie. *)
  overload_groups : bool;
}

let overload_groups (t : t) : bool = t.overload_groups

let empty = {
  funcs_by_name = None;
  project_funcs_by_name = None;
  funcs_by_module_qn = None;
  alias_to_module_qn = None;
  same_file_funcs_by_name = None;
  funcs_by_package = None;
  file_module_qn = None;
  local_imports = None;
  class_aliases = None;
  constructors = None;
  project_constructors = None;
  overload_groups = false;
}

let create
    ?funcs_by_name ?project_funcs_by_name
    ?funcs_by_module_qn ?alias_to_module_qn
    ?same_file_funcs_by_name ?funcs_by_package ?file_module_qn
    ?local_imports ?class_aliases ?constructors ?project_constructors
    ?(overload_groups = false) () =
  { funcs_by_name;
    project_funcs_by_name;
    funcs_by_module_qn;
    alias_to_module_qn;
    same_file_funcs_by_name;
    funcs_by_package;
    file_module_qn;
    local_imports;
    class_aliases;
    constructors;
    project_constructors;
    overload_groups }

(* [None] when the name is not an import alias for a class. *)
let resolve_class_alias t name =
  match t.class_aliases with
  | Some idx -> Hashtbl.find_opt idx name
  | None -> None

let with_local_imports t local_imports : t =
  { t with local_imports }

let is_locally_imported t name =
  match t.local_imports with
  | Some idx -> Hashtbl.mem idx name
  | None -> false

let funcs_with_bare_name t ~all_funcs bare_name =
  match t.project_funcs_by_name with
  | Some idx -> find_in_index idx bare_name
  | None ->
    List.filter (fun (func : Func_info.t) ->
      match List_.init_and_last_opt func.fn_id with
      | Some (_, Some name) -> String.equal (fst name.IL.ident) bare_name
      | _ -> false
    ) all_funcs

let narrow_candidates_by_bare_name t bare_name =
  match t.funcs_by_name with
  | Some idx ->
    Some (find_in_index idx bare_name)
  | None -> None

let constructors_of_class (t : t) (class_name : string)
  : Func_info.t list =
  let constructors_of (constructors_by_class : constructor_index)
      : Func_info.t list =
    Option.value (Common.SMap.find_opt class_name constructors_by_class)
      ~default:[]
  in
  match t.constructors with
  | Some (file_constructors : constructor_index) ->
    constructors_of file_constructors
  | None ->
    Option.fold ~none:[] ~some:constructors_of t.project_constructors

let nested_in_same_file t bare_name =
  match t.same_file_funcs_by_name with
  | Some idx -> find_in_index idx bare_name
  | None -> []

let resolve_alias t name =
  match t.alias_to_module_qn with
  | Some idx -> Hashtbl.find_opt idx name
  | None -> None

let module_qn_of_file t file =
  match t.file_module_qn with
  | Some idx -> Hashtbl.find_opt idx file
  | None -> None

let imports_indexed t =
  Option.is_some t.alias_to_module_qn
  && Option.is_some t.funcs_by_module_qn

let funcs_in_module t qn =
  match t.funcs_by_module_qn with
  | Some idx -> (Option.value (Hashtbl.find_opt idx qn) ~default:[])
  | None -> []

let funcs_in_package t pkg =
  match t.funcs_by_package with
  | Some idx -> find_in_index idx pkg
  | None -> []
