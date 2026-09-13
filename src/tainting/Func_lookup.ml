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

type constructor_index = Func_info.t list Common.SMap.t

type module_attribute =
  | Attr_functions of Func_info.t list
  | Attr_class of Names.Class_qn.t
  | Attr_module of Names.Module_qn.t

type module_attributes = module_attribute Common.SMap.t Common.SMap.t

type resolution_orders = Names.Class_qn.t list Common.SMap.t

module Class_qn_map = Map.Make (struct
  type t = Names.Class_qn.t
  let compare = Names.Class_qn.compare
end)

type methods_by_class = Func_info.t list Common.SMap.t Class_qn_map.t

type class_qn_by_definition =
  (Function_id.t * Names.Class_qn.t) list Common.SMap.t

let attributes_of_module (attributes : module_attributes)
    (qn : Names.Module_qn.t) : module_attribute Common.SMap.t =
  Option.value
    (Common.SMap.find_opt (Names.Module_qn.to_string qn) attributes)
    ~default:Common.SMap.empty

type scope_kind =
  | Scope_function of Func_info.t
  | Scope_class of Names.Class_qn.t
  | Scope_extension of Func_info.t
  | Scope_object of Func_info.t list Common.SMap.t
  | Scope_local_value

type scope_entry = {
  kind : scope_kind;
  parent_path : IL.name option list;
}

type scope_table = scope_entry list Common.SMap.t

let scope_table_of_map (bindings : scope_entry list Common.SMap.t)
    : scope_table = bindings

let class_of_entries (entries : scope_entry list) : Names.Class_qn.t option =
  List.find_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_class (class_qn : Names.Class_qn.t) -> Some class_qn
      | Scope_function _
      | Scope_object _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let functions_of_entries (entries : scope_entry list) : Func_info.t list =
  List.filter_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_function (func : Func_info.t) -> Some func
      | Scope_class _
      | Scope_object _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let extensions_of_entries (entries : scope_entry list) : Func_info.t list =
  List.filter_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_extension (func : Func_info.t) -> Some func
      | Scope_class _
      | Scope_object _
      | Scope_local_value
      | Scope_function _ -> None)
    entries

let object_of_entries (entries : scope_entry list)
    : Func_info.t list Common.SMap.t option =
  List.find_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_object (members : Func_info.t list Common.SMap.t) -> Some members
      | Scope_class _
      | Scope_function _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let empty_scope_table : scope_table = Common.SMap.empty

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
  (* Disambiguates method homonyms across same-basename packages by exact import path. *)
  file_module_qn : file_module_index option;
  local_imports : name_set option;
  constructors : constructor_index option;
  project_constructors : constructor_index option;
  (* The project index widens an overload group's representative to the
     union of the group (see [Structural_dispatch.emit_overload_edges]),
     so a same-arity tie resolves to it; a single-file graph has no such
     union and gives up on the tie. *)
  overload_groups : bool;
  scope_table : scope_table;
  own_modules : Names.Module_qn.t list;
  module_attributes : module_attributes;
  resolution_orders : resolution_orders;
  class_qn_by_definition : class_qn_by_definition;
  methods_by_class : methods_by_class;
}

let overload_groups (t : t) : bool = t.overload_groups

let own_modules (t : t) : Names.Module_qn.t list = t.own_modules

let resolve_in_scope (t : t) (name : string) : scope_entry list =
  Option.value (Common.SMap.find_opt name t.scope_table) ~default:[]

let module_attribute (t : t) (qn : Names.Module_qn.t) (name : string)
    : module_attribute option =
  Common.SMap.find_opt name (attributes_of_module t.module_attributes qn)

let resolution_order (t : t) (class_qn : Names.Class_qn.t)
    : Names.Class_qn.t list =
  Option.value
    (Common.SMap.find_opt (Names.Class_qn.to_string class_qn)
       t.resolution_orders)
    ~default:[]

let is_known_class (t : t) (class_qn : Names.Class_qn.t) : bool =
  Common.SMap.mem (Names.Class_qn.to_string class_qn) t.resolution_orders

let is_known_module (t : t) (module_qn : Names.Module_qn.t) : bool =
  match
    Common.SMap.find_opt (Names.Module_qn.to_string module_qn)
      t.module_attributes
  with
  | None -> false
  | Some (attributes : module_attribute Common.SMap.t) ->
    not (Common.SMap.is_empty attributes)

let class_qn_of_definition (t : t) (definition : IL.name)
    : Names.Class_qn.t option =
  match
    Common.SMap.find_opt (fst definition.IL.ident) t.class_qn_by_definition
  with
  | None -> None
  | Some (classes : (Function_id.t * Names.Class_qn.t) list) ->
    Option.map snd
      (List.find_opt
         (fun (((id : Function_id.t), _) :
                 Function_id.t * Names.Class_qn.t) ->
           Function_id.equal_name id definition)
         classes)

let find_along_order (t : t) (order : Names.Class_qn.t list)
    (names_of : Names.Class_qn.t -> string list) : Func_info.t list =
  let bound_on (class_qn : Names.Class_qn.t) : Func_info.t list =
    match Class_qn_map.find_opt class_qn t.methods_by_class with
    | None -> []
    | Some (by_name : Func_info.t list Common.SMap.t) ->
      List.concat_map
        (fun (name : string) ->
          Option.value (Common.SMap.find_opt name by_name) ~default:[])
        (names_of class_qn)
  in
  let rec first_binder (order : Names.Class_qn.t list) : Func_info.t list =
    match order with
    | [] -> []
    | class_qn :: rest -> (
      match bound_on class_qn with
      | [] -> first_binder rest
      | (_ :: _) as found -> found)
  in
  first_binder order

let empty = {
  funcs_by_name = None;
  project_funcs_by_name = None;
  funcs_by_module_qn = None;
  alias_to_module_qn = None;
  same_file_funcs_by_name = None;
  file_module_qn = None;
  local_imports = None;
  constructors = None;
  project_constructors = None;
  overload_groups = false;
  scope_table = empty_scope_table;
  own_modules = [];
  module_attributes = Common.SMap.empty;
  resolution_orders = Common.SMap.empty;
  class_qn_by_definition = Common.SMap.empty;
  methods_by_class = Class_qn_map.empty;
}

let create
    ?funcs_by_name ?project_funcs_by_name
    ?funcs_by_module_qn ?alias_to_module_qn
    ?same_file_funcs_by_name ?file_module_qn
    ?local_imports ?constructors ?project_constructors
    ?(overload_groups = false)
    ?(own_modules : Names.Module_qn.t list = [])
    ~(module_attributes : module_attributes)
    ~(resolution_orders : resolution_orders)
    ~(class_qn_by_definition : class_qn_by_definition)
    ~(methods_by_class : methods_by_class)
    ~(scope_table : scope_table) () =
  { funcs_by_name;
    project_funcs_by_name;
    funcs_by_module_qn;
    alias_to_module_qn;
    same_file_funcs_by_name;
    file_module_qn;
    local_imports;
    constructors;
    project_constructors;
    overload_groups;
    scope_table;
    own_modules;
    module_attributes;
    resolution_orders;
    class_qn_by_definition;
    methods_by_class }

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
