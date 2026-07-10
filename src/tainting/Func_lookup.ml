(* Index types wrap [Hashtbl.t]s (no snapshot): do not mutate after wrapping. *)

type leaf_index = (string, Func_info.t list) Hashtbl.t
type module_index = (Names.Module_qn.t, Func_info.t list) Hashtbl.t
type alias_index = (string, Names.Module_qn.t) Hashtbl.t
type file_module_index = (string, Names.Module_qn.t) Hashtbl.t
type name_set = (string, unit) Hashtbl.t

let leaf_index_of_hashtbl tbl = tbl
let module_index_of_hashtbl tbl = tbl
let alias_index_of_hashtbl tbl = tbl
let file_module_index_of_hashtbl tbl = tbl
let name_set_of_hashtbl tbl = tbl

type t = {
  funcs_by_name : leaf_index option;
  project_funcs_by_name : leaf_index option;
  funcs_by_module_qn : module_index option;
  alias_to_module_qn : alias_index option;
  same_file_funcs_by_name : leaf_index option;
  funcs_by_package : leaf_index option;
  (* Disambiguates method homonyms across same-basename packages by exact import path. *)
  file_module_qn : file_module_index option;
  local_imports : name_set option;
}

let empty = {
  funcs_by_name = None;
  project_funcs_by_name = None;
  funcs_by_module_qn = None;
  alias_to_module_qn = None;
  same_file_funcs_by_name = None;
  funcs_by_package = None;
  file_module_qn = None;
  local_imports = None;
}

let create
    ?funcs_by_name ?project_funcs_by_name
    ?funcs_by_module_qn ?alias_to_module_qn
    ?same_file_funcs_by_name ?funcs_by_package ?file_module_qn
    ?local_imports () =
  { funcs_by_name;
    project_funcs_by_name;
    funcs_by_module_qn;
    alias_to_module_qn;
    same_file_funcs_by_name;
    funcs_by_package;
    file_module_qn;
    local_imports }

let with_local_imports t local_imports : t =
  { t with local_imports }

let is_locally_imported t name =
  match t.local_imports with
  | Some idx -> Hashtbl.mem idx name
  | None -> false

let funcs_with_leaf t ~all_funcs leaf =
  match t.project_funcs_by_name with
  | Some idx -> (Option.value (Hashtbl.find_opt idx leaf) ~default:[])
  | None ->
    List.filter (fun (func : Func_info.t) ->
      match List_.init_and_last_opt func.fn_id with
      | Some (_, Some name) -> String.equal (fst name.IL.ident) leaf
      | _ -> false
    ) all_funcs

let narrow_candidates_by_leaf t leaf =
  match t.funcs_by_name with
  | Some idx ->
    Some (Option.value (Hashtbl.find_opt idx leaf) ~default:[])
  | None -> None

let nested_in_same_file t leaf =
  match t.same_file_funcs_by_name with
  | Some idx -> (Option.value (Hashtbl.find_opt idx leaf) ~default:[])
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
  | Some idx -> (Option.value (Hashtbl.find_opt idx pkg) ~default:[])
  | None -> []
