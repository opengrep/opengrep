(* [edges_for_file] is stateless, so [Project_index] runs it across files in parallel. *)

module G = AST_generic
open Types

(* Infers var classes from assignments like [x = f()] and stamps them onto
   [id_type]; defined in [Project_index.ml]. *)
type stamp_var_types =
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, G.name) Hashtbl.t ->
  G.program ->
  unit

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  all_funcs : Func_info.t list;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_funcs_by_package : (string, Func_info.t list) Hashtbl.t;
  project_class_names : G.name list;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  default_export_class : (string, G.name) Hashtbl.t;
  named_export_classes : (string * string, G.name) Hashtbl.t;
  default_export_fn : (string, Func_info.t) Hashtbl.t;
  path_suffix_index : (string, string list) Hashtbl.t option;
  slice_element_of_field : (string * string, G.name) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
  visible_names_for_file : file_info -> (string, unit) Hashtbl.t;
  stamp_var_types : stamp_var_types;
  resolve_ts_specifier :
    path_suffix_index:(string, string list) Hashtbl.t option ->
    current_file:Fpath.t -> string -> string list;
}

val edges_for_file :
  ctx -> file_info ->
  (Function_id.t * Function_id.t * Tok.t) list

