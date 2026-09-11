(* [edges_for_file] is stateless, so [Project_index] runs it across files in parallel. *)

module G = AST_generic
open Types

(* Infers var classes from assignments like [x = f()] and stamps them onto
   [id_instance_type]; defined in [Project_index.ml]. *)
type stamp_var_types =
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, G.name) Hashtbl.t ->
  G.program ->
  unit

(* A [required_files_narrowing] holds the project-wide inputs of the
   narrowing by required files (Ruby, PHP): the classes that the
   narrowing can change, the reversed path segments of each definition
   file, and the type state narrowed to each caller directory. One state
   per directory covers every file of that directory that the
   required-files pass did not narrow. The reversed segments are held for
   every file in the list of project files, so the lookup of a definition
   file always succeeds. The narrowed states cover every directory of that
   same list, so the lookup of a caller directory always succeeds. *)
type required_files_narrowing = {
  narrowable_classes : Names.Class_name.t list;
  rev_path_segs_by_file : string list Common.SMap.t;
}

type file_scope = {
  scope_table : Func_lookup.scope_table;
  bound_class_files : (Names.Class_name.t * Fpath.t) list;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  required_files_narrowing : required_files_narrowing option;
  definitions_by_qn : definition Common.SMap.t;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  resolution_orders : Func_lookup.resolution_orders;
  class_qn_by_definition : Func_lookup.class_qn_by_definition;
  methods_by_class : Func_lookup.methods_by_class;
  extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t;
  nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t;
  classes_by_file : class_info list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  global_imports : import list;
  project_constructors : Func_lookup.constructor_index;
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
  (* (module qn string, exported name) -> module-level bare-name alias
     value; see [build_value_alias_index]. *)
  value_alias_index : (string * string, AST_generic.expr) Hashtbl.t;
}

(* Module-level bare-name value aliases ([f = sink] at module top level),
   for import-value svalue stamping (issue #499, cross-file alias).
   Conflicting or non-bare-name assignments are dropped. *)
val build_value_alias_index :
  Types.file_info list -> (string * string, AST_generic.expr) Hashtbl.t

val build_scope_table :
  lang:Lang.t ->
  cfg:Index_lang_rules.t ->
  definitions_by_qn:definition Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  dunder_all:(string, unit) Hashtbl.t Common.SMap.t ->
  classes_by_file:class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  extensions_by_module:Func_info.t list Common.SMap.t Common.SMap.t ->
  nested_types_by_class:Names.Class_qn.t Common.SMap.t Common.SMap.t ->
  global_imports:import list ->
  file_info ->
  file_scope option

val edges_for_file :
  ctx -> file_info ->
  (Function_id.t * Function_id.t * Tok.t) list


(* Wall-clock seconds per stage of [edges_for_file], summed over every
   file and domain since the process started, largest first. *)
val edge_stage_report : unit -> (string * float) list
