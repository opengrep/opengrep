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

type file_scope = {
  scope_table : Func_lookup.scope_table;
  bound_class_files : (Names.Class_name.t * Fpath.t) list;
  own_modules : Names.Module_qn.t list;
  module_aliases : (string, Names.Module_qn.t) Hashtbl.t option;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  definitions_by_qn : definition Common.SMap.t;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  resolution_orders : Func_lookup.resolution_orders;
  class_qn_by_definition : Func_lookup.class_qn_by_definition;
  methods_by_class : Func_lookup.methods_by_class;
  singleton_names : Func_lookup.singleton_names;
  extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t;
  nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t;
  php_region_bindings : Scope_php.region_bindings Common.SMap.t;
  php_global_bindings : Scope_binding.positioned_binding list;
  include_map : Include_map.t;
  module_scope : Scope_module.project_scope;
  go_packages : Scope_go.package_index;
  top_level_scope : Func_lookup.scope_table;
  classes_by_file : class_info list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  global_imports : import list;
  project_constructors : Func_lookup.constructor_index;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_class_names : G.name list;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  slice_element_of_field : (string * string, G.name) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
  visible_names_for_file : file_info -> (string, unit) Hashtbl.t;
  stamp_var_types : stamp_var_types;
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
  php_region_bindings:Scope_php.region_bindings Common.SMap.t ->
  php_global_bindings:Scope_binding.positioned_binding list ->
  include_map:Include_map.t ->
  module_scope:Scope_module.project_scope ->
  go_packages:Scope_go.package_index ->
  top_level_scope:Func_lookup.scope_table ->
  file_info ->
  file_scope option

val edges_for_file :
  ctx -> file_info ->
  (Function_id.t * Function_id.t * Tok.t) list


(* Wall-clock seconds per stage of [edges_for_file], summed over every
   file and domain since the process started, largest first. *)
val edge_stage_report : unit -> (string * float) list
