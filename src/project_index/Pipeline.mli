(* [edges_for_file] is stateless, so [Project_index] runs it across files in parallel. *)

module G = AST_generic
open Types

(* Infers var classes from assignments like [x = f()] and stamps them onto
   [id_instance_type]; defined in [Project_index.ml]. *)
type stamp_var_types =
  table:Symbol_table.t ->
  type_state:Type_state.t ->
  caller:Function_id.t option ->
  G.program ->
  unit

type file_scope = {
  scope_table : Func_lookup.scope_table;
  own_modules : Names.Module_qn.t list;
  module_aliases : (string, Names.Module_qn.t) Hashtbl.t option;
  member_classes : Names.Class_qn.t list;
}

type project_classes = {
  class_table : Class_table.t;
  class_of_qn : Names.Class_qn.t -> Class_table.cls option;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  definitions_by_qn : definition Common.SMap.t;
  companions : Func_lookup.companion_index;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  object_classes : unit Common.SMap.t;
  extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t;
  nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t;
  namespace_scope_bindings : Scope_binding.namespace_scope_bindings Common.SMap.t;
  php_global_bindings : Scope_binding.positioned_binding list;
  include_map : Include_map.t;
  module_scope : Scope_module.project_scope;
  go_packages : Scope_go.package_index;
  build_constraints : Go_build_constraints.t;
  top_level_scope : Func_lookup.scope_table;
  module_object_by_module : Names.Class_qn.t Common.SMap.t;
  classes_by_file : entry list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  global_imports : import list;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_class_names : Object_initialization.class_names;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
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

val file_scope_of : ctx -> file_info -> file_scope option

val func_lookup_of :
  ctx ->
  class_of_qn:(Names.Class_qn.t -> Class_table.cls option) ->
  file_scope option ->
  file_info ->
  Func_lookup.t

val definition_of_target :
  definitions_by_qn:definition Common.SMap.t ->
  package:Names.Module_qn.t ->
  Names.Module_qn.t ->
  definition option

val project_table :
  ctx ->
  classes:project_classes ->
  file_table:Symbol_table.t ->
  file_scope option ->
  file_info ->
  Symbol_table.t * Func_lookup.t

val edges_for_file :
  ctx ->
  classes:project_classes ->
  table:Symbol_table.t ->
  func_lookup:Func_lookup.t ->
  file_info ->
  (Function_id.t * Function_id.t * Tok.t) list


(* Wall-clock seconds per stage of [edges_for_file], summed over every
   file and domain since the process started, largest first. *)
val edge_stage_report : unit -> (string * float) list
