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
  narrowed_type_state_by_caller_dir : Type_state.t Common.SMap.t;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  required_files_narrowing : required_files_narrowing option;
  all_funcs : Func_info.t list;
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

(* The result lists the directories of the given files, sorted and without
   repetition. *)
val distinct_dirs_of_files : Types.file_info list -> string list

(* The result is [type_state] with [narrowable_classes] narrowed to the
   methods and the defining files in [dir]. A file of that directory
   resolves its calls against this state when no earlier pass narrowed
   the state for that file. *)
val narrowed_type_state_for_dir :
  type_state:Type_state.t ->
  narrowable_classes:Names.Class_name.t list ->
  string ->
  Type_state.t

val edges_for_file :
  ctx -> file_info ->
  (Function_id.t * Function_id.t * Tok.t) list


(* Wall-clock seconds per stage of [edges_for_file], summed over every
   file and domain since the process started, largest first. *)
val edge_stage_report : unit -> (string * float) list
