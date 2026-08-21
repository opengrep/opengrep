(* Symbol collection: walk one file's AST into the projidx index shapes. *)

(* [Function_id.t] for a method synthesised on a class (no AST def of its
   own), anchored at the class's token. *)
val synth_function_id : Function_id.t -> string -> Function_id.t

(* Per-class set of method names owned directly by the class (from
   [K_method] entries).  The returned function creates the set on first
   lookup. *)
val methods_by_class :
  Types.entry list -> Function_id.t -> (string, unit) Hashtbl.t

(* Walk one file's AST and produce its entries (functions, methods, classes,
   synthesised dunders), class_info records, and file_info. *)
val collect_in_ast :
  cfg:Index_lang_rules.t ->
  lang:Lang.t ->
  module_path:Names.Module_qn.t ->
  file:Fpath.t ->
  AST_generic.program ->
  Types.entry list * Types.class_info list * Types.file_info

(* Synthesised dunder entries for dataclass-like wrapper classes
   ([cfg.wrapper_dunders]), skipping dunders the class already defines. *)
val dataclass_wrapper_synth_entries :
  cfg:Index_lang_rules.t ->
  wrappers:(string, Types.dataclass_wrapper) Hashtbl.t ->
  Types.entry list ->
  Types.class_info list ->
  Types.entry list
