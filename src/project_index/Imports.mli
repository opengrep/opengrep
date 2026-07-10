(* Per-file import collection.

   Returns the (local name -> module) bindings and, second, the raw import
   specifiers as (local, specifier, kind).  A ("*", M) binding is the
   wildcard sentinel consumed by the re-export pass.  Handles ImportAs /
   ImportFrom / ImportAll directives, CommonJS [require] variable
   definitions, and Clojure [(ns (:require ...))] forms. *)
val collect_imports :
  cfg:Index_lang_rules.t ->
  current_module_path:Names.Module_qn.t ->
  is_init_file:bool ->
  AST_generic.program ->
  (string * Names.Module_qn.t) list
  * (string * string * Types.import_kind) list
