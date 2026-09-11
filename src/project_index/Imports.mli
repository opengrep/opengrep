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
  Types.import list
  * (string * string * Types.import_kind) list

val wildcard_local : string

type binding =
  | Wildcard_from of Names.Module_qn.t
  | Named_binding of { local : string; target : Names.Module_qn.t }

val binding_of : Types.import -> binding
