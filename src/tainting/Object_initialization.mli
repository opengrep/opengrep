(* Object Initialization Detection for All Languages
 * 
 * This module provides comprehensive object initialization detection
 * for all languages supported by Semgrep, enabling sophisticated
 * taint analysis across object constructors and method calls.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Object mapping: variable -> class *)
type object_mapping = AST_generic.name * AST_generic.name

(*****************************************************************************)
(* Main API *)
(*****************************************************************************)

(* Detect object initialization patterns in an AST for the given language *)
val detect_object_initialization : AST_generic.program -> Lang.t -> object_mapping list

(* Collect all class names from an AST *)
val collect_class_names : AST_generic.program -> AST_generic.name list

(* Extract class name from a constructor expression *)
val extract_class_name_from_constructor : 
  AST_generic.expr -> Lang.t -> AST_generic.name list -> AST_generic.name option

(*****************************************************************************)
(* Constructor Detection Utilities *)
(*****************************************************************************)

(* Check if a function is a constructor for the given language *)
val is_constructor : Lang.t -> string -> string option -> bool

(* Get all constructor method names for a language *)
val get_constructor_names : Lang.t -> string list

(* Check if language uses 'new' keyword *)
val uses_new_keyword : Lang.t -> bool

(*****************************************************************************)
(* Unified Constructor Execution *)
(*****************************************************************************)

(* Unified language-independent constructor execution *)
val execute_unified_constructor : 'a -> 'b list -> 'c list -> 
  ('d -> 'a -> 'b list -> 'c list -> ('e * 'f * 'g) option) -> 'd -> ('e * 'f * 'g) option

(* Execute constructor call for any language - returns constructor info if valid *)
val execute_constructor_call : Lang.t -> string -> string option -> 'a list -> (string * string option * 'a list) option

(* Detect C++ constructor patterns in DefStmt - returns (var_name, class_name, params) if found *)
val detect_cpp_constructor_defstmt : AST_generic.stmt -> AST_generic.name list -> (string * string * AST_generic.parameter list) option

(*****************************************************************************)
(* Debugging and Display *)
(*****************************************************************************)

val show_object_mapping : object_mapping -> string
val show_object_mappings : object_mapping list -> string