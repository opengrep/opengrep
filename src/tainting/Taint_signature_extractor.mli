(** Function signature extraction from taint analysis *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type extraction_result = {
  signature : Shape_and_sig.Signature.t;
  mapping : Taint_lval_env.t Dataflow_core.mapping;
}
(** Result of signature extraction containing both the signature and taint
    mapping *)

type signature_database = Shape_and_sig.signature_database
(** Database of function signatures indexed by function name *)

(*****************************************************************************)
(* Main extraction functions *)
(*****************************************************************************)

val extract_signature :
  Taint_rule_inst.t ->
  Taint_shared_tables.t ->
  ?in_env:Taint_lval_env.t ->
  ?name:IL.name ->
  ?signature_db:signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  IL.fun_cfg ->
  extraction_result
(** Extract both signature and taint mapping from a function *)

val extract_signature_with_file_context :
  arity:Shape_and_sig.sig_arity ->
  ?db:signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  name:IL.name ->
  ?method_properties:AST_generic.expr list ->
  Taint_rule_inst.t ->
  Taint_shared_tables.t ->
  IL.fun_cfg ->
  signature_database * Shape_and_sig.Signature.t
(** [extract_signature] with each [this.x]/[self.x] the method reads taken as
    an input like a parameter; the signature is added to the database. *)

(*****************************************************************************)
(* Utility functions *)
(*****************************************************************************)

val show_signature_extraction :
  string option -> Shape_and_sig.Signature.t -> string
(** Pretty print signature extraction result *)

val extract_method_properties :
  AST_generic.function_definition -> AST_generic.expr list
(** Extract this.x and self.x property accesses from a function definition *)
