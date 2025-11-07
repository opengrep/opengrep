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
  ?in_env:Taint_lval_env.t ->
  ?name:Shape_and_sig.fn_id ->
  ?signature_db:signature_database ->
  IL.fun_cfg ->
  extraction_result
(** Extract both signature and taint mapping from a function *)

val mk_global_assumptions_with_sids :
  (string * AST_generic.SId.t) list -> Taint_lval_env.t
(** Create global variable taint assumptions with specific SIDs *)

val mk_global_tracking_without_taint :
  (string * AST_generic.SId.t) list -> Taint_lval_env.t
(** Register global variables for tracking without pre-tainting them *)

val extract_signature_with_file_context :
  arity:int ->
  ?db:signature_database ->
  Taint_rule_inst.t ->
  ?name:Shape_and_sig.fn_id ->
  ?method_properties:AST_generic.expr list ->
  IL.fun_cfg ->
  AST_generic.program ->
  signature_database * Shape_and_sig.Signature.t
(** Extract signature automatically including global variables from file context
    and database *)

(*****************************************************************************)
(* Utility functions *)
(*****************************************************************************)

val show_signature_extraction :
  string option -> Shape_and_sig.Signature.t -> string
(** Pretty print signature extraction result *)

val extract_method_properties :
  AST_generic.function_definition -> AST_generic.expr list
(** Extract this.x and self.x property accesses from a function definition *)


(*****************************************************************************)
(* Batch extraction *)
(*****************************************************************************)


val empty_signature_database : unit -> signature_database
(** Create an empty signature database *)

val lookup_signature :
  signature_database -> Shape_and_sig.fn_id -> int -> Shape_and_sig.Signature.t option
(** Look up a signature by function name *)

val show_signature_database : signature_database -> string
(** Show all signatures in the database *)

val detect_object_initialization : AST_generic.program -> Lang.t -> (AST_generic.name * AST_generic.name) list
(** Detect object initialization patterns in the AST for the given language *)
