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
  ?name:IL.name ->
  ?signature_db:signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?call_graph:Function_call_graph.FuncGraph.t option ->
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
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  name:IL.name ->
  ?method_properties:AST_generic.expr list ->
  ?call_graph:Function_call_graph.FuncGraph.t option ->
  Taint_rule_inst.t ->
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


val detect_object_initialization : AST_generic.program -> Lang.t -> (AST_generic.name * AST_generic.name) list
(** Detect object initialization patterns in the AST for the given language *)
