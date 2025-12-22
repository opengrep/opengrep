(** Instantiation of taint signatures *)

(** Like 'Shape_and_sig.Effect.t' but instantiated for a specific call site.
 * 'ToLval' effects refer to specific 'IL.lval's rather than to 'Taint.lval's.
 * 'ToSinkInCall' effects are preserved when the callback cannot be resolved
 * (e.g., during signature extraction when the callback is a parameter). *)
type call_effect =
  | ToSink of Shape_and_sig.Effect.taints_to_sink
  | ToReturn of Shape_and_sig.Effect.taints_to_return
  | ToLval of Taint.taints * IL.name * Taint.offset list
  | ToSinkInCall of {
      callee : IL.exp;
      arg : Taint.arg;
      args_taints : Shape_and_sig.Effect.args_taints;
    }

type call_effects = call_effect list

val instantiate_function_signature :
  Taint_lval_env.t ->
  Shape_and_sig.Signature.t ->
  callee:IL.exp ->
  args:IL.exp IL.argument list option (** actual arguments *) ->
  (Taint.Taint_set.t * Shape_and_sig.Shape.shape) IL.argument list ->
  ?lookup_sig:(IL.exp -> int -> Shape_and_sig.Signature.t option) ->
  ?depth:int ->
  unit ->
  call_effects option
(** Instantiation is meant to replace the taint and shape variables in the
 * signature of a callee function, with the taints and shapes of the parameters
 * at the call site. It also constructs the call trace.
 *)
