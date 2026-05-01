(** Instantiation of taint signatures *)

(** Like 'Shape_and_sig.Effect.t' but instantiated for a specific call site.
 * 'ToLval' effects refer to specific 'IL.lval's rather than to 'Taint.lval's.
 * 'ToSinkInCall' effects are preserved when the callback cannot be resolved
 * (e.g., during signature extraction when the callback is a parameter).
 *
 * The 'guards' field of 'ToSink'/'ToReturn' after instantiation contains only
 * guards rebound into the outer (caller's) parameter namespace — see
 * 'instantiate_function_signature'. Callee-frame guards are either consumed
 * (evaluated to a concrete bool at the call) or dropped on output. *)
type call_effect =
  | ToSink of Shape_and_sig.Effect.taints_to_sink
  | ToReturn of Shape_and_sig.Effect.taints_to_return
  | ToLval of Taint.taints * IL.name * Taint.offset list
  | ToSinkInCall of {
      callee : IL.exp;
      arg : Taint.arg;
      arg_offset : Taint.offset list;
      args_taints : Shape_and_sig.Effect.args_taints;
      guards : Effect_guard.t;
          (** Rebound guard on the preserved ToSinkInCall. When the
              caller resolves the callback at a deeper call chain the
              guard travels with the effect and may still drop it. *)
    }

type call_effects = call_effect list

val instantiate_function_signature :
  lang:Lang.t ->
  ?outer_params:IL.param list ->
  Taint_lval_env.t ->
  Shape_and_sig.Signature.t ->
  callee:IL.exp ->
  args:IL.exp IL.argument list option (** actual arguments *) ->
  (Taint.Taint_set.t * Shape_and_sig.Shape.shape) IL.argument list ->
  ?lookup_sig:(IL.exp -> int -> Shape_and_sig.Signature.t option) ->
  ?depth:int ->
  unit ->
  call_effects option
(** Replaces taint, shape and guard variables in the callee's signature
    with the caller-side values, and constructs the call trace.

    Each callee-frame guard is classified as follows at [inst_effect]:
    {ul
    {- its substituted [cond] reduces to [G.Lit (G.Bool true)]: guard
       dropped from the output, effect kept;}
    {- reduces to [G.Lit (G.Bool false)]: effect dropped;}
    {- otherwise (unknown), and every free [Fetch] in the substituted
       cond resolves to a parameter in [outer_params]: the guard is
       rebound with [param_refs] pointing into [outer_params] and
       attached to the output;}
    {- otherwise: the guard is dropped (sound but loses precision).}}

    [outer_params] is the formal parameter list of the function whose
    signature is currently being built. It is required to produce the
    [param_refs] of a rebound guard — a guard's [param_refs] is keyed
    by sig-param position, which the instantiator has no other way to
    obtain. When [outer_params] is omitted, rebinding does not apply
    and unknown guards are dropped. *)
