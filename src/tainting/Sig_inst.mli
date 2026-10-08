(** Instantiation of taint signatures *)

(** Like 'Shape_and_sig.Effect.t' but instantiated for a specific call site.
 * 'ToLval' effects refer to specific 'IL.lval's rather than to 'Taint.lval's.
 * 'ToSinkInCall' effects are preserved when the callback cannot be resolved
 * (e.g., during signature extraction when the callback is a parameter).
 *
 * The 'guards' field of 'ToSink'/'ToReturn'/'ToLval' after instantiation
 * contains only guards rebound into the outer (caller's) parameter namespace — see
 * 'instantiate_function_signature'. Guards anchored in the callee's parameters
 * are either consumed (evaluated to a concrete bool at the call) or dropped
 * on output. *)
type call_effect =
  | ToSink of Shape_and_sig.Effect.taints_to_sink
  | ToReturn of Shape_and_sig.Effect.taints_to_return
  | ToLval of {
      taints : Taint.taints;
      var : IL.name;
      offset : Taint.offset list;
      guards : Effect_guard.t;
          (** Guard on the write, rebound into the outer (caller's)
              parameter namespace. Carried so the caller re-emits the effect
              under the same condition rather than unconditionally. *)
    }
  | ToLvalCaptured of {
      taints : Taint.taints;
      var : IL.name;
      offset : Taint.offset list;
      guards : Effect_guard.t;
          (** A write to a variable the callee captured, a local of some
              enclosing function: it updates the caller's environment when
              the caller owns the variable, and is never an effect of the
              caller. *)
    }
  | ToLvalThis of {
      taints : Taint.taints;
      offset : Taint.offset list;
      guards : Effect_guard.t;
          (** Field write on enclosing receiver; kept [BThis] so it composes
              into the caller's own sig, not resolved to the receiver temp. *)
    }
  | ToSinkInCall of {
      callee : IL.exp;
      arg : Shape_and_sig.Effect.callee;
      arg_offset : Taint.offset list;
      args_taints : Shape_and_sig.Effect.args_taints;
      guards : Effect_guard.t;
          (** Rebound guard on the preserved ToSinkInCall. When the
              caller resolves the callback at a deeper call chain the
              guard travels with the effect and may still drop it. *)
    }

type call_effects = call_effect list

val show_call_effects : call_effects -> string

type sig_inst_cache

val merge_dispatch_signatures :
  ?representative_sig:Shape_and_sig.Signature.t ->
  Shape_and_sig.Signature.t list ->
  Shape_and_sig.Signature.t ->
  Shape_and_sig.Signature.t
(** Merges the dispatch implementation signatures. BArg is normalised to the
 * representative's params, else to the first impl's; receivers are stripped;
 * the effects are unioned, except the members' effects that depend on a
 * global variable (a BGlob base). The second argument is the interface
 * signature, returned unchanged when there are no impls. On incompatible
 * params the first signature is returned. *)

val arg_bound : Shape_and_sig.Signature.t -> Taint.arg -> bool
(** Whether the argument is one of the signature's own parameters. *)

val close_over :
  lang:Lang.t ->
  Taint_lval_env.t ->
  Shape_and_sig.Signature.t ->
  Shape_and_sig.Signature.t
(** Closes a lifted lambda signature where the closure is formed: each
    [BCaptured] variable takes its value in the given environment, the
    environment of the enclosing function at the definition. A read becomes
    the variable's taints; a call becomes a call of what it holds (a deferred
    call on the enclosing parameter it is, or the effects of the function it
    holds); a write stays a write to the variable, and is also a write to the
    parameter of the enclosing function the variable holds, for a closure that
    leaves the function. A variable the environment does not know stays a
    [BCaptured] placeholder, bound where the closure is called if that is in
    the function owning the variable. The lambda's own parameters, the receiver
    and the control taint are bound where the closure is called. *)

val instantiate_function_signature :
  lang:Lang.t ->
  ?max_offset:int ->
  ?outer_params:IL.param list ->
  Taint_lval_env.t ->
  Shape_and_sig.Signature.t ->
  callee:IL.exp ->
  args:IL.exp IL.argument list option (** actual arguments *) ->
  (Taint.Taint_set.t * Shape_and_sig.Shape.shape) IL.argument list ->
  ?lookup_sig:(IL.exp -> int -> Shape_and_sig.Signature.t option) ->
  ?depth:int ->
  ?recursive_cache:sig_inst_cache ->
  unit ->
  call_effects option
(** Replaces taint, shape and guard variables in the callee's signature
    with the caller-side values, and constructs the call trace.

    Each guard anchored in the callee's parameters is classified as follows
    at [inst_effect]:
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
