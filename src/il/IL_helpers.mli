val is_pro_resolved_global : IL.name -> bool
(** Test whether a name is global and has been resolved by Pro-naming. *)

val is_class_name : IL.name -> bool
val exp_of_arg : IL.exp IL.argument -> IL.exp

val pname_of_param : IL.param -> IL.name option
(** [Some pname] for [Param]/[ParamRest]/[ParamPattern]; [None] for
    [ParamFixme]. *)

val offset_is_resolvable : IL.offset -> bool
(** Whether the offset step can be resolved statically: a [Dot] field
    or an [Index] of a literal [Int]/[String]. *)

val param_index : IL.param list -> IL.name -> int option
(** Position (zero-based) of a parameter whose [pname] matches [name]
    via [IL.equal_name]. *)

val cond_param_refs :
  IL.param list -> IL.exp -> (IL.name * int) list option
(** Collect [(IL.name, index)] pairs for every free [Fetch] in [cond]
    whose base is a parameter in the given list (matched via
    [IL.equal_name]) with a statically-resolvable offset path. Descends
    into every [IL.exp] kind that can hold a nested [Fetch] ([Operator],
    [Cast], [Composite], [RecordOrDict], [FixmeExp]), so [Some]
    guarantees [cond] is fully anchored. Returns [None] if any free
    [Fetch] fails to anchor: a non-parameter base, a [VarSpecial]/[Mem]
    base, or an offset step that fails [offset_is_resolvable]. *)

val cond_partial_param_refs :
  IL.param list -> IL.exp -> (IL.name * int) list
(** Find which free [Fetch]es in a boolean expression refer to a given
    list of formal parameters.

    [cond_partial_param_refs params cond] walks [cond] and returns one
    [(IL.name, index)] pair per [Fetch] whose base matches one of
    [params] (via [IL.equal_name]) with a statically-resolvable offset
    path; [index] is the parameter's zero-based position in [params].

    Unlike [cond_param_refs], the walk never aborts: a [Fetch] that
    fails to anchor or has a non-resolvable offset is skipped instead
    of rejecting the whole cond. The walker descends through every
    [IL.exp] kind that can hold a nested expression ([Operator],
    [Cast], [Composite], [RecordOrDict], and a [FixmeExp]'s
    partial-translation slot); the only terminal kinds are [Literal]
    and a [Fetch] whose base is [VarSpecial]/[Mem] (or [Var] but
    unanchored).

    The reach of this walker is paired with [substitute_free_fetches]:
    any Fetch position reported here must be rewritable by that
    function. *)

(** Lvalue/Rvalue helpers working on the IL *)

val lval_of_var : IL.name -> IL.lval

val is_dots_offset : IL.offset list -> bool
(** Test whether an offset is of the form .a_1. ... .a_N.  *)

val lval_of_instr_opt : IL.instr -> IL.lval option
(** If the given instruction stores its result in [lval] then it is
    [Some lval], otherwise it is [None]. *)

val lvar_of_instr_opt : IL.instr -> IL.name option
(** If the given instruct stores its result in an lvalue of the form
    x.o_1. ... .o_N, then it is [Some x], otherwise it is [None]. *)

val lvals_of_exp : IL.exp -> IL.lval list
(** Every lvalue read in [e], including those nested in offsets and
    sub-expressions. *)

val rlvals_of_node : IL.node_kind -> IL.lval list
(** The lvalues that occur in the RHS of a node. *)

val orig_of_node : IL.node_kind -> IL.orig option

val reachable_nodes : IL.fun_cfg -> IL.node Seq.t
(** Get the reachable nodes from function's CFG, including the nodes in the lambdas' CFGs. *)

val lval_is_lambda : IL.lambdas_cfgs -> IL.lval -> (IL.name * IL.fun_cfg) option
(** Lookup an 'lval' in a 'lambdas_cfgs' table to obtain the lambda's CFG. *)

(** {2 Boolean smart constructors over [IL.exp]} *)

val lit_bool : eorig:IL.orig -> bool -> IL.exp
(** Lift a concrete boolean into an [IL.exp] using a fake token. *)

val is_lit_bool : bool -> IL.exp -> bool
(** [is_lit_bool b e] is true iff [e] is the literal [b]. *)

val wrap_not : IL.exp -> IL.exp
(** Wrap [cond] in [Operator(Not, [cond])]. *)

val il_exp_equal : IL.exp -> IL.exp -> bool
(** Compare two [IL.exp]s by their [IL_pp.pp_exp] form; matches
    [Effect_guard.compare_cond] so syntactic complement detection
    aligns with guard dedup. *)

val is_complement : IL.exp -> IL.exp -> bool
(** Direct syntactic complement: [a] vs [Not a]. Catches atom-level
    negation only; De-Morgan-equivalent compounds are not detected. *)

val wrap_and : IL.exp list -> IL.exp
(** Smart n-ary [And]. Empty list produces [true]; singletons returned
    as-is; nested [And] flattened; [true] absorbed; any [false]
    short-circuits to [false]; direct syntactic complement among args
    folds to [false]. General case is right-nested binary. *)

val wrap_or : IL.exp list -> IL.exp
(** Smart n-ary [Or]; dual to [wrap_and]. *)
