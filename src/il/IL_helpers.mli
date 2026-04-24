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
    [IL.equal_name]) with a statically-resolvable offset path. Returns
    [None] if any free [Fetch] fails to anchor, any offset step fails
    [offset_is_resolvable], or the cond contains an [IL.exp] kind not
    modelled here (e.g. [Composite], [RecordOrDict], [Cast],
    [FixmeExp]). *)

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

val rlvals_of_node : IL.node_kind -> IL.lval list
(** The lvalues that occur in the RHS of a node. *)

val orig_of_node : IL.node_kind -> IL.orig option

val reachable_nodes : IL.fun_cfg -> IL.node Seq.t
(** Get the reachable nodes from function's CFG, including the nodes in the lambdas' CFGs. *)

val lval_is_lambda : IL.lambdas_cfgs -> IL.lval -> (IL.name * IL.fun_cfg) option
(** Lookup an 'lval' in a 'lambdas_cfgs' table to obtain the lambda's CFG. *)
