(*****************************************************************************)
(* Const/sym ("svalue") propagation *)
(*****************************************************************************)

(* Bounds the number of times that we will follow an 'id_svalue' during
 * a cycle check. See 'Dataflow_svalue.no_cycles_in_svalue'. *)
let svalue_prop_MAX_VISIT_SYM_IN_CYCLE_CHECK = 1000

(* The height of the svalue lattice: a literal, then [Cst] of its type, then
 * [Cst Cany], then [NotCst] ('Eval_il_partial.union' and 'union_ctype'); a
 * symbolic value joins to [NotCst] directly. *)
let svalue_LATTICE_HEIGHT = 3

(* The visit bound of svalue propagation ('Dataflow_core.Recomputation').
 * The first visit computes a value; a variable whose value in the loop
 * depends on no other variable of the loop moves down the lattice at most
 * once per later visit, so it settles within the height. A chain of copies
 * longer than the height is truncated at the bound. The bound also stops
 * the iteration under a transfer that is not monotone (the refinement of a
 * stored symbolic value, 'Eval_il_partial.refine'), under which it need not
 * settle. *)
let svalue_MAX_VISITS_PER_NODE = 1 + svalue_LATTICE_HEIGHT

(*****************************************************************************)
(* Taint analysis *)
(*****************************************************************************)

(* We need to set some limits to prevent taint sets from exploding in some cases.
 * As we root cause these problems and fix them properly, we may be able to raise
 * these limits.
 *
 * These problems became more serious with field-sensitivity: where previously we
 * would just track taint for `x`, now we track taint for `x.a`, `x.b` and `x.c`.
 *
 * In the case of 'WebGoat/src/main/resources/webgoat/static/js/libs/ace.js',
 * for example, it seems that this problem would be greatly reduced if we did
 * not propagate taint for data with Boolean and integer type. Improving some
 * of the data structures involved may help too.
 *)

(* Maximum call chain depth followed by interfile taint when neither the
 * scan nor the rule sets one; negative is unbounded. *)
let taint_INTERFILE_DEPTH = 3
(* The depth at which a callback's signature stops being instantiated
 * recursively. *)
let taint_MAX_CALLBACK_INSTANTIATION_DEPTH = 4

(** Bounds the number of variables we can track. *)
let taint_MAX_TAINTED_VARS = 1000

(** Bounds the number of fields we can track per l-value, that is, the number of
 * fields in an 'Obj' shape, see 'Taint_sig.shape'. *)
let taint_MAX_OBJ_FIELDS = 10

(** Bounds the number of taints we can track per l-value.
 *
 * The size of the taint sets has a significant impact on performance and most
 * "reasonable" taint rules only require small taint sets to work. When the sets
 * grow large is often due to some bug or "inefficiency". The limit could be
 * insufficient for some pathological cases (e.g. rules that have very liberal
 * source specs that match essentially everything), but those we will not be
 * able to run inter-file, and they should be discouraged anyways.
 *)
let taint_MAX_TAINT_SET_SIZE = 25

(** Bounds the length of the offsets we can track per arg/poly-taint.
 *
 * [4] keeps field-sensitivity for nested destructuring / callback /
 * library-access patterns (Clojure, Elixir, Python, Ruby all have
 * cross-function tests that need it).
 *
 * Poly-taint set WIDTH grows combinatorially with this bound, though
 * (each extra level multiplies reachable fields × indexes), so on a
 * language with deep struct nesting over a large codebase it makes the
 * interfile fixpoint explode — grafana Go crawled for minutes at [4].
 * [Taint_shape.max_poly_offset] lowers such languages to
 * [taint_MAX_POLY_OFFSET_FLAT]. *)
let taint_MAX_POLY_OFFSET = 4
let taint_MAX_POLY_OFFSET_FLAT = 1

(** Maximum nesting of [Fun] shapes stored in a signature database (see
 * [Taint_shape.bound_fun_shape]).
 *
 * A call on an unknown callee, such as a parameter's method, is stored in
 * the signature as an effect with the shapes of its arguments. When an
 * argument is a function of the same SCC, its shape is that function's
 * signature, and that signature contains the same call effect. Each
 * fixpoint round then stores the previous round's signature inside the new
 * one, so the signature never stabilises and its size doubles or more per
 * round. A [Fun] shape nested deeper than this collapses to [Bot]: the
 * callback's own effects are kept, a callback it passes on to another
 * unknown callee is dropped. *)
let taint_MAX_SIG_FUN_DEPTH = 2

(** Maximum number of outer fixpoint passes for the self-sig convergence
 * loop in [Dataflow_tainting.fixpoint_aux]. Each pass re-runs the
 * inner dataflow fixpoint while a direct self-recursive call has
 * produced new effects; the loop stops once the effects set is stable
 * or this cap is reached. Only Clojure multi-arity self-recursion
 * currently triggers the loop. *)
let taint_MAX_SELF_SIG_PASSES = 5

(** Maximum number of characters of a guard cond that [Effect_guard.show]
 * renders into a log line; a longer cond is rendered as its first this-many
 * characters followed by "...". Guard conds from deep cross-call forwarding are
 * shared DAGs that unfold to 2^depth characters, so rendering one in full via
 * [IL_pp.pp_exp] under debug logging is exponential. *)
let taint_MAX_GUARD_LOG_CHARS = 200

(** Maximum number of distinct nodes in a guard-cond atom. An atom growing
 * past this (via cross-call substitution of computed actuals) is dropped
 * from its clause — a sound weakening: the clause's guard applies more
 * often, so widening can add findings but never drop them. Length atoms
 * ([length(x) <cmp> n], the shape arity dispatch compiles to) are a few
 * nodes and never approach this. *)
let taint_MAX_GUARD_COND_NODES = 512

(** Maximum number of clauses in a guard cond (the cond is in disjunctive
 * normal form; clause count grows by clause-set union at taint-set joins
 * and multiplies when two disjunctive guards are conjoined). Past the cap,
 * each clause is widened to its length literals — arity dispatch survives:
 * [or(and(len==1, P), and(len==2, Q))] widens to [or(len==1, len==2)] — and
 * a cond still over the cap widens to [true]. Sound in the same direction
 * as the atom cap. Lowering this trades guard precision for speed; near
 * zero it degenerates to arity-only guards. *)
let taint_MAX_GUARD_CLAUSES = 64

(*****************************************************************************)
(* Project index (interfile call graph) *)
(*****************************************************************************)

(* Cap on the number of changing steps of the return-type fixpoint of
 * Type_augment.augment_return_types_from_bodies. The loop ends by itself on a
 * step that writes no return type the state did not hold; the cap stops it
 * earlier, with a warning, when a chain of return types is deeper than the
 * cap. *)
let projidx_RETURN_TYPES_MAX_ITERS = 4
(* Iteration cap on the projidx type-augmentation fixpoint of
 * stamp_var_types_from_bodies; the passes are monotone, the cap only bounds
 * how deep chains propagate. See 'src/project_index/Main.ml'. *)
let projidx_OBJECT_MAPPINGS_MAX_ITERS = 5
(* Cap on the number of changing steps of the outer type inference fixpoint
 * of Project_index.build_project_call_graph. The loop ends by itself on a
 * step that writes no return or field type the state did not hold; the cap
 * stops it earlier, with a warning, when a chain of return or field types is
 * deeper than the cap. *)
let projidx_CALL_GRAPH_MAX_PASSES = 5
