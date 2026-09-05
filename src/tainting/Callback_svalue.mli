(* Argument-to-parameter symbolic value propagation (issue #499, "gap B").
 *
 * When every known call site of a function binds the same bare name (e.g.
 * [sink]) to a parameter the body calls, stamp that parameter's uses with
 * [Sym <name>] so that pattern matching with [symbolic_propagation]
 * matches through it — the callback analogue of naming's same-scope alias
 * stamping ([f = sink]). Stamps only fill empty [id_svalue] slots and are
 * inert unless a rule enables [symbolic_propagation]. *)

(* [(param_sid, value)] stamping decisions computed over a set of ASTs;
   sids are positional, so decisions computed on one parse apply to any
   other parse of the same bytes (interfile's extraction vs dispatch ASTs). *)
val collect_stamps :
  AST_generic.program list -> (AST_generic.SId.t * AST_generic.expr) list

(* Every [Sym] svalue in the AST, keyed by the carrying id's sid: mirrors
   projidx-published svalues onto fresh parses of the same bytes. *)
val collect_sym_stamps :
  AST_generic.program -> (AST_generic.SId.t * AST_generic.expr) list

(* Returns the number of identifiers stamped: a file with a non-zero count
   may now match formulas its raw text cannot, so content-based prefilters
   must not skip it. *)
val apply_stamps :
  (AST_generic.SId.t * AST_generic.expr) list -> AST_generic.program -> int

(* [stamp_program ast] = same-file collection and application in one step. *)
val stamp_program : AST_generic.program -> unit
