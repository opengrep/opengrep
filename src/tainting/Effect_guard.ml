(* Engine-emitted guard attached to taint effects.
 *
 * A guard represents a boolean condition that must hold at the caller
 * for an effect to apply. Each effect carries one [Effect_guard.t]; its
 * [cond] is built incrementally via [IL_helpers.wrap_and],
 * [IL_helpers.wrap_or] and [IL_helpers.wrap_not] from atomic branch
 * conditions accumulated as the dataflow walks the CFG and from
 * disjunctive provenance fused at joins. At signature instantiation,
 * [Sig_inst.classify_guards] substitutes the caller's actual arguments
 * into [cond] and partial-evaluates via [Eval_il_partial.eval]:
 *
 *   - [G.Lit (G.Bool true)]  : definitively satisfied.
 *   - [G.Lit (G.Bool false)] : definitively violated.
 *   - any other [G.svalue]   : undecided at this call site.
 *
 * The effect is dropped iff [cond] folds to definitively-false. A
 * guard whose [cond] folds to definitively-true is replaced by [top]
 * and contributes no constraint; undecided conds are kept (possibly
 * rebound into the outer frame's parameters).
 *
 * Representation. [cond] is the boolean condition as a single [IL.exp].
 * [param_refs] maps each free [Fetch] in [cond] whose base is a formal
 * parameter to the signature-param index of that parameter. The
 * mapping is built with [IL.equal_name] (which compares ident, sid,
 * and id_info). The evaluator uses the same [IL.equal_name] to look
 * up. *)

type t = {
  cond : IL.exp;
  param_refs : (IL.name * int) list;
}

(* Compare conds via [IL_pp.pp_exp]. [IL.exp] has no [@@deriving ord],
 * and [Stdlib.compare] on [IL.exp] is unsafe because
 * [id_info.id_svalue] is a mutable [ref] with potential cycles.
 * [IL_pp.pp_exp] renders unary operators (e.g. [Not]) recursively into
 * the inner expression, so [Not(Eq 1)] and [Not(Eq 2)] compare
 * distinct; [Display_IL.string_of_exp] elides children of non-binary
 * operators ("<OP Not ...>"), causing distinct guards to collide.
 * [pp_name] includes the [sid], so conds differing only by a
 * [Fetch]'s [IL.name] sid compare as distinct. *)
let compare_cond e1 e2 =
  String.compare (IL_pp.pp_exp e1) (IL_pp.pp_exp e2)

let compare_param_ref (n1, i1) (n2, i2) =
  let c = Int.compare i1 i2 in
  if c <> 0 then c
  else String.compare (IL.str_of_name n1) (IL.str_of_name n2)

let compare g1 g2 =
  let c = compare_cond g1.cond g2.cond in
  if c <> 0 then c
  else
    List.compare compare_param_ref
      (List.sort compare_param_ref g1.param_refs)
      (List.sort compare_param_ref g2.param_refs)

let equal g1 g2 = compare g1 g2 = 0
(* Use [IL_pp.pp_exp] for the same reason [compare_cond] does: it
 * renders unary children (e.g. [Not]) in full, whereas
 * [Display_IL.string_of_exp] elides them as "<OP Not ...>". *)
let show g = IL_pp.pp_exp g.cond

(* Identity for [And]: a [cond] that is the literal [true], with no
 * param refs. [is_top g] is the "no constraint" predicate; the
 * smart-constructor short-circuits use it to absorb identity guards. *)
let top : t =
  { cond = IL_helpers.lit_bool ~eorig:IL.NoOrig true; param_refs = [] }

let is_top (g : t) : bool = IL_helpers.is_lit_bool true g.cond
let is_bot (g : t) : bool = IL_helpers.is_lit_bool false g.cond

(* Bracketed rendering for signature dumps: empty when [is_top],
 * [<cond>] otherwise. *)
let show_in_brackets (g : t) : string =
  if is_top g then "" else "[" ^ show g ^ "]"

(* [Set] of atomic guards. Used by [Lval_env.active_guards] to track
 * which atoms are live at the current program point. The
 * per-program-point intersection at joins (cf. [Set.inter]) is the
 * single operation that justifies a set-of-atoms shape here. Effects
 * stamped at emission compress this to a single [t] via
 * [conjoin_atoms]. *)
module Set = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

let show_set gs =
  if Set.is_empty gs then ""
  else
    "[" ^ String.concat " && " (gs |> Set.elements |> List.map show) ^ "]"

let merge_param_refs (rs1 : (IL.name * int) list)
    (rs2 : (IL.name * int) list) : (IL.name * int) list =
  let module S = Stdlib.Set.Make (struct
    type t = IL.name * int

    let compare = compare_param_ref
  end) in
  S.elements (S.union (S.of_list rs1) (S.of_list rs2))

(* Conjoin two guards. [top] is absorbed. *)
let compose_and (g1 : t) (g2 : t) : t =
  if is_top g1 then g2
  else if is_top g2 then g1
  else
    {
      cond = IL_helpers.wrap_and [ g1.cond; g2.cond ];
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Disjoin two guards. A guard with [cond = false] is the [Or]
 * identity. *)
let compose_or (g1 : t) (g2 : t) : t =
  if is_bot g1 then g2
  else if is_bot g2 then g1
  else
    {
      cond = IL_helpers.wrap_or [ g1.cond; g2.cond ];
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Fold a list of guards into a single conjunction. Empty list yields
 * [top]. *)
let conjoin (gs : t list) : t = List.fold_left compose_and top gs
