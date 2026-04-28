(* Engine-emitted guards attached to taint effects.
 *
 * A guard represents a boolean condition that must hold at the caller
 * for an effect to apply. An effect carries a [Set.t] of guards,
 * interpreted as a conjunction. At signature instantiation, each guard's
 * [cond] is evaluated against the caller's actual arguments, yielding one
 * of three outcomes:
 *
 *   - [G.Lit (G.Bool true)]  : definitively satisfied.
 *   - [G.Lit (G.Bool false)] : definitively violated.
 *   - any other [G.svalue]    : undecided at this call site.
 *
 * The effect is dropped iff at least one guard evaluates to
 * [G.Lit (G.Bool false)]. Guards that evaluate to [G.Lit (G.Bool true)]
 * are removed from the surviving guard set; undecided guards are kept.
 *
 * Representation. [cond] is the branch condition as an [IL.exp].
 * [param_refs] maps each free [Fetch] in [cond] whose base is a formal
 * parameter to the signature-param index of that parameter. The
 * mapping is built with [IL.equal_name] (which compares ident, sid, and
 * id_info). The evaluator uses the same [IL.equal_name] to look up. *)

type t = {
  cond : IL.exp;
  param_refs : (IL.name * int) list;
}

(* [Effect_guard.Set] is a [Stdlib.Set] and requires a total order.
 *
 * We compare [cond] via [Display_IL.string_of_exp]. [IL.exp] has no
 * [@@deriving ord], and [Stdlib.compare] on [IL.exp] is unsafe because
 * [id_info.id_svalue] is a mutable [ref] with potential cycles.
 * [Display_IL.string_of_exp] includes [sid] in the output of [str_of_name],
 * so two conds that differ only by the sid of a [Fetch]'s [IL.name]
 * compare as distinct. *)
let compare_cond e1 e2 =
  String.compare (Display_IL.string_of_exp e1) (Display_IL.string_of_exp e2)

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
let show g = Display_IL.string_of_exp g.cond

module Set = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

let show_set gs =
  if Set.is_empty gs then ""
  else
    "["
    ^ String.concat " && " (gs |> Set.elements |> List.map show)
    ^ "]"
