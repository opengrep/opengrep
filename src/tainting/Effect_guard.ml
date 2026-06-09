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
 * rebound into the enclosing function's parameters).
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

(* Hash-consing of guard conds. Cross-call substitution ([Sig_inst]) and the
 * dataflow fixpoint rebuild structurally-equal conds as separate physical
 * trees; interning them to one canonical node makes [compare_cond] short-
 * circuit on [phys_equal] and lets the fixpoint stabilise without re-walking
 * the shared DAG. The table is domain-local (one per domain, no cross-domain
 * races) and strong: it must be cleared at each task boundary with
 * [reset_intern], because rules run on a domain in sequence (see
 * [Match_tainting_mode]) and a stale canonical from a previous rule's conds
 * would persist otherwise. *)
(* Per-node structural hash cached by physical identity. A parent's hash
 * combines its children's cached hashes, so it is full (depth-distinguishing)
 * and costs one cache lookup per immediate child — unlike the bounded
 * [IL_helpers.hash_exp], which collides for the deep self-similar conds and
 * would make interning quadratic in the bucket. Children are interned (and
 * cached) before their parent, so [child_h] always hits. *)
let node_hash_table : int IL_helpers.PhysExpTbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> IL_helpers.PhysExpTbl.create 1024)

let hashcons_hash (e : IL.exp) : int =
  let nh = Domain.DLS.get node_hash_table in
  let child_h (c : IL.exp) : int =
    match IL_helpers.PhysExpTbl.find_opt nh c with
    | Some h -> h
    | None -> IL_helpers.hash_exp c
  in
  match e.e with
  | IL.Literal _
  | IL.Fetch _ ->
      IL_helpers.hash_exp e
  | IL.Operator (_, args) ->
      Stdlib.Hashtbl.hash
        (1, List.map (fun a -> child_h (IL_helpers.exp_of_arg a)) args)
  | IL.Cast (_, a) -> Stdlib.Hashtbl.hash (2, child_h a)
  | IL.Composite (_, (_, xs, _)) -> Stdlib.Hashtbl.hash (3, List.map child_h xs)
  | IL.RecordOrDict _ -> 4
  | IL.FixmeExp (_, _, Some a) -> Stdlib.Hashtbl.hash (5, child_h a)
  | IL.FixmeExp (_, _, None) -> 6

module ICondTbl = Hashtbl.Make (struct
  type t = IL.exp

  let equal = IL_helpers.equal_exp
  let hash = hashcons_hash
end)

let intern_table : IL.exp ICondTbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> ICondTbl.create 1024)

let reset_intern () : unit =
  ICondTbl.clear (Domain.DLS.get intern_table);
  IL_helpers.PhysExpTbl.clear (Domain.DLS.get node_hash_table)

(* Canonicalise [cond] bottom-up: children are interned first, so a node's table
 * lookup compares already-shared children ([equal_exp] short-circuits on
 * [phys_equal]) and hashes from their cached hashes — both linear only in the
 * node's immediate children. The per-call [memo] keeps the walk linear in the
 * cond's distinct nodes (the cond is itself a shared DAG from [Sig_inst]). *)
let intern_cond (cond : IL.exp) : IL.exp =
  let tbl = Domain.DLS.get intern_table in
  let nh = Domain.DLS.get node_hash_table in
  let memo = IL_helpers.PhysExpTbl.create 64 in
  let rec go (e : IL.exp) : IL.exp =
    match IL_helpers.PhysExpTbl.find_opt memo e with
    | Some c -> c
    | None ->
        let e' = IL_helpers.rebuild_children go e in
        let canon =
          match ICondTbl.find_opt tbl e' with
          | Some c -> c
          | None ->
              ICondTbl.replace tbl e' e';
              IL_helpers.PhysExpTbl.add nh e' (hashcons_hash e');
              e'
        in
        IL_helpers.PhysExpTbl.add memo e canon;
        canon
  in
  go cond

(* Compare conds structurally via [IL_helpers.compare_exp]. [IL.exp] has no
 * [@@deriving ord] and [Stdlib.compare] on it is unsafe ([id_info.id_svalue]
 * is a mutable [ref] with potential cycles), so this is a hand-written order.
 * It short-circuits on physical identity, unlike [IL_pp.pp_exp], which renders
 * the whole tree and so is exponential on the shared-DAG conds from
 * [Sig_inst]. It is token-insensitive and orders names by [str_of_name]
 * (ident and sid), so two same-named variables with distinct sids stay
 * distinct, matching the previous [pp_exp] order up to token position. *)
let compare_cond e1 e2 = IL_helpers.compare_exp e1 e2

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
(* Render the cond. [~truncate_guards] (default [true]) renders at most
 * [Limits_semgrep.taint_MAX_GUARD_LOG_CHARS] characters (first that many, then
 * "..."), so debug logging of the shared-DAG guards from [Sig_inst] stays
 * bounded instead of pp-ing 2^depth characters; the signature dump passes
 * [~truncate_guards:false] for full output. [IL_pp] renders unary children (e.g.
 * [Not]) in full, unlike [Display_IL.string_of_exp]. *)
let show ?(truncate_guards = true) g =
  if truncate_guards then
    IL_pp.pp_exp_bounded ~max:Limits_semgrep.taint_MAX_GUARD_LOG_CHARS g.cond
  else IL_pp.pp_exp g.cond

(* Identity for [And]: a [cond] that is the literal [true], with no
 * param refs. [is_top g] is the "no constraint" predicate; the
 * smart-constructor short-circuits use it to absorb identity guards. *)
let top : t =
  { cond = IL_helpers.lit_bool ~eorig:IL.NoOrig true; param_refs = [] }

let is_top (g : t) : bool = IL_helpers.is_lit_bool true g.cond
let is_bot (g : t) : bool = IL_helpers.is_lit_bool false g.cond

(* Variables (base [Var] names) read by the guard's condition. A guard
 * becomes unreliable once one of these is reassigned: the IL is non-SSA,
 * so the recorded [cond] then refers to a value that may differ from the
 * one tested at the branch where the guard was established. The engine
 * drops such guards when stamping effects (see [Taint_lval_env]). *)
let cond_vars (g : t) : IL.name list =
  IL_helpers.lvals_of_exp g.cond
  |> List.filter_map (fun (lv : IL.lval) ->
         match lv.IL.base with
         | IL.Var name -> Some name
         | IL.VarSpecial _ | IL.Mem _ -> None)

(* Bracketed rendering for signature dumps: empty when [is_top],
 * [<cond>] otherwise. *)
let show_in_brackets ?(truncate_guards = true) (g : t) : string =
  if is_top g then "" else "[" ^ show ~truncate_guards g ^ "]"

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

let show_set ?(truncate_guards = true) gs =
  if Set.is_empty gs then ""
  else
    "["
    ^ String.concat " && " (gs |> Set.elements |> List.map (show ~truncate_guards))
    ^ "]"

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
      cond = intern_cond (IL_helpers.wrap_and [ g1.cond; g2.cond ]);
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Disjoin two guards. A guard with [cond = false] is the [Or]
 * identity. *)
let compose_or (g1 : t) (g2 : t) : t =
  if is_bot g1 then g2
  else if is_bot g2 then g1
  else
    {
      cond = intern_cond (IL_helpers.wrap_or [ g1.cond; g2.cond ]);
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Fold a list of guards into a single conjunction. Empty list yields
 * [top]. *)
let conjoin (gs : t list) : t = List.fold_left compose_and top gs
