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

module G = AST_generic

(* Hash-consed guard cond. [node] is the canonical [IL.exp]: structurally-equal
 * conds interned in the same epoch share it physically, and its child [exp]s
 * are themselves canonical. [hash] is the full (depth-distinguishing)
 * structural hash, combined at intern time from the children's stored [hash] —
 * unlike [Stdlib.Hashtbl.hash], whose bounded-prefix traversal collides for
 * canonical nodes that share their top levels and differ only deeply, exactly
 * the shape cross-call substitution produces; storing the hash in the wrapper
 * makes retrieving a child's hash one field read instead of a side-table
 * lookup that degrades with those collisions. *)
type hcond = {
  node : IL.exp;
  hash : int;
}

type t = {
  cond : hcond;
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
(* One-level structural hash: combines the node's own constructor (and leaf
 * data) with the children's stored hashes. [node]'s children are the
 * already-interned [kid_hashes]' nodes, in [fold_map_children] order
 * ([Entry] contributes key then value). *)
let node_hash (node : IL.exp) (kid_hashes : int list) : int =
  match node.e with
  | IL.Literal _
  | IL.Fetch _ ->
      IL_helpers.hash_exp node
  | IL.Operator ((op, _), _) ->
      Stdlib.Hashtbl.hash (1, AST_generic.hash_operator op, kid_hashes)
  | IL.Cast (_, _) -> Stdlib.Hashtbl.hash (2, kid_hashes)
  | IL.Composite (k, _) ->
      Stdlib.Hashtbl.hash (3, IL_helpers.composite_kind_tag k, kid_hashes)
  | IL.RecordOrDict fields ->
      let field_keys =
        List.map
          (fun (f : IL.field_or_entry) ->
            match f with
            | IL.Field (n, _) -> (0, IL.str_of_name n)
            | IL.Entry _ -> (1, "")
            | IL.Spread _ -> (2, ""))
          fields
      in
      Stdlib.Hashtbl.hash (4, field_keys, kid_hashes)
  | IL.FixmeExp (_, _, Some _) -> Stdlib.Hashtbl.hash (5, kid_hashes)
  | IL.FixmeExp (_, _, None) -> 6

(* The table key's [hash] is the precomputed [hcond.hash] (one field read).
 * [equal] compares the underlying nodes structurally: both sides' children
 * are canonical, so [equal_exp] short-circuits on [phys_equal] one level
 * down and is linear in the immediate children, except on (rare, full-hash)
 * collisions. *)
module ICondTbl = Hashtbl.Make (struct
  type t = hcond

  let equal (a : hcond) (b : hcond) : bool =
    IL_helpers.equal_exp a.node b.node

  let hash (h : hcond) : int = h.hash
end)

let intern_table : hcond ICondTbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> ICondTbl.create 1024)

let reset_intern () : unit = ICondTbl.clear (Domain.DLS.get intern_table)

(* Canonicalise [cond] bottom-up: children are interned first, so a node's table
 * lookup compares already-shared children ([equal_exp] short-circuits on
 * [phys_equal]) and hashes from their stored hashes — both linear only in the
 * node's immediate children. The per-call [memo] keeps the walk linear in the
 * cond's distinct nodes (the cond is itself a shared DAG from [Sig_inst]).
 * Also returns the number of distinct canonical nodes in the result ([seen]
 * collects them across duplicate inputs), for the size cap in
 * [intern_cond]. *)
let intern_counted (cond : IL.exp) : hcond * int =
  let tbl = Domain.DLS.get intern_table in
  let memo : hcond IL_helpers.PhysExpTbl.t =
    IL_helpers.PhysExpTbl.create 64
  in
  let seen : unit IL_helpers.PhysExpTbl.t =
    IL_helpers.PhysExpTbl.create 64
  in
  let rec go (e : IL.exp) : hcond =
    match IL_helpers.PhysExpTbl.find_opt memo e with
    | Some h -> h
    | None ->
        let rev_kids, node =
          IL_helpers.fold_map_children
            (fun acc c ->
              let k = go c in
              (k :: acc, k.node))
            [] e
        in
        let hash = node_hash node (List.rev_map (fun k -> k.hash) rev_kids) in
        let cand = { node; hash } in
        let canon =
          match ICondTbl.find_opt tbl cand with
          | Some c -> c
          | None ->
              ICondTbl.replace tbl cand cand;
              cand
        in
        if not (IL_helpers.PhysExpTbl.mem seen canon.node) then
          IL_helpers.PhysExpTbl.add seen canon.node ();
        IL_helpers.PhysExpTbl.add memo e canon;
        canon
  in
  let h = go cond in
  (h, IL_helpers.PhysExpTbl.length seen)

(* An atom of shape [length(e) <cmp> int-literal] (either operand order),
 * possibly under a single [Not]. This is the shape Clojure multi-arity
 * dispatch lowers to ([AST_to_IL.pm_len_cond] emits [Eq] and [GtE]) and the
 * arity guards of [Builtin_models]; [len(x) == n] guards in other languages
 * match too. *)
let is_length_atom (e : IL.exp) : bool =
  let is_cmp_of_length_and_int (e : IL.exp) : bool =
    match e.e with
    | IL.Operator
        (((G.Eq | G.NotEq | G.Lt | G.LtE | G.Gt | G.GtE), _), [ a1; a2 ]) ->
        let is_len (a : IL.exp IL.argument) : bool =
          match (IL_helpers.exp_of_arg a).e with
          | IL.Operator ((G.Length, _), [ _ ]) -> true
          | _ -> false
        in
        let is_int (a : IL.exp IL.argument) : bool =
          match (IL_helpers.exp_of_arg a).e with
          | IL.Literal (G.Int _) -> true
          | _ -> false
        in
        (is_len a1 && is_int a2) || (is_int a1 && is_len a2)
    | _ -> false
  in
  match e.e with
  | IL.Operator ((G.Not, _), [ IL.Unnamed inner ]) ->
      is_cmp_of_length_and_int inner
  | _ -> is_cmp_of_length_and_int e

(* A cond that is nothing but length atoms under [And]/[Or] — the dispatch
 * class: Clojure multi-arity legs and HOF arity guards. [Sig_inst] evaluates
 * these eagerly at instantiation (dispatch must prune wrong-arity effects
 * before their taints enter the caller's state); every other guard is
 * carried unevaluated and decided once, when the effect becomes a match.
 * The walk bails at the first non-atom leaf; And/Or spines are built fresh
 * by the smart constructors, so the spine is a tree and the walk is linear
 * in it. *)
let rec is_length_skeleton (e : IL.exp) : bool =
  is_length_atom e
  ||
  match e.e with
  | IL.Operator (((G.And | G.Or), _), [ IL.Unnamed a; IL.Unnamed b ]) ->
      is_length_skeleton a && is_length_skeleton b
  | _ -> false

(* The And/Or skeleton of [cond] over its length atoms: length atoms are
 * kept, every other leaf — including any non-atom [Not] subtree, since
 * replacing under a negation would strengthen — becomes [true], and the
 * [wrap_and]/[wrap_or] smart constructors fold the [true]s away. The result
 * is implied by [cond] (subformulas in positive positions are only
 * weakened). Keeping the skeleton rather than a conjunction of atoms
 * preserves disjunctive arity dispatch: a guard fused across multi-arity
 * legs, [or(and(len==1, P), and(len==2, Q))], widens to
 * [or(len==1, len==2)], which a wrong-arity call still refutes. Memoised on
 * physical identity so the walk is linear in the cond's distinct nodes. *)
let widen_to_length_skeleton (cond : IL.exp) : IL.exp =
  let memo : IL.exp IL_helpers.PhysExpTbl.t =
    IL_helpers.PhysExpTbl.create 16
  in
  let rec go (e : IL.exp) : IL.exp =
    match IL_helpers.PhysExpTbl.find_opt memo e with
    | Some w -> w
    | None ->
        let w =
          if is_length_atom e then e
          else
            match e.e with
            | IL.Operator ((G.And, _), [ IL.Unnamed a; IL.Unnamed b ]) ->
                IL_helpers.wrap_and [ go a; go b ]
            | IL.Operator ((G.Or, _), [ IL.Unnamed a; IL.Unnamed b ]) ->
                IL_helpers.wrap_or [ go a; go b ]
            | _ -> IL_helpers.lit_bool ~eorig:e.eorig true
        in
        IL_helpers.PhysExpTbl.add memo e w;
        w
  in
  go cond

(* Cap-and-widen. A cond with more than
 * [Limits_semgrep.taint_MAX_GUARD_COND_NODES] distinct nodes is replaced by
 * its length-atom skeleton — [true] when it has no length atoms, or if even
 * the skeleton exceeds the cap. The widened cond is implied by the original,
 * so the guarded effect applies at least as often: widening can add findings,
 * never drop them. Length atoms are kept because they are what Clojure
 * multi-arity dispatch compiles to; dropping one would apply a wrong-arity
 * leg's effects unconditionally. *)
let intern_cond (cond : IL.exp) : hcond =
  let h, distinct_nodes = intern_counted cond in
  if distinct_nodes <= Limits_semgrep.taint_MAX_GUARD_COND_NODES then h
  else
    let hw, widened_nodes =
      intern_counted (widen_to_length_skeleton h.node)
    in
    if widened_nodes <= Limits_semgrep.taint_MAX_GUARD_COND_NODES then hw
    else fst (intern_counted (IL_helpers.lit_bool ~eorig:IL.NoOrig true))

(* Compare conds structurally via [IL_helpers.compare_exp]. [IL.exp] has no
 * [@@deriving ord] and [Stdlib.compare] on it is unsafe ([id_info.id_svalue]
 * is a mutable [ref] with potential cycles), so this is a hand-written order.
 * It short-circuits on physical identity, unlike [IL_pp.pp_exp], which renders
 * the whole tree and so is exponential on the shared-DAG conds from
 * [Sig_inst]. It is token-insensitive and orders names by [str_of_name]
 * (ident and sid), so two same-named variables with distinct sids stay
 * distinct, matching the previous [pp_exp] order up to token position. *)
let compare_cond (h1 : hcond) (h2 : hcond) : int =
  IL_helpers.compare_exp h1.node h2.node

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
    IL_pp.pp_exp_bounded ~max:Limits_semgrep.taint_MAX_GUARD_LOG_CHARS
      g.cond.node
  else IL_pp.pp_exp g.cond.node

(* Identity for [And]: a [cond] that is the literal [true], with no
 * param refs. [is_top g] is the "no constraint" predicate; the
 * smart-constructor short-circuits use it to absorb identity guards.
 * Built outside the intern table: it exists before any epoch and never
 * enters a table, and [is_top]/[compare_cond] only read its [node]. *)
let top : t =
  let node = IL_helpers.lit_bool ~eorig:IL.NoOrig true in
  { cond = { node; hash = IL_helpers.hash_exp node }; param_refs = [] }

let is_top (g : t) : bool = IL_helpers.is_lit_bool true g.cond.node
let is_bot (g : t) : bool = IL_helpers.is_lit_bool false g.cond.node

(* Variables (base [Var] names) read by the guard's condition. A guard
 * becomes unreliable once one of these is reassigned: the IL is non-SSA,
 * so the recorded [cond] then refers to a value that may differ from the
 * one tested at the branch where the guard was established. The engine
 * drops such guards when stamping effects (see [Taint_lval_env]). *)
let cond_vars (g : t) : IL.name list =
  IL_helpers.lvals_of_exp g.cond.node
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
      cond = intern_cond (IL_helpers.wrap_and [ g1.cond.node; g2.cond.node ]);
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Disjoin two guards. A guard with [cond = false] is the [Or]
 * identity. *)
let compose_or (g1 : t) (g2 : t) : t =
  if is_bot g1 then g2
  else if is_bot g2 then g1
  else
    {
      cond = intern_cond (IL_helpers.wrap_or [ g1.cond.node; g2.cond.node ]);
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Fold a list of guards into a single conjunction. Empty list yields
 * [top]. *)
let conjoin (gs : t list) : t = List.fold_left compose_and top gs
