(* Engine-emitted guard attached to taint effects.
 *
 * A guard represents a boolean condition that must hold at the caller
 * for an effect to apply. Each effect carries one [Effect_guard.t]. At
 * signature instantiation, [Sig_inst.classify_guards] substitutes the
 * caller's actual arguments into the cond's atoms; dispatch (length)
 * atoms are partially evaluated there, every other atom is carried and
 * decided when the effect becomes a match
 * ([Match_tainting_mode.pms_of_effect]):
 *
 *   - definitively true:  the guard contributes no constraint;
 *   - definitively false: the effect (or sink item) is dropped;
 *   - undecided:          kept, conservatively reported.
 *
 * Representation. [cond] is in disjunctive normal form: a list of
 * clauses (disjunction), each clause a list of literals (conjunction),
 * each literal an interned atomic [IL.exp] with a polarity. [[]] is
 * false (no satisfiable clause); [[ [] ]] is true (one empty clause).
 * Atoms are the only variable-bearing parts: substitution rewrites
 * atoms and never the clause structure, so DNF is preserved across
 * rebinding and normalisation happens only at construction.
 *
 * Normalisation invariants, maintained by the smart constructors:
 *   - literals in a clause are sorted and deduped, and the clause is
 *     consistent (no [A] with [!A]; no [x == c1] with [x == c2] for
 *     distinct same-type constants) — inconsistent clauses are dropped;
 *   - clauses are sorted and deduped; a true clause ([]) collapses the
 *     cond to [top]; complementary singleton clauses ([A] and [!A])
 *     collapse the cond to [top];
 *   - an atom larger than [Limits_semgrep.taint_MAX_GUARD_COND_NODES]
 *     distinct nodes is dropped from its clause (weakening: sound);
 *   - more than [Limits_semgrep.taint_MAX_GUARD_CLAUSES] clauses widen
 *     each clause to its length literals (arity dispatch survives),
 *     then to [top].
 *
 * [param_refs] maps each free [Fetch] in the cond's atoms whose base is
 * a formal parameter to the signature-param index of that parameter.
 * The mapping is built with [IL.equal_name] (which compares ident, sid,
 * and id_info). The substitution uses the same [IL.equal_name] to look
 * up.
 *
 * Guard-blindness inventory. Equalities and comparisons over guarded
 * values fall in three classes; new code must pick deliberately:
 *
 *   - Guard-blind by design — identity and fusion keying: a set holds one
 *     element per guard-less identity and fuses guards on insertion.
 *     [Shape_and_sig.Effect.compare] (and [Effects]'s element order),
 *     [Taint.compare_taint] (the key of [Taint_set]'s map),
 *     [Shape_and_sig.Effect.compare_arg] inside effect identity.
 *
 *   - Guard-blind, accepted — [Shape_and_sig.SignatureSet]'s compare:
 *     storage dedup only; each function and arity is extracted once, in
 *     topological order, so a guard-only re-extraction never collides.
 *
 *   - Guard-aware — every insertion no-op check and every fixpoint
 *     stability test, or a fused wider guard is silently discarded for
 *     the narrower one (lost findings): [Effect.guards_equal],
 *     [Effects.equal_with_guards], [Taint_set.equal_with_guards],
 *     [Xtaint.equal_with_guards], [Shape.equal_cell_with_guards],
 *     [Signature.equal_with_guards], and [Taint_lval_env.equal] (the
 *     dataflow fixpoint's [eq_env]). A new stability test must use this
 *     family. *)

module G = AST_generic

(*****************************************************************************)
(* Atoms *)
(*****************************************************************************)

type atom_facts = {
  fetch_bases : (IL.name * bool) list;
  value : bool option;
  always_frozen : bool;
  freezing_names : IL.name list;
}

(* Hash-consed atom. [node] is the canonical [IL.exp]: structurally-equal
 * atoms interned in the same epoch share it physically, and its child [exp]s
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
  id : int;
  mutable facts : atom_facts option;
}

type literal = {
  atom : hcond;
  negated : bool;
}

type clause = literal list
type cond = clause list

type t = {
  cond : cond;
  param_refs : (IL.name * int) list;
}

(* Hash-consing of atoms. Cross-call substitution ([Sig_inst]) and the
 * dataflow fixpoint rebuild structurally-equal atoms as separate physical
 * trees; interning them to one canonical node makes comparison short-circuit
 * on [phys_equal] and lets the fixpoint stabilise without re-walking the
 * shared DAG. The table is domain-local (one per domain, no cross-domain
 * races) and strong: it must be cleared at each task boundary with
 * [reset_intern], because rules run on a domain in sequence (see
 * [Match_tainting_mode]) and a stale canonical from a previous rule's atoms
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

type atoms = {
  table : hcond ICondTbl.t;
  mutable next_id : int;
}

(* The table grows monotonically until the per-target [reset_intern] in
 * [Match_tainting_mode.check_rules]: the atom-size cap bounds each
 * entry, not the entry count, which is bounded only by the distinct
 * atoms the target's analysis creates. Accepted: atoms are small and
 * a target's atom population is proportional to its conds. *)
let create_atoms () : atoms = { table = ICondTbl.create 1024; next_id = 0 }

(* Canonicalise [e] bottom-up: children are interned first, so a node's table
 * lookup compares already-shared children ([equal_exp] short-circuits on
 * [phys_equal]) and hashes from their stored hashes — both linear only in the
 * node's immediate children. The per-call [memo] keeps the walk linear in the
 * atom's distinct nodes (the atom is itself a shared DAG from [Sig_inst]).
 * Also returns the number of distinct canonical nodes in the result ([seen]
 * collects them across duplicate inputs), for the atom-size cap. *)
let intern_counted (atoms : atoms) (e : IL.exp) : hcond * int =
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
        let cand = { node; hash; id = -1; facts = None } in
        let canon =
          match ICondTbl.find_opt atoms.table cand with
          | Some c -> c
          | None ->
              let c = { cand with id = atoms.next_id } in
              atoms.next_id <- atoms.next_id + 1;
              ICondTbl.replace atoms.table c c;
              c
        in
        if not (IL_helpers.PhysExpTbl.mem seen canon.node) then
          IL_helpers.PhysExpTbl.add seen canon.node ();
        IL_helpers.PhysExpTbl.add memo e canon;
        canon
  in
  let h = go e in
  (h, IL_helpers.PhysExpTbl.length seen)

let fetch_bases_of (e : IL.exp) : (IL.name * bool) list =
  let visited : unit IL_helpers.PhysExpTbl.t =
    IL_helpers.PhysExpTbl.create 16
  in
  let rec go (acc : (IL.name * bool) list) (e : IL.exp) :
      (IL.name * bool) list =
    if IL_helpers.PhysExpTbl.mem visited e then acc
    else (
      IL_helpers.PhysExpTbl.add visited e ();
      match e.e with
      | IL.Fetch { base = IL.Var name; rev_offset } ->
          let resolvable =
            List.for_all IL_helpers.offset_is_resolvable rev_offset
          in
          if
            List.exists
              (fun (n, r) -> IL.equal_name n name && Bool.equal r resolvable)
              acc
          then acc
          else (name, resolvable) :: acc
      | IL.Fetch _
      | IL.Literal _
      | IL.FixmeExp (_, _, None) ->
          acc
      | IL.Operator (_, args) ->
          List.fold_left (fun acc a -> go acc (IL_helpers.exp_of_arg a)) acc args
      | IL.Cast (_, sub)
      | IL.FixmeExp (_, _, Some sub) ->
          go acc sub
      | IL.Composite (_, (_, exps, _)) -> List.fold_left go acc exps
      | IL.RecordOrDict fields ->
          List.fold_left
            (fun acc (f : IL.field_or_entry) ->
              match f with
              | IL.Field (_, v)
              | IL.Spread v ->
                  go acc v
              | IL.Entry (k, v) -> go (go acc k) v)
            acc fields)
  in
  go [] e

(* A literal freezes when it crosses a call boundary if its atom can
 * neither be substituted further up nor folded at match time. An atom is
 * decidable through exactly two mechanisms: substitution grounds a Fetch
 * anchored in the enclosing function's params (resolvable offsets
 * included — a caller may pass a literal struct), and the final
 * evaluation folds literals and offset-free local variables via their
 * [id_svalue]. A Fetch with offsets on a non-param base, or a
 * [Mem]/[VarSpecial] base, is [NotCst] forever once its frame is gone —
 * e.g. Go's [_tmp != 0] temporaries and local field reads, which
 * dominate carried guards on real code and are pure carry/compare cost.
 * Dropping the literal weakens its clause (the guarded effect applies at
 * least as often), the same sound direction as the caps. Length atoms
 * are exempt (dispatch). *)
(* The walk is memoised on physical identity: substituted atoms are shared
 * DAGs whose tree unfolding is exponential ([lvals_of_exp] is a plain tree
 * walk and must not be used here). A [Fetch] freezes the atom when it is
 * an offsetted read that does not anchor in [params]; offset [Index]
 * expressions and [Mem] bases are descended into. *)
let frozen_facts_of (atom : IL.exp) : bool * IL.name list =
  let visited : unit IL_helpers.PhysExpTbl.t =
    IL_helpers.PhysExpTbl.create 16
  in
  let rec go ((always, _) as acc : bool * IL.name list) (e : IL.exp) :
      bool * IL.name list =
    if always || IL_helpers.PhysExpTbl.mem visited e then acc
    else (
      IL_helpers.PhysExpTbl.add visited e ();
      match e.e with
      | IL.Fetch lv -> go_lval acc lv
      | _ ->
          fst (IL_helpers.fold_map_children (fun acc c -> (go acc c, c)) acc e))
  and go_lval (acc : bool * IL.name list) (lv : IL.lval) : bool * IL.name list =
    let always, names =
      List.fold_left
        (fun acc (o : IL.offset) ->
          match o.IL.o with
          | IL.Index e -> go acc e
          | IL.Dot _
          | IL.Slice _ ->
              acc)
        acc lv.IL.rev_offset
    in
    if always then (always, names)
    else
      match lv.IL.base with
      | IL.Var name -> (
          match lv.IL.rev_offset with
          | [] -> (false, names)
          | offsets ->
              if not (List.for_all IL_helpers.offset_is_resolvable offsets) then
                (true, names)
              else if List.exists (IL.equal_name name) names then (false, names)
              else (false, name :: names))
      | IL.VarSpecial _
      | IL.Mem _ ->
          (true, names)
  in
  go (false, []) atom

let eval_atom ~(lang : Lang.t) (atom : IL.exp) : bool option =
  let eval_env = Eval_il_partial.mk_env lang Dataflow_var_env.VarMap.empty in
  match Eval_il_partial.eval eval_env atom with
  | G.Lit (G.Bool (b, _)) -> Some b
  | _ -> None

let facts_of (h : hcond) : atom_facts =
  match h.facts with
  | Some facts -> facts
  | None -> invalid_arg "Effect_guard.facts_of: node never formed as an atom"

let as_atom ~(lang : Lang.t) (h : hcond) : hcond =
  (match h.facts with
  | Some _ -> ()
  | None ->
      let always_frozen, freezing_names = frozen_facts_of h.node in
      h.facts <-
        Some
          {
            fetch_bases = fetch_bases_of h.node;
            value = eval_atom ~lang h.node;
            always_frozen;
            freezing_names;
          });
  h

let reads_any (names : IL.name list) (h : hcond) : bool =
  (facts_of h).fetch_bases
  |> List.exists (fun (n, _) -> List.exists (IL.equal_name n) names)

(* An atom of shape [length(e) <cmp> int-literal] (either operand order),
 * possibly under a single [Not] (atoms built before negation moved into
 * [literal.negated] may still carry one). This is the shape Clojure
 * multi-arity dispatch lowers to ([AST_to_IL.pm_len_cond] emits [Eq] and
 * [GtE]) and the arity guards of [Builtin_models]; [len(x) == n] guards in
 * other languages match too. *)
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

(*****************************************************************************)
(* Literals and clauses *)
(*****************************************************************************)

let compare_literal (l1 : literal) (l2 : literal) : int =
  let c = Int.compare l1.atom.id l2.atom.id in
  if c <> 0 then c else Bool.compare l1.negated l2.negated

let compare_clause (c1 : clause) (c2 : clause) : int =
  List.compare compare_literal c1 c2

let compare_cond (c1 : cond) (c2 : cond) : int =
  List.compare compare_clause c1 c2

(* [Some (e, lit)] when [atom] is [e == lit] with exactly one constant
 * operand; used by the clause-consistency check. *)
let eq_parts (atom : IL.exp) : (IL.exp * G.literal) option =
  match atom.e with
  | IL.Operator ((G.Eq, _), [ IL.Unnamed a; IL.Unnamed b ]) -> (
      match (a.e, b.e) with
      | IL.Literal _, IL.Literal _ -> None
      | IL.Literal la, _ -> Some (b, la)
      | _, IL.Literal lb -> Some (a, lb)
      | _ -> None)
  | _ -> None

(* Distinct constants of the same type cannot both equal the same value;
 * across types we make no judgement (e.g. [1 == 1.0] holds in Python).
 * String contents are compared as lexed, which under-determines the
 * runtime value when escapes are involved ('\n' vs a literal newline can
 * be the same string), so a backslash-bearing string yields no
 * judgement; escape-free contents denote their runtime value in every
 * supported language. *)
let distinct_same_type_constants (l1 : G.literal) (l2 : G.literal) : bool =
  match (l1, l2) with
  | G.Int (i1, _), G.Int (i2, _) -> not (Option.equal Int64.equal i1 i2)
  | G.String (_, (s1, _), _), G.String (_, (s2, _), _) ->
      (not (String.contains s1 '\\'))
      && (not (String.contains s2 '\\'))
      && not (String.equal s1 s2)
  | G.Bool (b1, _), G.Bool (b2, _) -> not (Bool.equal b1 b2)
  | _ -> false

let equalities_inconsistent (lits : (IL.exp * bool) list) : bool =
  let eqs =
    lits
    |> List.filter_map (fun (atom, negated) ->
           if negated then None else eq_parts atom)
  in
  let rec pairwise = function
    | (e1, v1) :: rest ->
        List.exists
          (fun (e2, v2) ->
            IL_helpers.equal_exp e1 e2 && distinct_same_type_constants v1 v2)
          rest
        || pairwise rest
    | [] -> false
  in
  pairwise eqs

(* A clause is unsatisfiable when it contains the same atom positive and
 * negated, or two positive equalities binding the same (canonical)
 * expression to distinct same-type constants — e.g. [x == 1 && x == 2],
 * or [length(v) == 1 && length(v) == 2] from cross-arity fusion. Atoms
 * are canonical, so the pairwise checks compare mostly by physical
 * identity; clauses are small. *)
let clause_inconsistent (c : clause) : bool =
  let complementary =
    (* Sorted by atom then polarity: a complementary pair is adjacent. *)
    let rec adjacent = function
      | l1 :: (l2 :: _ as rest) ->
          (Int.equal l1.atom.id l2.atom.id
          && not (Bool.equal l1.negated l2.negated))
          || adjacent rest
      | _ -> false
    in
    adjacent c
  in
  complementary
  || equalities_inconsistent
       (c |> List.map (fun l -> (l.atom.node, l.negated)))

let literals_consistent (lits : (IL.exp * bool) list) : bool =
  let rec complementary = function
    | (a1, n1) :: rest ->
        List.exists
          (fun (a2, n2) ->
            (not (Bool.equal n1 n2)) && IL_helpers.equal_exp a1 a2)
          rest
        || complementary rest
    | [] -> false
  in
  not (complementary lits || equalities_inconsistent lits)

let raw_clauses (c : cond) : (IL.exp * bool) list list =
  c |> List.map (List.map (fun l -> (l.atom.node, l.negated)))

(* Sort, dedup, and consistency-check a conjunction of literals.
 * [None] means the clause is unsatisfiable and must be dropped.
 * Constant boolean atoms decide their literal outright — substitution
 * ([map_atoms]) can ground an atom to a boolean literal: a satisfied
 * literal leaves the conjunction, a falsified one kills the clause, so a
 * dead clause is folded here instead of surviving normalisation (where
 * it would consume clause budget and bypass the [cond_is_bot] →
 * [Drop_effect] fast path until match time). *)
let mk_clause (lits : literal list) : clause option =
  let falsified (l : literal) : bool =
    Option.equal Bool.equal (facts_of l.atom).value (Some l.negated)
  in
  let satisfied (l : literal) : bool =
    Option.equal Bool.equal (facts_of l.atom).value (Some (not l.negated))
  in
  if List.exists falsified lits then None
  else
    let c =
      lits
      |> List.filter (fun l -> not (satisfied l))
      |> List.sort_uniq compare_literal
    in
    if clause_inconsistent c then None else Some c

(* Sort and dedup clauses; collapse to [top] when a clause is true ([])
 * or two singleton clauses are complementary ([A] or [!A] is a
 * tautology — this fold is what lets fan-in of complementary branch
 * guards reach a fixed point instead of growing). *)
let cond_true : cond = [ [] ]
let cond_false : cond = []
let cond_is_top (c : cond) : bool = List.exists List_.null c
let cond_is_bot (c : cond) : bool = List_.null c

let has_complementary_singletons (cs : cond) : bool =
  let rec adjacent = function
    | l1 :: (l2 :: _ as rest) ->
        (Int.equal l1.atom.id l2.atom.id
        && not (Bool.equal l1.negated l2.negated))
        || adjacent rest
    | _ -> false
  in
  adjacent (List.filter_map (function [ l ] -> Some l | _ -> None) cs)

let mk_cond (clauses : clause list) : cond =
  let cs = List.sort_uniq compare_clause clauses in
  if List.exists List_.null cs then cond_true
  else if has_complementary_singletons cs then cond_true
  else cs

(* Cap-and-widen on clause count. Each clause is widened to its length
 * literals (arity dispatch survives: [or(and(len==1, P), and(len==2, Q))]
 * widens to [or(len==1, len==2)], which a wrong-arity call still refutes);
 * a clause with no length literals becomes true, collapsing the cond to
 * [top]. The widened cond is implied by the original, so widening can add
 * findings, never drop them. *)
let cap_clauses (c : cond) : cond =
  if List.length c <= Limits_semgrep.taint_MAX_GUARD_CLAUSES then c
  else
    let widened =
      c
      |> List.map (fun clause ->
             clause |> List.filter (fun l -> is_length_atom l.atom.node))
      |> mk_cond
    in
    if List.length widened <= Limits_semgrep.taint_MAX_GUARD_CLAUSES then
      widened
    else cond_true

(*****************************************************************************)
(* Cond algebra *)
(*****************************************************************************)

let merge_clauses (c1 : cond) (c2 : cond) : cond =
  let rec go (acc : clause list) (a : cond) (b : cond) : cond =
    match (a, b) with
    | [], rest
    | rest, [] ->
        List.rev_append acc rest
    | x :: xs, y :: ys ->
        let k = compare_clause x y in
        if k = 0 then go (x :: acc) xs ys
        else if k < 0 then go (x :: acc) xs b
        else go (y :: acc) a ys
  in
  go [] c1 c2

let or_cond (c1 : cond) (c2 : cond) : cond =
  if cond_is_top c1 || cond_is_top c2 then cond_true
  else if c1 == c2 then c1
  else
    let cs = merge_clauses c1 c2 in
    if has_complementary_singletons cs then cond_true else cap_clauses cs

let and_cond (c1 : cond) (c2 : cond) : cond =
  if cond_is_bot c1 || cond_is_bot c2 then cond_false
  else if cond_is_top c1 then c2
  else if cond_is_top c2 then c1
  else
    (* Distribution: the only place clause count multiplies; capped. *)
    c1
    |> List.concat_map (fun cl1 ->
           c2
           |> List.filter_map (fun cl2 -> mk_clause (cl1 @ cl2)))
    |> mk_cond |> cap_clauses

(* DNF of an [IL.exp] branch condition, with [negated] tracking polarity
 * (negation-normal form on the fly: [!(a && b)] = [!a || !b]). N-ary
 * [And]/[Or] operator calls are folded over all unnamed operands, so e.g.
 * a Python [a or b or c] chain contributes one clause per disjunct
 * instead of one opaque atom. Leaves: boolean literals fold; an atom
 * larger than [taint_MAX_GUARD_COND_NODES] distinct nodes is dropped
 * (true / not contributing a literal — a sound weakening of its clause);
 * anything else becomes an interned literal. *)
let rec dnf_of ~(lang : Lang.t) (atoms : atoms) ~(negated : bool) (e : IL.exp) :
    cond =
  let all_unnamed args =
    if
      List.for_all
        (function
          | IL.Unnamed _ -> true
          | IL.Named _ -> false)
        args
    then Some (List.map IL_helpers.exp_of_arg args)
    else None
  in
  match e.e with
  | IL.Operator ((G.Not, _), [ IL.Unnamed inner ]) ->
      dnf_of ~lang atoms ~negated:(not negated) inner
  | IL.Operator ((G.And, _), args) when Option.is_some (all_unnamed args) ->
      let exps = Option.get (all_unnamed args) in
      let combine = if negated then or_cond else and_cond in
      let unit_ = if negated then cond_false else cond_true in
      List.fold_left (fun acc a -> combine acc (dnf_of ~lang atoms ~negated a)) unit_ exps
  | IL.Operator ((G.Or, _), args) when Option.is_some (all_unnamed args) ->
      let exps = Option.get (all_unnamed args) in
      let combine = if negated then and_cond else or_cond in
      let unit_ = if negated then cond_true else cond_false in
      List.fold_left (fun acc a -> combine acc (dnf_of ~lang atoms ~negated a)) unit_ exps
  | _ ->
      if IL_helpers.is_lit_bool (not negated) e then cond_true
      else if IL_helpers.is_lit_bool negated e then cond_false
      else
        let atom, distinct_nodes = intern_counted atoms e in
        if distinct_nodes > Limits_semgrep.taint_MAX_GUARD_COND_NODES then
          cond_true
        else
          match mk_clause [ { atom = as_atom ~lang atom; negated } ] with
          | None -> cond_false
          | Some [] -> cond_true
          | Some clause -> [ clause ]

let of_exp ~(lang : Lang.t) (atoms : atoms) (e : IL.exp) : cond =
  dnf_of ~lang atoms ~negated:false e

let literal_is_frozen (params : IL.param list) (l : literal) : bool =
  (not (is_length_atom l.atom.node))
  &&
  let facts = facts_of l.atom in
  facts.always_frozen
  || List.exists
       (fun (n : IL.name) -> Option.is_none (IL_helpers.param_index params n))
       facts.freezing_names
  || not
       (List.exists
          (fun ((n, _) : IL.name * bool) ->
            Option.is_some (IL_helpers.param_index params n))
          facts.fetch_bases)

let drop_frozen_literals (params : IL.param list) (c : cond) : cond =
  c
  |> List.map (fun clause ->
         clause |> List.filter (fun l -> not (literal_is_frozen params l)))
  |> mk_cond

(* The distinct atoms of a cond, for computing [param_refs] and for
 * substitution call sites that need the variable-bearing parts. *)
let atoms_of_cond (c : cond) : IL.exp list =
  c
  |> List.concat_map (fun clause -> clause |> List.map (fun l -> l.atom))
  |> List.sort_uniq (fun (a1 : hcond) (a2 : hcond) -> Int.compare a1.id a2.id)
  |> List.map (fun (a : hcond) -> a.node)

(* Rewrite every atom with [f] (substitution at a call site) and
 * re-normalise: substituted atoms are re-interned, re-capped, and the
 * consistency checks re-run — substitution can make atoms equal,
 * complementary, or contradictory. The clause structure never changes
 * under [f] (atoms are the only variable-bearing parts), so this is a
 * map, not a re-conversion. *)
let map_atoms ~(lang : Lang.t) (atoms : atoms) (substituted : IL.name list)
    (f : IL.exp -> IL.exp) (c : cond) : cond =
  let map_literal (l : literal) : bool * literal option =
    if not (reads_any substituted l.atom) then (false, Some l)
    else
      let e = f l.atom.node in
      if IL_helpers.is_lit_bool (not l.negated) e then
        (* literal true: contributes nothing to the clause *)
        (true, None)
      else if IL_helpers.is_lit_bool l.negated e then
        (* literal false: kills the clause *)
        ( true,
          Some
            {
              atom = as_atom ~lang (fst (intern_counted atoms e));
              negated = l.negated;
            }
        )
      else
        let atom, distinct_nodes = intern_counted atoms e in
        if distinct_nodes > Limits_semgrep.taint_MAX_GUARD_COND_NODES then
          (true, None)
        else (atom != l.atom, Some { atom = as_atom ~lang atom; negated = l.negated })
  in
  let rewritten, clauses =
    List.fold_left_map
      (fun (rewritten : bool) (clause : clause) ->
        let rewritten, lits = List.fold_left_map
            (fun (rewritten : bool) (l : literal) ->
              let changed, l' = map_literal l in
              (rewritten || changed, l'))
            rewritten clause
        in
        (rewritten, List.filter_map Fun.id lits))
      false c
  in
  if not rewritten then c
  else clauses |> List.filter_map mk_clause |> mk_cond |> cap_clauses

(* Three-valued evaluation: [Some b] when decided, [None] when some atom
 * is undecided in a way that leaves the verdict open. *)
let eval (c : cond) : bool option =
  let clause_value clause =
    List.fold_left
      (fun acc l ->
        match acc with
        | Some false -> Some false
        | _ -> (
            match (facts_of l.atom).value with
            | Some b ->
                let v = if l.negated then not b else b in
                if v then acc else Some false
            | None -> None))
      (Some true) clause
  in
  List.fold_left
    (fun acc clause ->
      match acc with
      | Some true -> Some true
      | _ -> (
          match clause_value clause with
          | Some true -> Some true
          | Some false -> acc
          | None -> None))
    (Some false) c

(*****************************************************************************)
(* Guards *)
(*****************************************************************************)

let compare_param_ref (n1, i1) (n2, i2) =
  let c = Int.compare i1 i2 in
  if c <> 0 then c
  else String.compare (IL.str_of_name n1) (IL.str_of_name n2)

let top : t = { cond = cond_true; param_refs = [] }
let is_top (g : t) : bool = cond_is_top g.cond
let is_bot (g : t) : bool = cond_is_bot g.cond

let compare (g1 : t) (g2 : t) : int =
  if is_top g1 && is_top g2 then 0
  else
    let c = compare_cond g1.cond g2.cond in
    if c <> 0 then c
    else List.compare compare_param_ref g1.param_refs g2.param_refs

let equal (g1 : t) (g2 : t) : bool = g1 == g2 || compare g1 g2 = 0

(* Render the cond as [(l1 && l2) || (l3)]. [~truncate_guards] (default
 * [true]) renders each atom into at most
 * [Limits_semgrep.taint_MAX_GUARD_LOG_CHARS] characters and the whole
 * cond into roughly that budget, so debug logging stays bounded (an
 * atom is a shared DAG whose tree rendering can be exponential); the
 * signature dump passes [~truncate_guards:false] for full output. *)
let show ?(truncate_guards = true) g =
  let max = Limits_semgrep.taint_MAX_GUARD_LOG_CHARS in
  let pp_atom (e : IL.exp) =
    if truncate_guards then IL_pp.pp_exp_bounded ~max e else IL_pp.pp_exp e
  in
  let pp_literal l =
    if l.negated then "!(" ^ pp_atom l.atom.node ^ ")" else pp_atom l.atom.node
  in
  let pp_clause c =
    match c with
    | [ l ] -> pp_literal l
    | _ -> "(" ^ String.concat " && " (List.map pp_literal c) ^ ")"
  in
  if cond_is_top g.cond then "true"
  else if cond_is_bot g.cond then "false"
  else
    let s = String.concat " || " (List.map pp_clause g.cond) in
    if truncate_guards && String.length s > max then String.sub s 0 max ^ "..."
    else s

(* Variables (base [Var] names) read by the guard's atoms. A guard
 * becomes unreliable once one of these is reassigned: the IL is non-SSA,
 * so the recorded cond then refers to a value that may differ from the
 * one tested at the branch where the guard was established. The engine
 * drops such guards when stamping effects (see [Taint_lval_env]). *)
let cond_vars (g : t) : IL.name list =
  atoms_of_cond g.cond
  |> List.concat_map IL_helpers.lvals_of_exp
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
 * stamped at emission compress this to a single [t] via [conjoin]. *)
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

module Param_ref_set = Stdlib.Set.Make (struct
  type t = IL.name * int

  let compare = compare_param_ref
end)

let merge_param_refs (rs1 : (IL.name * int) list)
    (rs2 : (IL.name * int) list) : (IL.name * int) list =
  Param_ref_set.elements
    (Param_ref_set.union
       (Param_ref_set.of_list rs1)
       (Param_ref_set.of_list rs2))

(* Conjoin two guards. [top] is absorbed. *)
let compose_and (g1 : t) (g2 : t) : t =
  if is_top g1 then g2
  else if is_top g2 then g1
  else
    {
      cond = and_cond g1.cond g2.cond;
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Disjoin two guards. A guard with [cond = false] is the [Or]
 * identity. *)
let compose_or (g1 : t) (g2 : t) : t =
  if is_top g1 || is_top g2 then top
  else if is_bot g1 then g2
  else if is_bot g2 then g1
  else
    {
      cond = or_cond g1.cond g2.cond;
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Fold a list of guards into a single conjunction. Empty list yields
 * [top]. *)
let conjoin (gs : t list) : t = List.fold_left compose_and top gs

(* Guards for a branch condition at a [TrueNode] ([negated:false]) or
 * [FalseNode] ([negated:true]). A single-clause cond is split into one
 * guard per literal — the active-guard set tracks atoms individually so
 * reassignment ([cond_vars]) drops exactly the affected atoms — while a
 * disjunctive cond stays one guard. [param_refs] anchor each guard's
 * atoms in [params]. *)
let of_branch_cond ~(lang : Lang.t) (atoms : atoms) ~(negated : bool) (params : IL.param list)
    (e : IL.exp) :
    t list =
  let refs_of_cond (c : cond) : (IL.name * int) list =
    atoms_of_cond c
    |> List.fold_left
         (fun acc atom ->
           merge_param_refs acc (IL_helpers.cond_partial_param_refs params atom))
         []
  in
  let mk (c : cond) : t = { cond = c; param_refs = refs_of_cond c } in
  match dnf_of ~lang atoms ~negated e with
  | [ clause ] when List.length clause > 1 ->
      clause |> List.map (fun l -> mk [ [ l ] ])
  | c -> if cond_is_top c then [] else [ mk c ]
