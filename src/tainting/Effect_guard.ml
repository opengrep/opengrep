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
 * up. *)

module G = AST_generic

(*****************************************************************************)
(* Atoms *)
(*****************************************************************************)

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

let intern_table : hcond ICondTbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> ICondTbl.create 1024)

let reset_intern () : unit = ICondTbl.clear (Domain.DLS.get intern_table)

(* Canonicalise [e] bottom-up: children are interned first, so a node's table
 * lookup compares already-shared children ([equal_exp] short-circuits on
 * [phys_equal]) and hashes from their stored hashes — both linear only in the
 * node's immediate children. The per-call [memo] keeps the walk linear in the
 * atom's distinct nodes (the atom is itself a shared DAG from [Sig_inst]).
 * Also returns the number of distinct canonical nodes in the result ([seen]
 * collects them across duplicate inputs), for the atom-size cap. *)
let intern_counted (e : IL.exp) : hcond * int =
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
  let h = go e in
  (h, IL_helpers.PhysExpTbl.length seen)

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
  let c = IL_helpers.compare_exp l1.atom.node l2.atom.node in
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
 * across types we make no judgement (e.g. [1 == 1.0] holds in Python). *)
let distinct_same_type_constants (l1 : G.literal) (l2 : G.literal) : bool =
  match (l1, l2) with
  | G.Int (i1, _), G.Int (i2, _) -> not (Option.equal Int64.equal i1 i2)
  | G.String (_, (s1, _), _), G.String (_, (s2, _), _) ->
      not (String.equal s1 s2)
  | G.Bool (b1, _), G.Bool (b2, _) -> not (Bool.equal b1 b2)
  | _ -> false

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
          (IL_helpers.equal_exp l1.atom.node l2.atom.node
          && not (Bool.equal l1.negated l2.negated))
          || adjacent rest
      | _ -> false
    in
    adjacent c
  in
  complementary
  ||
  let eqs =
    c
    |> List.filter_map (fun l ->
           if l.negated then None else eq_parts l.atom.node)
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

(* Sort, dedup, and consistency-check a conjunction of literals.
 * [None] means the clause is unsatisfiable and must be dropped. *)
let mk_clause (lits : literal list) : clause option =
  let c = List.sort_uniq compare_literal lits in
  if clause_inconsistent c then None else Some c

(* Sort and dedup clauses; collapse to [top] when a clause is true ([])
 * or two singleton clauses are complementary ([A] or [!A] is a
 * tautology — this fold is what lets fan-in of complementary branch
 * guards reach a fixed point instead of growing). *)
let cond_true : cond = [ [] ]
let cond_false : cond = []
let cond_is_top (c : cond) : bool = List.exists List_.null c
let cond_is_bot (c : cond) : bool = List_.null c

let mk_cond (clauses : clause list) : cond =
  let cs = List.sort_uniq compare_clause clauses in
  if List.exists List_.null cs then cond_true
  else
    let complementary_singletons =
      let singles = cs |> List.filter_map (function [ l ] -> Some l | _ -> None) in
      let rec pairwise = function
        | l1 :: rest ->
            List.exists
              (fun l2 ->
                IL_helpers.equal_exp l1.atom.node l2.atom.node
                && not (Bool.equal l1.negated l2.negated))
              rest
            || pairwise rest
        | [] -> false
      in
      pairwise singles
    in
    if complementary_singletons then cond_true else cs

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

let or_cond (c1 : cond) (c2 : cond) : cond =
  if cond_is_top c1 || cond_is_top c2 then cond_true
  else cap_clauses (mk_cond (c1 @ c2))

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
let rec dnf_of ~(negated : bool) (e : IL.exp) : cond =
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
      dnf_of ~negated:(not negated) inner
  | IL.Operator ((G.And, _), args) when Option.is_some (all_unnamed args) ->
      let exps = Option.get (all_unnamed args) in
      let combine = if negated then or_cond else and_cond in
      let unit_ = if negated then cond_false else cond_true in
      List.fold_left (fun acc a -> combine acc (dnf_of ~negated a)) unit_ exps
  | IL.Operator ((G.Or, _), args) when Option.is_some (all_unnamed args) ->
      let exps = Option.get (all_unnamed args) in
      let combine = if negated then and_cond else or_cond in
      let unit_ = if negated then cond_true else cond_false in
      List.fold_left (fun acc a -> combine acc (dnf_of ~negated a)) unit_ exps
  | _ ->
      if IL_helpers.is_lit_bool (not negated) e then cond_true
      else if IL_helpers.is_lit_bool negated e then cond_false
      else
        let atom, distinct_nodes = intern_counted e in
        if distinct_nodes > Limits_semgrep.taint_MAX_GUARD_COND_NODES then
          cond_true
        else [ [ { atom; negated } ] ]

let of_exp (e : IL.exp) : cond = dnf_of ~negated:false e

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
let atom_has_frozen_fetch (params : IL.param list) (atom : IL.exp) : bool =
  let memo : bool IL_helpers.PhysExpTbl.t = IL_helpers.PhysExpTbl.create 16 in
  let rec go (e : IL.exp) : bool =
    match IL_helpers.PhysExpTbl.find_opt memo e with
    | Some r -> r
    | None ->
        let r =
          match e.e with
          | IL.Fetch lv -> go_lval lv
          | _ ->
              let found, _ =
                IL_helpers.fold_map_children
                  (fun acc c -> (acc || go c, c))
                  false e
              in
              found
        in
        IL_helpers.PhysExpTbl.add memo e r;
        r
  and go_lval (lv : IL.lval) : bool =
    let offset_exps_frozen =
      lv.IL.rev_offset
      |> List.exists (fun (o : IL.offset) ->
             match o.IL.o with
             | IL.Index e -> go e
             | IL.Dot _
             | IL.Slice _ ->
                 false)
    in
    offset_exps_frozen
    ||
    match lv.IL.base with
    | IL.Var name -> (
        match lv.IL.rev_offset with
        | [] -> false (* offset-free local: svalue-foldable *)
        | offsets ->
            not
              (List.for_all IL_helpers.offset_is_resolvable offsets
              && Option.is_some (IL_helpers.param_index params name)))
    | IL.VarSpecial _ -> true
    | IL.Mem _ -> true
  in
  go atom

let literal_is_frozen (params : IL.param list) (l : literal) : bool =
  (not (is_length_atom l.atom.node))
  && atom_has_frozen_fetch params l.atom.node

let drop_frozen_literals (params : IL.param list) (c : cond) : cond =
  c
  |> List.map (fun clause ->
         clause |> List.filter (fun l -> not (literal_is_frozen params l)))
  |> mk_cond

(* The distinct atoms of a cond, for computing [param_refs] and for
 * substitution call sites that need the variable-bearing parts. *)
let atoms_of_cond (c : cond) : IL.exp list =
  c
  |> List.concat_map (fun clause -> clause |> List.map (fun l -> l.atom.node))
  |> List.sort_uniq IL_helpers.compare_exp

(* Rewrite every atom with [f] (substitution at a call site) and
 * re-normalise: substituted atoms are re-interned, re-capped, and the
 * consistency checks re-run — substitution can make atoms equal,
 * complementary, or contradictory. The clause structure never changes
 * under [f] (atoms are the only variable-bearing parts), so this is a
 * map, not a re-conversion. *)
let map_atoms (f : IL.exp -> IL.exp) (c : cond) : cond =
  c
  |> List.map (fun clause ->
         clause
         |> List.filter_map (fun l ->
                let e = f l.atom.node in
                if IL_helpers.is_lit_bool (not l.negated) e then
                  (* literal true: contributes nothing to the clause *)
                  None
                else if IL_helpers.is_lit_bool l.negated e then
                  (* literal false: kills the clause *)
                  Some { atom = fst (intern_counted e); negated = l.negated }
                else
                  let atom, distinct_nodes = intern_counted e in
                  if
                    distinct_nodes
                    > Limits_semgrep.taint_MAX_GUARD_COND_NODES
                  then None
                  else Some { atom; negated = l.negated }))
  |> List.filter_map mk_clause
  |> mk_cond |> cap_clauses

(* Simplify with a partial atom evaluator: [eval_atom] returns
 * [Some true]/[Some false] for atoms it can decide and [None] for the
 * rest. A decided-true literal leaves its clause; a decided-false
 * literal kills its clause. Used by [Sig_inst.classify_guards] to fold
 * dispatch (length) atoms at instantiation while carrying the rest. *)
let simplify_with (eval_atom : IL.exp -> bool option) (c : cond) : cond =
  c
  |> List.filter_map (fun clause ->
         let exception Clause_false in
         try
           Some
             (clause
             |> List.filter (fun l ->
                    match eval_atom l.atom.node with
                    | Some b ->
                        let lit_value = if l.negated then not b else b in
                        if lit_value then false (* drop satisfied literal *)
                        else raise Clause_false
                    | None -> true))
         with
         | Clause_false -> None)
  |> mk_cond

(* Three-valued evaluation: [Some b] when decided, [None] when some atom
 * is undecided in a way that leaves the verdict open. *)
let eval_with (eval_atom : IL.exp -> bool option) (c : cond) : bool option =
  let clause_value clause =
    List.fold_left
      (fun acc l ->
        match acc with
        | Some false -> Some false
        | _ -> (
            match eval_atom l.atom.node with
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

let compare g1 g2 =
  let c = compare_cond g1.cond g2.cond in
  if c <> 0 then c
  else
    List.compare compare_param_ref
      (List.sort compare_param_ref g1.param_refs)
      (List.sort compare_param_ref g2.param_refs)

let equal g1 g2 = compare g1 g2 = 0
let top : t = { cond = cond_true; param_refs = [] }
let is_top (g : t) : bool = cond_is_top g.cond
let is_bot (g : t) : bool = cond_is_bot g.cond

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
      cond = and_cond g1.cond g2.cond;
      param_refs = merge_param_refs g1.param_refs g2.param_refs;
    }

(* Disjoin two guards. A guard with [cond = false] is the [Or]
 * identity. *)
let compose_or (g1 : t) (g2 : t) : t =
  if is_bot g1 then g2
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
let of_branch_cond ~(negated : bool) (params : IL.param list) (e : IL.exp) :
    t list =
  let refs_of_cond (c : cond) : (IL.name * int) list =
    atoms_of_cond c
    |> List.fold_left
         (fun acc atom ->
           merge_param_refs acc (IL_helpers.cond_partial_param_refs params atom))
         []
  in
  let mk (c : cond) : t = { cond = c; param_refs = refs_of_cond c } in
  match dnf_of ~negated e with
  | [ clause ] when List.length clause > 1 ->
      clause |> List.map (fun l -> mk [ [ l ] ])
  | c -> if cond_is_top c then [] else [ mk c ]
