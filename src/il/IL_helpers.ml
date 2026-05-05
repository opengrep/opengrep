(* Yoann Padioleau
 * Iago Abal
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open IL

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(***********************************************)
(* L-values *)
(***********************************************)

let exp_of_arg arg =
  match arg with
  | Unnamed exp -> exp
  | Named (_, exp) -> exp

(***********************************************)
(* Parameter / offset anchoring *)
(***********************************************)

let pname_of_param (p : IL.param) : IL.name option =
  match p with
  | IL.Param { pname; _ } -> Some pname
  | IL.ParamRest { pname; _ } -> Some pname
  | IL.ParamPattern ({ pname; _ }, _) -> Some pname
  | IL.ParamFixme -> None

let offset_is_resolvable (off : IL.offset) : bool =
  match off.o with
  | IL.Dot _ -> true
  | IL.Index { e = IL.Literal (AST_generic.Int _ | AST_generic.String _); _ }
    ->
      true
  | IL.Index _ -> false
  | IL.Slice _ -> true

let param_index (params : IL.param list) (name : IL.name) : int option =
  let rec find i = function
    | [] -> None
    | p :: rest -> (
        match pname_of_param p with
        | Some pname when IL.equal_name pname name -> Some i
        | _ -> find (i + 1) rest)
  in
  find 0 params

let cond_param_refs (params : IL.param list) (cond : IL.exp) :
    (IL.name * int) list option =
  let merge refs1 refs2 =
    List.fold_left
      (fun acc ((n, _) as r) ->
        if List.exists (fun (n', _) -> IL.equal_name n' n) acc then acc
        else r :: acc)
      refs1 refs2
  in
  let rec of_exp (e : IL.exp) : (IL.name * int) list option =
    match e.e with
    | IL.Literal _ -> Some []
    | IL.Fetch lval -> (
        match lval.base with
        | IL.Var name -> (
            match param_index params name with
            | None -> None
            | Some idx ->
                if List.for_all offset_is_resolvable lval.rev_offset then
                  Some [ (name, idx) ]
                else None)
        | IL.VarSpecial _ | IL.Mem _ -> None)
    | IL.Operator (_, args) -> of_args [] args
    | IL.Composite _ | IL.RecordOrDict _ | IL.Cast _ | IL.FixmeExp _ -> None
  and of_args acc = function
    | [] -> Some acc
    | a :: rest -> (
        match of_exp (exp_of_arg a) with
        | None -> None
        | Some refs -> of_args (merge acc refs) rest)
  in
  of_exp cond

let rexps_of_instr x =
  match x.i with
  | Assign (({ base = Var _; rev_offset = _ :: _ } as lval), exp) ->
      [ { e = Fetch { lval with rev_offset = [] }; eorig = NoOrig }; exp ]
  | Assign (_, exp) -> [ exp ]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1 :: List_.map exp_of_arg args
  | New (_, _, _, args)
  | CallSpecial (_, _, args) ->
      List_.map exp_of_arg args
  | FixmeInstr _ -> []

(* opti: could use a set *)
let rec lvals_of_exp e =
  match e.e with
  | Fetch lval -> lval :: lvals_in_lval lval
  | Literal _ -> []
  | Cast (_, e) -> lvals_of_exp e
  | Composite (_, (_, xs, _)) -> lvals_of_exps xs
  | Operator (_, xs) -> lvals_of_exps (List_.map exp_of_arg xs)
  | RecordOrDict ys ->
      lvals_of_exps
        (ys
        |> List.concat_map @@ function
           | Field (_, e)
           | Spread e ->
               [ e ]
           | Entry (ke, ve) -> [ ke; ve ])
  | FixmeExp (_, _, Some e) -> lvals_of_exp e
  | FixmeExp (_, _, None) -> []

and lvals_in_lval lval =
  let base_lvals =
    match lval.base with
    | Mem e -> lvals_of_exp e
    | _else_ -> []
  in
  let offset_lvals =
    List.concat_map
      (fun offset ->
        match offset.o with
        | Index e -> lvals_of_exp e
        | Dot _ -> []
        | Slice _ -> [])
      lval.rev_offset
  in
  base_lvals @ offset_lvals

and lvals_of_exps xs = xs |> List.concat_map lvals_of_exp

(** The lvals in the rvals of the instruction. *)
let rlvals_of_instr x =
  let exps = rexps_of_instr x in
  lvals_of_exps exps

(*****************************************************************************)
(* Public *)
(*****************************************************************************)

let is_pro_resolved_global name =
  match !(name.id_info.id_resolved) with
  | Some (GlobalName _, _sid) -> true
  | Some _
  | None ->
      false

(* HACK: Because we don't have a "Class" type, classes have themselves as types. *)
let is_class_name (name : name) =
  match (!(name.id_info.id_resolved), !(name.id_info.id_type)) with
  | Some resolved1, Some { t = TyN (Id (_, { id_resolved; _ })); _ } -> (
      match !id_resolved with
      | None -> false
      | Some resolved2 ->
          (* If 'name' has type 'name' then we assume it's a class. *)
          AST_generic.equal_resolved_name resolved1 resolved2)
  | _, None
  | _, Some _ ->
      false

(***********************************************)
(* L-values *)
(***********************************************)

let lval_of_var var = { IL.base = Var var; rev_offset = [] }

let is_dots_offset offset =
  offset
  |> List.for_all (fun o ->
         match o.o with
         | Dot _ -> true
         | Index _
         | Slice _ ->
             false)

let lval_of_instr_opt x =
  match x.i with
  | Assign (lval, _)
  | AssignAnon (lval, _)
  | Call (Some lval, _, _)
  | New (lval, _, _, _)
  | CallSpecial (Some lval, _, _) ->
      Some lval
  | Call _
  | CallSpecial _ ->
      None
  | FixmeInstr _ -> None

let lvar_of_instr_opt x =
  match lval_of_instr_opt x with
  | Some { base = Var x; _ } -> Some x
  | Some _
  | None ->
      None

let rlvals_of_node = function
  | Enter
  | Exit
  (* must ignore exp in True and False *)
  | TrueNode _
  | FalseNode _
  | NGoto _
  | Join ->
      []
  | NInstr x -> rlvals_of_instr x
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) ->
      lvals_of_exp e
  | NOther _
  | NTodo _ ->
      []

let orig_of_node = function
  | Enter
  | Exit ->
      None
  | TrueNode e
  | FalseNode e
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) ->
      Some e.eorig
  | NInstr i -> Some i.iorig
  | NGoto _
  | Join
  | NOther _
  | NTodo _ ->
      None

(***********************************************)
(* CFG *)
(***********************************************)

let rec reachable_nodes fun_cfg =
  let main_nodes = CFG.reachable_nodes fun_cfg.cfg in
  let lambdas_nodes =
    fun_cfg.lambdas |> NameMap.to_seq
    |> Seq.map (fun (_lname, lcfg) -> reachable_nodes lcfg)
  in
  Seq.concat (Seq.cons main_nodes lambdas_nodes)

(***********************************************)
(* Lambdas *)
(***********************************************)

let lval_is_lambda lambdas_cfgs lval =
  match lval with
  | { base = Var name; rev_offset = [] } ->
      let* lambda_cfg = IL.NameMap.find_opt name lambdas_cfgs in
      Some (name, lambda_cfg)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

(***********************************************)
(* Boolean smart constructors over IL.exp      *)
(***********************************************)

(* All taint-engine boolean composition flows through these. The taint
 * engine uses [Effect_guard.t] (an [IL.exp] cond plus its param refs)
 * as its single guard representation; smart constructors here keep the
 * cond in a partially-normalised compound form without expanding to
 * full CNF or DNF. Composition via [wrap_and]/[wrap_or] supports both
 * the recogniser's incremental atom collection and the engine's
 * disjunctive provenance from joins / fan-in. *)

let lit_bool ~(eorig : IL.orig) (b : bool) : IL.exp =
  let tok = Tok.unsafe_fake_tok (if b then "true" else "false") in
  { IL.e = IL.Literal (G.Bool (b, tok)); eorig }

let is_lit_bool (b : bool) (e : IL.exp) : bool =
  match e.e with
  | IL.Literal (G.Bool (b', _)) -> Bool.equal b b'
  | _ -> false

(* Wrap [cond] in [Operator(Not, [cond])]. Used at [FalseNode] so the
 * evaluator's single "cond must resolve to [G.Lit (G.Bool true)]" rule
 * applies whether the effect was recorded under the true or false
 * branch. *)
let wrap_not (cond : IL.exp) : IL.exp =
  let tok = Tok.unsafe_fake_tok "!" in
  {
    IL.e = IL.Operator ((G.Not, tok), [ IL.Unnamed cond ]);
    eorig = cond.eorig;
  }

(* Compare two [IL.exp]s by their pretty-printed form; matches
 * [Effect_guard.compare_cond] so syntactic complement detection is
 * consistent with guard set dedup semantics. *)
let il_exp_equal (a : IL.exp) (b : IL.exp) : bool =
  String.equal (IL_pp.pp_exp a) (IL_pp.pp_exp b)

(* Direct syntactic complement: [a] vs [Not a].
 *
 * NOTE: Catches direct atom-level negation only. Compound shapes like
 * [Op Or [a; b]] vs [Op And [Op Not a; Op Not b]] are
 * De-Morgan-equivalent but not detected here; they require a vector
 * walker, deferred. Sound either way: an unsimplified compound cond is
 * correctly evaluated by [Eval_il_partial.eval] when concrete arguments
 * are known. *)
let is_complement (a : IL.exp) (b : IL.exp) : bool =
  let inner_of_not (e : IL.exp) =
    match e.e with
    | IL.Operator ((G.Not, _), [ IL.Unnamed inner ]) -> Some inner
    | _ -> None
  in
  match (inner_of_not a, inner_of_not b) with
  | Some a', _ -> il_exp_equal a' b
  | _, Some b' -> il_exp_equal a b'
  | None, None -> false

(* Flatten nested [Operator(op, _)] of the same kind into a list. *)
let flatten_same_op (op : G.operator) (es : IL.exp list) : IL.exp list =
  List.concat_map
    (fun (e : IL.exp) ->
      match e.e with
      | IL.Operator ((op', _), args) when AST_generic.equal_operator op op' ->
          List.map exp_of_arg args
      | _ -> [ e ])
    es

(* Order-preserving dedup by [IL_pp.pp_exp] equality. Used to keep
 * [wrap_and]/[wrap_or] from growing their operand list with duplicates
 * across fixpoint iterations: [wrap_or [X; X]] becomes [X], so the
 * lattice on guards reaches a fixed point on its own rather than
 * relying on [taint_MAX_VISITS_PER_NODE] to bail. *)
let dedup_exps (es : IL.exp list) : IL.exp list =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun e ->
      let key = IL_pp.pp_exp e in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    es

(* Return true iff [es] contains a pair [(a, b)] with [is_complement a b]. *)
let rec has_complement_pair (es : IL.exp list) : bool =
  match es with
  | [] | [ _ ] -> false
  | x :: rest -> List.exists (is_complement x) rest || has_complement_pair rest

(* Build a right-nested binary [Operator(op, _)] from a non-empty list.
 * Right-nesting matches [Eval_il_partial.eval]'s binary [Op And/Or]
 * pattern, which folds two [Lit Bool] operands. *)
let rec binary_fold_op (op : G.operator) (es : IL.exp list) : IL.exp =
  let tok =
    Tok.unsafe_fake_tok
      (match op with
      | G.And -> "&&"
      | G.Or -> "||"
      | _ -> "?")
  in
  match es with
  | [] -> assert false (* unreachable: callers ensure non-empty *)
  | [ e ] -> e
  | first :: rest ->
      let right = binary_fold_op op rest in
      {
        IL.e = IL.Operator ((op, tok), [ IL.Unnamed first; IL.Unnamed right ]);
        eorig = first.eorig;
      }

(* Smart [And] over [IL.exp]: empty -> true, singleton returned as-is,
 * nested same-kind flattened, [true] absorbed, any [false]
 * short-circuits to false, direct syntactic complement among args
 * folds to false. Right-nested binary in the general case so
 * [Eval_il_partial.eval] can fold it. *)
let wrap_and (es : IL.exp list) : IL.exp =
  let es = es |> flatten_same_op G.And |> dedup_exps in
  match es with
  | [] -> lit_bool ~eorig:IL.NoOrig true
  | [ e ] -> e
  | _ when List.exists (is_lit_bool false) es ->
      lit_bool ~eorig:(List.hd es).eorig false
  | _ ->
      let es = List.filter (fun e -> not (is_lit_bool true e)) es in
      (match es with
      | [] -> lit_bool ~eorig:IL.NoOrig true
      | [ e ] -> e
      | _ when has_complement_pair es ->
          lit_bool ~eorig:(List.hd es).eorig false
      | _ -> binary_fold_op G.And es)

(* Smart [Or] over [IL.exp]: dual to [wrap_and]. *)
let wrap_or (es : IL.exp list) : IL.exp =
  let es = es |> flatten_same_op G.Or |> dedup_exps in
  match es with
  | [] -> lit_bool ~eorig:IL.NoOrig false
  | [ e ] -> e
  | _ when List.exists (is_lit_bool true) es ->
      lit_bool ~eorig:(List.hd es).eorig true
  | _ ->
      let es = List.filter (fun e -> not (is_lit_bool false e)) es in
      (match es with
      | [] -> lit_bool ~eorig:IL.NoOrig false
      | [ e ] -> e
      | _ when has_complement_pair es ->
          lit_bool ~eorig:(List.hd es).eorig true
      | _ -> binary_fold_op G.Or es)
