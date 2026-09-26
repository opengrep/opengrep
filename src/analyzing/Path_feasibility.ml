(* Opengrep authors
 *
 * Copyright (C) 2026 Opengrep authors
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
module G = AST_generic
module H = AST_generic_helpers
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module LV = IL_helpers
module Eval = Eval_il_partial

type verdict = Feasible | Infeasible | Unknown

type anchor =
  | Entry
  | Token of Tok.t
  | Range of Tok.location * Tok.location
  | Call of IL.exp
  | Exit

type state =
  | Dead
  | Live of { values : G.svalue Var_env.t; literals : (IL.exp * bool) list }

type span = { span_node : IL.nodei; first : int; last : int; stop : int }

type index = {
  entry_node : IL.nodei;
  exit_node : IL.nodei;
  spans : span list;
  calls : (IL.nodei * IL.exp) list;
  params : int list;
  exposed : IL.name list;
  nonlocal_written : IL.name list;
}

module Stage = struct
  type t = IL.nodei * int

  let compare ((n1, i1) : t) ((n2, i2) : t) : int =
    match Int.compare n1 n2 with
    | 0 -> Int.compare i1 i2
    | c -> c
end

module StageSet = Set.Make (Stage)
module StageMap = Map.Make (Stage)

type stage_node = { il_node : IL.node option; stage : int }

module Product = Dataflow_core.Make (struct
  type node = stage_node
  type edge = IL.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node (n : node) : string =
    match n.il_node with
    | Some node -> Display_IL.short_string_of_node_kind node.n
    | None -> "start"
end)

let start_of (tok : Tok.t) : int option =
  match Tok.loc_of_tok tok with
  | Ok loc -> Some loc.pos.bytepos
  | Error _ -> None

let is_local (name : IL.name) : bool =
  match !(name.id_info.id_resolved) with
  | Some ((G.LocalVar | G.Parameter), _) -> true
  | _ -> false

let base_vars (lvals : IL.lval list) : IL.name list =
  List.filter_map
    (fun (lval : IL.lval) ->
      match lval.base with
      | Var name -> Some name
      | VarSpecial _
      | Mem _ ->
          None)
    lvals

let rec written_in_lambdas (fun_cfg : IL.fun_cfg) : IL.name list =
  IL.NameMap.fold
    (fun (_ : IL.name) (lambda : IL.fun_cfg) (acc : IL.name list) ->
      (LV.reachable_nodes lambda
      |> Seq.filter_map (fun (node : IL.node) ->
             match node.n with
             | NInstr instr -> LV.lval_of_instr_opt instr
             | _ -> None)
      |> List.of_seq |> base_vars)
      @ written_in_lambdas lambda @ acc)
    fun_cfg.lambdas []

let referenced (fun_cfg : IL.fun_cfg) : IL.name list =
  LV.reachable_nodes fun_cfg
  |> Seq.flat_map (fun (node : IL.node) ->
         match node.n with
         | NInstr { i = CallSpecial (_, (Ref, _), args); _ } ->
             List.to_seq
               (base_vars
                  (List.concat_map
                     (fun (arg : IL.exp IL.argument) ->
                       LV.lvals_of_exp (LV.exp_of_arg arg))
                     args))
         | _ -> Seq.empty)
  |> List.of_seq

let index (fun_cfg : IL.fun_cfg) : index =
  let flow = fun_cfg.cfg in
  let nodes =
    CFG.NodeiSet.elements flow.reachable
    |> List.map (fun (ni : IL.nodei) -> (ni, flow.graph#nodes#assoc ni))
  in
  let spans =
    nodes
    |> List.filter_map (fun ((ni, node) : IL.nodei * IL.node) ->
           match LV.orig_of_node node.n with
           | None -> None
           | Some orig -> (
               match H.range_of_any_opt (IL.any_of_orig orig) with
               | None -> None
               | Some (first, last) ->
                   Some
                     {
                       span_node = ni;
                       first = first.pos.bytepos;
                       last = last.pos.bytepos;
                       stop = last.pos.bytepos + String.length last.str;
                     }))
  in
  let calls =
    nodes
    |> List.filter_map (fun ((ni, node) : IL.nodei * IL.node) ->
           match node.n with
           | NInstr { i = Call (_, callee, _, _); _ } -> Some (ni, callee)
           | _ -> None)
  in
  let params =
    fun_cfg.params
    |> List.filter_map (fun (param : IL.param) ->
           Option.bind (LV.pname_of_param param) (fun (name : IL.name) ->
               start_of (snd name.ident)))
  in
  {
    entry_node = flow.entry;
    exit_node = flow.exit;
    spans;
    calls;
    params;
    exposed = written_in_lambdas fun_cfg @ referenced fun_cfg;
    nonlocal_written =
      nodes
      |> List.filter_map (fun ((_, node) : IL.nodei * IL.node) ->
             match node.n with
             | NInstr instr -> LV.lval_of_instr_opt instr
             | _ -> None)
      |> base_vars
      |> List.filter (fun (name : IL.name) -> not (is_local name));
  }

let covering (ix : index) (first : int) (last : int) : IL.nodei list =
  ix.spans
  |> List.filter (fun (s : span) -> s.first <= first && last <= s.last)
  |> List.map (fun (s : span) -> s.span_node)

let nodes_of_anchor (ix : index) (anchor : anchor) : IL.nodei list =
  match anchor with
  | Entry -> [ ix.entry_node ]
  | Exit -> [ ix.exit_node ]
  | Token tok -> (
      match start_of tok with
      | None -> []
      | Some pos ->
          if List.exists (Int.equal pos) ix.params then [ ix.entry_node ]
          else
            ix.spans
            |> List.filter (fun (s : span) -> s.first <= pos && pos < s.stop)
            |> List.map (fun (s : span) -> s.span_node))
  | Range (first, last) -> covering ix first.pos.bytepos last.pos.bytepos
  | Call callee -> (
      match
        ix.calls
        |> List.filter (fun ((_, e) : IL.nodei * IL.exp) ->
               Common.phys_equal e callee)
      with
      | _ :: _ as calls -> List.map fst calls
      | [] -> (
          match H.range_of_any_opt (IL.any_of_orig callee.eorig) with
          | Some (first, last) ->
              covering ix first.pos.bytepos last.pos.bytepos
          | None -> []))

let entry_state (bindings : (IL.name * G.svalue) list) : state =
  Live
    {
      values =
        List.fold_left
          (fun (values : G.svalue Var_env.t) ((name, v) : IL.name * G.svalue) ->
            Dataflow_svalue.update_env_with values name v)
          VarMap.empty bindings;
      literals = [];
    }

let value (lang : Lang.t) (s : state) (e : IL.exp) : G.svalue =
  match s with
  | Dead -> G.NotCst
  | Live { values; _ } -> Eval.eval (Eval.mk_env lang values) e

let literals (s : state) : (IL.exp * bool) list =
  match s with
  | Dead -> []
  | Live { literals; _ } -> literals

let refutes (s : state) (lits : (IL.exp * bool) list) : bool =
  match s with
  | Dead -> false
  | Live { literals; _ } -> not (LV.literals_consistent (lits @ literals))

let equal_literal ((a1, n1) : IL.exp * bool) ((a2, n2) : IL.exp * bool) : bool
    =
  Bool.equal n1 n2 && LV.equal_exp a1 a2

let equal_state (s1 : state) (s2 : state) : bool =
  match (s1, s2) with
  | Dead, Dead -> true
  | Live l1, Live l2 ->
      Var_env.eq_env Eval.eq l1.values l2.values
      && Int.equal (List.length l1.literals) (List.length l2.literals)
      && List.for_all
           (fun (l : IL.exp * bool) -> List.exists (equal_literal l) l2.literals)
           l1.literals
  | Dead, Live _
  | Live _, Dead ->
      false

let join (s1 : state) (s2 : state) : state =
  match (s1, s2) with
  | Dead, s
  | s, Dead ->
      s
  | Live l1, Live l2 ->
      Live
        {
          values = Dataflow_svalue.union_env l1.values l2.values;
          literals =
            List.filter
              (fun (l : IL.exp * bool) -> List.exists (equal_literal l) l2.literals)
              l1.literals;
        }

let rec literals_of_condition (cond : IL.exp) (positive : bool) :
    (IL.exp * bool) list =
  let unnamed (args : IL.exp IL.argument list) : IL.exp list option =
    List.fold_right
      (fun (arg : IL.exp IL.argument) (acc : IL.exp list option) ->
        match (arg, acc) with
        | IL.Unnamed e, Some es -> Some (e :: es)
        | IL.Named _, _
        | _, None ->
            None)
      args (Some [])
  in
  match cond.e with
  | Operator ((G.Not, _), [ Unnamed inner ]) ->
      literals_of_condition inner (not positive)
  | Operator ((G.And, _), args) when positive -> (
      match unnamed args with
      | Some es -> List.concat_map (fun e -> literals_of_condition e true) es
      | None -> [ (cond, not positive) ])
  | Operator ((G.Or, _), args) when not positive -> (
      match unnamed args with
      | Some es -> List.concat_map (fun e -> literals_of_condition e false) es
      | None -> [ (cond, not positive) ])
  | _ -> [ (cond, not positive) ]

let rec has_untranslated (e : IL.exp) : bool =
  match e.e with
  | FixmeExp _ -> true
  | Literal _ -> false
  | Fetch lval -> lval_has_untranslated lval
  | Composite (_, (_, es, _)) -> List.exists has_untranslated es
  | RecordOrDict fields ->
      List.exists
        (fun (field : IL.field_or_entry) ->
          match field with
          | Field (_, e)
          | Spread e ->
              has_untranslated e
          | Entry (k, v) -> has_untranslated k || has_untranslated v)
        fields
  | Cast (_, e) -> has_untranslated e
  | Operator (_, args) ->
      List.exists (fun (arg : IL.exp IL.argument) -> has_untranslated (LV.exp_of_arg arg)) args

and lval_has_untranslated (lval : IL.lval) : bool =
  (match lval.base with
  | Mem e -> has_untranslated e
  | Var _
  | VarSpecial _ ->
      false)
  || List.exists
       (fun (o : IL.offset) ->
         match o.o with
         | Index e -> has_untranslated e
         | Dot _
         | Slice _ ->
             false)
       lval.rev_offset

let assume (lang : Lang.t) (values : G.svalue Var_env.t)
    (known : (IL.exp * bool) list) (cond : IL.exp) (positive : bool) : state =
  let eval_env = Eval.mk_env lang values in
  let decided ((atom, negated) : IL.exp * bool) : bool option =
    match Eval.eval eval_env atom with
    | G.Lit (G.Bool (b, _)) -> Some (not (Bool.equal b negated))
    | _ -> None
  in
  let assumed = literals_of_condition cond positive in
  if List.exists (fun l -> Option.equal Bool.equal (decided l) (Some false)) assumed
  then Dead
  else
    let undecided =
      List.filter
        (fun ((atom, _) as l : IL.exp * bool) ->
          Option.is_none (decided l)
          && (not (has_untranslated atom))
          && not (List.is_empty (LV.lvals_of_exp atom)))
        assumed
    in
    if LV.literals_consistent (undecided @ known) then
      Live
        {
          values;
          literals =
            List.filter
              (fun (l : IL.exp * bool) -> not (List.exists (equal_literal l) known))
              undecided
            @ known;
        }
    else Dead

let survives (ix : index) (instr : IL.instr) ((atom, _) : IL.exp * bool) :
    bool =
  let reads = LV.lvals_of_exp atom in
  let read = base_vars reads in
  let object_read =
    List.exists
      (fun (lval : IL.lval) ->
        match lval with
        | { base = Var _; rev_offset = [] } -> false
        | { base = Var _ | VarSpecial _ | Mem _; _ } -> true)
      reads
  in
  let written =
    LV.lval_of_instr_opt instr |> Option.to_list |> base_vars
  in
  let writes_object =
    match LV.lval_of_instr_opt instr with
    | Some { base = Var _; rev_offset = [] }
    | None ->
        false
    | Some { base = Var _ | VarSpecial _ | Mem _; _ } -> true
  in
  let is_call, passed =
    match instr.i with
    | Call (_, callee, args, _) ->
        (true, callee :: List.map LV.exp_of_arg args)
    | CallSpecial (_, _, args)
    | New (_, _, _, args) ->
        (true, List.map LV.exp_of_arg args)
    | Assign _
    | AugmentedAssign _
    | AssignAnon _
    | FixmeInstr _ ->
        (false, [])
  in
  let passed_vars = base_vars (List.concat_map LV.lvals_of_exp passed) in
  let reads_any (names : IL.name list) : bool =
    List.exists
      (fun (v : IL.name) -> List.exists (IL.equal_name v) names)
      read
  in
  let reads_locals_only =
    List.for_all
      (fun (lval : IL.lval) ->
        match lval.base with
        | Var name -> is_local name
        | VarSpecial _
        | Mem _ ->
            false)
      reads
  in
  (not (reads_any written))
  && ((not is_call)
     || reads_locals_only
        && (not (reads_any passed_vars))
        && not (reads_any ix.exposed))
  && ((not writes_object) || not (object_read || reads_any ix.exposed))

let transfer (lang : Lang.t) (fun_cfg : IL.fun_cfg) (ix : index) (s : state)
    (node : IL.node) : state =
  match s with
  | Dead -> Dead
  | Live { values; literals } -> (
      let values' = Dataflow_svalue.node_transfer lang fun_cfg values node in
      match node.n with
      | TrueNode cond -> assume lang values literals cond true
      | FalseNode cond -> assume lang values literals cond false
      | NInstr instr ->
          let forgotten =
            match instr.i with
            | Call _
            | CallSpecial _
            | New _ ->
                ix.exposed @ ix.nonlocal_written
            | Assign _
            | AugmentedAssign _
            | AssignAnon _
            | FixmeInstr _ -> (
                match LV.lval_of_instr_opt instr with
                | Some { base = Var _; rev_offset = [] }
                | None ->
                    []
                | Some { base = Var _ | VarSpecial _ | Mem _; _ } -> ix.exposed)
          in
          Live
            {
              values =
                List.fold_left
                  (fun (values : G.svalue Var_env.t) (name : IL.name) ->
                    VarMap.remove (IL.str_of_name name) values)
                  values' forgotten;
              literals = List.filter (survives ix instr) literals;
            }
      | Enter
      | Exit
      | Join
      | NCond _
      | NGoto _
      | NReturn _
      | NThrow _
      | NOther _
      | NTodo _ ->
          Live { values = values'; literals })

let check (lang : Lang.t) (fun_cfg : IL.fun_cfg) (ix : index)
    ~(entry : state) (anchors : anchor list) : verdict * state option list =
  let sets = anchors |> List.map (nodes_of_anchor ix) |> Array.of_list in
  let last = Array.length sets - 1 in
  let unknown = (Unknown, List.map (fun (_ : anchor) -> None) anchors) in
  if last < 0 || Array.exists List.is_empty sets then unknown
  else
    let flow = fun_cfg.cfg in
    let mem (i : int) (n : IL.nodei) : bool = List.exists (Int.equal n) sets.(i) in
    let rec advance (i : int) (n : IL.nodei) (acc : int list) : int list =
      if i < last && mem (i + 1) n then advance (i + 1) n ((i + 1) :: acc)
      else acc
    in
    let stages_at (i : int) (n : IL.nodei) : Stage.t list =
      List.map (fun (j : int) -> (n, j)) (advance i n [ i ])
    in
    let successors ((n, i) : Stage.t) : Stage.t list =
      CFG.successors flow n
      |> List.concat_map (fun ((m, _) : IL.nodei * IL.edge) -> stages_at i m)
    in
    let starts =
      if mem 0 ix.entry_node then stages_at 0 ix.entry_node else []
    in
    let rec forward (seen : StageSet.t) (edges : (Stage.t * Stage.t) list)
        (todo : Stage.t list) : StageSet.t * (Stage.t * Stage.t) list =
      match todo with
      | [] -> (seen, edges)
      | p :: rest ->
          let succs = successors p in
          let fresh =
            List.filter (fun (q : Stage.t) -> not (StageSet.mem q seen)) succs
          in
          forward
            (List.fold_left (fun s q -> StageSet.add q s) seen fresh)
            (List.rev_append (List.map (fun (q : Stage.t) -> (p, q)) succs) edges)
            (List.rev_append fresh rest)
    in
    let reached, edges =
      forward (StageSet.of_list starts) [] starts
    in
    let finals =
      StageSet.filter (fun ((n, i) : Stage.t) -> Int.equal i last && mem last n) reached
    in
    let predecessors =
      List.fold_left
        (fun (acc : Stage.t list StageMap.t) ((p, q) : Stage.t * Stage.t) ->
          StageMap.update q
            (fun (ps : Stage.t list option) -> Some (p :: Option.value ps ~default:[]))
            acc)
        StageMap.empty edges
    in
    let rec backward (live : StageSet.t) (todo : Stage.t list) : StageSet.t =
      match todo with
      | [] -> live
      | q :: rest ->
          let preds =
            StageMap.find_opt q predecessors
            |> Option.value ~default:[]
            |> List.filter (fun (p : Stage.t) -> not (StageSet.mem p live))
          in
          backward
            (List.fold_left (fun s p -> StageSet.add p s) live preds)
            (List.rev_append preds rest)
    in
    let relevant = backward finals (StageSet.elements finals) in
    if StageSet.is_empty finals then unknown
    else
      let graph = new Ograph_extended.ograph_mutable in
      let start = graph#add_node { il_node = None; stage = 0 } in
      let ids =
        StageSet.fold
          (fun ((n, i) as p : Stage.t) (acc : IL.nodei StageMap.t) ->
            StageMap.add p
              (graph#add_node
                 { il_node = Some (flow.graph#nodes#assoc n); stage = i })
              acc)
          relevant StageMap.empty
      in
      List.iter
        (fun (p : Stage.t) ->
          match StageMap.find_opt p ids with
          | Some id -> graph#add_arc ((start, id), IL.Direct)
          | None -> ())
        starts;
      List.iter
        (fun ((p, q) : Stage.t * Stage.t) ->
          match (StageMap.find_opt p ids, StageMap.find_opt q ids) with
          | Some pid, Some qid -> graph#add_arc ((pid, qid), IL.Direct)
          | _ -> ())
        edges;
      let product = CFG.make graph start start in
      let trans (mapping : state D.mapping) (pi : IL.nodei) : state D.inout =
        let stage_node = product.graph#nodes#assoc pi in
        let in_state =
          if Int.equal pi start then entry
          else
            CFG.predecessors product pi
            |> List.fold_left
                 (fun (acc : state) ((pp, _) : IL.nodei * IL.edge) ->
                   join acc mapping.(pp).D.out_env)
                 Dead
        in
        let out_state =
          match stage_node.il_node with
          | Some node -> transfer lang fun_cfg ix in_state node
          | None -> in_state
        in
        { D.in_env = in_state; out_env = out_state }
      in
      let mapping =
        Product.fixpoint ~eq_env:equal_state ~join
          ~init:(Product.new_node_array product { D.in_env = Dead; out_env = Dead })
          ~trans ~flow:product
      in
      let state_at (i : int) : state option =
        StageMap.fold
          (fun ((n, j) : Stage.t) (id : IL.nodei) (acc : state option) ->
            if Int.equal i j && mem i n then
              Some (join (Option.value acc ~default:Dead) mapping.(id).D.in_env)
            else acc)
          ids None
      in
      let states = List.mapi (fun (i : int) (_ : anchor) -> state_at i) anchors in
      let verdict =
        match state_at last with
        | Some (Live _) -> Feasible
        | Some Dead -> Infeasible
        | None -> Unknown
      in
      (verdict, states)
