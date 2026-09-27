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

type trace_step =
  | Entry
  | Token of Tok.t
  | Range of Tok.location * Tok.location
  | Call of IL.exp
  | Exit

type state =
  | Unreachable
  | Reachable of { values : G.svalue Var_env.t; literals : (IL.exp * bool) list }

type span = { span_node : IL.nodei; first : int; last : int; stop : int }

type index = {
  entry_node : IL.nodei;
  exit_node : IL.nodei;
  spans : span list;
  calls : (IL.nodei * IL.exp) list;
  params : int list;
  aliased : IL.name list;
  aliased_keys : string list;
  call_killed_keys : string list;
}

module Product_state = struct
  type t = IL.nodei * int
end


type product_node = {
  il_node : IL.node option;
  (* The state of the automaton over the trace steps, in the product of the
     CFG with that automaton: the index of the last trace step matched. *)
  automaton_state : int;
}

module Product = Dataflow_core.Make (struct
  type node = product_node
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
  let aliased = written_in_lambdas fun_cfg @ referenced fun_cfg in
  let nonlocal_written =
    nodes
    |> List.filter_map (fun ((_, node) : IL.nodei * IL.node) ->
           match node.n with
           | NInstr instr -> LV.lval_of_instr_opt instr
           | _ -> None)
    |> base_vars
    |> List.filter (fun (name : IL.name) -> not (is_local name))
  in
  {
    entry_node = flow.entry;
    exit_node = flow.exit;
    spans;
    calls;
    params;
    aliased;
    aliased_keys = List.map IL.str_of_name aliased;
    call_killed_keys = List.map IL.str_of_name (aliased @ nonlocal_written);
  }

let covering (ix : index) (first : int) (last : int) : IL.nodei list =
  ix.spans
  |> List.filter (fun (s : span) -> s.first <= first && last <= s.last)
  |> List.map (fun (s : span) -> s.span_node)

let nodes_of_trace_step (ix : index) (trace_step : trace_step) : IL.nodei list =
  match trace_step with
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

let equal_location_of_tok (tok1 : Tok.t) (tok2 : Tok.t) : bool =
  match (Tok.loc_of_tok tok1, Tok.loc_of_tok tok2) with
  | Ok loc1, Ok loc2 -> Tok.equal_location loc1 loc2
  | Error _, _
  | _, Error _ ->
      false

let equal_trace_step (a1 : trace_step) (a2 : trace_step) : bool =
  match (a1, a2) with
  | Entry, Entry
  | Exit, Exit ->
      true
  | Token tok1, Token tok2 ->
      Common.phys_equal tok1 tok2 || equal_location_of_tok tok1 tok2
  | Range (first1, last1), Range (first2, last2) ->
      Tok.equal_location first1 first2 && Tok.equal_location last1 last2
  | Call e1, Call e2 -> Common.phys_equal e1 e2
  | (Entry | Exit | Token _ | Range _ | Call _), _ -> false

let hash_trace_steps (trace_steps : trace_step list) : int =
  trace_steps
  |> List.map (fun (trace_step : trace_step) ->
         match trace_step with
         | Entry -> -1
         | Exit -> -2
         | Call _ -> -3
         | Token tok -> Option.value (start_of tok) ~default:(-4)
         | Range (first, _) -> first.pos.bytepos)
  |> Hashtbl.hash

let entry_state (bindings : (IL.name * G.svalue) list) : state =
  Reachable
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
  | Unreachable -> G.NotCst
  | Reachable { values; _ } -> Eval.eval (Eval.mk_env lang values) e

let literals (s : state) : (IL.exp * bool) list =
  match s with
  | Unreachable -> []
  | Reachable { literals; _ } -> literals

let refutes (s : state) (lits : (IL.exp * bool) list) : bool =
  match s with
  | Unreachable -> false
  | Reachable { literals; _ } -> not (LV.literals_consistent (lits @ literals))

let equal_literal ((a1, n1) : IL.exp * bool) ((a2, n2) : IL.exp * bool) : bool
    =
  Bool.equal n1 n2 && LV.equal_exp a1 a2

let equal_state (s1 : state) (s2 : state) : bool =
  match (s1, s2) with
  | Unreachable, Unreachable -> true
  | Reachable l1, Reachable l2 ->
      Var_env.eq_env Eval.eq l1.values l2.values
      && Int.equal (List.length l1.literals) (List.length l2.literals)
      && List.for_all
           (fun (l : IL.exp * bool) -> List.exists (equal_literal l) l2.literals)
           l1.literals
  | Unreachable, Reachable _
  | Reachable _, Unreachable ->
      false

let join (s1 : state) (s2 : state) : state =
  match (s1, s2) with
  | Unreachable, s
  | s, Unreachable ->
      s
  | Reachable l1, Reachable l2 ->
      Reachable
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
  then Unreachable
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
      Reachable
        {
          values;
          literals =
            List.filter
              (fun (l : IL.exp * bool) -> not (List.exists (equal_literal l) known))
              undecided
            @ known;
        }
    else Unreachable

let not_killed_by (ix : index) (instr : IL.instr) : IL.exp * bool -> bool =
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
  fun ((atom, _) : IL.exp * bool) ->
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
  let reads_any (names : IL.name list) : bool =
    List.exists
      (fun (v : IL.name) -> List.exists (IL.equal_name v) names)
      read
  in
  let reads_aliased = reads_any ix.aliased in
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
        && not reads_aliased)
  && ((not writes_object) || not (object_read || reads_aliased))

let transfer (lang : Lang.t) (fun_cfg : IL.fun_cfg) (ix : index) (s : state)
    (node : IL.node) : state =
  match s with
  | Unreachable -> Unreachable
  | Reachable { values; literals } -> (
      let values' = Dataflow_svalue.transfer_node_without_writes lang fun_cfg values node in
      match node.n with
      | TrueNode cond -> assume lang values literals cond true
      | FalseNode cond -> assume lang values literals cond false
      | NInstr instr ->
          let killed =
            match instr.i with
            | Call _
            | CallSpecial _
            | New _ ->
                ix.call_killed_keys
            | Assign _
            | AugmentedAssign _
            | AssignAnon _
            | FixmeInstr _ -> (
                match LV.lval_of_instr_opt instr with
                | Some { base = Var _; rev_offset = [] }
                | None ->
                    []
                | Some { base = Var _ | VarSpecial _ | Mem _; _ } ->
                    ix.aliased_keys)
          in
          Reachable
            {
              values =
                List.fold_left
                  (fun (values : G.svalue Var_env.t) (key : string) ->
                    VarMap.remove key values)
                  values' killed;
              literals =
                (match literals with
                | [] -> []
                | _ -> List.filter (not_killed_by ix instr) literals);
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
          Reachable { values = values'; literals })

let check (lang : Lang.t) (fun_cfg : IL.fun_cfg) (ix : index)
    ~(entry : state) (trace_steps : trace_step list) : verdict * state option list =
  let sets =
    trace_steps
    |> List.map (fun (trace_step : trace_step) ->
           CFG.NodeiSet.of_list (nodes_of_trace_step ix trace_step))
    |> Array.of_list
  in
  let last = Array.length sets - 1 in
  let unknown = (Unknown, List.map (fun (_ : trace_step) -> None) trace_steps) in
  if last < 0 || Array.exists CFG.NodeiSet.is_empty sets then unknown
  else
    let flow = fun_cfg.cfg in
    let mem (i : int) (n : IL.nodei) : bool = CFG.NodeiSet.mem n sets.(i) in
    let rec advance (i : int) (n : IL.nodei) (acc : int list) : int list =
      if i < last && mem (i + 1) n then advance (i + 1) n ((i + 1) :: acc)
      else acc
    in
    let product_states_at (i : int) (n : IL.nodei) : Product_state.t list =
      List.map (fun (j : int) -> (n, j)) (advance i n [ i ])
    in
    let successors ((n, i) : Product_state.t) : Product_state.t list =
      CFG.successors flow n
      |> List.concat_map (fun ((m, _) : IL.nodei * IL.edge) -> product_states_at i m)
    in
    let starts =
      if mem 0 ix.entry_node then product_states_at 0 ix.entry_node else []
    in
    let width = last + 1 in
    let slot ((n, i) : Product_state.t) : int = (n * width) + i in
    let product_state_of_slot (k : int) : Product_state.t = (k / width, k mod width) in
    let slots = Array.length flow.order_index * width in
    let reached = Array.make slots false in
    List.iter (fun (p : Product_state.t) -> reached.(slot p) <- true) starts;
    let rec forward (edges : (Product_state.t * Product_state.t) list) (seen : int list)
        (todo : Product_state.t list) : (Product_state.t * Product_state.t) list * int list =
      match todo with
      | [] -> (edges, seen)
      | p :: rest ->
          let succs = successors p in
          let fresh =
            List.filter (fun (q : Product_state.t) -> not reached.(slot q)) succs
          in
          List.iter (fun (q : Product_state.t) -> reached.(slot q) <- true) fresh;
          forward
            (List.rev_append (List.map (fun (q : Product_state.t) -> (p, q)) succs) edges)
            (List.rev_append (List.map slot fresh) seen)
            (List.rev_append fresh rest)
    in
    let edges, seen = forward [] (List.map slot starts) starts in
    let reached_slots = List.sort_uniq Int.compare seen in
    let finals =
      List.filter
        (fun (k : int) ->
          let n, i = product_state_of_slot k in
          Int.equal i last && mem last n)
        reached_slots
    in
    let predecessors = Array.make slots [] in
    List.iter
      (fun ((p, q) : Product_state.t * Product_state.t) ->
        predecessors.(slot q) <- p :: predecessors.(slot q))
      edges;
    let coreachable = Array.make slots false in
    List.iter (fun (k : int) -> coreachable.(k) <- true) finals;
    let rec backward (todo : Product_state.t list) : unit =
      match todo with
      | [] -> ()
      | q :: rest ->
          let preds =
            List.filter (fun (p : Product_state.t) -> not coreachable.(slot p))
              predecessors.(slot q)
          in
          List.iter (fun (p : Product_state.t) -> coreachable.(slot p) <- true) preds;
          backward (List.rev_append preds rest)
    in
    backward (List.map product_state_of_slot finals);
    if List.is_empty finals then unknown
    else
      let graph = new Ograph_extended.ograph_mutable in
      let start = graph#add_node { il_node = None; automaton_state = 0 } in
      let relevant = List.filter (fun (k : int) -> coreachable.(k)) reached_slots in
      let ids = Array.make slots (-1) in
      List.iter
        (fun (k : int) ->
          let n, i = product_state_of_slot k in
          ids.(k) <-
            graph#add_node
              { il_node = Some (flow.graph#nodes#assoc n); automaton_state = i })
        relevant;
      let id_of (p : Product_state.t) : IL.nodei option =
        match ids.(slot p) with
        | -1 -> None
        | id -> Some id
      in
      List.iter
        (fun (p : Product_state.t) ->
          match id_of p with
          | Some id -> graph#add_arc ((start, id), IL.Direct)
          | None -> ())
        starts;
      List.iter
        (fun ((p, q) : Product_state.t * Product_state.t) ->
          match (id_of p, id_of q) with
          | Some pid, Some qid -> graph#add_arc ((pid, qid), IL.Direct)
          | _ -> ())
        edges;
      let product = CFG.make graph start start in
      let trans (mapping : state D.mapping) (pi : IL.nodei) : state D.inout =
        let product_node = product.graph#nodes#assoc pi in
        let in_state =
          if Int.equal pi start then entry
          else
            CFG.predecessors product pi
            |> List.fold_left
                 (fun (acc : state) ((pp, _) : IL.nodei * IL.edge) ->
                   join acc mapping.(pp).D.out_env)
                 Unreachable
        in
        let out_state =
          match product_node.il_node with
          | Some node -> transfer lang fun_cfg ix in_state node
          | None -> in_state
        in
        { D.in_env = in_state; out_env = out_state }
      in
      let mapping =
        Product.fixpoint ~eq_env:equal_state ~join
          ~init:(Product.new_node_array product { D.in_env = Unreachable; out_env = Unreachable })
          ~trans ~flow:product
      in
      let state_at (i : int) : state option =
        List.fold_left
          (fun (acc : state option) (k : int) ->
            let n, j = product_state_of_slot k in
            if Int.equal i j && mem i n then
              Some
                (join (Option.value acc ~default:Unreachable) mapping.(ids.(k)).D.in_env)
            else acc)
          None relevant
      in
      let states = List.mapi (fun (i : int) (_ : trace_step) -> state_at i) trace_steps in
      let verdict =
        match state_at last with
        | Some (Reachable _) -> Feasible
        | Some Unreachable -> Infeasible
        | None -> Unknown
      in
      (verdict, states)
