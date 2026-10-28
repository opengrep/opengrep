(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
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
module Log = Log_analyzing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Dataflow analysis "framework".
 *
 * The goal of a dataflow analysis is to store information about each
 * variable at each program point, that is each node in a CFG
 * (e.g. whether a variable is "live" at a program point).
 * As you may want different kinds of information, the types below
 * are polymorphic. But each take as a key a variable name.
 *
 * This file used to be called Dataflow.ml, but this conflicts with
 * OCaml compiler-libs/dataflow.ml which is exposed when compiling
 * with 4.12.0+domains (a.k.a. OCaml multicore preview).
 *
 * todo:
 *  - could use a functor, so would not have all those 'a?
 *  - do we need other kind of information than variable environment?
 *    Dataflow analysis talks only about variables? for the belief analysis
 *    we actually want expressions instead.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* I was using directly Controlflow.xxx before, but now that we have both
 * Controlflow.flow and Il.cfg, we need to functorize things.
 *)

module type Flow = sig
  type node
  type edge
  type flow = (node, edge) CFG.t

  val short_string_of_node : node -> string
end

type nodei = int

module NodeiSet = Set.Make (Int)

(* The final dataflow result; a map from each program point to a map containing
 * information for each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to Iain
 * so let's use array.
 *)
type 'env mapping = 'env inout array

(* the In and Out sets, as in Appel Modern Compiler in ML book *)
and 'env inout = { in_env : 'env; out_env : 'env }

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

let eq_inout eq_env io1 io2 =
  let eqe = eq_env in
  eqe io1.in_env io2.in_env && eqe io1.out_env io2.out_env

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v = if String.length s =|= 0 then v else s ^ "," ^ v

let array_fold_left_idx f =
  let idx = ref 0 in
  Array.fold_left (fun v e ->
      let r = f v !idx e in
      incr idx;
      r)

let ns_to_str ns =
  "{" ^ NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^ "}"

let (inout_to_str : ('env -> string) -> 'env inout -> string) =
 fun env_to_str inout ->
  spf "IN= %15s  OUT = %15s" (env_to_str inout.in_env)
    (env_to_str inout.out_env)

(*****************************************************************************)
(* Main generic entry point *)
(*****************************************************************************)

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'env transfn = 'env mapping -> nodei -> 'env inout

module Make (F : Flow) = struct
  let mapping_to_str (f : F.flow) env_to_str mapping =
    array_fold_left_idx
      (fun s ni v ->
        s
        ^ spf "%2d <- %7s: %15s %s\n" ni
            ((f.graph#predecessors ni)#fold
               (fun s (ni, _) -> csv_append s (string_of_int ni))
               "")
            (F.short_string_of_node (f.graph#nodes#find ni))
            (inout_to_str env_to_str v))
      "" mapping

  let (display_mapping : F.flow -> 'env mapping -> ('env -> string) -> unit) =
   fun flow mapping env_to_str ->
    (* nosemgrep: no-print-in-semgrep *)
    UCommon.pr (mapping_to_str flow env_to_str mapping)

  module WTO = Graph.WeakTopological

  (* Bourdoncle's recursive iteration strategy over the weak topological
     order: a vertex is computed once, after its predecessors outside the
     cycles it is in; a component computes its head, then its body, until the
     head is stable or has used its rounds. *)
  let fixpoint_worker ~timeout:_ eq_env mapping trans (flow : F.flow) succs
      (start : nodei) workset =
    let max_visits_per_node = Limits_semgrep.taint_MAX_VISITS_PER_NODE in
    (* Whether every component stopped because its head was stable, not
       because it had used its rounds: the result is then a fixpoint. *)
    let converged = ref true in
    (* A node is computed on its first visit and again only when a
       predecessor has changed since: [dirty] holds the nodes to compute. *)
    let dirty = Array.make (Array.length mapping) false in
    NodeiSet.iter (fun (ni : nodei) -> dirty.(ni) <- true) workset;
    (* Computes [ni] again if it is dirty; whether its value changed. *)
    let update (ni : nodei) : bool =
      if not dirty.(ni) then false
      else (
        dirty.(ni) <- false;
        let old = mapping.(ni) in
        let new_ = trans mapping ni in
        if eq_inout eq_env old new_ then false
        else (
          mapping.(ni) <- new_;
          (* [update] reads only the nodes of [workset]. *)
          NodeiSet.iter (fun (succ : nodei) -> dirty.(succ) <- true) (succs flow ni);
          true))
    in
    (* The rounds of a component, counted over the whole fixpoint: a loop
       nested in another gets [taint_MAX_VISITS_PER_NODE] rounds in all, not
       that many in every round of the enclosing loop. *)
    let rounds_done = Array.make (Array.length mapping) 0 in
    let rec run (element : nodei WTO.element) : unit =
      match element with
      | WTO.Vertex ni -> ignore (update ni : bool)
      | WTO.Component (head, body) ->
          ignore (update head : bool);
          let rec rounds () =
            WTO.fold_left (fun () (element : nodei WTO.element) -> run element) () body;
            (* The head is computed after the last round too, so that the
               nodes after the component see what the body last produced. *)
            if update head then
              if rounds_done.(head) < max_visits_per_node then (
                rounds_done.(head) <- rounds_done.(head) + 1;
                rounds ())
              else converged := false
          in
          rounds ()
    in
    (* The weak topological order (Bourdoncle, "Efficient chaotic iteration
       strategies with widenings", 1993) of the nodes of [workset] reachable
       from [start]; the nodes it does not reach follow, in index order. *)
    let module Workset_graph = struct
      type t = unit

      module V = struct
        type t = nodei

        let compare = Int.compare
        let equal = Int.equal
        let hash = Hashtbl.hash
      end

      let iter_vertex (f : nodei -> unit) () : unit = NodeiSet.iter f workset

      let iter_succ (f : nodei -> unit) () (ni : nodei) : unit =
        NodeiSet.iter (fun (succ : nodei) -> if NodeiSet.mem succ workset then f succ) (succs flow ni)
    end in
    let module Order = WTO.Make (Workset_graph) in
    let rec vertices (acc : NodeiSet.t) (element : nodei WTO.element) : NodeiSet.t =
      match element with
      | WTO.Vertex ni -> NodeiSet.add ni acc
      | WTO.Component (head, body) -> WTO.fold_left vertices (NodeiSet.add head acc) body
    in
    (if NodeiSet.mem start workset then (
       let order = Order.recursive_scc () start in
       WTO.fold_left (fun () (element : nodei WTO.element) -> run element) () order;
       NodeiSet.diff workset (WTO.fold_left vertices NodeiSet.empty order)
       |> NodeiSet.iter (fun (ni : nodei) -> ignore (update ni : bool)))
     else NodeiSet.iter (fun (ni : nodei) -> ignore (update ni : bool)) workset);
    if !converged then (mapping, `Ok) else (mapping, `Timeout)

  let forward_succs (f : F.flow) n =
    (f.graph#successors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let backward_succs (f : F.flow) n =
    (f.graph#predecessors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let (fixpoint :
        timeout:float ->
        eq_env:('env -> 'env -> bool) ->
        init:'env mapping ->
        trans:'env transfn ->
        flow:F.flow ->
        forward:bool ->
        'env mapping * [ `Ok | `Timeout ]) =
   fun ~timeout ~eq_env ~init ~trans ~flow ~forward ->
    let succs = if forward then forward_succs else backward_succs in
    let work =
      (* This prevents dead code from getting analyzed. *)
      flow.reachable
    in
    let start = if forward then flow.entry else flow.exit in
    fixpoint_worker ~timeout eq_env init trans flow succs start work

  (*****************************************************************************)
  (* Helpers *)
  (*****************************************************************************)

  let new_node_array (f : F.flow) v =
    let nb_nodes = f.graph#nb_nodes in
    let max_nodei = ref (-1) in

    f.graph#nodes#tolist
    |> List.iter (fun (ni, _nod) ->
           (* actually there are some del_node done in cfg_build, for
            * switch, so sometimes ni is >= len
            *
            * old:
            * if ni >= nb_nodes
            * then pr2 "the CFG nodei is bigger than the number of nodes"
            *)
           if ni > !max_nodei then max_nodei := ni);
    assert (!max_nodei + 1 >= nb_nodes);
    Array.make (!max_nodei + 1) v
end

(*
module F = Controlflow
module X1 = Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  let short_string_of_node = F.short_string_of_node
end
)
*)
