(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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

(* TODO: we should use ocamlgraph instead of ograph and its Ograph_extended.ml
 *)

type nodei = Ograph_extended.nodei

module NodeiSet = Set.Make (Int)

(* A graph with its entry point and (for convenience) the pre-computed set of
 * reachable nodes. Since the node type is abstract, we do need the id of the
 * entry node to compute the set of reachable nodes in the graph. *)
type ('node, 'edge) t = {
  graph : ('node, 'edge) Ograph_extended.ograph_mutable;
  entry : nodei;
  exit : nodei;
  reachable : NodeiSet.t;
  (* The reachable nodes in Bourdoncle's weak topological ordering
   * (F. Bourdoncle, "Efficient chaotic iteration strategies with
   * widenings", 1993). Each strongly connected region is a component: its
   * head comes first, its nested components follow inside it, and every
   * node after the component comes after all of it. *)
  weak_topological_order : nodei array;
  order_index : int array;
  loop_header : int array;
  max_loop_depth : int;
}

type ('node, 'edge) cfg = ('node, 'edge) t

type wto_element = Vertex of nodei | Component of nodei * wto_element list

let make (graph : _ Ograph_extended.ograph_mutable) entry exit : _ t =
  let max_nodei =
    graph#nodes#fold (fun acc (ni, _) -> Int.max acc ni) (-1)
  in
  let successors (ni : nodei) : nodei list =
    (graph#successors ni)#fold
      (fun s (succ, _) -> NodeiSet.add succ s)
      NodeiSet.empty
    |> NodeiSet.elements
  in
  let dfn = Array.make (max_nodei + 1) 0 in
  let rec visit (v : nodei)
      ((num, stack, partition) : int * nodei list * wto_element list) :
      int * (int * nodei list * wto_element list) =
    let v_num = num + 1 in
    dfn.(v) <- v_num;
    let head, loop, (num, stack, partition) =
      List.fold_left
        (fun (head, loop, state) (w : nodei) ->
          let min, state =
            if Int.equal dfn.(w) 0 then visit w state else (dfn.(w), state)
          in
          if min <= head then (min, true, state) else (head, loop, state))
        (v_num, false, (v_num, v :: stack, partition))
        (successors v)
    in
    if not (Int.equal head v_num) then (head, (num, stack, partition))
    else (
      dfn.(v) <- max_int;
      let rec unwind (stack : nodei list) : nodei list =
        match stack with
        | w :: rest when not (Int.equal w v) ->
            dfn.(w) <- 0;
            unwind rest
        | _ :: rest
        | ([] as rest) ->
            rest
      in
      let stack = unwind stack in
      if loop then
        let nested, (num, stack) = component v (num, stack) in
        (head, (num, stack, Component (v, nested) :: partition))
      else (head, (num, stack, Vertex v :: partition)))
  and component (v : nodei) ((num, stack) : int * nodei list) :
      wto_element list * (int * nodei list) =
    let num, stack, partition =
      List.fold_left
        (fun state (w : nodei) ->
          if Int.equal dfn.(w) 0 then snd (visit w state) else state)
        (num, stack, []) (successors v)
    in
    (partition, (num, stack))
  in
  let _, (_, _, wto) = visit entry (0, [], []) in
  let rec flatten (header : int) (depth : int)
      (acc : (nodei * int * int) list) (element : wto_element) :
      (nodei * int * int) list =
    match element with
    | Vertex ni -> (ni, header, depth) :: acc
    | Component (head, nested) ->
        List.fold_left
          (flatten head (depth + 1))
          ((head, head, depth + 1) :: acc)
          nested
  in
  let placed = List.rev (List.fold_left (flatten (-1) 0) [] wto) in
  let weak_topological_order =
    Array.of_list (List.map (fun (ni, _, _) -> ni) placed)
  in
  let order_index = Array.make (max_nodei + 1) (-1) in
  let loop_header = Array.make (max_nodei + 1) (-1) in
  List.iteri
    (fun i (ni, header, _) ->
      order_index.(ni) <- i;
      loop_header.(ni) <- header)
    placed;
  {
    graph;
    entry;
    exit;
    reachable = NodeiSet.of_seq (Array.to_seq weak_topological_order);
    weak_topological_order;
    order_index;
    loop_header;
    max_loop_depth =
      List.fold_left (fun acc (_, _, depth) -> Int.max acc depth) 0 placed;
  }

let reachable_nodes cfg =
  cfg.reachable |> NodeiSet.to_seq |> Seq.map cfg.graph#nodes#assoc

(* Predecessors of a node (that can be reached from the entry node). *)
let predecessors cfg nodei : (nodei * 'node) list =
  (cfg.graph#predecessors nodei)#tolist
  |> List.filter (fun (pi, _) -> NodeiSet.mem pi cfg.reachable)

(* Successors of a node (returns an empty list for unreachable nodes). *)
let successors cfg nodei : (nodei * 'node) list =
  if NodeiSet.mem nodei cfg.reachable then (cfg.graph#successors nodei)#tolist
  else []
