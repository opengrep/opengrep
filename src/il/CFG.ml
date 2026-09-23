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
  reverse_postorder : nodei array;
  reverse_postorder_index : int array;
  loop_header : int array;
  max_loop_depth : int;
}

type ('node, 'edge) cfg = ('node, 'edge) t

(* For each node, the entry node of its innermost loop (-1 outside loops),
 * and the deepest loop nesting. *)
let loops_of (graph : _ Ograph_extended.ograph_mutable)
    (reverse_postorder : nodei array) (reverse_postorder_index : int array) :
    int array * int =
  let index (ni : nodei) : int = reverse_postorder_index.(ni) in
  let has_back_edge =
    Array.exists
      (fun (h : nodei) ->
        (graph#predecessors h)#fold
          (fun found (p, _) -> found || index p >= index h)
          false)
      reverse_postorder
  in
  if not has_back_edge then ([||], 0)
  else
    let size = Array.length reverse_postorder_index in
    let loop_header = Array.make size (-1) in
    let loop_depth = Array.make size 0 in
    let mark = Array.make size (-1) in
    let stack = Array.make size 0 in
    let top = ref 0 in
    let header = ref 0 in
    let enter (ni : nodei) : unit =
      mark.(ni) <- !header;
      loop_header.(ni) <- !header;
      loop_depth.(ni) <- loop_depth.(ni) + 1
    in
    let push () ((p, _) : nodei * _) : unit =
      if index p >= 0 && not (Int.equal mark.(p) !header) then (
        enter p;
        stack.(!top) <- p;
        incr top)
    in
    let push_back_source () ((p, _) as edge : nodei * _) : unit =
      if index p >= index !header then push () edge
    in
    Array.iter
      (fun (h : nodei) ->
        header := h;
        top := 0;
        (graph#predecessors h)#fold push_back_source ();
        if !top > 0 then (
          if not (Int.equal mark.(h) h) then enter h;
          while !top > 0 do
            decr top;
            let ni = stack.(!top) in
            if not (Int.equal ni h) then (graph#predecessors ni)#fold push ()
          done))
      reverse_postorder;
    (loop_header, Array.fold_left Int.max 0 loop_depth)

let make (graph : _ Ograph_extended.ograph_mutable) entry exit : _ t =
  let rec aux nodei ((seen, finished) : NodeiSet.t * nodei list) =
    if NodeiSet.mem nodei seen then (seen, finished)
    else
      let seen = NodeiSet.add nodei seen in
      let succs =
        (graph#successors nodei)#fold
          (fun s (ni, _) -> NodeiSet.add ni s)
          NodeiSet.empty
      in
      let seen, finished = NodeiSet.fold aux succs (seen, finished) in
      (seen, nodei :: finished)
  in
  let reachable, finished = aux entry (NodeiSet.empty, []) in
  let reverse_postorder = Array.of_list finished in
  let max_nodei =
    graph#nodes#fold (fun acc (ni, _) -> Int.max acc ni) (-1)
  in
  let reverse_postorder_index = Array.make (max_nodei + 1) (-1) in
  Array.iteri
    (fun i ni -> reverse_postorder_index.(ni) <- i)
    reverse_postorder;
  let loop_header, max_loop_depth =
    loops_of graph reverse_postorder reverse_postorder_index
  in
  {
    graph;
    entry;
    exit;
    reachable;
    reverse_postorder;
    reverse_postorder_index;
    loop_header;
    max_loop_depth;
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
