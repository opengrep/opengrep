module type LATTICE = sig
  type t

  val equal : t -> t -> bool
  val join : t -> t -> t
end

module type GRAPH = sig
  type t

  module V : sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

  val mem_edge : t -> V.t -> V.t -> bool
end

module type STORE = sig
  type t
  type node
  type lattice

  val get : node -> t -> lattice
  val set : node -> lattice -> t -> t
end

module Make
    (G : GRAPH)
    (L : LATTICE)
    (S : STORE with type node = G.V.t and type lattice = L.t) =
struct
  type analyze = G.V.t -> S.t -> S.t

  let analyze_joined ~(analyze : analyze) (v : G.V.t) (store : S.t) : S.t =
    let before = S.get v store in
    let store' = analyze v store in
    S.set v (L.join before (S.get v store')) store'

  (* A round replaces the value of every member while the rounds produce
     new values. A round that repeats the values of an earlier round would
     make them cycle; from that round on a member's value joins its value
     of the previous round, so the rounds ascend and stop. *)
  let run_cycle ~max_iter ~(on_max_iter : G.V.t list -> unit)
      ~(analyze : analyze) (members : G.V.t list) (store : S.t) : S.t =
    let values (store : S.t) : L.t list =
      List.map (fun (v : G.V.t) -> S.get v store) members
    in
    let rec loop ~(joining : bool) (history : L.t list list) (store : S.t)
        (iter : int) : S.t =
      if iter >= max_iter then (
        on_max_iter members;
        store)
      else
        let before = values store in
        let step = if joining then analyze_joined ~analyze else analyze in
        let store' =
          List.fold_left (fun (s : S.t) (v : G.V.t) -> step v s) store members
        in
        let after = values store' in
        if List.for_all2 L.equal before after then store'
        else
          let repeats = List.exists (List.for_all2 L.equal after) history in
          loop ~joining:(joining || repeats) (before :: history) store'
            (iter + 1)
    in
    loop ~joining:false [] store 0

  let run ?(max_iter = 20) ?(on_max_iter = fun _ -> ()) ~sccs ~graph ~analyze
      (store : S.t) : S.t =
    List.fold_left
      (fun store scc ->
        match scc with
        | [] -> store
        | [ v ] when not (G.mem_edge graph v v) -> analyze v store
        | members -> run_cycle ~max_iter ~on_max_iter ~analyze members store)
      store sccs
end
