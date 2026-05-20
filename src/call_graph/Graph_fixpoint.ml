module type LATTICE = sig
  type t

  val equal : t -> t -> bool
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

  let run_singleton ~graph ~(analyze : analyze) ~max_iter
      ~(on_max_iter : G.V.t list -> unit) (v : G.V.t) (store : S.t) : S.t =
    if not (G.mem_edge graph v v) then analyze v store
    else
      let rec loop store iter =
        if iter >= max_iter then (
          on_max_iter [ v ];
          store)
        else
          let before = S.get v store in
          let store' = analyze v store in
          let after = S.get v store' in
          if L.equal before after then store' else loop store' (iter + 1)
      in
      loop store 0

  let run_scc ~max_iter ~(on_max_iter : G.V.t list -> unit)
      ~(analyze : analyze) (members : G.V.t list) (store : S.t) : S.t =
    let rec loop store iter =
      if iter >= max_iter then (
        on_max_iter members;
        store)
      else
        let snapshot = List.map (fun v -> (v, S.get v store)) members in
        let store' =
          List.fold_left (fun s v -> analyze v s) store members
        in
        let stable =
          List.for_all
            (fun (v, prev) -> L.equal prev (S.get v store'))
            snapshot
        in
        if stable then store' else loop store' (iter + 1)
    in
    loop store 0

  let run ?(max_iter = 20) ?(on_max_iter = fun _ -> ()) ~sccs ~graph ~analyze
      (store : S.t) : S.t =
    List.fold_left
      (fun store scc ->
        match scc with
        | [] -> store
        | [ v ] -> run_singleton ~graph ~analyze ~max_iter ~on_max_iter v store
        | members -> run_scc ~max_iter ~on_max_iter ~analyze members store)
      store sccs
end
