type placement =
  | Prepended
  | Appended

type 'c parent =
  | Bound of placement * 'c
  | Unbound of placement

type 'c linearisation = {
  order : 'c list;
  complete : bool;
}

type 'c element =
  | Class of 'c
  | Unbound_parent of 'c * int
  | Rest_of of 'c

let is_prepended (placement : placement) : bool =
  match placement with
  | Prepended -> true
  | Appended -> false

let c3 (type c) ~(equal : c -> c -> bool) ~(hash : c -> int)
    ~(parents : c -> c parent list list) : c -> c linearisation =
  let module Memo = Hashtbl.Make (struct
    type t = c

    let equal = equal
    let hash = hash
  end) in
  let memo : c linearisation Memo.t = Memo.create 64 in
  let same (left : c element) (right : c element) : bool =
    match (left, right) with
    | Class left, Class right
    | Rest_of left, Rest_of right ->
        equal left right
    | Unbound_parent (left, left_index), Unbound_parent (right, right_index) ->
        equal left right && Int.equal left_index right_index
    | (Class _ | Unbound_parent _ | Rest_of _), _ -> false
  in
  let is_known (element : c element) : bool =
    match element with
    | Class _ -> true
    | Unbound_parent _
    | Rest_of _ ->
        false
  in
  let in_tail (sequences : c element list list) (element : c element) : bool =
    List.exists
      (fun (sequence : c element list) ->
        match sequence with
        | _ :: tail -> List.exists (same element) tail
        | [] -> false)
      sequences
  in
  let successors (sequences : c element list list) (element : c element) :
      c element list =
    let rec after (sequence : c element list) : c element list =
      match sequence with
      | current :: (next :: _ as rest) ->
          if same current element then next :: after rest else after rest
      | [ _ ]
      | [] ->
          []
    in
    List.concat_map after sequences
  in
  let followers (sequences : c element list list) (element : c element) :
      c element list =
    let rec visit (seen : c element list) (pending : c element list) =
      match pending with
      | [] -> seen
      | current :: rest ->
          let fresh =
            successors sequences current
            |> List.filter (fun (next : c element) ->
                   not (List.exists (same next) seen))
            |> List_.uniq_by same
          in
          visit (fresh @ seen) (fresh @ rest)
    in
    visit [] [ element ]
  in
  let certain (sequences : c element list list) (candidate : c element) : bool
      =
    let unknown =
      List.filter (fun (element : c element) -> not (is_known element))
        (List.concat sequences)
    in
    match unknown with
    | [] -> true
    | _ :: _ ->
        let after = followers sequences candidate in
        List.for_all
          (fun (element : c element) -> List.exists (same element) after)
          unknown
  in
  let remove (head : c element) (sequences : c element list list) :
      c element list list =
    List.filter_map
      (fun (sequence : c element list) ->
        match sequence with
        | first :: rest when same first head -> (
            match rest with
            | [] -> None
            | _ :: _ -> Some rest)
        | _ :: _ -> Some sequence
        | [] -> None)
      sequences
  in
  let rec merge (merged : c list) (sequences : c element list list) :
      c list * bool =
    match sequences with
    | [] -> (List.rev merged, true)
    | _ :: _ -> (
        let head =
          List.find_map
            (fun (sequence : c element list) ->
              match sequence with
              | first :: _ when not (in_tail sequences first) -> Some first
              | _ -> None)
            sequences
        in
        match head with
        | Some (Class cls as head) when certain sequences head ->
            merge (cls :: merged) (remove head sequences)
        | Some _
        | None ->
            (List.rev merged, false))
  in
  let rec known_prefix (prefix : c list) (elements : c element list) :
      c list * bool =
    match elements with
    | Class cls :: rest -> known_prefix (cls :: prefix) rest
    | (Unbound_parent _ | Rest_of _) :: _ -> (List.rev prefix, false)
    | [] -> (List.rev prefix, true)
  in
  let rec linearise (active : c list) (cls : c) : c linearisation =
    match Memo.find_opt memo cls with
    | Some linearisation -> linearisation
    | None when List.exists (equal cls) active ->
        { order = [ cls ]; complete = true }
    | None ->
        let _, definitions =
          List.fold_left_map
            (fun (next : int) (sequence : c parent list) ->
              ( next + List.length sequence,
                List.mapi
                  (fun (index : int) (parent : c parent) ->
                    chain (cls :: active) cls (next + index) parent)
                  sequence ))
            0 (parents cls)
        in
        let is_prepended_chain
            ((placement, _, _) : placement * c element * c element list) : bool
            =
          is_prepended placement
        in
        let prepended =
          List.filter is_prepended_chain (List.concat definitions)
        in
        let appended =
          List.map
            (List.filter (fun chain -> not (is_prepended_chain chain)))
            definitions
        in
        let before, before_complete =
          known_prefix []
            (List.concat_map (fun (_, _, sequence) -> sequence) prepended)
        in
        let linearisation =
          if before_complete then
            let sequences =
              List.concat_map
                (List.map (fun (_, _, sequence) -> sequence))
                appended
              @ List.map (List.map (fun (_, element, _) -> element)) appended
              |> List.filter (fun (sequence : c element list) ->
                     match sequence with
                     | [] -> false
                     | _ :: _ -> true)
            in
            let merged, complete = merge [] sequences in
            { order = List_.uniq_by equal (before @ (cls :: merged)); complete }
          else { order = List_.uniq_by equal before; complete = false }
        in
        Memo.replace memo cls linearisation;
        linearisation
  and chain (active : c list) (owner : c) (index : int) (parent : c parent) :
      placement * c element * c element list =
    match parent with
    | Bound (placement, cls) ->
        let linearisation = linearise active cls in
        let rest = if linearisation.complete then [] else [ Rest_of cls ] in
        ( placement,
          Class cls,
          List.map (fun (known : c) -> Class known) linearisation.order @ rest )
    | Unbound placement ->
        let element = Unbound_parent (owner, index) in
        (placement, element, [ element ])
  in
  linearise []
