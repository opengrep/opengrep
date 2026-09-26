type relation =
  | Extends of { constructed : bool }
  | Implements
  | Mixin
  | Embedded
  | Included
  | Prepended

type 'c parent =
  | Bound of relation * 'c
  | Unbound of relation

type superclass =
  | Written_as_extends
  | Carrying_constructor_arguments
  | Of_class_kind

type mixins =
  | Applied_in_the_chain
  | Flattened_into_the_class

type strategy =
  | C3 of { bases_listed_most_base_first : bool }
  | Scala_class_linearisation
  | Ruby_ancestor_chain
  | Single_inheritance of {
      superclass : superclass;
      interface_bodies_inherited : bool;
      mixins : mixins;
    }
  | Go_embedding_promotion

type 'c candidate = {
  cls : 'c;
  hides : 'c list;
  paths : int;
}

type 'c tier =
  | Candidates of 'c candidate list
  | Unknown_classes

type 'c linearisation = {
  order : 'c list;
  complete : bool;
  tiers : 'c tier list;
  super_tiers : 'c tier list;
}

type ('c, 'a) selection =
  | Selected of 'c * 'a list
  | Ambiguous
  | Undefined
  | Unknown

type 'c element =
  | Class of 'c
  | Unbound_parent of 'c * int
  | Rest_of of 'c

type merge_end =
  | Merged
  | Uncertain
  | Rejected

let follows_receiver (strategy : strategy) : bool =
  match strategy with
  | C3 _
  | Scala_class_linearisation
  | Ruby_ancestor_chain ->
      true
  | Single_inheritance _
  | Go_embedding_promotion ->
      false

let alone (type c) (cls : c) : c candidate = { cls; hides = []; paths = 1 }

let is_unknown (type c) (tier : c tier) : bool =
  match tier with
  | Unknown_classes -> true
  | Candidates _ -> false

let truncated (type c) (tiers : c tier list) : c tier list =
  let rec keep (remaining : c tier list) : c tier list =
    match remaining with
    | [] -> []
    | Unknown_classes :: _ -> [ Unknown_classes ]
    | (Candidates _ as tier) :: rest -> tier :: keep rest
  in
  keep tiers

(* The tiers after the one holding [cls]; when no tier holds it, the lookup
   reaches only what the tiers leave unknown. *)
let after (type c) ~(equal : c -> c -> bool) (cls : c) (tiers : c tier list) :
    c tier list =
  let holds (tier : c tier) : bool =
    match tier with
    | Candidates candidates ->
        List.exists
          (fun (candidate : c candidate) -> equal candidate.cls cls)
          candidates
    | Unknown_classes -> false
  in
  let rec drop (remaining : c tier list) : c tier list =
    match remaining with
    | [] -> if List.exists is_unknown tiers then [ Unknown_classes ] else []
    | tier :: rest -> if holds tier then rest else drop rest
  in
  drop tiers

let select (type c a) ~(equal : c -> c -> bool) ~(defines : c -> a list)
    ~(overrides : nearer:a -> farther:a -> bool) ~(declared_only : a -> bool)
    ~(accumulate : bool) (tiers : c tier list) : (c, a) selection =
  let overridden (nearer : a list) (farther : a) : bool =
    List.exists (fun (found : a) -> overrides ~nearer:found ~farther) nearer
  in
  let finish (definer : c option) (visible : a list) (blocked : a list)
      (unknown : bool) : (c, a) selection =
    match (definer, visible, blocked) with
    | Some definer, _ :: _, _ -> Selected (definer, visible)
    | _, _, _ :: _ -> Ambiguous
    | _ -> if unknown then Unknown else Undefined
  in
  let rec walk (definer : c option) (visible : a list) (blocked : a list)
      (remaining : c tier list) : (c, a) selection =
    match remaining with
    | [] -> finish definer visible blocked false
    | Unknown_classes :: _ -> finish definer visible blocked true
    | Candidates candidates :: rest -> (
        let defining =
          List.concat
            (List.mapi
               (fun (position : int) (candidate : c candidate) ->
                 match
                   List.filter
                     (fun (found : a) ->
                       not (overridden (visible @ blocked) found))
                     (defines candidate.cls)
                 with
                 | [] -> []
                 | found -> [ (position, candidate, found) ])
               candidates)
        in
        let unhidden =
          List.map
            (fun ((position : int), (candidate : c candidate), (found : a list)) ->
              ( position,
                candidate,
                List.filter
                  (fun (farther : a) ->
                    not
                      (List.exists
                         (fun ((other : int), (hiding : c candidate),
                               (nearer : a list)) ->
                           (not (Int.equal other position))
                           && List.exists (equal candidate.cls) hiding.hides
                           && overridden nearer farther)
                         defining))
                  found ))
            defining
        in
        let conflicts (position : int) (candidate : c candidate) (found : a) :
            bool =
          (candidate.paths > 1 && not (declared_only found))
          || List.exists
               (fun ((other : int), (_ : c candidate), (nearer : a list)) ->
                 (not (Int.equal other position))
                 && List.exists
                      (fun (other_found : a) ->
                        overrides ~nearer:other_found ~farther:found
                        && not (declared_only other_found && declared_only found))
                      nearer)
               unhidden
        in
        let selected, ambiguous =
          List.fold_left
            (fun ((selected : (c * a) list), (ambiguous : a list))
                 ((position : int), (candidate : c candidate),
                  (found : a list)) ->
              let clashing, clear =
                List.partition (conflicts position candidate) found
              in
              ( selected
                @ List.map (fun (found : a) -> (candidate.cls, found)) clear,
                ambiguous @ clashing ))
            ([], []) unhidden
        in
        let definer =
          match (definer, selected) with
          | Some _, _ -> definer
          | None, (first, _) :: _ -> Some first
          | None, [] -> None
        in
        let visible = visible @ List.map snd selected in
        let blocked = blocked @ ambiguous in
        match (selected, ambiguous) with
        | [], [] -> walk definer visible blocked rest
        | _ :: _, _
        | _, _ :: _ ->
            if accumulate then walk definer visible blocked rest
            else finish definer visible blocked false)
  in
  walk None [] [] tiers

let linearise (type c) (strategy : strategy) ~(equal : c -> c -> bool)
    ~(hash : c -> int) ~(parents : c -> c parent list list)
    ~(is_interface : c -> bool) ~(defined_outside : c -> bool) : c -> c linearisation
    =
  let module Memo = Hashtbl.Make (struct
    type t = c

    let equal = equal
    let hash = hash
  end) in
  let memo : c linearisation Memo.t = Memo.create 64 in
  let links_memo : c tier list Memo.t = Memo.create 64 in
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
  let rec known_prefix (prefix : c list) (elements : c element list) :
      c list * bool =
    match elements with
    | Class cls :: rest -> known_prefix (cls :: prefix) rest
    | (Unbound_parent _ | Rest_of _) :: _ -> (List.rev prefix, false)
    | [] -> (List.rev prefix, true)
  in
  let unknown_after (complete : bool) : c tier list =
    if complete then [] else [ Unknown_classes ]
  in
  let along_order (cls : c) (order : c list) (complete : bool) :
      c linearisation =
    let tiers =
      List.map (fun (found : c) -> Candidates [ alone found ]) order
      @ unknown_after complete
    in
    { order; complete; tiers; super_tiers = after ~equal cls tiers }
  in
  (* A lookup that finds a definition among the known candidates of a tier
     selects it: an unbound class of the same tier cannot hide it, since its
     ancestors hold no known class, and a second definition there would make
     the program ill formed. A name the known candidates do not define is
     unknown. *)
  let known_then_unknown (known : c candidate list) (complete : bool) :
      c tier list =
    (match known with
    | [] -> []
    | _ :: _ -> [ Candidates known ])
    @ unknown_after complete
  in
  let of_elements (cls : c) (elements : c element list) : c linearisation =
    let order, complete = known_prefix [] elements in
    along_order cls order complete
  in
  let class_alone (cls : c) : c linearisation =
    along_order cls [ cls ] true
  in
  let relation_of (parent : c parent) : relation =
    match parent with
    | Bound (relation, _)
    | Unbound relation ->
        relation
  in
  let is_extends (parent : c parent) : bool =
    match relation_of parent with
    | Extends _ -> true
    | Implements
    | Mixin
    | Embedded
    | Included
    | Prepended ->
        false
  in
  let is_mixin (parent : c parent) : bool =
    match relation_of parent with
    | Mixin -> true
    | Extends _
    | Implements
    | Embedded
    | Included
    | Prepended ->
        false
  in
  let in_elements (elements : c element list) (element : c element) : bool =
    List.exists (same element) elements
  in
  let rec linearisation (active : c list) (cls : c) : c linearisation =
    match Memo.find_opt memo cls with
    | Some found -> found
    | None when List.exists (equal cls) active -> class_alone cls
    | None ->
        let found =
          if defined_outside cls then
            {
              order = [ cls ];
              complete = false;
              tiers = [ Candidates [ alone cls ]; Unknown_classes ];
              super_tiers = [ Unknown_classes ];
            }
          else
            let active = cls :: active in
            match strategy with
            | C3 { bases_listed_most_base_first } ->
                c3 active cls ~bases_listed_most_base_first
            | Scala_class_linearisation -> scala active cls
            | Ruby_ancestor_chain -> ruby active cls
            | Single_inheritance rule ->
                single_inheritance active cls
                  ~superclass:rule.superclass
                  ~interface_bodies_inherited:rule.interface_bodies_inherited
                  ~mixins:rule.mixins
            | Go_embedding_promotion -> go cls
        in
        Memo.replace memo cls found;
        found
  and elements (active : c list) (owner : c) (index : int) (parent : c parent)
      : c element list =
    match parent with
    | Bound (_, cls) ->
        let found = linearisation active cls in
        List.map (fun (known : c) -> Class known) found.order
        @ if found.complete then [] else [ Rest_of cls ]
    | Unbound _ -> [ Unbound_parent (owner, index) ]
  and c3 (active : c list) (cls : c) ~(bases_listed_most_base_first : bool) :
      c linearisation =
    let in_tail (sequences : c element list list) (element : c element) : bool
        =
      List.exists
        (fun (sequence : c element list) ->
          match sequence with
          | _ :: tail -> List.exists (same element) tail
          | [] -> false)
        sequences
    in
    let successors (sequences : c element list list) (element : c element) :
        c element list =
      let rec following (sequence : c element list) : c element list =
        match sequence with
        | current :: (next :: _ as rest) ->
            if same current element then next :: following rest
            else following rest
        | [ _ ]
        | [] ->
            []
      in
      List.concat_map following sequences
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
    let certain (sequences : c element list list) (candidate : c element) :
        bool =
      let unknown =
        List.filter
          (fun (element : c element) -> not (is_known element))
          (List.concat sequences)
      in
      match unknown with
      | [] -> true
      | _ :: _ ->
          let following = followers sequences candidate in
          List.for_all
            (fun (element : c element) -> List.exists (same element) following)
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
        c list * merge_end =
      match sequences with
      | [] -> (List.rev merged, Merged)
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
          | Some (Class found as head) when certain sequences head ->
              merge (found :: merged) (remove head sequences)
          | Some _ -> (List.rev merged, Uncertain)
          | None ->
              ( List.rev merged,
                if List.for_all (List.for_all is_known) sequences then Rejected
                else Uncertain ))
    in
    let _, written =
      List.fold_left_map
        (fun (next : int) (sequence : c parent list) ->
          ( next + List.length sequence,
            List.mapi
              (fun (index : int) (parent : c parent) ->
                let element =
                  match parent with
                  | Bound (_, parent) -> Class parent
                  | Unbound _ -> Unbound_parent (cls, next + index)
                in
                (element, elements active cls (next + index) parent))
              (if bases_listed_most_base_first then List.rev sequence
               else sequence) ))
        0 (parents cls)
    in
    let sequences =
      List.concat_map (List.map snd) written
      @ List.map (List.map fst) written
      |> List.filter (fun (sequence : c element list) ->
             match sequence with
             | [] -> false
             | _ :: _ -> true)
    in
    match merge [] sequences with
    | _, Rejected -> class_alone cls
    | merged, Merged -> along_order cls (List_.uniq_by equal (cls :: merged)) true
    | merged, Uncertain ->
        along_order cls (List_.uniq_by equal (cls :: merged)) false
  and scala (active : c list) (cls : c) : c linearisation =
    let written =
      List.mapi (elements active cls) (List.concat (parents cls))
    in
    let rightmost =
      List.fold_right
        (fun (element : c element) (kept : c element list) ->
          if in_elements kept element then kept else element :: kept)
        (Class cls :: List.concat (List.rev written))
        []
    in
    of_elements cls rightmost
  and ruby (active : c list) (cls : c) : c linearisation =
    let written = List.mapi (fun (index : int) parent -> (index, parent))
        (List.concat (parents cls))
    in
    let superclasses =
      List.filter
        (fun ((_ : int), (parent : c parent)) -> is_extends parent)
        written
      |> List_.uniq_by
           (fun ((_ : int), (left : c parent)) ((_ : int), (right : c parent)) ->
             match (left, right) with
             | Bound (_, left), Bound (_, right) -> equal left right
             | (Bound _ | Unbound _), _ -> false)
    in
    let above =
      match superclasses with
      | [] -> Some []
      | [ (index, parent) ] -> Some (elements active cls index parent)
      | several -> (
          match
            List.find_opt
              (fun ((_ : int), (parent : c parent)) ->
                match parent with
                | Unbound _ -> true
                | Bound _ -> false)
              several
          with
          | Some (index, _) -> Some [ Unbound_parent (cls, index) ]
          | None -> None)
    in
    match above with
    | None -> class_alone cls
    | Some above ->
        let before, after_class =
          List.fold_left
            (fun ((before : c element list), (after_class : c element list))
                 ((index : int), (parent : c parent)) ->
              let fresh =
                List_.uniq_by same (elements active cls index parent)
                |> List.filter (fun (element : c element) ->
                       not
                         (in_elements
                            (before @ (Class cls :: after_class))
                            element))
              in
              match relation_of parent with
              | Included -> (before, fresh @ after_class)
              | Prepended -> (fresh @ before, after_class)
              | Extends _
              | Implements
              | Mixin
              | Embedded ->
                  (before, after_class))
            ([], above) written
        in
        of_elements cls (before @ (Class cls :: after_class))
  and links (active : c list) (cls : c) ~(superclass : superclass)
      ~(mixins : mixins) : c tier list =
    match Memo.find_opt links_memo cls with
    | Some found -> found
    | None when List.exists (equal cls) active -> [ Candidates [ alone cls ] ]
    | None ->
        let found =
          match chain_parts cls ~superclass ~mixins with
          | Some parts -> chain_of (cls :: active) parts ~superclass ~mixins
          | None -> [ Candidates [ alone cls ] ]
        in
        Memo.replace links_memo cls found;
        found
  and chain_of (active : c list)
      (((own : c tier list), (applied : c tier list), (above : c parent option)) :
        c tier list * c tier list * c parent option) ~(superclass : superclass)
      ~(mixins : mixins) : c tier list =
    let inherited =
      match above with
      | Some (Bound (_, parent)) -> links active parent ~superclass ~mixins
      | Some (Unbound _) -> [ Unknown_classes ]
      | None -> []
    in
    truncated (own @ applied @ inherited)
  and chain_parts (cls : c) ~(superclass : superclass) ~(mixins : mixins) :
      (c tier list * c tier list * c parent option) option =
    let written = List.concat (parents cls) in
    if is_interface cls then Some ([ Candidates [ alone cls ] ], [], None)
    else
      match superclasses written ~superclass with
      | _ :: _ :: _ -> None
      | chosen -> (
          let mixin_parents = List.filter is_mixin written in
          let above =
            match chosen with
            | [ parent ] -> Some parent
            | _ -> None
          in
          match mixins with
          | Applied_in_the_chain ->
              Some
                ( [ Candidates [ alone cls ] ],
                  List.map
                    (fun (parent : c parent) ->
                      match parent with
                      | Bound (_, mixin) -> Candidates [ alone mixin ]
                      | Unbound _ -> Unknown_classes)
                    (List.rev mixin_parents),
                  above )
          | Flattened_into_the_class ->
              let used =
                let found, complete = traits mixin_parents in
                known_then_unknown found complete
              in
              Some (Candidates [ alone cls ] :: used, [], above))
  and superclasses (written : c parent list) ~(superclass : superclass) :
      c parent list =
    match superclass with
    | Written_as_extends -> List.filter is_extends written
    | Carrying_constructor_arguments ->
        List.filter
          (fun (parent : c parent) ->
            match relation_of parent with
            | Extends { constructed } -> constructed
            | Implements
            | Mixin
            | Embedded
            | Included
            | Prepended ->
                false)
          written
    | Of_class_kind -> (
        match List.filter is_extends written with
        | (Bound (_, parent) as first) :: _ when not (is_interface parent) ->
            [ first ]
        | (Unbound _ as first) :: _ -> [ first ]
        | Bound _ :: _
        | [] ->
            [])
  and traits (written : c parent list) : c candidate list * bool =
    let used (trait : c) : c parent list =
      List.filter is_mixin (List.concat (parents trait))
    in
    let rec reach (seen : c list) (complete : bool) (pending : c parent list) :
        c list * bool =
      match pending with
      | [] -> (seen, complete)
      | Unbound _ :: rest -> reach seen false rest
      | Bound (_, trait) :: rest ->
          if List.exists (equal trait) seen then reach seen complete rest
          else reach (seen @ [ trait ]) complete (used trait @ rest)
    in
    let reached, complete = reach [] true written in
    ( List.map
        (fun (trait : c) ->
          { cls = trait; hides = fst (reach [] true (used trait)); paths = 1 })
        reached,
      complete )
  and single_inheritance (active : c list) (cls : c)
      ~(superclass : superclass) ~(interface_bodies_inherited : bool)
      ~(mixins : mixins) : c linearisation =
    match chain_parts cls ~superclass ~mixins with
    | None -> class_alone cls
    | Some ((_, applied, above) as parts) ->
        let chain = chain_of active parts ~superclass ~mixins in
        Memo.replace links_memo cls chain;
        let ancestors =
          List.map
            (fun (parent : c parent) ->
              match parent with
              | Bound (_, parent) -> Some (linearisation active parent)
              | Unbound _ -> None)
            (List.concat (parents cls))
        in
        let order =
          List_.uniq_by equal
            (cls
            :: List.concat_map
                 (fun (ancestor : c linearisation option) ->
                   match ancestor with
                   | Some found -> found.order
                   | None -> [])
                 ancestors)
        in
        let complete =
          List.for_all
            (fun (ancestor : c linearisation option) ->
              match ancestor with
              | Some found -> found.complete
              | None -> false)
            ancestors
        in
        let interfaces =
          List.filter
            (fun (found : c) -> is_interface found && not (equal found cls))
            order
          |> List.map (fun (interface : c) ->
                 {
                   cls = interface;
                   hides =
                     (match (linearisation active interface).order with
                     | _ :: above -> above
                     | [] -> []);
                   paths = 1;
                 })
        in
        let interface_tiers =
          if
            List.exists is_unknown chain
            || not (interface_bodies_inherited || is_interface cls)
          then []
          else known_then_unknown interfaces complete
        in
        let super_tiers =
          if is_interface cls then []
          else
            truncated
              (applied
              @
              match above with
              | Some (Bound (_, parent)) -> (linearisation active parent).tiers
              | Some (Unbound _) -> [ Unknown_classes ]
              | None -> [])
        in
        { order; complete; tiers = chain @ interface_tiers; super_tiers }
  and go (cls : c) : c linearisation =
    let embedded (found : c) : c parent list = List.concat (parents found) in
    let add (next : (c * int) list) (parent : c) (paths : int) :
        (c * int) list =
      if List.exists (fun ((earlier : c), (_ : int)) -> equal earlier parent) next
      then
        List.map
          (fun ((earlier : c), (count : int)) ->
            if equal earlier parent then (earlier, count + paths)
            else (earlier, count))
          next
      else next @ [ (parent, paths) ]
    in
    let rec levels (seen : c list) (level : (c * int) list)
        (tiers : c tier list) (known : bool) (unknown_here : bool) :
        c list * bool * c tier list =
      let tiers =
        if known then
          tiers
          @ known_then_unknown
              (List.map
                 (fun ((found : c), (paths : int)) ->
                   { cls = found; hides = []; paths })
                 level)
              (not unknown_here)
        else tiers
      in
      let known = known && not unknown_here in
      let next, unbound =
        List.fold_left
          (fun (((next : (c * int) list), (unbound : bool)))
               ((found : c), (paths : int)) ->
            List.fold_left
              (fun (((next : (c * int) list), (unbound : bool)))
                   (parent : c parent) ->
                match parent with
                | Unbound _ -> (next, true)
                | Bound (_, parent) ->
                    if List.exists (equal parent) seen then (next, unbound)
                    else (add next parent paths, unbound))
              (next, unbound) (embedded found))
          ([], false) level
      in
      match (next, unbound) with
      | [], false -> (seen, known, tiers)
      | _ -> levels (seen @ List.map fst next) next tiers known unbound
    in
    let order, complete, tiers = levels [ cls ] [ (cls, 1) ] [] true false in
    (* The method set of an interface is the union of the sets it embeds: a
       method reached along two embeddings is one method. *)
    if is_interface cls then along_order cls order complete
    else
      {
        order;
        complete;
        tiers;
        super_tiers =
          (match tiers with
          | _ :: rest -> rest
          | [] -> []);
      }
  in
  linearisation []
