type relation =
  | Extends of {
      constructed : bool;
      virtual_base : bool;
    }
  | Implements
  | Mixin
  | Embedded
  | Included
  | Prepended

type 'c parent =
  | Resolved of relation * 'c
  | Unresolved of relation

type superclass =
  | Written_as_extends
  | Class_supertype_specifier
  | First_parent_if_class

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
  | Cpp_member_lookup
  | Rust_method_probing

type 'c candidate = {
  cls : 'c;
  hides : 'c list;
  paths : int;
}

type 'c base =
  | Base of {
      cls : 'c;
      virtual_base : bool;
      bases : 'c base list;
    }
  | Unknown_base

type 'c level =
  | Candidates of 'c candidate list
  | Base_subobjects of 'c base list
  | Partially_ordered of 'c candidate list
  | Unknown_classes
  | External_member

type 'c element =
  | Class of 'c
  | Unresolved_parent of 'c * int
  | Unknown_tail of 'c

(* The sequences the linearisation continues with after an incomplete
   [order]: each is a subsequence of what follows [order]. *)
type 'c remainder = 'c element list list

type 'c lookup_order = {
  order : 'c list;
  complete : bool;
  remainder : 'c remainder;
  levels : 'c level list;
  super_levels : 'c level list;
}

type ('c, 'a) selection =
  | Selected of 'c * 'a list
  | Ambiguous
  | Undefined
  | Unknown

type merge_outcome =
  | Merged
  | Uncertain
  | Rejected

(* A base class subobject: the virtual base subobject is the virtual base
   the path starts from, or none for the most derived object; the path lists
   the classes reached through non virtual bases from there. *)
type 'c subobject = {
  virtual_base_subobject : 'c option;
  path : 'c list;
  bases : 'c base list;
}

type ('c, 'a) lookup_set =
  | Nothing_found
  | Unknown_set
  | Found of {
      declaring_class : 'c;
      defs : 'a list;
      subobjects : 'c subobject list;
    }
  | Invalid of {
      defs : 'a list;
      subobjects : 'c subobject list;
    }

let super_follows_receiver_order (strategy : strategy) : bool =
  match strategy with
  | C3 _
  | Scala_class_linearisation
  | Ruby_ancestor_chain ->
      true
  | Single_inheritance _
  | Go_embedding_promotion
  | Cpp_member_lookup
  | Rust_method_probing ->
      false

let hides_inherited_overloads (strategy : strategy) : bool =
  match strategy with
  | Cpp_member_lookup -> true
  | C3 _
  | Scala_class_linearisation
  | Ruby_ancestor_chain
  | Single_inheritance _
  | Go_embedding_promotion
  | Rust_method_probing ->
      false

let rec base_classes : type c. c base list -> c list =
 fun (bases : c base list) ->
  List.concat_map
    (fun (base : c base) ->
      match base with
      | Base { cls; bases; _ } -> cls :: base_classes bases
      | Unknown_base -> [])
    bases

let rec has_unknown_base : type c. c base list -> bool =
 fun (bases : c base list) ->
  List.exists
    (fun (base : c base) ->
      match base with
      | Base { bases; _ } -> has_unknown_base bases
      | Unknown_base -> true)
    bases

let level_classes (type c) (level : c level) : c list =
  match level with
  | Candidates candidates
  | Partially_ordered candidates ->
      List.map (fun (candidate : c candidate) -> candidate.cls) candidates
  | Base_subobjects bases -> base_classes bases
  | Unknown_classes
  | External_member ->
      []

let single_candidate (type c) (cls : c) : c candidate = { cls; hides = []; paths = 1 }

let is_unknown (type c) (level : c level) : bool =
  match level with
  | Unknown_classes
  | External_member ->
      true
  | Candidates _
  | Partially_ordered _
  | Base_subobjects _ ->
      false

(* The levels after the one holding [cls]; when no level holds it, the lookup
   reaches only what the levels leave unknown. *)
let levels_after (type c) ~(equal : c -> c -> bool) (cls : c) (levels : c level list) :
    c level list =
  let holds (candidates : c candidate list) : bool =
    List.exists (fun (candidate : c candidate) -> equal candidate.cls cls) candidates
  in
  let rec drop (remaining : c level list) : c level list =
    match remaining with
    | [] -> if List.exists is_unknown levels then [ Unknown_classes ] else []
    | Candidates candidates :: rest when holds candidates -> rest
    (* In a partially ordered level, the classes that may follow [cls] are
       the unknown ones and the candidates that do not certainly precede it. *)
    | Partially_ordered candidates :: rest when holds candidates -> (
        match
          List.filter
            (fun (candidate : c candidate) ->
              not (equal candidate.cls cls || List.exists (equal cls) candidate.hides))
            candidates
        with
        | [] -> Unknown_classes :: rest
        | following -> Unknown_classes :: Partially_ordered following :: rest)
    | ( Candidates _
      | Partially_ordered _
      | Base_subobjects _
      | Unknown_classes
      | External_member )
      :: rest ->
        drop rest
  in
  drop levels

(* [class.member.lookup]: the lookup set of a subobject is its class's own
   declarations, else the merge of its bases' sets, where a set whose
   subobjects are all base subobjects of the other set's subobjects is
   dominated, two sets from the same declaring class join, and any other
   pair makes the set invalid. *)
let subobject_lookup (type c a) ~(equal : c -> c -> bool)
    ~(defines : c -> a list) (bases : c base list) : (c, a) lookup_set =
  let same_subobject (left : c subobject) (right : c subobject) : bool =
    Option.equal equal left.virtual_base_subobject right.virtual_base_subobject
    && List.equal equal left.path right.path
  in
  let child (parent : c subobject) (cls : c) (virtual_base : bool)
      (bases : c base list) : c subobject =
    if virtual_base then { virtual_base_subobject = Some cls; path = []; bases }
    else
      {
        virtual_base_subobject = parent.virtual_base_subobject;
        path = parent.path @ [ cls ];
        bases;
      }
  in
  let rec reachable (subobject : c subobject) : c subobject list =
    List.concat_map
      (fun (base : c base) ->
        match base with
        | Base { cls; virtual_base; bases } ->
            let found = child subobject cls virtual_base bases in
            found :: reachable found
        | Unknown_base -> [])
      subobject.bases
  in
  let dominated (lower : c subobject list) (upper : c subobject list) : bool =
    List.for_all
      (fun (found : c subobject) ->
        List.exists
          (fun (derived : c subobject) ->
            List.exists (same_subobject found) (reachable derived))
          upper)
      lower
  in
  let subobjects_of (set : (c, a) lookup_set) : c subobject list =
    match set with
    | Found { subobjects; _ }
    | Invalid { subobjects; _ } ->
        subobjects
    | Nothing_found
    | Unknown_set ->
        []
  in
  let defs_of (set : (c, a) lookup_set) : a list =
    match set with
    | Found { defs; _ }
    | Invalid { defs; _ } ->
        defs
    | Nothing_found
    | Unknown_set ->
        []
  in
  let merge (known : (c, a) lookup_set) (next : (c, a) lookup_set) :
      (c, a) lookup_set =
    match (known, next) with
    | Nothing_found, found
    | found, Nothing_found ->
        found
    | Unknown_set, found
    | found, Unknown_set -> (
        match found with
        | Unknown_set -> Unknown_set
        | Found _
        | Invalid _
        | Nothing_found ->
            found)
    | (Found _ | Invalid _), (Found _ | Invalid _) -> (
        let upper = subobjects_of known in
        let lower = subobjects_of next in
        if dominated lower upper then known
        else if dominated upper lower then next
        else
          match (known, next) with
          | Found left, Found right when equal left.declaring_class right.declaring_class ->
              Found { left with subobjects = upper @ lower }
          | _ ->
              Invalid
                { defs = defs_of known @ defs_of next; subobjects = upper @ lower })
  in
  let rec lookup (subobject : c subobject) (cls : c) : (c, a) lookup_set =
    match defines cls with
    | _ :: _ as defs -> Found { declaring_class = cls; defs; subobjects = [ subobject ] }
    | [] -> merged subobject subobject.bases
  and merged (subobject : c subobject) (bases : c base list) :
      (c, a) lookup_set =
    List.fold_left
      (fun (known : (c, a) lookup_set) (base : c base) ->
        merge known
          (match base with
          | Base { cls; virtual_base; bases } ->
              lookup (child subobject cls virtual_base bases) cls
          | Unknown_base -> Unknown_set))
      Nothing_found bases
  in
  match merged { virtual_base_subobject = None; path = []; bases } bases with
  | Found found ->
      Found
        { found with subobjects = List_.uniq_by same_subobject found.subobjects }
  | set -> set

module Key_map = Map.Make (Int)

let select (type c a) ~(equal : c -> c -> bool) ~(defines : c -> a list)
    ~(overrides : nearer:a -> farther:a -> bool) ~(overload_key : a -> int)
    ~(declared_only : a -> bool) ~(is_static_member : a -> bool) ~(accumulate : bool)
    (levels : c level list) : (c, a) selection =
  let may_override (nearer : a) (farther : a) : bool =
    Int.equal (overload_key nearer) (overload_key farther)
    && overrides ~nearer ~farther
  in
  let overridden (nearer : a list) (farther : a) : bool =
    List.exists (fun (found : a) -> may_override found farther) nearer
  in
  let overridden_earlier (earlier : a list Key_map.t) (farther : a) : bool =
    match Key_map.find_opt (overload_key farther) earlier with
    | Some nearer ->
        List.exists (fun (found : a) -> overrides ~nearer:found ~farther) nearer
    | None -> false
  in
  let indexed (earlier : a list Key_map.t) (found : a list) :
      a list Key_map.t =
    List.fold_left
      (fun (earlier : a list Key_map.t) (found : a) ->
        Key_map.update (overload_key found)
          (fun (same_key : a list option) ->
            Some (found :: Option.value same_key ~default:[]))
          earlier)
      earlier found
  in
  let finish (definer : c option) (visible : a list) (blocked : a list)
      (unknown : bool) : (c, a) selection =
    match (definer, visible, blocked) with
    | Some definer, _ :: _, _ -> Selected (definer, visible)
    | _, _, _ :: _ -> if unknown then Unknown else Ambiguous
    | _ -> if unknown then Unknown else Undefined
  in
  let defining_candidates (earlier : a list Key_map.t)
      (candidates : c candidate list) : (int * c candidate * a list) list =
    List.concat
      (List.mapi
         (fun (position : int) (candidate : c candidate) ->
           match
             List.filter
               (fun (found : a) -> not (overridden_earlier earlier found))
               (defines candidate.cls)
           with
           | [] -> []
           | found -> [ (position, candidate, found) ])
         candidates)
  in
  let unhidden_candidates (defining : (int * c candidate * a list) list) :
      (int * c candidate * a list) list =
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
  let first_definer (definer : c option) (selected : (c * a) list) : c option =
    match (definer, selected) with
    | Some _, _ -> definer
    | None, (first, _) :: _ -> Some first
    | None, [] -> None
  in
  let rec walk (definer : c option) (visible : a list) (blocked : a list)
      (earlier : a list Key_map.t) (unknown : bool) (remaining : c level list) :
      (c, a) selection =
    match remaining with
    | [] -> finish definer visible blocked unknown
    | Unknown_classes :: rest -> walk definer visible blocked earlier true rest
    | External_member :: _ -> finish definer visible blocked true
    | Base_subobjects bases :: rest -> (
        let next (definer : c option) (visible : a list) (blocked : a list)
            (found : a list) =
          if accumulate then
            walk definer visible blocked (indexed earlier found) unknown rest
          else finish definer visible blocked unknown
        in
        match subobject_lookup ~equal ~defines bases with
        | Nothing_found -> walk definer visible blocked earlier unknown rest
        | Unknown_set -> walk definer visible blocked earlier true rest
        | Found { declaring_class; defs; subobjects } -> (
            match subobjects with
            | _ :: _ :: _ when not (List.for_all is_static_member defs) ->
                next definer visible (blocked @ defs) defs
            | _ ->
                next
                  (match definer with
                  | Some _ -> definer
                  | None -> Some declaring_class)
                  (visible @ defs) blocked defs)
        | Invalid { defs; _ } -> next definer visible (blocked @ defs) defs)
    (* Two defining candidates neither of which certainly precedes the other
       are two possible selections, not an ill formed program. *)
    | Partially_ordered candidates :: rest -> (
        let selected =
          List.concat_map
            (fun ((_ : int), (candidate : c candidate), (found : a list)) ->
              List.map (fun (found : a) -> (candidate.cls, found)) found)
            (unhidden_candidates (defining_candidates earlier candidates))
        in
        let definer = first_definer definer selected in
        let visible = visible @ List.map snd selected in
        match selected with
        | [] -> walk definer visible blocked earlier unknown rest
        | _ :: _ ->
            if accumulate then
              walk definer visible blocked
                (indexed earlier (List.map snd selected))
                unknown rest
            else finish definer visible blocked unknown)
    | Candidates candidates :: rest -> (
        let defining = defining_candidates earlier candidates in
        let unhidden = unhidden_candidates defining in
        let conflicts (position : int) (candidate : c candidate) (found : a) :
            bool =
          (candidate.paths > 1 && not (declared_only found))
          || List.exists
               (fun ((other : int), (_ : c candidate), (nearer : a list)) ->
                 (not (Int.equal other position))
                 && List.exists
                      (fun (other_found : a) ->
                        may_override other_found found
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
        let definer = first_definer definer selected in
        let visible = visible @ List.map snd selected in
        let blocked = blocked @ ambiguous in
        match (selected, ambiguous) with
        | [], [] -> walk definer visible blocked earlier unknown rest
        | _ :: _, _
        | _, _ :: _ ->
            if accumulate then
              walk definer visible blocked
                (indexed (indexed earlier (List.map snd selected)) ambiguous)
                unknown rest
            else finish definer visible blocked unknown)
  in
  walk None [] [] Key_map.empty false levels

let lookup_order (type c) (strategy : strategy) ~(equal : c -> c -> bool)
    ~(hash : c -> int) ~(parents : c -> c parent list list)
    ~(is_interface : c -> bool) ~(is_external : c -> bool)
    ~(dereferences : c -> c option) : c -> c lookup_order =
  let module Memo = Hashtbl.Make (struct
    type t = c

    let equal = equal
    let hash = hash
  end) in
  let memo : c lookup_order Memo.t = Memo.create 64 in
  let superclass_chain_memo : c level list Memo.t = Memo.create 64 in
  let bases_memo : c base list Memo.t = Memo.create 64 in
  let same (left : c element) (right : c element) : bool =
    match (left, right) with
    | Class left, Class right
    | Unknown_tail left, Unknown_tail right ->
        equal left right
    | Unresolved_parent (left, left_index), Unresolved_parent (right, right_index) ->
        equal left right && Int.equal left_index right_index
    | (Class _ | Unresolved_parent _ | Unknown_tail _), _ -> false
  in
  let is_known (element : c element) : bool =
    match element with
    | Class _ -> true
    | Unresolved_parent _
    | Unknown_tail _ ->
        false
  in
  let rec known_prefix (prefix : c list) (elements : c element list) :
      c list * c element list =
    match elements with
    | Class cls :: rest -> known_prefix (cls :: prefix) rest
    | (Unresolved_parent _ | Unknown_tail _) :: _
    | [] ->
        (List.rev prefix, elements)
  in
  let unknown_suffix (complete : bool) : c level list =
    if complete then [] else [ Unknown_classes ]
  in
  let unknown_remainder (cls : c) (complete : bool) : c remainder =
    if complete then [] else [ [ Unknown_tail cls ] ]
  in
  let of_levels (cls : c) (order : c list) (complete : bool)
      (remainder : c remainder) (levels : c level list) : c lookup_order =
    {
      order;
      complete;
      remainder;
      levels;
      super_levels = levels_after ~equal cls levels;
    }
  in
  let order_levels (order : c list) : c level list =
    List.map (fun (found : c) -> Candidates [ single_candidate found ]) order
  in
  let of_order (cls : c) (order : c list) (complete : bool) :
      c lookup_order =
    of_levels cls order complete
      (unknown_remainder cls complete)
      (order_levels order @ unknown_suffix complete)
  in
  (* A lookup that finds a definition among the known candidates of a level
     selects it: an unbound class of the same level cannot hide it, since its
     ancestors hold no known class, and a second definition there would make
     the program ill formed. A name the known candidates do not define is
     looked up in the levels after the unknown one. *)
  let known_then_unknown (known : c candidate list) (complete : bool) :
      c level list =
    (match known with
    | [] -> []
    | _ :: _ -> [ Candidates known ])
    @ unknown_suffix complete
  in
  (* The entries after an unknown one keep their order: the classes the
     unknown one brings in go between it and them. *)
  let levels_of_elements (elements : c element list) : c level list =
    List.fold_right
      (fun (element : c element) (levels : c level list) ->
        match (element, levels) with
        | Class found, _ -> Candidates [ single_candidate found ] :: levels
        | (Unresolved_parent _ | Unknown_tail _), Unknown_classes :: _ -> levels
        | (Unresolved_parent _ | Unknown_tail _), _ -> Unknown_classes :: levels)
      elements []
  in
  let of_elements (cls : c) (elements : c element list) : c lookup_order =
    let order, rest = known_prefix [] elements in
    of_levels cls order (List.is_empty rest)
      (match rest with
      | [] -> []
      | _ :: _ -> [ rest ])
      (levels_of_elements elements)
  in
  let of_class_only (cls : c) : c lookup_order =
    of_order cls [ cls ] true
  in
  let relation_of (parent : c parent) : relation =
    match parent with
    | Resolved (relation, _)
    | Unresolved relation ->
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
  let rec lookup_order_of (active : c list) (cls : c) : c lookup_order =
    match Memo.find_opt memo cls with
    | Some found -> found
    | None when List.exists (equal cls) active -> of_class_only cls
    | None ->
        let found =
          if is_external cls then
            {
              order = [ cls ];
              complete = false;
              remainder = unknown_remainder cls false;
              levels = [ Candidates [ single_candidate cls ]; Unknown_classes ];
              super_levels = [ Unknown_classes ];
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
            | Cpp_member_lookup -> cpp cls
            | Rust_method_probing -> rust active cls
        in
        Memo.replace memo cls found;
        found
  and elements (active : c list) (owner : c) (index : int) (parent : c parent)
      : c element list =
    match parent with
    | Resolved (_, cls) ->
        let found = lookup_order_of active cls in
        List.map (fun (known : c) -> Class known) found.order
        @ List.concat found.remainder
    | Unresolved _ -> [ Unresolved_parent (owner, index) ]
  (* A parent whose merge stopped enters the merge as its known prefix
     followed by each sequence of its remainder. Every completion of the
     unknown bases respects the known sequences, so it gives the parent a
     linearisation that holds each of these as a subsequence, and C3 keeps
     that linearisation as a subsequence of the child's: the child's merge
     takes no order that some completion does not have. *)
  and parent_sequences (active : c list) (owner : c) (index : int)
      (parent : c parent) : c element list list =
    match parent with
    | Resolved (_, cls) -> (
        let found = lookup_order_of active cls in
        let prefix = List.map (fun (known : c) -> Class known) found.order in
        match found.remainder with
        | [] -> [ prefix ]
        | remainder ->
            List.map (fun (sequence : c element list) -> prefix @ sequence) remainder)
    | Unresolved _ -> [ [ Unresolved_parent (owner, index) ] ]
  and c3 (active : c list) (cls : c) ~(bases_listed_most_base_first : bool) :
      c lookup_order =
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
        c list * merge_outcome * c element list list =
      match sequences with
      | [] -> (List.rev merged, Merged, [])
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
          | Some _ -> (List.rev merged, Uncertain, sequences)
          | None ->
              ( List.rev merged,
                (if List.for_all (List.for_all is_known) sequences then Rejected
                 else Uncertain),
                sequences ))
    in
    (* After the merge stops, a candidate hides the known classes that follow
       it in the transitive closure of the remaining sequences: they follow it
       in every completion of the unknown elements. *)
    let partially_ordered_levels (remaining : c element list list) :
        c level list =
      let known (elements : c element list) : c list =
        List.filter_map
          (fun (element : c element) ->
            match element with
            | Class found -> Some found
            | Unresolved_parent _
            | Unknown_tail _ ->
                None)
          elements
      in
      match
        List.map
          (fun (found : c) ->
            {
              cls = found;
              hides = known (followers remaining (Class found));
              paths = 1;
            })
          (List_.uniq_by equal (known (List.concat remaining)))
      with
      | [] -> [ Unknown_classes ]
      | candidates -> [ Unknown_classes; Partially_ordered candidates ]
    in
    let _, written =
      List.fold_left_map
        (fun (next : int) (sequence : c parent list) ->
          ( next + List.length sequence,
            List.mapi
              (fun (index : int) (parent : c parent) ->
                let element =
                  match parent with
                  | Resolved (_, parent) -> Class parent
                  | Unresolved _ -> Unresolved_parent (cls, next + index)
                in
                (element, parent_sequences active cls (next + index) parent))
              (if bases_listed_most_base_first then List.rev sequence
               else sequence) ))
        0 (parents cls)
    in
    let sequences =
      List.concat_map (List.concat_map snd) written
      @ List.map (List.map fst) written
      |> List.filter (fun (sequence : c element list) ->
             match sequence with
             | [] -> false
             | _ :: _ -> true)
    in
    match merge [] sequences with
    | _, Rejected, _ -> of_class_only cls
    | merged, Merged, _ -> of_order cls (List_.uniq_by equal (cls :: merged)) true
    | merged, Uncertain, remaining ->
        let order = List_.uniq_by equal (cls :: merged) in
        of_levels cls order false
          (List_.uniq_by (List.equal same) remaining)
          (order_levels order @ partially_ordered_levels remaining)
  and scala (active : c list) (cls : c) : c lookup_order =
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
  and ruby (active : c list) (cls : c) : c lookup_order =
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
             | Resolved (_, left), Resolved (_, right) -> equal left right
             | (Resolved _ | Unresolved _), _ -> false)
    in
    let superclass_elements =
      match superclasses with
      | [] -> Some []
      | [ (index, parent) ] -> Some (elements active cls index parent)
      | several -> (
          match
            List.find_opt
              (fun ((_ : int), (parent : c parent)) ->
                match parent with
                | Unresolved _ -> true
                | Resolved _ -> false)
              several
          with
          | Some (index, _) -> Some [ Unresolved_parent (cls, index) ]
          | None -> None)
    in
    match superclass_elements with
    | None -> of_class_only cls
    | Some superclass_elements ->
        let prepended, ancestors_after_class =
          List.fold_left
            (fun ((prepended : c element list),
                  (ancestors_after_class : c element list))
                 ((index : int), (parent : c parent)) ->
              let fresh =
                List_.uniq_by same (elements active cls index parent)
                |> List.filter (fun (element : c element) ->
                       not
                         (in_elements
                            (prepended @ (Class cls :: ancestors_after_class))
                            element))
              in
              match relation_of parent with
              | Included -> (prepended, fresh @ ancestors_after_class)
              | Prepended -> (fresh @ prepended, ancestors_after_class)
              | Extends _
              | Implements
              | Mixin
              | Embedded ->
                  (prepended, ancestors_after_class))
            ([], superclass_elements) written
        in
        of_elements cls (prepended @ (Class cls :: ancestors_after_class))
  and superclass_chain (active : c list) (cls : c) ~(superclass : superclass)
      ~(mixins : mixins) : c level list =
    match Memo.find_opt superclass_chain_memo cls with
    | Some found -> found
    | None when List.exists (equal cls) active -> [ Candidates [ single_candidate cls ] ]
    | None ->
        let found =
          match chain_parts cls ~superclass ~mixins with
          | Some parts -> chain_of (cls :: active) parts ~superclass ~mixins
          | None -> [ Candidates [ single_candidate cls ] ]
        in
        Memo.replace superclass_chain_memo cls found;
        found
  and chain_of (active : c list)
      (((own : c level list), (applied : c level list), (superclass_parent : c parent option)) :
        c level list * c level list * c parent option) ~(superclass : superclass)
      ~(mixins : mixins) : c level list =
    let inherited =
      match superclass_parent with
      | Some (Resolved (_, parent)) -> superclass_chain active parent ~superclass ~mixins
      | Some (Unresolved _) -> [ Unknown_classes ]
      | None -> []
    in
    own @ applied @ inherited
  and chain_parts (cls : c) ~(superclass : superclass) ~(mixins : mixins) :
      (c level list * c level list * c parent option) option =
    let written = List.concat (parents cls) in
    if is_interface cls then Some ([ Candidates [ single_candidate cls ] ], [], None)
    else
      match superclasses written ~superclass with
      | _ :: _ :: _ -> None
      | chosen -> (
          let mixin_parents = List.filter is_mixin written in
          let superclass_parent =
            match chosen with
            | [ parent ] -> Some parent
            | _ -> None
          in
          match mixins with
          | Applied_in_the_chain ->
              Some
                ( [ Candidates [ single_candidate cls ] ],
                  List.map
                    (fun (parent : c parent) ->
                      match parent with
                      | Resolved (_, mixin) -> Candidates [ single_candidate mixin ]
                      | Unresolved _ -> Unknown_classes)
                    (List.rev mixin_parents),
                  superclass_parent )
          | Flattened_into_the_class ->
              let used =
                let found, complete = traits mixin_parents in
                known_then_unknown found complete
              in
              Some (Candidates [ single_candidate cls ] :: used, [], superclass_parent))
  and superclasses (written : c parent list) ~(superclass : superclass) :
      c parent list =
    match superclass with
    | Written_as_extends -> List.filter is_extends written
    | Class_supertype_specifier ->
        List.filter
          (fun (parent : c parent) ->
            match parent with
            | Resolved (Extends _, parent) -> not (is_interface parent)
            | Unresolved (Extends { constructed; _ }) -> constructed
            | Resolved ((Implements | Mixin | Embedded | Included | Prepended), _)
            | Unresolved (Implements | Mixin | Embedded | Included | Prepended) ->
                false)
          written
    | First_parent_if_class -> (
        match List.filter is_extends written with
        | (Resolved (_, parent) as first) :: _ when not (is_interface parent) ->
            [ first ]
        | (Unresolved _ as first) :: _ -> [ first ]
        | Resolved _ :: _
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
      | Unresolved _ :: rest -> reach seen false rest
      | Resolved (_, trait) :: rest ->
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
      ~(mixins : mixins) : c lookup_order =
    match chain_parts cls ~superclass ~mixins with
    | None -> of_class_only cls
    | Some ((_, applied, superclass_parent) as parts) ->
        let chain = chain_of active parts ~superclass ~mixins in
        Memo.replace superclass_chain_memo cls chain;
        let ancestors =
          List.map
            (fun (parent : c parent) ->
              match parent with
              | Resolved (_, parent) -> Some (lookup_order_of active parent)
              | Unresolved _ -> None)
            (List.concat (parents cls))
        in
        let order =
          List_.uniq_by equal
            (cls
            :: List.concat_map
                 (fun (ancestor : c lookup_order option) ->
                   match ancestor with
                   | Some found -> found.order
                   | None -> [])
                 ancestors)
        in
        let complete =
          List.for_all
            (fun (ancestor : c lookup_order option) ->
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
                     (match (lookup_order_of active interface).order with
                     | _ :: rest -> rest
                     | [] -> []);
                   paths = 1;
                 })
        in
        let interface_levels =
          if not (interface_bodies_inherited || is_interface cls) then []
          else known_then_unknown interfaces complete
        in
        let super_levels =
          if is_interface cls then []
          else
            applied
            @
            match superclass_parent with
            | Some (Resolved (_, parent)) -> (lookup_order_of active parent).levels
            | Some (Unresolved _) -> [ Unknown_classes ]
            | None -> []
        in
        {
          order;
          complete;
          remainder = unknown_remainder cls complete;
          levels = chain @ interface_levels;
          super_levels;
        }
  and bases_of (active : c list) (cls : c) : c base list =
    match Memo.find_opt bases_memo cls with
    | Some found -> found
    | None when List.exists (equal cls) active -> []
    | None ->
        let found =
          if is_external cls then [ Unknown_base ]
          else
            List.map
              (fun (parent : c parent) ->
                match parent with
                | Resolved (relation, base) ->
                    Base
                      {
                        cls = base;
                        virtual_base =
                          (match relation with
                          | Extends { virtual_base; _ } -> virtual_base
                          | Implements
                          | Mixin
                          | Embedded
                          | Included
                          | Prepended ->
                              false);
                        bases = bases_of (cls :: active) base;
                      }
                | Unresolved _ -> Unknown_base)
              (List.concat (parents cls))
        in
        Memo.replace bases_memo cls found;
        found
  and cpp (cls : c) : c lookup_order =
    let bases = bases_of [] cls in
    let base_levels =
      match bases with
      | [] -> []
      | _ :: _ -> [ Base_subobjects bases ]
    in
    let complete = not (has_unknown_base bases) in
    {
      order = List_.uniq_by equal (cls :: base_classes bases);
      complete;
      remainder = unknown_remainder cls complete;
      levels = Candidates [ single_candidate cls ] :: base_levels;
      super_levels = base_levels;
    }
  and rust (active : c list) (cls : c) : c lookup_order =
    let written = List.concat (parents cls) in
    let bound (relation_of_parent : relation -> bool) : c list =
      List.filter_map
        (fun (parent : c parent) ->
          match parent with
          | Resolved (relation, found) when relation_of_parent relation ->
              Some found
          | Resolved _
          | Unresolved _ ->
              None)
        written
    in
    let is_implements (relation : relation) : bool =
      match relation with
      | Implements -> true
      | Extends _
      | Mixin
      | Embedded
      | Included
      | Prepended ->
          false
    in
    let impls = bound is_implements in
    let traits_of (impl : c) : c list =
      List.filter_map
        (fun (parent : c parent) ->
          match parent with
          | Resolved (_, trait) -> Some trait
          | Unresolved _ -> None)
        (List.concat (parents impl))
    in
    let impl_complete (impl : c) : bool =
      List.for_all
        (fun (parent : c parent) ->
          match parent with
          | Resolved _ -> true
          | Unresolved _ -> false)
        (List.concat (parents impl))
    in
    let implemented =
      match
        List.concat_map
          (fun (impl : c) ->
            { cls = impl; hides = traits_of impl; paths = 1 }
            :: List.map single_candidate (traits_of impl))
          impls
      with
      | [] -> []
      | candidates -> [ Candidates candidates ]
    in
    let impls_complete =
      List.for_all
        (fun (parent : c parent) ->
          match parent with
          | Resolved (relation, impl) ->
              (not (is_implements relation)) || impl_complete impl
          | Unresolved relation -> not (is_implements relation))
        written
    in
    let inherited =
      List.concat_map
        (fun (parent : c parent) ->
          match parent with
          | Resolved (relation, found) when not (is_implements relation) ->
              (lookup_order_of active found).levels
          | Resolved _ -> []
          | Unresolved relation ->
              if is_implements relation then [] else [ Unknown_classes ])
        written
    in
    let dereferenced =
      match dereferences cls with
      | Some target when not (List.exists (equal target) active) ->
          (lookup_order_of active target).levels
      | Some _
      | None ->
          []
    in
    let levels =
      (Candidates [ single_candidate cls ] :: implemented) @ inherited @ dereferenced
    in
    let complete =
      impls_complete && not (List.exists is_unknown (implemented @ inherited))
    in
    {
      order =
        List_.uniq_by equal
          (cls
          :: List.concat_map
               (fun (level : c level) -> level_classes level)
               (implemented @ inherited));
      complete;
      remainder = unknown_remainder cls complete;
      levels;
      super_levels = [];
    }
  and go (cls : c) : c lookup_order =
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
    let rec by_depth (seen : c list) (at_depth : (c * int) list)
        (levels : c level list) (unknown_here : bool) : c list * c level list =
      let levels =
        levels
        @ known_then_unknown
            (List.map
               (fun ((found : c), (paths : int)) ->
                 { cls = found; hides = []; paths })
               at_depth)
            (not unknown_here)
      in
      let next, unbound =
        List.fold_left
          (fun (((next : (c * int) list), (unbound : bool)))
               ((found : c), (paths : int)) ->
            List.fold_left
              (fun (((next : (c * int) list), (unbound : bool)))
                   (parent : c parent) ->
                match parent with
                | Unresolved _ -> (next, true)
                | Resolved (_, parent) ->
                    if List.exists (equal parent) seen then (next, unbound)
                    else (add next parent paths, unbound))
              (next, unbound) (embedded found))
          ([], false) at_depth
      in
      match (next, unbound) with
      | [], false -> (seen, levels)
      | _ -> by_depth (seen @ List.map fst next) next levels unbound
    in
    let order, levels = by_depth [ cls ] [ (cls, 1) ] [] false in
    let complete = not (List.exists is_unknown levels) in
    (* The method set of an interface is the union of the sets it embeds: a
       method reached along two embeddings is one method. *)
    if is_interface cls then of_order cls order complete
    else
      {
        order;
        complete;
        remainder = unknown_remainder cls complete;
        levels;
        super_levels =
          (match levels with
          | _ :: rest -> rest
          | [] -> []);
      }
  in
  lookup_order_of []
