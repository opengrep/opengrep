open Types
module G = AST_generic
module FA = Graph_from_AST

(* Class and module qns share one dotted-string space; [known_class_qns] is
   tested by re-interpreting the module qn. *)
let chase_reexport
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (qn : Names.Module_qn.t) : Names.Module_qn.t option =
  let mem_as_class (m : Names.Module_qn.t) =
    Hashtbl.mem known_class_qns
      (Names.Class_qn.of_string (Names.Module_qn.to_string m))
  in
  let visited : (Names.Module_qn.t, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec walk current =
    if mem_as_class current then Some current
    else if Hashtbl.mem visited current then None
    else begin
      Hashtbl.replace visited current ();
      match Hashtbl.find_opt reexport_map current with
      | None -> None
      | Some next -> walk next
    end
  in
  walk qn

let resolve_parent_qn ~(imports : (string * Names.Module_qn.t) list)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (path : string list) : Names.Module_qn.t option =
  match path with
  | [] -> None
  | head :: rest ->
    (match List.assoc_opt head imports with
     | None -> None
     | Some target ->
       let candidate =
         if rest = [] then target
         else
           Names.Module_qn.of_string
             (Names.Module_qn.to_string target ^ "."
              ^ String.concat "." rest)
       in
       chase_reexport ~reexport_map ~known_class_qns candidate)

let resolve_parent_by_scope
    ~(by_qn : (Names.Class_qn.t, class_info) Hashtbl.t)
    ~(qns_by_leaf : (Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t)
    (ci : class_info) (path : string list) : Names.Module_qn.t option =
  match List.rev path with
  | [] -> None
  | leaf :: _ ->
    let candidates =
      Option.value
        (Hashtbl.find_opt qns_by_leaf (Names.Class_name.of_string leaf))
        ~default:[]
    in
    if candidates = [] then None
    else begin
      let path_str = String.concat "." path in
      let plen = String.length path_str in
      let dotted_ends_with_path qn =
        let qs = Names.Class_qn.to_string qn in
        let qlen = String.length qs in
        qlen >= plen
        && String.equal (String.sub qs (qlen - plen) plen) path_str
        && (qlen = plen || Char.equal qs.[qlen - plen - 1] '.')
      in
      let tier1 = List.filter dotted_ends_with_path candidates in
      (* Most shared leading qn parts with [ci], shortest qn on ties;
         [require_shared] drops zero-overlap candidates. *)
      let pick_best ~require_shared qns =
        let ci_parts = Names.Class_qn.parts ci.ci_qn in
        let score qn =
          let parts = Names.Class_qn.parts qn in
          let rec common a b =
            match a, b with
            | x :: ax, y :: bx when String.equal x y -> 1 + common ax bx
            | _ -> 0
          in
          let shared = common ci_parts parts in
          if require_shared && shared = 0 then None
          else Some (shared, -(String.length (Names.Class_qn.to_string qn)))
        in
        let best =
          List.fold_left (fun acc qn ->
            match score qn, acc with
            | Some s, None -> Some (qn, s)
            | Some s, Some (_, sb) when s > sb -> Some (qn, s)
            | _ -> acc
          ) None qns
        in
        match best with
        | Some (qn, _) when Hashtbl.mem by_qn qn ->
          Some (Names.Module_qn.of_string (Names.Class_qn.to_string qn))
        | _ -> None
      in
      match tier1 with
      | _ :: _ -> pick_best ~require_shared:false tier1
      | [] -> pick_best ~require_shared:true candidates
    end

let inherit_into_type_state
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(class_infos : class_info list)
    ~(func_def_file : FA.func_info -> string option)
    (ts : Type_state.t) : Type_state.t =
  let by_qn : (Names.Class_qn.t, class_info) Hashtbl.t = Hashtbl.create 8192 in
  List.iter (fun ci -> Hashtbl.replace by_qn ci.ci_qn ci) class_infos;
  let known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t =
    Hashtbl.create 8192
  in
  List.iter (fun ci -> Hashtbl.replace known_class_qns ci.ci_qn ())
    class_infos;
  let qns_by_leaf : (Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t =
    Hashtbl.create 8192
  in
  List.iter (fun ci ->
    let leaf = Names.Class_qn.leaf ci.ci_qn in
    if leaf <> "" then begin
      let leaf_key = Names.Class_name.of_string leaf in
      let cur =
        Option.value (Hashtbl.find_opt qns_by_leaf leaf_key) ~default:[]
      in
      Hashtbl.replace qns_by_leaf leaf_key (ci.ci_qn :: cur)
    end
  ) class_infos;
  let resolve_parent ci p_path =
    match resolve_parent_qn ~imports:ci.ci_imports
            ~reexport_map ~known_class_qns p_path with
    | Some _ as r -> r
    | None -> resolve_parent_by_scope ~by_qn ~qns_by_leaf ci p_path
  in
  let synth_inherited (child_cls_il : IL.name) (parent_method : FA.func_info)
    : FA.func_info option =
    Option.map (fun (_, m_il) ->
      { parent_method with
        FA.fn_id = Func_info.method_id ~cls:child_cls_il ~meth:m_il })
      (Func_info.as_method parent_method.FA.fn_id)
  in
  let parents_of (ci : class_info) : class_info list =
    List.filter_map (fun p_path ->
      match resolve_parent ci p_path with
      | None -> None
      | Some pq ->
        Hashtbl.find_opt by_qn
          (Names.Class_qn.of_string (Names.Module_qn.to_string pq)))
      ci.ci_parent_paths
  in
  (* C3 linearization; on cycles / inconsistent hierarchies, force the next head so the merge terminates. *)
  let ci_eq (a : class_info) (b : class_info) =
    Function_id.equal a.ci_id b.ci_id
  in
  let remove_head (h : class_info) (seqs : class_info list list) =
    List.filter_map (fun s ->
      match s with
      | x :: tl when ci_eq x h -> if tl = [] then None else Some tl
      | _ -> Some s)
      seqs
  in
  let rec c3_merge (seqs : class_info list list) : class_info list =
    match List.filter (fun s -> s <> []) seqs with
    | [] -> []
    | seqs ->
      let in_tail (c : class_info) =
        List.exists (fun s ->
          match s with _ :: tl -> List.exists (ci_eq c) tl | [] -> false)
          seqs
      in
      let head =
        List.find_map (fun s ->
          match s with h :: _ when not (in_tail h) -> Some h | _ -> None)
          seqs
      in
      let h =
        match head with
        | Some h -> h
        | None -> (match seqs with (h :: _) :: _ -> h | _ -> assert false)
      in
      h :: c3_merge (remove_head h seqs)
  in
  let lin_cache : (Function_id.t, class_info list) Hashtbl.t =
    Hashtbl.create 256
  in
  let rec linearize (active : Function_id.t list) (ci : class_info)
    : class_info list =
    match Hashtbl.find_opt lin_cache ci.ci_id with
    | Some l -> l
    | None ->
      if List.exists (Function_id.equal ci.ci_id) active then [ci]
      else begin
        let parents = parents_of ci in
        let seqs = List.map (linearize (ci.ci_id :: active)) parents
                   @ [parents] in
        let result = ci :: c3_merge seqs in
        Hashtbl.replace lin_cache ci.ci_id result;
        result
      end
  in
  let mro_ancestors (ci : class_info) : class_info list =
    match linearize [] ci with _ :: rest -> rest | [] -> []
  in
  List.fold_left (fun (state : Type_state.t) ci ->
    let child_simple = Names.Class_qn.leaf ci.ci_qn in
    let child_name = Names.Class_name.of_string child_simple in
    let own =
      Option.value (Type_state.get_methods state child_name) ~default:[]
    in
    let child_cls_il =
      match own with
      | f :: _ ->
        (match f.FA.fn_id with
         | Some c :: _ -> Some c
         | _ -> None)
      | [] ->
        (* Empty subclass has no own method to borrow the class IL.name from; synthesise one or it inherits nothing. *)
        Some IL.{ ident = (child_simple, Tok.unsafe_fake_tok child_simple);
                  sid = G.SId.unsafe_default;
                  id_info = G.empty_id_info () }
    in
    match child_cls_il with
    | None -> state
    | Some child_cls_il ->
      let initial_names =
        List.filter_map (fun (f : FA.func_info) ->
          Option.map (fun (_, m) -> fst m.IL.ident)
            (Func_info.as_method f.FA.fn_id)
        ) own |> List.sort_uniq String.compare
      in
      let collect_from (names, added) (pci : class_info) =
        let parent_simple = Names.Class_qn.leaf pci.ci_qn in
        let pmethods =
          Option.value
            (Type_state.get_methods state
               (Names.Class_name.of_string parent_simple))
            ~default:[]
        in
        (* Restrict to methods defined in THIS parent class + file, not homonym classes elsewhere. *)
        let pci_file_str = Fpath.to_string pci.ci_file in
        let same_file (f : FA.func_info) : bool =
          match func_def_file f with
          | Some f -> String.equal f pci_file_str
          | None -> false
        in
        let pmethods = List.filter (fun (f : FA.func_info) ->
          match Func_info.as_method f.FA.fn_id with
          | Some (c, _) ->
            String.equal (fst c.IL.ident) parent_simple
            && same_file f
          | None -> false
        ) pmethods in
        List.fold_left (fun (names, added) pm ->
          match Func_info.as_method pm.FA.fn_id with
          | Some (_, m) ->
            let mname = fst m.IL.ident in
            if List.mem mname names then (names, added)
            else
              (match synth_inherited child_cls_il pm with
               | Some inh -> (mname :: names, inh :: added)
               | None -> (names, added))
          | None -> (names, added)
        ) (names, added) pmethods
      in
      let _, added =
        List.fold_left collect_from (initial_names, []) (mro_ancestors ci)
      in
      if added <> [] then
        Type_state.add_inherited state
          (Names.Class_name.of_string child_simple) added
      else state
  ) ts class_infos
