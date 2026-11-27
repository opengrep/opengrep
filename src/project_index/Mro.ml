open Types
module G = AST_generic
module FA = Graph_from_AST

(* Class and module qns share one dotted-string space; [known_class_qns] is
   tested by re-interpreting the module qn. *)
let chase_reexport
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(is_known : Names.Module_qn.t -> bool)
    (qn : Names.Module_qn.t) : Names.Module_qn.t option =
  let visited : (Names.Module_qn.t, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec walk current =
    if is_known current then Some current
    else if Hashtbl.mem visited current then None
    else begin
      Hashtbl.replace visited current ();
      match Hashtbl.find_opt reexport_map current with
      | None -> None
      | Some next -> walk next
    end
  in
  walk qn

let is_known_class ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (mod_qn : Names.Module_qn.t) : bool =
  Hashtbl.mem known_class_qns
    (Names.Class_qn.of_string (Names.Module_qn.to_string mod_qn))

let within (target : Names.Module_qn.t) (parts : string list)
    : Names.Module_qn.t =
  List.fold_left Names.Module_qn.concat target parts

(* The class that one of [bindings], in import order, binds: the last one
   where imports bind in statement order ([imports_bind_in_statement_order]),
   and otherwise the class only when every binding agrees on it. *)
let single_binding ~(cfg : Index_lang_rules.t)
    (bindings : Names.Module_qn.t list) : Names.Module_qn.t option =
  if cfg.Index_lang_rules.imports_bind_in_statement_order then
    List_.last_opt bindings
  else
    match List.sort_uniq Names.Module_qn.compare bindings with
    | [ (bound : Names.Module_qn.t) ] -> Some bound
    | []
    | _ :: _ :: _ -> None

(* The class [qn] refers to: [qn] itself, the target of a named re-export
   of it ([reexport_map]), or the class that a wildcard re-export of its
   module ([wildcard_reexports]) binds to its last part, chosen among several
   by [single_binding]. *)
let chase_class ~(cfg : Index_lang_rules.t)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(wildcard_reexports : import list Common.SMap.t)
    ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (qn : Names.Module_qn.t) : Names.Module_qn.t option =
  let rec walk (visited : Names.Module_qn.t list) (current : Names.Module_qn.t)
      : Names.Module_qn.t option =
    if is_known_class ~known_class_qns current then Some current
    else if List.exists (Names.Module_qn.equal current) visited then None
    else
      let visited = current :: visited in
      match Hashtbl.find_opt reexport_map current with
      | Some (next : Names.Module_qn.t) -> walk visited next
      | None -> through_wildcards visited current
  and through_wildcards (visited : Names.Module_qn.t list)
      (current : Names.Module_qn.t) : Names.Module_qn.t option =
    match Names.Module_qn.split_last current with
    | None -> None
    | Some ((module_qn : Names.Module_qn.t), (name : string)) ->
      single_binding ~cfg
        (List.filter_map
           (fun (imp : import) ->
             match Imports.binding_of imp with
             | Imports.Wildcard_from (target : Names.Module_qn.t)
               when (not (List.exists (String.equal name) imp.im_hidden))
                    && Reexports.star_exported ~cfg ~dunder_all target name ->
               walk visited (Names.Module_qn.concat target name)
             | Imports.Wildcard_from _
             | Imports.Named_binding _ -> None)
           (Option.value ~default:[]
              (Common.SMap.find_opt (Names.Module_qn.to_string module_qn)
                 wildcard_reexports)))
  in
  walk [] qn

let resolve_parent_qn ~(imports : import list)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (path : string list) : Names.Module_qn.t option =
  match path with
  | [] -> None
  | head :: rest ->
    (match
       List.find_opt
         (fun (imp : import) -> String.equal imp.im_local head) imports
     with
     | None -> None
     | Some (imp : import) ->
       let target = imp.im_target in
       let candidate =
         if rest = [] then target
         else
           Names.Module_qn.of_string
             (Names.Module_qn.to_string target ^ "."
              ^ String.concat "." rest)
       in
       let is_known (mod_qn : Names.Module_qn.t) : bool =
         Hashtbl.mem known_class_qns
           (Names.Class_qn.of_string (Names.Module_qn.to_string mod_qn))
       in
       chase_reexport ~reexport_map ~is_known candidate)

(* The module that the local of the import [imp] of [target] stands for.
   An unaliased import whose local is the first part of a dotted target
   and not its last part is an [import a.b] that binds [a]
   ([Index_lang_rules.First_segment_binds]); every other import binds its
   local to [target]. *)
let module_bound_by_import (imp : import) (target : Names.Module_qn.t)
    : Names.Module_qn.t =
  match (imp.im_alias, Names.Module_qn.split_last target) with
  | None, Some ((_ : Names.Module_qn.t), (last : string))
    when (not (String.equal imp.im_local last))
         && (match Names.Module_qn.parts target with
             | (first : string) :: _ -> String.equal imp.im_local first
             | [] -> false) ->
    Names.Module_qn.of_string imp.im_local
  | _ -> target

(* The class that the wildcard import [imp] binds to [path]: the import binds
   the head of [path] ([Reexports.wildcard_binds]) and its module defines the
   class. *)
let class_through_wildcard ~(cfg : Index_lang_rules.t)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(chase : Names.Module_qn.t -> Names.Module_qn.t option)
    (path : string list) (imp : import) : Names.Module_qn.t option =
  match (Imports.binding_of imp, path) with
  | Imports.Wildcard_from (target : Names.Module_qn.t), head :: _
    when Reexports.wildcard_binds ~cfg ~dunder_all imp target head ->
    chase (within target path)
  | Imports.Wildcard_from _, _
  | Imports.Named_binding _, _ -> None

(* A wildcard import binds the class when its module defines it. *)
let resolve_parent_through_wildcard ~(cfg : Index_lang_rules.t)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(imports : import list)
    ~(chase : Names.Module_qn.t -> Names.Module_qn.t option)
    (path : string list) : Names.Module_qn.t option =
  single_binding ~cfg
    (List.filter_map (class_through_wildcard ~cfg ~dunder_all ~chase path)
       imports)

(* The class that the import [imp] of the head of [path] binds, read
   through the module that the import's local stands for
   ([module_bound_by_import]) and through wildcard re-exports ([chase]). *)
let class_through_import
    ~(chase : Names.Module_qn.t -> Names.Module_qn.t option)
    (path : string list) (imp : import) : Names.Module_qn.t option =
  match (Imports.binding_of imp, path) with
  | Imports.Named_binding { local; target }, head :: (rest : string list)
    when String.equal local head ->
    chase (within (module_bound_by_import imp target) rest)
  | Imports.Named_binding _, _
  | Imports.Wildcard_from _, _ -> None

let resolve_parent_through_import ~(cfg : Index_lang_rules.t)
    ~(imports : import list)
    ~(chase : Names.Module_qn.t -> Names.Module_qn.t option)
    (path : string list) : Names.Module_qn.t option =
  single_binding ~cfg
    (List.filter_map (class_through_import ~chase path) imports)

let resolve_parent_by_scope
    ~(by_qn : (Names.Class_qn.t, class_info) Hashtbl.t)
    ~(qns_by_bare_name : (Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t)
    ~(admits : Names.Class_qn.t -> bool)
    (ci : class_info) (path : string list) : Names.Module_qn.t option =
  match List.rev path with
  | [] -> None
  | bare_name :: _ ->
    let candidates =
      Option.value
        (Hashtbl.find_opt qns_by_bare_name (Names.Class_name.of_string bare_name))
        ~default:[]
      |> List.filter admits
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
      let path_suffix_matches = List.filter dotted_ends_with_path candidates in
      (* Most shared leading qn parts with [ci], shortest qn on ties;
         [require_shared] drops zero-overlap candidates. *)
      let pick_best ~require_shared qns =
        let ci_parts = Names.Class_qn.parts ci.ci_qn in
        let score qn =
          let parts = Names.Class_qn.parts qn in
          let rec common left right =
            match left, right with
            | left_head :: ax, right_head :: bx when String.equal left_head right_head ->
              1 + common ax bx
            | _ -> 0
          in
          let shared = common ci_parts parts in
          if require_shared && shared = 0 then None
          else Some (shared, -(String.length (Names.Class_qn.to_string qn)))
        in
        let best =
          List.fold_left (fun acc qn ->
            match score qn, acc with
            | Some qn_score, None -> Some (qn, qn_score)
            | Some qn_score, Some (_, sb) when qn_score > sb -> Some (qn, qn_score)
            | _ -> acc
          ) None qns
        in
        match best with
        | Some (qn, _) when Hashtbl.mem by_qn qn ->
          Some (Names.Module_qn.of_string (Names.Class_qn.to_string qn))
        | _ -> None
      in
      (* A candidate that shares no leading part of its qualified name with
         the child is an unrelated class with the same simple name in every
         language, so a parent binds only through a shared prefix. *)
      match path_suffix_matches with
      | _ :: _ -> pick_best ~require_shared:true path_suffix_matches
      | [] -> pick_best ~require_shared:true candidates
    end

(* Lexical constant resolution: a bare parent reference [path] seen from [ci]'s
   body resolves against [ci]'s enclosing namespaces innermost-first, then
   top-level — [Svc::Box < Base] tries [Svc::Base] then [::Base]; a top-level
   [Sub < Base] tries [Base] directly.  An EXACT qn match wins, so a class
   with the same simple name that requires a different qualifier
   ([Other::Base], [Dec::Base]) is never chosen.  Only meaningful once class
   qns are the constant path (Ruby); with a
   file-path prefix the candidates don't match cross-file, so it no-ops. *)
let resolve_parent_lexical
    ~(by_qn : (Names.Class_qn.t, class_info) Hashtbl.t)
    ~(admits : Names.Class_qn.t -> bool)
    (ci : class_info) (path : string list) : Names.Module_qn.t option =
  match path with
  | [] -> None
  | _ ->
    let drop_last (xs : string list) : string list =
      match List.rev xs with _ :: rest -> List.rev rest | [] -> []
    in
    let enclosing = drop_last (Names.Class_qn.parts ci.ci_qn) in
    let rec try_scopes (enc : string list) : Names.Class_qn.t option =
      let candidate = Names.Class_qn.of_parts (enc @ path) in
      if Hashtbl.mem by_qn candidate && admits candidate then Some candidate
      else match enc with [] -> None | _ -> try_scopes (drop_last enc)
    in
    Option.map
      (fun qn -> Names.Module_qn.of_string (Names.Class_qn.to_string qn))
      (try_scopes enclosing)

let resolve_parent_in_own_scope
    ~(known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t)
    (ci : class_info) (path : string list) : Names.Module_qn.t option =
  match Names.Class_qn.split_last ci.ci_qn with
  | None -> None
  | Some ((owner : Names.Class_qn.t), _) ->
    let candidate = List.fold_left Names.Class_qn.concat owner path in
    if Hashtbl.mem known_class_qns candidate then
      Some (Names.Module_qn.of_string (Names.Class_qn.to_string candidate))
    else None

let inherit_into_type_state
    ~(lang : Lang.t)
    ~(cfg : Index_lang_rules.t)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(file_infos : file_info list)
    ~(class_infos : class_info list)
    ~(func_def_file : FA.func_info -> string option)
    (ts : Type_state.t)
    : Type_state.t * class_fun_info list * (FA.func_info * FA.func_info) list
      * (Names.Class_qn.t * Names.Class_qn.t list) list =
  (* Each table holds at most one entry per class. *)
  let n_classes = List.length class_infos in
  let by_qn : (Names.Class_qn.t, class_info) Hashtbl.t =
    Hashtbl.create n_classes in
  List.iter (fun ci -> Hashtbl.replace by_qn ci.ci_qn ci) class_infos;
  let known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t =
    Hashtbl.create n_classes
  in
  List.iter (fun ci -> Hashtbl.replace known_class_qns ci.ci_qn ())
    class_infos;
  let qns_by_bare_name : (Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t =
    Hashtbl.create n_classes
  in
  List.iter (fun ci ->
    let bare_name = Names.Class_qn.bare_name ci.ci_qn in
    if bare_name <> "" then begin
      let bare_name_key = Names.Class_name.of_string bare_name in
      let cur =
        Option.value (Hashtbl.find_opt qns_by_bare_name bare_name_key)
          ~default:[]
      in
      Hashtbl.replace qns_by_bare_name bare_name_key (ci.ci_qn :: cur)
    end
  ) class_infos;
  (* All class ids sharing a qn — a class reopened across files (Ruby) has one
     qn but several ids.  Used to UNION a reopened parent's methods. *)
  let ci_ids_by_qn : (Names.Class_qn.t, Function_id.t list) Hashtbl.t =
    Hashtbl.create n_classes in
  List.iter (fun ci ->
    let cur = Option.value (Hashtbl.find_opt ci_ids_by_qn ci.ci_qn) ~default:[] in
    Hashtbl.replace ci_ids_by_qn ci.ci_qn (ci.ci_id :: cur)
  ) class_infos;
  let wildcard_reexports : import list Common.SMap.t =
    Reexports.build_wildcard_reexports ~cfg file_infos
  in
  (* The class that each reference [module.name] resolves to through
     re-exports, so that a chain of wildcard re-exports is walked once per
     reference and not once per class. *)
  let chased : (Names.Module_qn.t, Names.Module_qn.t option) Hashtbl.t =
    Hashtbl.create n_classes
  in
  let chase (qn : Names.Module_qn.t) : Names.Module_qn.t option =
    match Hashtbl.find_opt chased qn with
    | Some (result : Names.Module_qn.t option) -> result
    | None ->
      let result =
        chase_class ~cfg ~dunder_all ~reexport_map ~wildcard_reexports
          ~known_class_qns qn
      in
      Hashtbl.replace chased qn result;
      result
  in
  let resolve_parent_by_import_or_scope
      ~(admits : Names.Class_qn.t -> bool) (ci : class_info)
      (p_path : string list) : Names.Module_qn.t option =
    match
      resolve_parent_qn ~imports:ci.ci_imports ~reexport_map ~known_class_qns
        p_path
    with
    | Some _ as resolved -> resolved
    | None -> (
      match cfg.Index_lang_rules.parent_resolution with
      | Index_lang_rules.Parent_in_own_scope ->
        resolve_parent_in_own_scope ~known_class_qns ci p_path
      | Index_lang_rules.Parent_by_lexical_scope ->
        resolve_parent_lexical ~by_qn ~admits ci p_path
      | Index_lang_rules.Parent_by_lexical_scope_then_simple_name -> (
        match resolve_parent_lexical ~by_qn ~admits ci p_path with
        | Some _ as resolved -> resolved
        | None ->
          resolve_parent_by_scope ~by_qn ~qns_by_bare_name ~admits ci
            p_path))
  in
  (* Without a parent by import or in scope, a wildcard import of a module
     that defines the class binds it, and then an import of the head of the
     path read through wildcard re-exports or through the module that an
     [import a.b] binds. Where imports bind in statement order, only the
     imports before the class take part, and the last of them that binds
     the class, wildcard or not, binds it. *)
  let resolve_parent ~(admits : Names.Class_qn.t -> bool) (ci : class_info)
      (p_path : string list) : Names.Module_qn.t option =
    match resolve_parent_by_import_or_scope ~admits ci p_path with
    | Some _ as resolved -> resolved
    | None -> (
      let imports : import list =
        if cfg.Index_lang_rules.imports_bind_in_statement_order then
          match Scope_binding.position_of_tok (Function_id.tok ci.ci_id) with
          | Some (class_position : Pos.t) ->
            List.filter
              (fun (imp : import) ->
                match Scope_binding.position_of_tok imp.im_tok with
                | Some (position : Pos.t) ->
                  Pos.compare position class_position < 0
                | None -> true)
              ci.ci_imports
          | None -> ci.ci_imports
        else ci.ci_imports
      in
      if cfg.Index_lang_rules.imports_bind_in_statement_order then
        (* The last import that binds the head of the path binds the
           parent. It binds no parent when what it binds is not a class of
           the index: an earlier import of the same identifier is rebound. *)
        let binds_head (imp : import) : bool =
          match (Imports.binding_of imp, p_path) with
          | Imports.Named_binding { local; _ }, (head : string) :: _ ->
            String.equal local head
          | Imports.Named_binding _, []
          | Imports.Wildcard_from _, _ -> false
        in
        let bindings : Names.Module_qn.t option list =
          List.filter_map
            (fun (imp : import) ->
              match
                class_through_wildcard ~cfg ~dunder_all ~chase p_path imp
              with
              | Some (bound : Names.Module_qn.t) -> Some (Some bound)
              | None ->
                if binds_head imp then
                  Some (class_through_import ~chase p_path imp)
                else None)
            imports
        in
        Option.join (List_.last_opt bindings)
      else
        match
          resolve_parent_through_wildcard ~cfg ~dunder_all ~imports ~chase
            p_path
        with
        | Some _ as resolved -> resolved
        | None -> resolve_parent_through_import ~cfg ~imports ~chase p_path)
  in
  let synth_inherited (child_cls_il : IL.name) (parent_method : FA.func_info)
    : FA.func_info option =
    Option.map (fun (_, m_il) ->
      { parent_method with
        FA.fn_id = Func_info.method_id ~cls:child_cls_il ~meth:m_il })
      (Func_info.as_method parent_method.FA.fn_id)
  in
  (* Where the superclass position takes no mixin, a lookup of a superclass
     by simple identifier skips the mixins. *)
  let admits_parent (parent : Index_lang_rules.class_parent)
      (qn : Names.Class_qn.t) : bool =
    not
      (cfg.Index_lang_rules.superclass_is_never_a_mixin
       && parent.Index_lang_rules.cp_is_superclass)
    ||
    match Hashtbl.find_opt by_qn qn with
    | Some { ci_class_kind = G.Trait; _ } -> false
    | Some { ci_class_kind = G.Class | G.Interface | G.Object; _ }
    | None -> true
  in
  let parents_at (position : Index_lang_rules.parent_position)
      (ci : class_info) : class_info list =
    List.filter_map (fun (parent : Index_lang_rules.class_parent) ->
      if
        not
          (Index_lang_rules.equal_parent_position
             parent.Index_lang_rules.cp_position position)
      then None
      else
        match
          resolve_parent ~admits:(admits_parent parent) ci
            parent.Index_lang_rules.cp_path
        with
        | None -> None
        | Some pq ->
          Hashtbl.find_opt by_qn
            (Names.Class_qn.of_string (Names.Module_qn.to_string pq)))
      ci.ci_parent_paths
  in
  (* C3 linearization; on cycles / inconsistent hierarchies, force the next head so the merge terminates. *)
  let ci_eq (left : class_info) (right : class_info) =
    Function_id.equal left.ci_id right.ci_id
  in
  let keep_first (order : class_info list) : class_info list =
    List_.uniq_by ci_eq order
  in
  let remove_head (head : class_info) (seqs : class_info list list) =
    List.filter_map (fun seq ->
      match seq with
      | seq_head :: tl when ci_eq seq_head head -> if tl = [] then None else Some tl
      | _ -> Some seq)
      seqs
  in
  let rec c3_merge (seqs : class_info list list) : class_info list =
    match List.filter (fun seq -> seq <> []) seqs with
    | [] -> []
    | seqs ->
      let in_tail (candidate : class_info) =
        List.exists (fun seq ->
          match seq with _ :: tl -> List.exists (ci_eq candidate) tl | [] -> false)
          seqs
      in
      let head =
        List.find_map (fun seq ->
          match seq with seq_head :: _ when not (in_tail seq_head) -> Some seq_head | _ -> None)
          seqs
      in
      let chosen_head =
        match head with
        | Some chosen_head -> chosen_head
        | None -> (match seqs with (chosen_head :: _) :: _ -> chosen_head | _ -> assert false)
      in
      chosen_head :: c3_merge (remove_head chosen_head seqs)
  in
  let lin_cache : (Function_id.t, class_info list) Hashtbl.t =
    Hashtbl.create n_classes
  in
  let rec linearize (active : Function_id.t list) (ci : class_info)
    : class_info list =
    match Hashtbl.find_opt lin_cache ci.ci_id with
    | Some cached_lin -> cached_lin
    | None ->
      if List.exists (Function_id.equal ci.ci_id) active then [ci]
      else begin
        let prepended = parents_at Index_lang_rules.Prepended ci in
        let parents = parents_at Index_lang_rules.Appended ci in
        let seqs = List.map (linearize (ci.ci_id :: active)) parents
                   @ [parents] in
        let before =
          List.concat_map (linearize (ci.ci_id :: active)) prepended
        in
        let result = keep_first (before @ (ci :: c3_merge seqs)) in
        Hashtbl.replace lin_cache ci.ci_id result;
        result
      end
  in
  let state, inherited, overrides, resolution_orders =
  List.fold_left (fun ((state : Type_state.t),
                       (inherited_acc : class_fun_info list),
                       (override_acc : (FA.func_info * FA.func_info) list),
                       (orders : (Names.Class_qn.t * Names.Class_qn.t list) list))
                      ci ->
    let order = linearize [] ci in
    let mro_ancestors : class_info list =
      match order with _ :: rest -> rest | [] -> []
    in
    let constrained_order =
      keep_first
        (order
         @ List.concat_map (linearize [ ci.ci_id ])
             (parents_at Index_lang_rules.Constraint ci))
    in
    let orders =
      (ci.ci_qn,
       List.map (fun (ancestor : class_info) -> ancestor.ci_qn)
         constrained_order)
      :: orders
    in
    let child_simple = Names.Class_qn.bare_name ci.ci_qn in
    let child_name = Names.Class_name.of_string child_simple in
    let own =
      Option.value (Type_state.get_methods state child_name) ~default:[]
    in
    let child_cls_il =
      match own with
      | func :: _ ->
        (match func.FA.fn_id with
         | Some cls :: _ -> Some cls
         | _ -> None)
      | [] ->
        (* Empty subclass has no own method to borrow the class IL.name from; synthesise one or it inherits nothing. *)
        Some IL.{ ident = (child_simple, Tok.unsafe_fake_tok child_simple);
                  sid = G.SId.unsafe_default;
                  id_info = G.empty_id_info () }
    in
    match child_cls_il with
    | None -> (state, inherited_acc, override_acc, orders)
    | Some child_cls_il ->
      let initial_names =
        List.filter_map (fun (func : FA.func_info) ->
          Option.map (fun (_, meth) -> fst meth.IL.ident)
            (Func_info.as_method func.FA.fn_id)
        ) own |> List.sort_uniq String.compare
      in
      let collect_from (names, added, ovr) (pci : class_info) =
        let parent_simple = Names.Class_qn.bare_name pci.ci_qn in
        let pmethods =
          Option.value
            (Type_state.get_methods state
               (Names.Class_name.of_string parent_simple))
            ~default:[]
        in
        (* A candidate parent method carries the same bare name and may be
           defined in any file; a class reopened across files contributes a
           method from each of them. *)
        let by_bare_name = List.filter (fun (func : FA.func_info) ->
          match Func_info.as_method func.FA.fn_id with
          | Some (cls, _) -> String.equal (fst cls.IL.ident) parent_simple
          | None -> false
        ) pmethods in
        (* The parent is pinned to the class's identity, not to its bare name.
           Two classes of one bare name
           ([namespace A { class Base }] and [namespace B { class Base }])
           are otherwise indistinguishable, and a subclass of [B::Base] would
           inherit [A::Base]'s methods.  The pin is the set of ids sharing the
           parent's qualified name, so a class reopened across files
           ([class Base] in two files) keeps the methods of both
           definitions.  Fall back to the parent's own file when no method
           carries a matching class id
           (keeps the prior protection against a class with the same
           simple name in another file). *)
        let pinned_ids =
          Option.value (Hashtbl.find_opt ci_ids_by_qn pci.ci_qn)
            ~default:[pci.ci_id]
        in
        let pmethods =
          match
            List.filter (fun (func : FA.func_info) ->
              match Func_info.as_method func.FA.fn_id with
              | Some (cls, _) ->
                let cls_id = Function_id.of_il_name cls in
                List.exists (Function_id.equal cls_id) pinned_ids
              | None -> false
            ) by_bare_name
          with
          | [] ->
            let pci_file_str = Fpath.to_string pci.ci_file in
            List.filter (fun (func : FA.func_info) ->
              match func_def_file func with
              | Some def_file -> String.equal def_file pci_file_str
              | None -> false
            ) by_bare_name
          | pinned -> pinned
        in
        List.fold_left (fun (names, added, ovr) pm ->
          match Func_info.as_method pm.FA.fn_id with
          | Some (_, meth) ->
            let mname = fst meth.IL.ident in
            if List.mem mname names then
              (* Not inherited: the child (or a nearer ancestor) defines
                 [mname].  When the ancestor's version is a BODY-LESS decl
                 (abstract method) and the child's own same-arity method
                 shadows it, that is an override — record the pair so a
                 dispatch edge connects it to the decl (a base-class
                 self-call binds the decl; without the edge its merged
                 signature stays empty and taint dies there).  Concrete
                 ancestor methods are not wired: unioning a real body with
                 its overrides is a precision call, not a gap. *)
              let ovr =
                match pm.FA.fdef.G.fbody with
                | G.FBDecl _ | G.FBNothing ->
                  let arity (f : FA.func_info) : int =
                    Receiver.arity lang ~is_method:(Receiver.is_method f.FA.fdef)
                      ~is_static:(Receiver.is_static f.FA.entity)
                      (Tok.unbracket f.FA.fdef.G.fparams)
                  in
                  let pm_arity = arity pm in
                  (match
                     List.find_opt
                       (fun (om : FA.func_info) ->
                          (match Func_info.as_method om.FA.fn_id with
                           | Some (_, ometh) ->
                             String.equal (fst ometh.IL.ident) mname
                           | None -> false)
                          && Int.equal (arity om) pm_arity)
                       own
                   with
                   | Some om -> (om, pm) :: ovr
                   | None -> ovr)
                | _ -> ovr
              in
              (names, added, ovr)
            else
              (match synth_inherited child_cls_il pm with
               | Some inh -> (mname :: names, inh :: added, ovr)
               | None -> (names, added, ovr))
          | None -> (names, added, ovr)
        ) (names, added, ovr) pmethods
      in
      let _, added, overrides =
        List.fold_left collect_from (initial_names, [], []) mro_ancestors
      in
      let override_acc = List.rev_append overrides override_acc in
      if added <> [] then
        (Type_state.add_inherited state
           (Names.Class_name.of_string child_simple) ci.ci_file added,
         (ci, added) :: inherited_acc,
         override_acc,
         orders)
      else (state, inherited_acc, override_acc, orders)
  ) (ts, [], [], []) class_infos
  in
  (state, inherited, overrides, List.rev resolution_orders)
