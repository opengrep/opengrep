module G = AST_generic
module SMap = Common.SMap

module SId_tbl = Hashtbl.Make (struct
  type t = G.SId.t

  let equal = G.SId.equal
  let hash = G.SId.hash
end)

module Field_path_map = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

type kind =
  | Class_kind of G.class_kind
  | Module_kind

type role =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent =
  | Bound of G.SId.t
  | Unbound of G.type_
  | Impl of Function_id.t

type class_scope = {
  binding : G.SId.t;
  role : role;
  members : Func_info.t list SMap.t;
  fields : Func_info.t list Field_path_map.t;
  parents : (parent * Linearisation.placement) list;
  class_side_parents : parent list;
  kind : kind;
  singleton_exposure : Class_parents.singleton_exposure;
  bound_functions : Func_info.t list;
  object_fields : Func_info.t list Field_path_map.t;
  extensions : Func_info.t list SMap.t;
  reopens : bool;
}

type scope_id = {
  scope_binding : G.SId.t;
  scope_role : role;
}

let equal_role (left : role) (right : role) : bool =
  match (left, right) with
  | Singleton_object, Singleton_object -> true
  | Definition left, Definition right
  | Trait_impl left, Trait_impl right ->
      Function_id.equal left right
  | (Definition _ | Singleton_object | Trait_impl _), _ -> false

let equal_scope_id (left : scope_id) (right : scope_id) : bool =
  G.SId.equal left.scope_binding right.scope_binding
  && equal_role left.scope_role right.scope_role

let hash_scope_id (id : scope_id) : int =
  match id.scope_role with
  | Singleton_object -> Hashtbl.hash (G.SId.hash id.scope_binding, 1)
  | Definition site ->
      Hashtbl.hash (G.SId.hash id.scope_binding, 0, Function_id.hash site)
  | Trait_impl site ->
      Hashtbl.hash (G.SId.hash id.scope_binding, 2, Function_id.hash site)

module Scope_tbl = Hashtbl.Make (struct
  type t = scope_id

  let equal = equal_scope_id
  let hash = hash_scope_id
end)

let binding_of_id_info (info : G.id_info) : G.SId.t option =
  match !(info.G.id_resolved) with
  | Some (_, sid) when not (G.SId.is_unsafe_default sid) -> Some sid
  | Some _
  | None ->
      None

let id_info_of_name (name : G.name) : G.id_info =
  match name with
  | G.Id (_, info) -> info
  | G.IdQualified { G.name_info; _ } -> name_info

let name_of_type (ty : G.type_) : G.name option =
  match ty.G.t with
  | G.TyN name
  | G.TyApply ({ G.t = G.TyN name; _ }, _)
  | G.TyPointer (_, { G.t = G.TyN name; _ })
  | G.TyRef (_, { G.t = G.TyN name; _ })
  | G.TyExpr { G.e = G.N name; _ } ->
      Some name
  | _ -> None

let rec written_path (e : G.expr) : (G.name * string list) option =
  match e.G.e with
  | G.N (name : G.name) -> Some (name, [])
  | G.DotAccess (inner, _, G.FN (G.Id (((segment : string), _), _))) ->
      Option.map
        (fun ((head : G.name), (rest : string list)) ->
          (head, rest @ [ segment ]))
        (written_path inner)
  | _ -> None

let path_of_type (ty : G.type_) : (G.name * string list) option =
  match ty.G.t with
  | G.TyExpr (e : G.expr) -> written_path e
  | _ -> Option.map (fun (name : G.name) -> (name, [])) (name_of_type ty)

let qualified_path (name : G.name) : string list =
  match name with
  | G.Id ((id, _), _) -> [ id ]
  | G.IdQualified
      { G.name_last = (id, _), _; name_middle = Some (G.QDots middle); _ } ->
      List.map (fun (((segment, _), _) : G.ident * G.type_arguments option) ->
          segment)
        middle
      @ [ id ]
  | G.IdQualified { G.name_last = (id, _), _; _ } -> [ id ]

let site_of_name (name : G.name) : G.SId.t option =
  let (_, (tok : Tok.t)) : G.ident =
    match name with
    | G.Id (id, _) -> id
    | G.IdQualified { G.name_last = id, _; _ } -> id
  in
  match Tok.loc_of_tok tok with
  | Ok (loc : Tok.location) ->
      Some
        (G.SId.of_site
           ~file:(Fpath.to_string (Fpath.normalize loc.Tok.pos.Pos.file))
           tok)
  | Error _ -> None

let definition_binding (name : G.name) : G.SId.t option =
  match binding_of_id_info (id_info_of_name name) with
  | Some _ as bound -> bound
  | None -> site_of_name name

let role_rank (role : role) : int * Function_id.t option =
  match role with
  | Definition site -> (0, Some site)
  | Singleton_object -> (1, None)
  | Trait_impl site -> (2, Some site)

let compare_scope (left : class_scope) (right : class_scope) : int =
  let file_of (scope : class_scope) : string =
    let _, (file : string), _, _ = G.SId.to_loc scope.binding in
    file
  in
  match String.compare (file_of left) (file_of right) with
  | 0 -> (
      match G.SId.compare left.binding right.binding with
      | 0 ->
          let left_rank, left_site = role_rank left.role in
          let right_rank, right_site = role_rank right.role in
          (match Int.compare left_rank right_rank with
          | 0 -> Option.compare Function_id.compare left_site right_site
          | order -> order)
      | order -> order)
  | order -> order

let scope_id_of (scope : class_scope) : scope_id =
  { scope_binding = scope.binding; scope_role = scope.role }

let definition_scope (sid : G.SId.t) : scope_id =
  { scope_binding = sid; scope_role = Definition (Function_id.of_sid sid) }

type cls = {
  id : int;
  scopes : class_scope list;
}

type entry = {
  parents : (cls option * Linearisation.placement) list list;
  class_side_parents : cls option list;
  order : cls Linearisation.linearisation;
  subclasses : cls list;
}

type position =
  | Term_position
  | Type_position

type t = {
  entries : entry array;
  classes : cls array;
  by_scope : cls Scope_tbl.t;
  definitions : cls list SId_tbl.t;
  outside : position -> scope_id option -> G.name * string list -> cls option;
  descendants : cls list option array;
}

let same (left : cls) (right : cls) : bool = Int.equal left.id right.id
let hash (cls : cls) : int = Hashtbl.hash cls.id
let index (cls : cls) : int = cls.id
let scopes (cls : cls) : class_scope list = cls.scopes

let same_definition (left : Func_info.t) (right : Func_info.t) : bool =
  left.Func_info.fdef == right.Func_info.fdef

let concat_scopes (cls : cls) (of_scope : class_scope -> Func_info.t list) :
    Func_info.t list =
  List.concat_map of_scope cls.scopes |> List_.uniq_by same_definition

let own_members (cls : cls) (name : string) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) ->
      Option.value (SMap.find_opt name scope.members) ~default:[])

let member_table (cls : cls) : Func_info.t list SMap.t =
  List.fold_left
    (fun (members : Func_info.t list SMap.t) (scope : class_scope) ->
      SMap.union
        (fun (_ : string) (earlier : Func_info.t list) (later : Func_info.t list)
           ->
          Some (List_.uniq_by same_definition (earlier @ later)))
        members scope.members)
    SMap.empty cls.scopes

let instance_fields (cls : cls) (path : string list) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) ->
      Option.value (Field_path_map.find_opt path scope.fields) ~default:[])

let object_fields (cls : cls) (path : string list) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) ->
      Option.value (Field_path_map.find_opt path scope.object_fields) ~default:[])

let extensions (cls : cls) (name : string) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) ->
      Option.value (SMap.find_opt name scope.extensions) ~default:[])

let bound_functions (cls : cls) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) -> scope.bound_functions)

let exposes (cls : cls) (name : string) : bool =
  List.exists
    (fun (scope : class_scope) ->
      Class_parents.exposes scope.singleton_exposure name)
    cls.scopes

let is_abstraction (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.kind with
      | Class_kind (G.Interface | G.Trait) -> true
      | Class_kind (G.Class | G.Struct | G.Object)
      | Module_kind ->
          false)
    cls.scopes

let is_interface (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.kind with
      | Class_kind G.Interface -> true
      | Class_kind (G.Class | G.Struct | G.Object | G.Trait)
      | Module_kind ->
          false)
    cls.scopes

let is_trait_impl (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.role with
      | Trait_impl _ -> true
      | Definition _
      | Singleton_object ->
          false)
    cls.scopes

let entry (t : t) (cls : cls) : entry = t.entries.(cls.id)
let order (t : t) (cls : cls) : cls Linearisation.linearisation =
  (entry t cls).order

let subclasses (t : t) (cls : cls) : cls list = (entry t cls).subclasses

let class_side_parents (t : t) (cls : cls) : cls option list =
  (entry t cls).class_side_parents

let parents (t : t) (cls : cls) : cls option list =
  List.concat_map (List.map fst) (entry t cls).parents

let classes (t : t) : cls list = Array.to_list t.classes

let class_of_scope (t : t) (id : scope_id) : cls option =
  Scope_tbl.find_opt t.by_scope id

let class_of_binding_in (by_scope : cls Scope_tbl.t)
    (definitions : cls list SId_tbl.t) (sid : G.SId.t) : cls option =
  match Scope_tbl.find_opt by_scope (definition_scope sid) with
  | Some _ as found -> found
  | None -> (
      match
        List_.uniq_by same
          (Option.value (SId_tbl.find_opt definitions sid) ~default:[])
      with
      | [ cls ] -> Some cls
      | []
      | _ :: _ :: _ ->
          Scope_tbl.find_opt by_scope
            { scope_binding = sid; scope_role = Singleton_object })

let class_of_binding (t : t) (sid : G.SId.t) : cls option =
  class_of_binding_in t.by_scope t.definitions sid

let object_of_binding (t : t) (sid : G.SId.t) : cls option =
  match class_of_scope t { scope_binding = sid; scope_role = Singleton_object } with
  | Some _ as found -> found
  | None -> class_of_binding t sid

let class_of_name_in (by_scope : cls Scope_tbl.t)
    (definitions : cls list SId_tbl.t)
    (outside :
      position -> scope_id option -> G.name * string list -> scope_id option)
    (name : G.name) : cls option =
  let own =
    Option.bind
      (binding_of_id_info (id_info_of_name name))
      (class_of_binding_in by_scope definitions)
  in
  match own with
  | Some _ -> own
  | None ->
      Option.bind
        (outside Type_position None (name, []))
        (Scope_tbl.find_opt by_scope)

let class_of_name (t : t) ~(position : position)
    ~(context : scope_id option) (name : G.name) : cls option =
  match
    Option.bind
      (binding_of_id_info (id_info_of_name name))
      (class_of_binding t)
  with
  | Some _ as found -> found
  | None -> t.outside position context (name, [])

let class_of_path (t : t) ~(position : position) ~(context : scope_id option)
    (((head : G.name), (rest : string list)) as path) : cls option =
  match rest with
  | [] -> class_of_name t ~position ~context head
  | _ :: _ -> t.outside position context path

let same_type_by_class (class_of : G.name -> cls option)
    ~(required : G.type_) ~(candidate : G.type_) : bool option =
  match (name_of_type required, name_of_type candidate) with
  | Some required, Some candidate -> (
      match (class_of required, class_of candidate) with
      | Some required, Some candidate -> Some (same required candidate)
      | Some _, None
      | None, Some _ ->
          None
      | None, None -> (
          match
            ( binding_of_id_info (id_info_of_name required),
              binding_of_id_info (id_info_of_name candidate) )
          with
          | None, None ->
              Some
                (List.equal String.equal (qualified_path required)
                   (qualified_path candidate))
          | Some required, Some candidate when G.SId.equal required candidate ->
              Some true
          | Some _, _
          | None, Some _ ->
              None))
  | _ -> None

let structural_methods (members : Func_info.t list SMap.t) :
    (Func_info.t * Structural_typing.method_) list =
  SMap.fold
    (fun (name : string) (funcs : Func_info.t list)
         (methods : (Func_info.t * Structural_typing.method_) list) ->
      List.map
        (fun (func : Func_info.t) ->
          ( func,
            {
              Structural_typing.name;
              entity = func.Func_info.entity;
              fdef = func.Func_info.fdef;
            } ))
        funcs
      @ methods)
    members []

let satisfied_in_one_build ~(lang : Lang.t)
    ~(equal_type : Structural_typing.equal_type)
    ~(compiled_together : Func_info.t list -> bool)
    ~(interface : (Func_info.t * Structural_typing.method_) list)
    ~(candidate : (Func_info.t * Structural_typing.method_) list) : bool =
  let options =
    List.map
      (fun ((_ : Func_info.t), (required : Structural_typing.method_)) ->
        List.filter_map
          (fun ((func : Func_info.t), (offered : Structural_typing.method_)) ->
            if
              Structural_typing.method_satisfies ~lang ~equal_type ~required
                offered
            then Some func
            else None)
          candidate)
      interface
  in
  let rec choose (chosen : Func_info.t list) (remaining : Func_info.t list list)
      : bool =
    match remaining with
    | [] -> true
    | alternatives :: rest ->
        List.exists
          (fun (func : Func_info.t) ->
            compiled_together (func :: chosen) && choose (func :: chosen) rest)
          alternatives
  in
  (not (List_.null interface)) && choose (List.map fst interface) options

let members_along (classes : cls list) : Func_info.t list SMap.t =
  List.fold_left
    (fun (members : Func_info.t list SMap.t) (cls : cls) ->
      SMap.union
        (fun (_ : string) (nearer : Func_info.t list) (_ : Func_info.t list) ->
          Some nearer)
        members (member_table cls))
    SMap.empty classes

let build ~(lang : Lang.t) ~(classes : class_scope list list)
    ~(compiled_together : Func_info.t list -> bool)
    ~(defined : class_scope -> bool)
    ~(link : class_scope -> parent -> scope_id option)
    ~(outside :
       position -> scope_id option -> G.name * string list -> scope_id option)
    ~(may_implement : interface:cls -> cls -> bool) : t =
  let classes =
    Array.of_list
      (List.mapi
         (fun (id : int) (scopes : class_scope list) -> { id; scopes })
         classes)
  in
  let by_scope = Scope_tbl.create (Array.length classes) in
  Array.iter
    (fun (cls : cls) ->
      List.iter
        (fun (scope : class_scope) ->
          Scope_tbl.replace by_scope (scope_id_of scope) cls)
        cls.scopes)
    classes;
  let definitions = SId_tbl.create (Array.length classes) in
  Array.iter
    (fun (cls : cls) ->
      List.iter
        (fun (scope : class_scope) ->
          match scope.role with
          | Definition _ ->
              SId_tbl.replace definitions scope.binding
                (cls
                :: Option.value
                     (SId_tbl.find_opt definitions scope.binding)
                     ~default:[])
          | Singleton_object
          | Trait_impl _ ->
              ())
        cls.scopes)
    classes;
  let class_of_id (id : scope_id) : cls option =
    match (Scope_tbl.find_opt by_scope id, id.scope_role) with
    | (Some _ as found), _ -> found
    | None, Definition _ ->
        class_of_binding_in by_scope definitions id.scope_binding
    | None, (Singleton_object | Trait_impl _) -> None
  in
  let linked (scope : class_scope) (parent : parent) : cls option =
    Option.bind (link scope parent) class_of_id
  in
  let written_parents =
    Array.map
      (fun (cls : cls) ->
        List.map
          (fun (scope : class_scope) ->
            List.map
              (fun ((parent, placement) : parent * Linearisation.placement) ->
                (linked scope parent, placement))
              scope.parents)
          cls.scopes)
      classes
  in
  let external_class (cls : cls) : bool =
    not (List.exists defined cls.scopes)
  in
  let linearisation_parents (cls : cls) : cls Linearisation.parent list list =
    let written =
      List.map
        (List.map
           (fun ((parent, placement) : cls option * Linearisation.placement) ->
             match parent with
             | Some parent -> Linearisation.Bound (placement, parent)
             | None -> Linearisation.Unbound placement))
        written_parents.(cls.id)
    in
    if external_class cls then
      written @ [ [ Linearisation.Unbound Linearisation.Appended ] ]
    else written
  in
  let linearise =
    Linearisation.c3 ~equal:same ~hash ~parents:linearisation_parents
  in
  let orders = Array.map linearise classes in
  let direct_subclasses = Array.make (Array.length classes) [] in
  Array.iter
    (fun (cls : cls) ->
      List.iter
        (fun ((parent, _) : cls option * Linearisation.placement) ->
          Option.iter
            (fun (parent : cls) ->
              direct_subclasses.(parent.id) <-
                cls :: direct_subclasses.(parent.id))
            parent)
        (List.concat written_parents.(cls.id)))
    classes;
  (if Lang_config.interfaces_are_structural lang then
     let members_of (cls : cls) : Func_info.t list SMap.t =
       members_along orders.(cls.id).Linearisation.order
     in
     let interfaces, candidates =
       List.partition is_interface (Array.to_list classes)
     in
     let by_member_name = Hashtbl.create 256 in
     List.iter
       (fun (cls : cls) ->
         let members = members_of cls in
         let candidate = (cls, structural_methods members) in
         SMap.iter
           (fun (name : string) (_ : Func_info.t list) ->
             Hashtbl.replace by_member_name name
               (candidate
               :: Option.value (Hashtbl.find_opt by_member_name name)
                    ~default:[]))
           members)
       candidates;
     List.iter
       (fun (interface : cls) ->
         let members = members_of interface in
         let required = structural_methods members in
         let fewest =
           SMap.fold
             (fun (name : string) (_ : Func_info.t list)
                  (fewest :
                    (cls * (Func_info.t * Structural_typing.method_) list) list
                    option) ->
               let having =
                 Option.value (Hashtbl.find_opt by_member_name name) ~default:[]
               in
               match fewest with
               | Some known when List.compare_lengths known having <= 0 ->
                   fewest
               | Some _
               | None ->
                   Some having)
             members None
         in
         List.iter
           (fun ((cls, methods) :
                  cls * (Func_info.t * Structural_typing.method_) list) ->
             if
               may_implement ~interface cls
               && satisfied_in_one_build ~lang
                    ~equal_type:
                      (same_type_by_class
                         (class_of_name_in by_scope definitions outside))
                    ~compiled_together ~interface:required ~candidate:methods
             then
               direct_subclasses.(interface.id) <-
                 cls :: direct_subclasses.(interface.id))
           (Option.value fewest ~default:[]))
       interfaces);
  let entries =
    Array.map
      (fun (cls : cls) ->
        {
          parents = written_parents.(cls.id);
          class_side_parents =
            List.concat_map
              (fun (scope : class_scope) ->
                List.map (linked scope) scope.class_side_parents)
              cls.scopes;
          order = orders.(cls.id);
          subclasses = List_.uniq_by same direct_subclasses.(cls.id);
        })
      classes
  in
  {
    entries;
    classes;
    by_scope;
    definitions;
    outside =
      (fun (position : position) (context : scope_id option)
           (path : G.name * string list) ->
        Option.bind (outside position context path) class_of_id);
    descendants = Array.make (Array.length classes) None;
  }

let name_of_class (t : t) (cls : cls) : G.name option =
  List.find_map
    (fun (scope : class_scope) ->
      match (scope.role, class_of_binding t scope.binding) with
      | Definition _, Some found when same found cls ->
          let text, _, _, _ = G.SId.to_loc scope.binding in
          let info = G.empty_id_info () in
          info.G.id_resolved := Some (G.TypeName, scope.binding);
          Some (G.Id ((text, Tok.unsafe_fake_tok text), info))
      | (Definition _ | Singleton_object | Trait_impl _), _ -> None)
    cls.scopes

let equal_type (t : t) : Structural_typing.equal_type =
  same_type_by_class (class_of_name t ~position:Type_position ~context:None)

module Index_set = Set.Make (Int)

let descendants (t : t) (cls : cls) : cls list =
  match t.descendants.(cls.id) with
  | Some found -> found
  | None ->
      let rec visit (seen : Index_set.t) (found : cls list)
          (pending : cls list) : cls list =
        match pending with
        | [] -> found
        | current :: rest ->
            let direct =
              List.filter
                (fun (sub : cls) -> not (Index_set.mem sub.id seen))
                (subclasses t current)
            in
            visit
              (List.fold_left
                 (fun (seen : Index_set.t) (sub : cls) ->
                   Index_set.add sub.id seen)
                 seen direct)
              (direct @ found) (direct @ rest)
      in
      let found = visit (Index_set.singleton cls.id) [] [ cls ] in
      t.descendants.(cls.id) <- Some found;
      found
