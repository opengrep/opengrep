module G = AST_generic
module SMap = Common.SMap

(* A definition is keyed by its identity: two structurally equal definitions
   are two keys. *)
module Fdef_tbl = Hashtbl.Make (struct
  type t = G.function_definition

  let equal = ( == )

  let hash (fdef : t) : int =
    match Tok.loc_of_tok (snd fdef.G.fkind) with
    | Ok (loc : Tok.location) -> loc.Tok.pos.Pos.bytepos
    | Error _ -> Hashtbl.hash (snd fdef.G.fkind)
end)

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

type declaration_kind =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent =
  | Bound of G.SId.t
  | Unbound of G.type_
  | Impl of Function_id.t

type parent_clause = {
  parent : parent;
  relation : Member_lookup.relation;
  written : G.type_ option;
  arguments : G.arguments option;
  delegate : G.expr option;
}

type member_import = {
  source : parent option;
  members : (string * string) list;
}

type class_scope = {
  binding : G.SId.t;
  declaration_kind : declaration_kind;
  members : Func_info.t list SMap.t;
  fields : Func_info.t list Field_path_map.t;
  parents : parent_clause list;
  class_side_parents : parent list;
  member_imports : member_import list;
  type_members : (string * parent) list;
  requirements : string list;
  kind : kind;
  declaration : Lang_config.class_declaration;
  module_functions : Class_parents.module_functions;
  constructor_functions : Func_info.t list;
  object_fields : Func_info.t list Field_path_map.t;
  extensions : Func_info.t list SMap.t;
  reopens : bool;
}

type scope_id = {
  scope_binding : G.SId.t;
  scope_declaration_kind : declaration_kind;
}

let equal_declaration_kind (left : declaration_kind)
    (right : declaration_kind) : bool =
  match (left, right) with
  | Singleton_object, Singleton_object -> true
  | Definition left, Definition right
  | Trait_impl left, Trait_impl right ->
      Function_id.equal left right
  | (Definition _ | Singleton_object | Trait_impl _), _ -> false

let equal_scope_id (left : scope_id) (right : scope_id) : bool =
  G.SId.equal left.scope_binding right.scope_binding
  && equal_declaration_kind left.scope_declaration_kind
       right.scope_declaration_kind

let hash_scope_id (id : scope_id) : int =
  match id.scope_declaration_kind with
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

let parent_of_type (ty : G.type_) : parent =
  match
    Option.bind (name_of_type ty) (fun (name : G.name) ->
        binding_of_id_info (id_info_of_name name))
  with
  | Some parent -> Bound parent
  | None -> Unbound ty

(* The class a delegate expression holds: the declared type of the name it
   reads. *)
let delegate_type (e : G.expr) : parent option =
  match e.G.e with
  | G.N name ->
      Option.map parent_of_type
        (Ty_bare_name.instance_or_declared_type (id_info_of_name name))
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

let declaration_kind_rank (declaration_kind : declaration_kind) :
    int * Function_id.t option =
  match declaration_kind with
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
          let left_rank, left_site =
            declaration_kind_rank left.declaration_kind
          in
          let right_rank, right_site =
            declaration_kind_rank right.declaration_kind
          in
          (match Int.compare left_rank right_rank with
          | 0 -> Option.compare Function_id.compare left_site right_site
          | order -> order)
      | order -> order)
  | order -> order

let scope_id_of (scope : class_scope) : scope_id =
  {
    scope_binding = scope.binding;
    scope_declaration_kind = scope.declaration_kind;
  }

let definition_scope (sid : G.SId.t) : scope_id =
  {
    scope_binding = sid;
    scope_declaration_kind = Definition (Function_id.of_sid sid);
  }

type cls = {
  id : int;
  scopes : class_scope list;
}

type declared_type =
  | Project_class of cls
  | Unbound_path of string list
  | Not_compared

type definition = {
  func : Func_info.t;
  signature : declared_type Structural_typing.signature;
}

type selected = {
  definitions : (cls, definition) Member_lookup.selection;
  functions : (cls, Func_info.t) Member_lookup.selection;
}

let equal_side (left : Class_parents.side) (right : Class_parents.side) : bool =
  match (left, right) with
  | Class_parents.Instance_side, Class_parents.Instance_side
  | Class_parents.Class_side, Class_parents.Class_side ->
      true
  | (Class_parents.Instance_side | Class_parents.Class_side), _ -> false

module Selection_key = struct
  type t = {
    build_configuration : int;
    side : Class_parents.side;
    name : string;
    importing : int list;
    levels : (int * int) list list;
  }

  let equal (left : t) (right : t) : bool =
    Int.equal left.build_configuration right.build_configuration
    && equal_side left.side right.side
    && String.equal left.name right.name
    && List.equal Int.equal left.importing right.importing
    && List.equal
         (List.equal (fun ((left_class : int), (left_paths : int))
                          ((right_class : int), (right_paths : int)) ->
              Int.equal left_class right_class && Int.equal left_paths right_paths))
         left.levels right.levels

  let hash (key : t) : int =
    Hashtbl.hash
      (key.build_configuration, key.side, key.name, key.importing, key.levels)
end

module Selection_tbl = Hashtbl.Make (Selection_key)

module Overriding_key = struct
  type t = {
    build_configuration : int;
    cls : int;
    name : string;
  }

  let equal (left : t) (right : t) : bool =
    Int.equal left.build_configuration right.build_configuration
    && Int.equal left.cls right.cls
    && String.equal left.name right.name

  let hash (key : t) : int =
    Hashtbl.hash (key.build_configuration, key.cls, key.name)
end

module Overriding_tbl = Hashtbl.Make (Overriding_key)

type position =
  | Term_position
  | Type_position

module Type_name_key = struct
  type t = {
    position : position;
    context : scope_id option;
    file : string;
    resolved : G.resolved_name option;
    head : string list;
    rest : string list;
  }

  let equal_position (left : position) (right : position) : bool =
    match (left, right) with
    | Term_position, Term_position
    | Type_position, Type_position -> true
    | (Term_position | Type_position), _ -> false

  let equal_resolved (((left_kind : G.resolved_name_kind), (left : G.SId.t)))
      (((right_kind : G.resolved_name_kind), (right : G.SId.t))) : bool =
    G.equal_resolved_name_kind left_kind right_kind
    && G.SId.equal left right
    && G.SId.same_site left right

  let equal (left : t) (right : t) : bool =
    equal_position left.position right.position
    && Option.equal equal_scope_id left.context right.context
    && String.equal left.file right.file
    && Option.equal equal_resolved left.resolved right.resolved
    && List.equal String.equal left.head right.head
    && List.equal String.equal left.rest right.rest

  let hash (key : t) : int =
    Hashtbl.hash
      ( key.position,
        Option.map hash_scope_id key.context,
        key.file,
        Option.map G.hash_resolved_name key.resolved,
        key.head,
        key.rest )
end

module Type_name_tbl = Hashtbl.Make (Type_name_key)

type memo = {
  selections : selected Selection_tbl.t;
  overriding : definition list Overriding_tbl.t;
  dispatched : (definition list * Func_info.t list) list Overriding_tbl.t;
  type_name_memo : scope_id option Type_name_tbl.t;
}

let create_memo () : memo =
  {
    selections = Selection_tbl.create 64;
    overriding = Overriding_tbl.create 64;
    dispatched = Overriding_tbl.create 64;
    type_name_memo = Type_name_tbl.create 64;
  }

(* A lookup the class table's memo does not hold is recorded there when one
   domain owns the class table (the project table in the sequential stages,
   a per-file table in the worker that builds it), and in the own memo of a
   Symbol_table (Symbol_table.with_own_memo) in a pass whose domains share
   the class table, so no two domains write one memo. *)
type memo_target =
  | Class_table_memo
  | Own_memo of memo

let find_memoised (type key found) (class_table_memo : memo)
    (target : memo_target) (find : memo -> key -> found option) (key : key) :
    found option =
  match find class_table_memo key with
  | Some _ as found -> found
  | None -> (
      match target with
      | Class_table_memo -> None
      | Own_memo memo -> find memo key)

let record_memoised (type key found) (class_table_memo : memo)
    (target : memo_target) (record : memo -> key -> found -> unit) (key : key)
    (found : found) : unit =
  match target with
  | Class_table_memo -> record class_table_memo key found
  | Own_memo memo -> record memo key found

(* Every memo entry is a function of its key, so two memos that hold one key
   hold the same value for it, and a union replaces no value by another. *)
let merge_memo ~(into : memo) (from : memo) : unit =
  Selection_tbl.replace_seq into.selections (Selection_tbl.to_seq from.selections);
  Overriding_tbl.replace_seq into.overriding (Overriding_tbl.to_seq from.overriding);
  Overriding_tbl.replace_seq into.dispatched (Overriding_tbl.to_seq from.dispatched);
  Type_name_tbl.replace_seq into.type_name_memo
    (Type_name_tbl.to_seq from.type_name_memo)

type import_origin =
  | Imported_from of cls
  | Imported_from_mixins
  | Imported_from_unknown

type class_relations = {
  parents : (cls option * parent_clause) list list;
  imports : (string * (import_origin * string)) list;
  delegations : (cls option * cls option) list;
  class_side_parents : cls option list;
  order : cls Member_lookup.lookup_order;
  subclasses : cls list;
  descendants : cls list;
}

type t = {
  lang : Lang.t;
  relations : class_relations array;
  classes : cls array;
  by_scope : cls Scope_tbl.t;
  definitions : cls list SId_tbl.t;
  cross_file_resolver :
    memo_target -> position -> scope_id option -> G.name * string list -> cls option;
  own_definitions : definition list SMap.t array;
  compiled_in : int -> Func_info.t -> bool;
  memo : memo;
}

let same (left : cls) (right : cls) : bool = Int.equal left.id right.id
let hash (cls : cls) : int = Hashtbl.hash cls.id
let index (cls : cls) : int = cls.id
let scopes (cls : cls) : class_scope list = cls.scopes

let distinct_by (type item) (func_of : item -> Func_info.t) (items : item list)
    : item list =
  let seen : unit Fdef_tbl.t = Fdef_tbl.create (List.length items) in
  List.rev
    (List.fold_left
       (fun (kept : item list) (item : item) ->
         let fdef = (func_of item).Func_info.fdef in
         if Fdef_tbl.mem seen fdef then kept
         else (
           Fdef_tbl.replace seen fdef ();
           item :: kept))
       [] items)

let distinct_definitions (funcs : Func_info.t list) : Func_info.t list =
  distinct_by Fun.id funcs

let concat_scopes (cls : cls) (of_scope : class_scope -> Func_info.t list) :
    Func_info.t list =
  List.concat_map of_scope cls.scopes |> distinct_definitions

let own_members (cls : cls) (name : string) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) ->
      Option.value (SMap.find_opt name scope.members) ~default:[])

let member_table (cls : cls) : Func_info.t list SMap.t =
  List.fold_left
    (fun (members : Func_info.t list SMap.t) (scope : class_scope) ->
      SMap.union
        (fun (_ : string) (earlier : Func_info.t list) (later : Func_info.t list)
           ->
          Some (distinct_definitions (earlier @ later)))
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

let constructor_functions (cls : cls) : Func_info.t list =
  concat_scopes cls (fun (scope : class_scope) -> scope.constructor_functions)

let is_module_function (cls : cls) (name : string) : bool =
  List.exists
    (fun (scope : class_scope) ->
      Class_parents.is_module_function scope.module_functions name)
    cls.scopes

let is_abstract_type (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.kind with
      | Class_kind (G.Interface | G.Trait) -> true
      | Class_kind (G.Class | G.Struct | G.Object | G.Extension _)
      | Module_kind ->
          false)
    cls.scopes

let declarations (cls : cls) : Lang_config.class_declaration list =
  List.map (fun (scope : class_scope) -> scope.declaration) cls.scopes

let is_interface (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.kind with
      | Class_kind G.Interface -> true
      | Class_kind (G.Class | G.Struct | G.Object | G.Trait | G.Extension _)
      | Module_kind ->
          false)
    cls.scopes

let is_trait (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.kind with
      | Class_kind G.Trait -> true
      | Class_kind (G.Class | G.Struct | G.Object | G.Interface | G.Extension _)
      | Module_kind ->
          false)
    cls.scopes

let is_trait_impl (cls : cls) : bool =
  List.exists
    (fun (scope : class_scope) ->
      match scope.declaration_kind with
      | Trait_impl _ -> true
      | Definition _
      | Singleton_object ->
          false)
    cls.scopes

let relations_of (t : t) (cls : cls) : class_relations = t.relations.(cls.id)
let order (t : t) (cls : cls) : cls Member_lookup.lookup_order =
  (relations_of t cls).order

let subclasses (t : t) (cls : cls) : cls list = (relations_of t cls).subclasses
let descendants (t : t) (cls : cls) : cls list = (relations_of t cls).descendants

let class_side_parents (t : t) (cls : cls) : cls option list =
  (relations_of t cls).class_side_parents

let parents (t : t) (cls : cls) : cls option list =
  List.concat_map (List.map fst) (relations_of t cls).parents

let parent_clauses (t : t) (cls : cls) : (cls option * parent_clause) list =
  List.concat (relations_of t cls).parents

let delegations (t : t) (cls : cls) : (cls option * cls option) list =
  (relations_of t cls).delegations

let requirements (cls : cls) : string list =
  List.concat_map (fun (scope : class_scope) -> scope.requirements) cls.scopes

let imports (t : t) (cls : cls) (name : string) : (import_origin * string) list
    =
  List.filter_map
    (fun ((alias_name : string), (imported : import_origin * string)) ->
      if String.equal alias_name name then Some imported else None)
    (relations_of t cls).imports

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
            { scope_binding = sid; scope_declaration_kind = Singleton_object })

let class_of_binding (t : t) (sid : G.SId.t) : cls option =
  class_of_binding_in t.by_scope t.definitions sid

let object_of_binding (t : t) (sid : G.SId.t) : cls option =
  match class_of_scope t { scope_binding = sid; scope_declaration_kind = Singleton_object } with
  | Some _ as found -> found
  | None -> class_of_binding t sid

let class_of_name (t : t) ~(memo_target : memo_target) ~(position : position)
    ~(context : scope_id option) (name : G.name) : cls option =
  match
    Option.bind
      (binding_of_id_info (id_info_of_name name))
      (class_of_binding t)
  with
  | Some _ as found -> found
  | None -> t.cross_file_resolver memo_target position context (name, [])

let class_of_path (t : t) ~(memo_target : memo_target) ~(position : position)
    ~(context : scope_id option)
    (((head : G.name), (rest : string list)) as path) : cls option =
  match rest with
  | [] -> class_of_name t ~memo_target ~position ~context head
  | _ :: _ -> t.cross_file_resolver memo_target position context path

let declared_type (class_of : G.name -> cls option) (ty : G.type_) :
    declared_type =
  match name_of_type ty with
  | None -> Not_compared
  | Some name -> (
      match class_of name with
      | Some cls -> Project_class cls
      | None -> (
          match binding_of_id_info (id_info_of_name name) with
          | None -> Unbound_path (qualified_path name)
          | Some _ -> Not_compared))

let same_declared_type ~(required : declared_type) ~(candidate : declared_type)
    : bool option =
  match (required, candidate) with
  | Project_class required, Project_class candidate ->
      Some (same required candidate)
  | Unbound_path required, Unbound_path candidate ->
      Some (List.equal String.equal required candidate)
  | (Project_class _ | Unbound_path _ | Not_compared), _ -> None

let satisfies ~(required : definition) (candidate : definition) : bool =
  Structural_typing.satisfies ~equal_type:same_declared_type
    ~required:required.signature candidate.signature

let structural_methods (members : definition list SMap.t) :
    (string * definition) list =
  SMap.fold
    (fun (name : string) (definitions : definition list)
         (methods : (string * definition) list) ->
      List.map (fun (definition : definition) -> (name, definition)) definitions
      @ methods)
    members []

let satisfied_in_one_build ~(compiled_together : Func_info.t list -> bool)
    ~(interface : (string * definition) list)
    ~(candidate : (string * definition) list) : bool =
  let options =
    List.map
      (fun ((name : string), (required : definition)) ->
        List.filter_map
          (fun ((offered_name : string), (offered : definition)) ->
            if String.equal name offered_name && satisfies ~required offered
            then Some offered.func
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
  (not (List_.null interface))
  && choose
       (List.map (fun ((_ : string), (required : definition)) -> required.func)
          interface)
       options

let overrides ~(lang : Lang.t) ~(nearer : definition) ~(farther : definition) :
    bool =
  (not (Lang_config.overloads_by_type lang))
  || satisfies ~required:farther nearer

let overload_key (lang : Lang.t) (definition : definition) : int =
  if Lang_config.overloads_by_type lang then
    definition.signature.Structural_typing.arity
  else 0

let select_member ~(lang : Lang.t) (levels : cls Member_lookup.level list)
    ~(defines : cls -> definition list) :
    (cls, definition) Member_lookup.selection =
  Member_lookup.select ~equal:same ~defines ~overrides:(overrides ~lang)
    ~overload_key:(overload_key lang)
    ~declared_only:(fun (definition : definition) ->
      not (Func_info.has_body definition.func.Func_info.fdef))
    ~is_static_member:(fun (definition : definition) ->
      Receiver.is_static definition.func.Func_info.entity)
    ~accumulate:
      (Lang_config.overloads_by_type lang
      && not
           (Member_lookup.hides_inherited_overloads
              (Lang_config.member_lookup lang)))
    levels

let level_classes (levels : cls Member_lookup.level list) : cls list =
  List.concat_map Member_lookup.level_classes levels

let members_by_levels ~(lang : Lang.t)
    ~(definition_table : cls -> definition list SMap.t)
    (levels : cls Member_lookup.level list) : definition list SMap.t =
  List.fold_left
    (fun (names : definition list SMap.t) (cls : cls) ->
      SMap.union
        (fun (_ : string) (known : definition list) (_ : definition list) ->
          Some known)
        names (definition_table cls))
    SMap.empty (level_classes levels)
  |> SMap.filter_map (fun (name : string) (_ : definition list) ->
         match
           select_member ~lang levels ~defines:(fun (cls : cls) ->
               Option.value (SMap.find_opt name (definition_table cls))
                 ~default:[])
         with
         | Member_lookup.Selected (_, defined) -> Some defined
         | Member_lookup.Ambiguous
         | Member_lookup.Undefined
         | Member_lookup.Unknown ->
             None)

(* The written path of a trait, as its import resolves it or as it is
   written, is one of the language's dereference traits. *)
let is_dereference_trait (lang : Lang.t) (written : G.type_) : bool =
  match (Lang_config.dereference lang, name_of_type written) with
  | Some dereference, Some name ->
      let path =
        match !((id_info_of_name name).G.id_resolved) with
        | Some (G.ImportedEntity canonical, _) -> canonical
        | Some _
        | None ->
            qualified_path name
      in
      List.exists (List.equal String.equal path) dereference.Lang_config.traits
  | None, _
  | _, None ->
      false

module Index_set = Set.Make (Int)

let descendants_of ~(subclasses : cls -> cls list) (cls : cls) : cls list =
  let rec visit (seen : Index_set.t) (found : cls list)
      (pending : cls list) : cls list =
    match pending with
    | [] -> found
    | current :: rest ->
        let direct =
          List.filter
            (fun (sub : cls) -> not (Index_set.mem sub.id seen))
            (subclasses current)
        in
        visit
          (List.fold_left
             (fun (seen : Index_set.t) (sub : cls) ->
               Index_set.add sub.id seen)
             seen direct)
          (direct @ found) (direct @ rest)
  in
  visit (Index_set.singleton cls.id) [] [ cls ]

let build ~(lang : Lang.t) ~(classes : class_scope list list)
    ~(compiled_together : Func_info.t list -> bool)
    ~(compiled_in : int -> Func_info.t -> bool)
    ~(defined : class_scope -> bool)
    ~(link : class_scope -> parent -> scope_id option)
    ~(cross_file_resolver :
       memo ->
       memo_target ->
       position ->
       scope_id option ->
       G.name * string list ->
       scope_id option)
    ~(may_implement : interface:cls -> cls -> bool) : t =
  let memo = create_memo () in
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
          match scope.declaration_kind with
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
    match (Scope_tbl.find_opt by_scope id, id.scope_declaration_kind) with
    | (Some _ as found), _ -> found
    | None, Definition _ ->
        class_of_binding_in by_scope definitions id.scope_binding
    | None, (Singleton_object | Trait_impl _) -> None
  in
  let linked (scope : class_scope) (parent : parent) : cls option =
    Option.bind (link scope parent) class_of_id
  in
  let class_of_type_name (name : G.name) : cls option =
    match
      Option.bind
        (binding_of_id_info (id_info_of_name name))
        (class_of_binding_in by_scope definitions)
    with
    | Some _ as found -> found
    | None ->
        Option.bind
          (cross_file_resolver memo Class_table_memo Type_position None
             (name, []))
          class_of_id
  in
  let own_definitions =
    Array.map
      (fun (cls : cls) ->
        SMap.map
          (List.map (fun (func : Func_info.t) ->
               {
                 func;
                 signature =
                   Structural_typing.signature ~lang
                     ~declared:(declared_type class_of_type_name)
                     func.Func_info.entity func.Func_info.fdef;
               }))
          (member_table cls))
      classes
  in
  let written_parents =
    Array.map
      (fun (cls : cls) ->
        List.map
          (fun (scope : class_scope) ->
            List.map
              (fun (clause : parent_clause) ->
                (linked scope clause.parent, clause))
              scope.parents)
          cls.scopes)
      classes
  in
  let external_class (cls : cls) : bool =
    not (List.exists defined cls.scopes)
  in
  let lookup_parents (cls : cls) : cls Member_lookup.parent list list =
    List.map
      (List.map
         (fun ((parent, clause) : cls option * parent_clause) ->
           match parent with
           | Some parent -> Member_lookup.Resolved (clause.relation, parent)
           | None -> Member_lookup.Unresolved clause.relation))
      written_parents.(cls.id)
  in
  let linked_type_members =
    Array.map
      (fun (cls : cls) ->
        List.concat_map
          (fun (scope : class_scope) ->
            List.map
              (fun ((name : string), (member : parent)) ->
                (name, linked scope member))
              scope.type_members)
          cls.scopes)
      classes
  in
  let dereferences (cls : cls) : cls option =
    match Lang_config.dereference lang with
    | None -> None
    | Some dereference ->
        let dereferencing (clause : parent_clause) : bool =
          Option.fold ~none:false
            ~some:(is_dereference_trait lang)
            clause.written
        in
        List.find_map
          (fun ((impl, clause) : cls option * parent_clause) ->
            match (impl, clause.relation) with
            | Some impl, Member_lookup.Implements
              when List.exists
                     (fun ((_ : cls option), (trait : parent_clause)) ->
                       dereferencing trait)
                     (List.concat written_parents.(impl.id)) ->
                List.find_map
                  (fun ((name : string), (target : cls option)) ->
                    if String.equal name dereference.Lang_config.target then
                      target
                    else None)
                  linked_type_members.(impl.id)
            | _ -> None)
          (List.concat written_parents.(cls.id))
  in
  let lookup_order =
    Member_lookup.lookup_order
      (Lang_config.member_lookup lang)
      ~equal:same ~hash ~parents:lookup_parents ~is_interface
      ~is_external:external_class ~dereferences
  in
  let orders = Array.map lookup_order classes in
  let direct_subclasses = Array.make (Array.length classes) [] in
  Array.iter
    (fun (cls : cls) ->
      List.iter
        (fun ((parent, _) : cls option * parent_clause) ->
          Option.iter
            (fun (parent : cls) ->
              direct_subclasses.(parent.id) <-
                cls :: direct_subclasses.(parent.id))
            parent)
        (List.concat written_parents.(cls.id)))
    classes;
  (if Lang_config.interfaces_are_structural lang then
     let members_of (cls : cls) : definition list SMap.t =
       members_by_levels ~lang
         ~definition_table:(fun (cls : cls) -> own_definitions.(cls.id))
         orders.(cls.id).Member_lookup.levels
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
           (fun (name : string) (_ : definition list) ->
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
             (fun (name : string) (_ : definition list)
                  (fewest : (cls * (string * definition) list) list option) ->
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
           (fun ((cls, methods) : cls * (string * definition) list) ->
             if
               may_implement ~interface cls
               && satisfied_in_one_build ~compiled_together
                    ~interface:required ~candidate:methods
             then
               direct_subclasses.(interface.id) <-
                 cls :: direct_subclasses.(interface.id))
           (Option.value fewest ~default:[]))
       interfaces);
  let subclasses =
    Array.map
      (fun (cls : cls) -> List_.uniq_by same direct_subclasses.(cls.id))
      classes
  in
  let relations =
    Array.map
      (fun (cls : cls) ->
        {
          parents = written_parents.(cls.id);
          imports =
            List.concat_map
              (fun (scope : class_scope) ->
                List.concat_map
                  (fun (import : member_import) ->
                    let origin =
                      match import.source with
                      | None -> Imported_from_mixins
                      | Some source -> (
                          match linked scope source with
                          | Some found -> Imported_from found
                          | None -> Imported_from_unknown)
                    in
                    List.map
                      (fun ((imported : string), (alias_name : string)) ->
                        (alias_name, (origin, imported)))
                      import.members)
                  scope.member_imports)
              cls.scopes;
          delegations =
            List.concat_map
              (fun (scope : class_scope) ->
                List.filter_map
                  (fun (clause : parent_clause) ->
                    Option.map
                      (fun (delegate : G.expr) ->
                        ( linked scope clause.parent,
                          Option.bind (delegate_type delegate) (linked scope) ))
                      clause.delegate)
                  scope.parents)
              cls.scopes;
          class_side_parents =
            List.concat_map
              (fun (scope : class_scope) ->
                List.map (linked scope) scope.class_side_parents)
              cls.scopes;
          order = orders.(cls.id);
          subclasses = subclasses.(cls.id);
          descendants =
            descendants_of
              ~subclasses:(fun (cls : cls) -> subclasses.(cls.id))
              cls;
        })
      classes
  in
  {
    lang;
    relations;
    classes;
    by_scope;
    definitions;
    cross_file_resolver =
      (fun (target : memo_target) (position : position)
           (context : scope_id option) (path : G.name * string list) ->
        Option.bind
          (cross_file_resolver memo target position context path)
          class_of_id);
    own_definitions;
    compiled_in;
    memo;
  }

let name_of_class (t : t) (cls : cls) : G.name option =
  List.find_map
    (fun (scope : class_scope) ->
      match (scope.declaration_kind, class_of_binding t scope.binding) with
      | Definition _, Some found when same found cls ->
          let text, _, _, _ = G.SId.to_loc scope.binding in
          let info = G.empty_id_info () in
          info.G.id_resolved := Some (G.TypeName, scope.binding);
          Some (G.Id ((text, Tok.unsafe_fake_tok text), info))
      | (Definition _ | Singleton_object | Trait_impl _), _ -> None)
    cls.scopes

let definition_table (t : t) (cls : cls) : definition list SMap.t =
  t.own_definitions.(cls.id)

let own_definitions (t : t) (cls : cls) (name : string) : definition list =
  Option.value (SMap.find_opt name (definition_table t cls)) ~default:[]

let member_definitions (t : t) (cls : cls) : definition list SMap.t =
  members_by_levels ~lang:t.lang ~definition_table:(definition_table t)
    (order t cls).Member_lookup.levels

let members (t : t) (cls : cls) : Func_info.t list SMap.t =
  SMap.map
    (List.map (fun (definition : definition) -> definition.func))
    (member_definitions t cls)

let compiled_in (t : t) ~(build_configuration : int) (func : Func_info.t) :
    bool =
  t.compiled_in build_configuration func

let memo (t : t) : memo = t.memo

let selected_of (definitions : (cls, definition) Member_lookup.selection) :
    selected =
  {
    definitions;
    functions =
      (match definitions with
      | Member_lookup.Selected (definer, found) ->
          Member_lookup.Selected
            ( definer,
              List.map (fun (definition : definition) -> definition.func) found
            )
      | Member_lookup.Ambiguous -> Member_lookup.Ambiguous
      | Member_lookup.Undefined -> Member_lookup.Undefined
      | Member_lookup.Unknown -> Member_lookup.Unknown);
  }

let nearest (type found) (levels : cls Member_lookup.level list)
    ~(defines : cls -> found list) : (cls, found) Member_lookup.selection =
  Member_lookup.select ~equal:same ~defines
    ~overrides:(fun ~nearer:_ ~farther:_ -> true)
    ~overload_key:(fun (_ : found) -> 0)
    ~declared_only:(fun (_ : found) -> false)
    ~is_static_member:(fun (_ : found) -> false)
    ~accumulate:false levels

let find_nearest (type found) (levels : cls Member_lookup.level list)
    (lookup : cls -> found option) : found option =
  match nearest levels ~defines:(fun (cls : cls) -> Option.to_list (lookup cls)) with
  | Member_lookup.Selected (_, [ found ]) -> Some found
  | Member_lookup.Selected _
  | Member_lookup.Ambiguous
  | Member_lookup.Undefined
  | Member_lookup.Unknown ->
      None
