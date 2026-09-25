(* The definitions one file makes, reached through the bindings naming
   recorded on its names: a binding holds function definitions, a class its
   members, the functions stored in the fields of its instances and its
   written parents, and the object a binding holds the functions stored in
   its fields. *)

module G = AST_generic
module SMap = Common.SMap
module SId_tbl = Class_table.SId_tbl
module Scope_tbl = Class_table.Scope_tbl
module Field_path_map = Class_table.Field_path_map

(* a definition is itself, not an equal copy *)
module Fdef_tbl = Hashtbl.Make (struct
  type t = G.function_definition

  let equal = ( == )
  let hash (fdef : t) = Hashtbl.hash (snd fdef.G.fkind)
end)

module Node_tbl = Hashtbl.Make (struct
  type t = Function_id.t

  let equal = Function_id.equal
  let hash = Function_id.hash
end)

module Path_tbl = Hashtbl.Make (struct
  type t = string list

  let equal = List.equal String.equal
  let hash = Hashtbl.hash
end)

type kind = Class_table.kind =
  | Class_kind of G.class_kind
  | Module_kind

type role = Class_table.role =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent = Class_table.parent =
  | Bound of G.SId.t
  | Unbound of G.type_
  | Impl of Function_id.t

type class_scope = Class_table.class_scope = {
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

type scope_id = Class_table.scope_id = {
  scope_binding : G.SId.t;
  scope_role : role;
}

type held_class =
  | Of_class of Class_table.cls
  | Of_external_class
  | Of_unknown_class

type held_object = {
  holder : G.SId.t;
  path : string list;
  held_class : held_class;
}

type receiver_class =
  | Class of Class_table.cls
  | Exact of Class_table.cls
  | Class_object of Class_table.cls
  | Ancestors_of of Class_table.cls
  | Object_of of held_object
  | External_class
  | Root
  | Unknown

type resolution =
  | Defined of Func_info.t list
  | External

type receiver_role =
  | Method_of
  | Extension_of

module Table_ops (Table : Hashtbl.S) = struct
  let add_to (table : 'a list Table.t) (key : Table.key) (values : 'a list) :
      unit =
    Table.replace table key
      (Option.value (Table.find_opt table key) ~default:[] @ values)

  let joined (funcs : Func_info.t list) (held : Func_info.t list option) :
      Func_info.t list option =
    Some
      (List_.uniq_by
         (fun (left : Func_info.t) (right : Func_info.t) ->
           left.Func_info.fdef == right.Func_info.fdef)
         (Option.value held ~default:[] @ funcs))

  let add_named (table : Func_info.t list SMap.t Table.t) (key : Table.key)
      (name : string) (funcs : Func_info.t list) : unit =
    match funcs with
    | [] -> ()
    | _ :: _ ->
        Table.replace table key
          (SMap.update name (joined funcs)
             (Option.value (Table.find_opt table key) ~default:SMap.empty))

  let add_at_path (table : Func_info.t list Field_path_map.t Table.t)
      (key : Table.key) (path : string list) (funcs : Func_info.t list) : unit
      =
    match funcs with
    | [] -> ()
    | _ :: _ ->
        Table.replace table key
          (Field_path_map.update path (joined funcs)
             (Option.value (Table.find_opt table key)
                ~default:Field_path_map.empty))

  let at_path (table : Func_info.t list Field_path_map.t Table.t)
      (key : Table.key) (path : string list) : Func_info.t list =
    Option.bind (Table.find_opt table key) (Field_path_map.find_opt path)
    |> Option.value ~default:[]
end

module By_binding = Table_ops (SId_tbl)
module By_scope = Table_ops (Scope_tbl)

(* What [this] denotes in a function's body: an instance of a class, or the
   object at a field path of the object a binding holds when the function is
   stored in a field of it. *)
type self_type =
  | Instance_of of scope_id
  | Instance_of_type of G.type_
  | Class_of of scope_id
  | Held_by of G.SId.t * string list

type field_owner =
  | Instance_field of scope_id * string list
  | Object_field of G.SId.t * string list
  | Prototype_member of scope_id

type context = {
  class_body : scope_id option;
  self_type : self_type option;
  namespace : string list option;
  enclosing_record : field_owner option;
  in_function : bool;
}

type use =
  | Called
  | Referenced

type assigned_value = {
  value : G.expr;
  in_function : bool;
}

let holds_function_value (e : G.expr) : bool =
  match e.G.e with
  | G.N _
  | G.DotAccess _
  | G.Ref (_, { G.e = G.N _; _ }) ->
      true
  | _ -> false

type t = {
  lang : Lang.t;
  functions : Func_info.t list SId_tbl.t;
  scopes : class_scope Scope_tbl.t;
  classes : Class_table.t;
  defined_here : unit Scope_tbl.t;
  owners : scope_id Scope_tbl.t;
  function_fields : Func_info.t list Field_path_map.t SId_tbl.t;
  unbound_receivers : (receiver_role * G.name * Func_info.t list) list;
  selves : self_type Fdef_tbl.t;
  receivers : self_type SId_tbl.t;
  qualified_functions : Func_info.t list Path_tbl.t;
  qualified_classes : scope_id Path_tbl.t;
  type_aliases : G.type_ SId_tbl.t;
  class_definitions : (scope_id * G.definition_kind) list;
  class_sites : G.SId.t list SId_tbl.t;
  values : assigned_value list SId_tbl.t;
  by_node : Func_info.t Node_tbl.t;
  extension_visible : string -> Func_info.t -> bool;
  compiled_with_file : Func_info.t -> bool;
  outside : t -> caller:Function_id.t option -> G.expr -> resolution;
  types : Type_state.t;
  declared_types : (G.SId.t * G.SId.t option, receiver_class) Hashtbl.t;
}

let binding_of_id_info = Class_table.binding_of_id_info
let id_info_of_name = Class_table.id_info_of_name
let name_of_type = Class_table.name_of_type
let qualified_path = Class_table.qualified_path
let definition_scope = Class_table.definition_scope
let scope_id_of = Class_table.scope_id_of

let last_ident_of_name (name : G.name) : G.ident =
  match name with
  | G.Id (id, _) -> id
  | G.IdQualified { G.name_last = id, _; _ } -> id

let defined_elsewhere (info : G.id_info) : bool =
  match !(info.G.id_resolved) with
  | Some ((G.ImportedEntity _ | G.ImportedModule _ | G.GlobalName _), _)
  | None ->
      true
  | Some
      ( ( G.Global | G.LocalVar | G.Parameter | G.EnclosedVar | G.TypeName
        | G.Macro | G.EnumConstant ),
        _ ) ->
      false

let binding_of_function (func : Func_info.t) : G.SId.t option =
  match func.Func_info.entity with
  | Some { G.name = G.EN name; _ } -> binding_of_id_info (id_info_of_name name)
  | Some _
  | None ->
      None

(* The name a function is selected by as a member: the name its definition
   gives it, [f] in [M.f = function () {}] too. *)
let member_name (func : Func_info.t) : string option =
  match func.Func_info.entity with
  | Some { G.name = G.EN name; _ }
  | Some { G.name = G.EDynamic { G.e = G.DotAccess (_, _, G.FN name); _ }; _ }
    ->
      Some (fst (last_ident_of_name name))
  | Some _
  | None ->
      Option.map
        (fun (name : IL.name) -> fst name.IL.ident)
        (Func_info.bare_name func.Func_info.fn_id)

let same_definition (left : Func_info.t) (right : Func_info.t) : bool =
  left.Func_info.fdef == right.Func_info.fdef

(* JavaScript: [C.prototype.m = ...] defines a method of the instances [C]
   constructs. *)
let prototype_holds_methods (lang : Lang.t) : bool =
  match lang with
  | Lang.Js
  | Lang.Ts
  | Lang.Vue ->
      true
  | _ -> false

(* Ruby and Crystal: a [def] at the top level is a method of the root object,
   an ancestor of every class, and a bare name no local binds is a call on
   [self]. *)
let top_level_defs_are_methods_of_object (lang : Lang.t) : bool =
  match lang with
  | Lang.Ruby
  | Lang.Crystal ->
      true
  | _ -> false

(* The definitions a use with the binding [sid] sees: in a language with
   overloads, every definition under the binding (an overload set); elsewhere
   only the definition whose own binding carries the use's site, or none. *)
let names_class (class_sites : G.SId.t list SId_tbl.t) (sid : G.SId.t) : bool =
  List.exists (G.SId.same_site sid)
    (Option.value (SId_tbl.find_opt class_sites sid) ~default:[])

let definitions_seen ~(lang : Lang.t) (functions : Func_info.t list SId_tbl.t)
    (sid : G.SId.t) : Func_info.t list =
  match SId_tbl.find_opt functions sid with
  | None -> []
  | Some defined when Lang_config.overloads_by_type lang -> defined
  | Some defined ->
      List.filter
        (fun (func : Func_info.t) ->
          match binding_of_function func with
          | Some own -> G.SId.same_site own sid
          | None -> false)
        defined

let class_sites_of (ast : G.program) : G.SId.t list SId_tbl.t =
  let sites = SId_tbl.create 16 in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_definition env ((ent, def) as definition) =
        (match (ent.G.name, def) with
        | G.EN name, G.ClassDef _ ->
            Option.iter
              (fun (sid : G.SId.t) -> By_binding.add_to sites sid [ sid ])
              (binding_of_id_info (id_info_of_name name))
        | _ -> ());
        super#visit_definition env definition
    end
  in
  visitor#visit_program () ast;
  sites

(* A member declared in the class body and defined out of it ([void A::m()])
   and the members of an overload set share one binding. *)
let definitions_of_member (functions : Func_info.t list SId_tbl.t)
    (func : Func_info.t) : Func_info.t list =
  match Option.bind (binding_of_function func) (SId_tbl.find_opt functions) with
  | Some (_ :: _ as defined) -> defined
  | Some []
  | None ->
      [ func ]

(* A chain of member accesses: its root and the field names read from it. *)
let rec field_chain (e : G.expr) : G.expr * string list =
  match e.G.e with
  | G.DotAccess (inner, _, G.FN name) ->
      let root, path = field_chain inner in
      (root, path @ [ fst (last_ident_of_name name) ])
  | _ -> (e, [])

(* A member chain rooted at a name naming leaves unbound: the qualified name
   the chain writes, which the language resolves in the global namespace. *)
let unbound_chain_path (e : G.expr) : string list option =
  let root, prefix = field_chain e in
  match root.G.e with
  | G.N name when Option.is_none (binding_of_id_info (id_info_of_name name))
    ->
      Some (qualified_path name @ prefix)
  | _ -> None

type receiver_type =
  | File_class of G.SId.t
  | Other_type of G.type_ * G.name
  | No_receiver_type

let site_of_name (name : G.name) : Function_id.t =
  Function_id.of_il_name (AST_to_IL.var_of_name name)

let create ~(lang : Lang.t) (ast : G.program) (funcs : Func_info.t list) : t =
  let config = Lang_config.get lang in
  let functions = SId_tbl.create (List.length funcs) in
  funcs
  |> List.iter (fun (func : Func_info.t) ->
         match binding_of_function func with
         | Some sid -> By_binding.add_to functions sid [ func ]
         | None -> ());
  let by_node = Node_tbl.create (List.length funcs) in
  funcs
  |> List.iter (fun (func : Func_info.t) ->
         match Func_info.bare_name func.Func_info.fn_id with
         | Some name ->
             Node_tbl.replace by_node (Function_id.of_il_name name) func
         | None -> ());
  let of_fdef = Fdef_tbl.create (List.length funcs) in
  funcs
  |> List.iter (fun (func : Func_info.t) ->
         Fdef_tbl.replace of_fdef func.Func_info.fdef func);
  let defined_by (fdef : G.function_definition) : Func_info.t list =
    Option.to_list (Fdef_tbl.find_opt of_fdef fdef)
  in
  let class_sites = class_sites_of ast in
  let values = SId_tbl.create 16 in
  let assign (context : context) (name : G.name) (value : G.expr) : unit =
    if holds_function_value value then
      Option.iter
        (fun (sid : G.SId.t) ->
          By_binding.add_to values sid
            [ { value; in_function = context.in_function } ])
        (binding_of_id_info (id_info_of_name name))
  in
  let held_by_name (name : G.name) : Func_info.t list =
    match binding_of_id_info (id_info_of_name name) with
    | Some sid -> definitions_seen ~lang functions sid
    | None -> []
  in
  let kinds = Scope_tbl.create 16 in
  let external_classes = Scope_tbl.create 16 in
  let written_parents = Scope_tbl.create 16 in
  let written_class_side_parents = Scope_tbl.create 16 in
  let exposures = Scope_tbl.create 16 in
  let reopening = Scope_tbl.create 16 in
  let owners = Scope_tbl.create 16 in
  let members = Scope_tbl.create 16 in
  let instance_fields = Scope_tbl.create 16 in
  let function_fields = SId_tbl.create 16 in
  let extensions = SId_tbl.create 16 in
  let unbound_receivers = ref [] in
  let selves = Fdef_tbl.create (List.length funcs) in
  let receivers = SId_tbl.create 16 in
  let qualified_functions = Path_tbl.create 16 in
  let qualified_classes = Path_tbl.create 16 in
  let type_aliases = SId_tbl.create 16 in
  let class_definitions = ref [] in
  let initialisers = Scope_tbl.create 16 in
  let add_initialiser (scope : scope_id) (ent : G.entity)
      (def : G.definition_kind) : unit =
    match def with
    | G.ClassDef (cdef : G.class_definition)
      when Lang_config.class_header_is_constructor lang ->
        Option.iter
          (fun (initialiser : Func_info.t) ->
            Scope_tbl.replace initialisers scope initialiser)
          (Option.bind (Visit_function_defs.initialised_class_name ent cdef)
             (fun (class_name : G.name) ->
               Node_tbl.find_opt by_node
                 (Function_id.of_il_name
                    (Visit_function_defs.class_initialiser_il_name class_name))))
    | _ -> ()
  in
  let add_member (cls : scope_id) (func : Func_info.t) : unit =
    match member_name func with
    | Some name ->
        By_scope.add_named members cls name (definitions_of_member functions func)
    | None -> ()
  in
  let add_parents (scope : scope_id) (def : G.definition_kind) : unit =
    let parent_of (written : G.type_) : parent =
      match written.G.t with
      | G.TyExpr { G.e = G.IdSpecial (G.Self, _); _ } -> Bound scope.scope_binding
      | _ -> (
          match
            Option.bind (name_of_type written) (fun (name : G.name) ->
                binding_of_id_info (id_info_of_name name))
          with
          | Some parent -> Bound parent
          | None -> Unbound written)
    in
    let instance_side, class_side =
      List.partition
        (fun (written : Class_parents.t) ->
          match written.Class_parents.side with
          | Class_parents.Instance_side -> true
          | Class_parents.Class_side -> false)
        (Class_parents.of_definition lang def)
    in
    By_scope.add_to written_parents scope
      (List.map
         (fun (written : Class_parents.t) ->
           (parent_of written.Class_parents.written, written.Class_parents.placement))
         instance_side);
    By_scope.add_to written_class_side_parents scope
      (List.map
         (fun (written : Class_parents.t) ->
           parent_of written.Class_parents.written)
         class_side);
    Scope_tbl.replace exposures scope
      (Class_parents.singleton_exposure lang def)
  in
  let declare_class (context : context) (scope : scope_id) (kind : kind)
      (ent : G.entity) (name : G.name) (def : G.definition_kind) : unit =
    Scope_tbl.replace kinds scope kind;
    class_definitions := (scope, def) :: !class_definitions;
    add_initialiser scope ent def;
    if Class_parents.reopens lang ent def then
      Scope_tbl.replace reopening scope ();
    Option.iter (Scope_tbl.replace owners scope) context.class_body;
    add_parents scope def;
    match context.namespace with
    | Some path ->
        Path_tbl.add qualified_classes (path @ qualified_path name) scope
    | None -> ()
  in
  let opaque =
    {
      class_body = None;
      self_type = None;
      namespace = None;
      enclosing_record = None;
      in_function = false;
    }
  in
  let definition_of (ent : G.entity) (name : G.name) (def : G.definition_kind)
      (sid : G.SId.t) : scope_id =
    if Class_parents.reopens lang ent def then definition_scope sid
    else { scope_binding = sid; scope_role = Definition (site_of_name name) }
  in
  let inside_class (scope : scope_id) : context =
    { opaque with class_body = Some scope; self_type = Some (Instance_of scope) }
  in
  let field_owner (context : context) (receiver : G.expr) : field_owner option =
    let root, prefix = field_chain receiver in
    let of_self_type (self_type : self_type) : field_owner option =
      match self_type with
      | Instance_of cls -> Some (Instance_field (cls, prefix))
      | Instance_of_type _
      | Class_of _ ->
          None
      | Held_by (holder, path) -> Some (Object_field (holder, path @ prefix))
    in
    match (root.G.e, prefix) with
    | G.IdSpecial ((G.This | G.Self), _), _ ->
        Option.bind context.self_type of_self_type
    | G.N name, [ "prototype" ] when prototype_holds_methods lang ->
        Option.map
          (fun (cls : G.SId.t) -> Prototype_member (definition_scope cls))
          (binding_of_id_info (id_info_of_name name))
    | G.N name, _ ->
        Option.bind
          (binding_of_id_info (id_info_of_name name))
          (fun (sid : G.SId.t) ->
            match SId_tbl.find_opt receivers sid with
            | Some self_type -> of_self_type self_type
            | None -> Some (Object_field (sid, prefix)))
    | _ -> None
  in
  let store (owner : field_owner) (field : string) (stored : Func_info.t list) :
      unit =
    match owner with
    | Instance_field (cls, path) ->
        By_scope.add_at_path instance_fields cls (path @ [ field ]) stored
    | Object_field (holder, path) ->
        By_binding.add_at_path function_fields holder (path @ [ field ]) stored
    | Prototype_member cls -> By_scope.add_named members cls field stored
  in
  let self_type_of_owner (owner : field_owner) : self_type option =
    match owner with
    | Instance_field (cls, []) -> Some (Instance_of cls)
    | Instance_field (_, _ :: _) -> None
    | Prototype_member cls -> Some (Instance_of cls)
    | Object_field (holder, path) -> Some (Held_by (holder, path))
  in
  let deeper (owner : field_owner) (field : string) : field_owner option =
    match owner with
    | Instance_field (cls, path) -> Some (Instance_field (cls, path @ [ field ]))
    | Object_field (holder, path) -> Some (Object_field (holder, path @ [ field ]))
    | Prototype_member _ -> None
  in
  let inside_record (owner : field_owner) : context =
    {
      opaque with
      self_type = self_type_of_owner owner;
      enclosing_record = Some owner;
    }
  in
  let add_qualified (path : string list) (defined : Func_info.t list) : unit =
    Path_tbl.replace qualified_functions path
      (Option.value (Path_tbl.find_opt qualified_functions path) ~default:[]
      @ defined)
  in
  let store_defined (context : context) (receiver : G.expr) (field : string)
      (stored : Func_info.t list) : self_type option =
    match field_owner context receiver with
    | Some owner ->
        store owner field stored;
        self_type_of_owner owner
    | None ->
        Option.iter
          (fun (path : string list) -> add_qualified (path @ [ field ]) stored)
          (unbound_chain_path receiver);
        None
  in
  let hold_record (owner : field_owner) (fields : G.field list) : unit =
    fields
    |> List.iter (fun (G.F stmt) ->
           match stmt.G.s with
           | G.DefStmt ({ G.name = G.EN name; _ }, def) -> (
               let field = fst (last_ident_of_name name) in
               match def with
               | G.FuncDef fdef
               | G.VarDef { G.vinit = Some { G.e = G.Lambda fdef; _ }; _ }
               | G.FieldDefColon { G.vinit = Some { G.e = G.Lambda fdef; _ }; _ }
                 ->
                   store owner field (defined_by fdef)
               | G.VarDef { G.vinit = Some { G.e = G.N value; _ }; _ }
               | G.FieldDefColon { G.vinit = Some { G.e = G.N value; _ }; _ } ->
                   store owner field (held_by_name value)
               | _ -> ())
           | _ -> ())
  in
  let receiver_type_of (fdef : G.function_definition) : receiver_type =
    match Tok.unbracket fdef.G.fparams with
    | G.ParamReceiver { G.ptype = Some ty; _ } :: _ -> (
        match name_of_type ty with
        | Some name -> (
            match !((id_info_of_name name).G.id_resolved) with
            | Some (G.TypeName, sid) -> File_class sid
            | Some ((G.ImportedEntity _ | G.ImportedModule _ | G.GlobalName _), _)
            | None ->
                Other_type (ty, name)
            | Some
                ( ( G.Global | G.LocalVar | G.Parameter | G.EnclosedVar
                  | G.Macro | G.EnumConstant ),
                  _ ) ->
                No_receiver_type)
        | None -> No_receiver_type)
    | _ -> No_receiver_type
  in
  let define_function (context : context) (ent : G.entity)
      (fdef : G.function_definition) : self_type option =
    let defined = defined_by fdef in
    let by_definition_site () : self_type option =
      match context.class_body with
      | Some cls ->
          List.iter (add_member cls) defined;
          context.self_type
      | None -> (
          match ent.G.name with
          | G.EDynamic { G.e = G.DotAccess (receiver, _, G.FN name); _ } ->
              store_defined context receiver
                (fst (last_ident_of_name name))
                defined
          | G.EN name -> (
              (match context.namespace with
              | Some path ->
                  add_qualified (path @ [ fst (last_ident_of_name name) ]) defined
              | None -> ());
              match (name, binding_of_id_info (id_info_of_name name)) with
              | ( G.IdQualified
                    {
                      G.name_middle = Some (G.QDots (_ :: _ as qualifier));
                      name_top;
                      _;
                    },
                  None ) -> (
                  match List.rev qualifier with
                  | ((owner : G.ident), (_ : G.type_arguments option)) :: rest ->
                      let owner_name =
                        match rest with
                        | [] -> G.Id (owner, G.empty_id_info ())
                        | _ :: _ ->
                            G.IdQualified
                              {
                                G.name_last = (owner, None);
                                name_middle = Some (G.QDots (List.rev rest));
                                name_top;
                                name_info = G.empty_id_info ();
                              }
                      in
                      unbound_receivers :=
                        (Method_of, owner_name, defined) :: !unbound_receivers;
                      Some (Instance_of_type (G.TyN owner_name |> G.t))
                  | [] -> context.self_type)
              | _ -> context.self_type)
          | G.EDynamic _
          | G.EPattern _
          | G.OtherEntity _ ->
              context.self_type)
    in
    let add_extension (cls : G.SId.t) : unit =
      List.iter
        (fun (func : Func_info.t) ->
          match member_name func with
          | Some name -> By_binding.add_named extensions cls name [ func ]
          | None -> ())
        defined
    in
    let self_type =
      match (receiver_type_of fdef, config.Lang_config.receiver_parameter) with
      | _, Lang_config.Declares_method when Option.is_some context.class_body ->
          by_definition_site ()
      | File_class cls, Lang_config.Declares_method ->
          List.iter (add_member (definition_scope cls)) defined;
          Some (Instance_of (definition_scope cls))
      | File_class cls, Lang_config.Declares_extension ->
          add_extension cls;
          Option.iter
            (fun (body : scope_id) -> List.iter (add_member body) defined)
            context.class_body;
          Some (Instance_of (definition_scope cls))
      | Other_type (ty, name), Lang_config.Declares_method ->
          unbound_receivers := (Method_of, name, defined) :: !unbound_receivers;
          Some (Instance_of_type ty)
      | Other_type (ty, name), Lang_config.Declares_extension ->
          unbound_receivers :=
            (Extension_of, name, defined) :: !unbound_receivers;
          Option.iter
            (fun (body : scope_id) -> List.iter (add_member body) defined)
            context.class_body;
          Some (Instance_of_type ty)
      | No_receiver_type, _ -> by_definition_site ()
    in
    (match (self_type, Tok.unbracket fdef.G.fparams) with
    | Some self_type, first :: _
      when Receiver.implicit_param lang ~is_method:(Receiver.is_method fdef)
             ~is_static:(Receiver.is_static (Some ent)) ~is_first:true first
           || Lang_config.method_receiver_is_first_parameter lang
              && Receiver.is_method fdef -> (
        match first with
        | G.ParamReceiver { G.pinfo; _ }
        | G.Param { G.pinfo; _ } ->
            Option.iter
              (fun (sid : G.SId.t) -> SId_tbl.replace receivers sid self_type)
              (binding_of_id_info pinfo)
        | _ -> ())
    | _ -> ());
    self_type
  in
  let visitor =
    object (self)
      inherit [_] G.iter_no_id_info as super

      method! visit_definition (context : context) ((ent, def) as definition) =
        let named =
          match ent.G.name with
          | G.EN name ->
              Option.map
                (fun (sid : G.SId.t) -> (name, sid))
                (binding_of_id_info (id_info_of_name name))
          | _ -> None
        in
        let class_named =
          match ent.G.name with
          | G.EN name ->
              Option.map
                (fun (sid : G.SId.t) -> (name, sid))
                (Class_table.definition_binding name)
          | _ -> None
        in
        match (def, class_named) with
        | G.ClassDef cdef, Some (name, sid) ->
            let scope =
              {
                scope_binding = sid;
                scope_role =
                  (match fst cdef.G.ckind with
                  | G.Object when Lang_config.companion_object_has_own_name lang
                    ->
                      Singleton_object
                  | G.Object
                  | G.Class
                  | G.Struct
                  | G.Interface
                  | G.Trait ->
                      (definition_of ent name def sid).scope_role);
              }
            in
            declare_class context scope (Class_kind (fst cdef.G.ckind)) ent name def;
            super#visit_definition (inside_class scope) definition
        | G.ClassDef _, None -> super#visit_definition opaque definition
        | G.ModuleDef { G.mbody = G.ModuleStruct _ }, Some (name, sid) ->
            let scope = definition_of ent name def sid in
            declare_class context scope Module_kind ent name def;
            super#visit_definition
              {
                (inside_class scope) with
                namespace =
                  Option.map
                    (fun (path : string list) ->
                      path @ [ fst (last_ident_of_name name) ])
                    context.namespace;
              }
              definition
        | G.TypeDef { G.tbody = G.AliasType aliased }, Some (_, sid) ->
            SId_tbl.replace type_aliases sid aliased;
            super#visit_definition context definition
        | ( G.TypeDef
              {
                G.tbody =
                  G.NewType
                    { G.t = G.TyRecordAnon ((kind, _), (_, fields, _)); _ };
              },
            Some (name, sid) ) ->
            let scope = definition_of ent name def sid in
            declare_class context scope (Class_kind kind) ent name def;
            List.iter (self#visit_field (inside_class scope)) fields
        | G.OtherDef _, _ -> (
            match Visit_function_defs.class_scope_of_definition ent def with
            | Some
                (( { G.name = G.EN name; _ },
                   (G.ClassDef { G.cextends; _ } as impl_def) ) as reshaped)
              -> (
                let info = id_info_of_name name in
                match binding_of_id_info info with
                | Some sid ->
                    let own = definition_scope sid in
                    if not (Scope_tbl.mem kinds own) then
                      Scope_tbl.replace kinds own (Class_kind G.Class);
                    if defined_elsewhere info then
                      Scope_tbl.replace external_classes own ();
                    let body =
                      match cextends with
                      | [] ->
                          add_parents own impl_def;
                          own
                      | _ :: _ ->
                          let site = site_of_name name in
                          let impl =
                            { scope_binding = sid; scope_role = Trait_impl site }
                          in
                          Scope_tbl.replace kinds impl (Class_kind G.Class);
                          add_parents impl impl_def;
                          By_scope.add_to written_parents own
                            [ (Impl site, Linearisation.Appended) ];
                          impl
                    in
                    super#visit_definition
                      { (inside_class body) with self_type = Some (Instance_of own) }
                      reshaped
                | None -> super#visit_definition opaque definition)
            | Some _
            | None ->
                super#visit_definition context definition)
        | G.FuncDef fdef, _
        | G.VarDef { G.vinit = Some { G.e = G.Lambda fdef; _ }; _ }, _ ->
            let self_type =
              match
                ( define_function context ent fdef,
                  config.Lang_config.method_sets )
              with
              | Some (Instance_of id), Lang_config.Separate_for_class_and_instance
                when Receiver.is_static (Some ent) ->
                  Some (Class_of id)
              | self_type, _ -> self_type
            in
            super#visit_definition { context with self_type } definition
        | ( ( G.VarDef { G.vinit = Some { G.e = G.Record (_, fields, _); _ }; _ }
            | G.FieldDefColon
                { G.vinit = Some { G.e = G.Record (_, fields, _); _ }; _ } ),
            _ ) ->
            let owner =
              match (context.enclosing_record, ent.G.name, named) with
              | Some enclosing, G.EN name, _ ->
                  deeper enclosing (fst (last_ident_of_name name))
              | None, _, Some (_, sid) -> Some (Object_field (sid, []))
              | Some _, (G.EDynamic _ | G.EPattern _ | G.OtherEntity _), _
              | None, _, None ->
                  None
            in
            self#hold_fields owner fields
        | G.VarDef { G.vinit = Some value; _ }, _ -> (
            match ent.G.name with
            | G.EN name ->
                assign context name value;
                super#visit_definition context definition
            | G.EPattern (G.PatId (id, info)) ->
                assign context (G.Id (id, info)) value;
                super#visit_definition context definition
            | G.EDynamic _
            | G.EPattern _
            | G.OtherEntity _ ->
                super#visit_definition context definition)
        | _ -> super#visit_definition context definition

      method! visit_expr (context : context) (e : G.expr) =
        match e.G.e with
        | G.Assign ({ G.e = G.N name; _ }, _, { G.e = G.Record (_, fields, _); _ })
          ->
            self#hold_fields
              (Option.map
                 (fun (sid : G.SId.t) -> Object_field (sid, []))
                 (binding_of_id_info (id_info_of_name name)))
              fields
        | G.Assign
            ( { G.e = G.DotAccess (receiver, _, G.FN name); _ },
              _,
              { G.e = G.Record (_, fields, _); _ } ) ->
            self#visit_expr context receiver;
            self#hold_fields
              (Option.bind (field_owner context receiver)
                 (fun (owner : field_owner) ->
                   deeper owner (fst (last_ident_of_name name))))
              fields
        | G.Assign
            ( { G.e = G.DotAccess (receiver, _, G.FN name); _ },
              _,
              ({ G.e = G.Lambda fdef; _ } as value) ) ->
            let self_type =
              store_defined context receiver
                (fst (last_ident_of_name name))
                (defined_by fdef)
            in
            self#visit_expr context receiver;
            self#visit_expr { context with self_type } value
        | G.Assign
            ( { G.e = G.DotAccess (receiver, _, G.FN name); _ },
              _,
              { G.e = G.N value; _ } ) ->
            Option.iter
              (fun (owner : field_owner) ->
                store owner (fst (last_ident_of_name name)) (held_by_name value))
              (field_owner context receiver);
            super#visit_expr context e
        | G.Record (_, fields, _) -> List.iter (self#visit_field opaque) fields
        | G.Assign ({ G.e = G.N name; _ }, _, value) ->
            assign context name value;
            super#visit_expr context e
        | _ -> super#visit_expr context e

      method hold_fields (owner : field_owner option) (fields : G.field list) :
          unit =
        match owner with
        | Some owner ->
            hold_record owner fields;
            List.iter (self#visit_field (inside_record owner)) fields
        | None -> List.iter (self#visit_field opaque) fields

      method! visit_function_definition (context : context)
          (fdef : G.function_definition) =
        Option.iter (Fdef_tbl.replace selves fdef) context.self_type;
        super#visit_function_definition
          {
            context with
            class_body = None;
            namespace = None;
            enclosing_record = None;
            in_function = true;
          }
          fdef
    end
  in
  let (_ : string list) =
    List.fold_left
      (fun (namespace : string list) (stmt : G.stmt) ->
        match stmt.G.s with
        | G.DirectiveStmt { G.d = G.Package (_, idents); _ } ->
            List.map fst idents
        | G.DirectiveStmt { G.d = G.PackageEnd _; _ } -> []
        | _ ->
            visitor#visit_stmt { opaque with namespace = Some namespace } stmt;
            namespace)
      [] ast
  in
  let scopes = Scope_tbl.create 16 in
  let add_scope (id : scope_id) : unit =
    let held_by_binding (type held) (table : held SId_tbl.t) (default : held) :
        held =
      match id.scope_role with
      | Definition _
      | Singleton_object ->
          Option.value (SId_tbl.find_opt table id.scope_binding) ~default
      | Trait_impl _ -> default
    in
    if not (Scope_tbl.mem scopes id) then
      Scope_tbl.replace scopes id
        {
          binding = id.scope_binding;
          role = id.scope_role;
          members = Option.value (Scope_tbl.find_opt members id) ~default:SMap.empty;
          fields =
            Option.value
              (Scope_tbl.find_opt instance_fields id)
              ~default:Field_path_map.empty;
          parents =
            Option.value (Scope_tbl.find_opt written_parents id) ~default:[];
          class_side_parents =
            Option.value
              (Scope_tbl.find_opt written_class_side_parents id)
              ~default:[];
          kind =
            Option.value (Scope_tbl.find_opt kinds id)
              ~default:(Class_kind G.Class);
          singleton_exposure =
            Option.value
              (Scope_tbl.find_opt exposures id)
              ~default:Class_parents.No_singleton_exposure;
          bound_functions =
            (match id.scope_role with
            | Definition _ -> (
                Option.to_list (Scope_tbl.find_opt initialisers id)
                @
                let held = held_by_binding functions [] in
                match Scope_tbl.find_opt kinds id with
                | None -> held
                | Some _ when Naming_AST.constructor_named_after_class lang ->
                    let own =
                      List.concat_map snd
                        (SMap.bindings
                           (Option.value (Scope_tbl.find_opt members id)
                              ~default:SMap.empty))
                    in
                    List.filter
                      (fun (func : Func_info.t) ->
                        List.exists (same_definition func) own)
                      held
                | Some _ -> [])
            | Singleton_object
            | Trait_impl _ ->
                []);
          object_fields = held_by_binding function_fields Field_path_map.empty;
          extensions =
            (match id.scope_role with
            | Definition _ -> held_by_binding extensions SMap.empty
            | Singleton_object
            | Trait_impl _ ->
                SMap.empty);
          reopens = Scope_tbl.mem reopening id;
        }
  in
  Scope_tbl.iter (fun (id : scope_id) (_ : kind) -> add_scope id) kinds;
  Scope_tbl.iter
    (fun (id : scope_id) (_ : Func_info.t list SMap.t) -> add_scope id)
    members;
  SId_tbl.iter
    (fun (sid : G.SId.t) (_ : Func_info.t list SMap.t) ->
      add_scope (definition_scope sid))
    extensions;
  let defined_here = Scope_tbl.create (Scope_tbl.length scopes) in
  Scope_tbl.iter
    (fun (id : scope_id) (_ : class_scope) ->
      if
        (Scope_tbl.mem kinds id || SId_tbl.mem functions id.scope_binding)
        && not (Scope_tbl.mem external_classes id)
      then Scope_tbl.replace defined_here id ())
    scopes;
  let classes =
    Class_table.build ~lang
      ~compiled_together:(fun (_ : Func_info.t list) -> true)
      ~classes:
        (Scope_tbl.fold
           (fun (_ : scope_id) (scope : class_scope)
                (classes : class_scope list) ->
             scope :: classes)
           scopes []
        |> List.sort Class_table.compare_scope
        |> List.map (fun (scope : class_scope) -> [ scope ]))
      ~defined:(fun (scope : class_scope) ->
        Scope_tbl.mem defined_here (scope_id_of scope))
      ~link:(fun (scope : class_scope) (parent : parent) ->
        match parent with
        | Bound sid -> Some (definition_scope sid)
        | Impl site ->
            Some { scope_binding = scope.binding; scope_role = Trait_impl site }
        | Unbound _ -> None)
      ~outside:
        (fun
          (_ : Class_table.position)
          (_ : scope_id option)
          (_ : G.name * string list)
        -> None)
      ~may_implement:(fun ~interface:_ (_ : Class_table.cls) -> true)
  in
  {
    lang;
    functions;
    scopes;
    classes;
    defined_here;
    owners;
    function_fields;
    unbound_receivers = List.rev !unbound_receivers;
    selves;
    receivers;
    qualified_functions;
    qualified_classes;
    type_aliases;
    class_definitions = List.rev !class_definitions;
    class_sites;
    values;
    by_node;
    extension_visible = (fun (_ : string) (_ : Func_info.t) -> true);
    compiled_with_file = (fun (_ : Func_info.t) -> true);
    outside = (fun (_ : t) ~caller:_ (_ : G.expr) -> External);
    types = Type_state.empty;
    declared_types = Hashtbl.create 64;
  }

let functions_of_binding (t : t) (sid : G.SId.t) : Func_info.t list =
  definitions_seen ~lang:t.lang t.functions sid

let class_of_binding (t : t) (sid : G.SId.t) : class_scope option =
  Option.bind (Class_table.class_of_binding t.classes sid)
    (fun (cls : Class_table.cls) ->
      List.find_opt
        (fun (scope : class_scope) -> Scope_tbl.mem t.scopes (scope_id_of scope))
        (Class_table.scopes cls))

let classes (t : t) : class_scope list =
  Scope_tbl.fold
    (fun (_ : scope_id) (scope : class_scope) (classes : class_scope list) ->
      scope :: classes)
    t.scopes []
  |> List.sort Class_table.compare_scope

let defined_here (t : t) (cls : class_scope) : bool =
  Scope_tbl.mem t.defined_here (scope_id_of cls)

let owner (t : t) (cls : class_scope) : class_scope option =
  Option.bind
    (Scope_tbl.find_opt t.owners (scope_id_of cls))
    (Scope_tbl.find_opt t.scopes)

let unbound_receivers (t : t) : (receiver_role * G.name * Func_info.t list) list
    =
  t.unbound_receivers

let class_table (t : t) : Class_table.t = t.classes

let is_class_binding (t : t) (sid : G.SId.t) : bool =
  Option.is_some (Class_table.class_of_binding t.classes sid)

let with_project (t : t) (classes : Class_table.t)
    ~(extension_visible : string -> Func_info.t -> bool)
    ~(compiled_with_file : Func_info.t -> bool)
    ~(outside : t -> caller:Function_id.t option -> G.expr -> resolution) : t =
  {
    t with
    classes;
    extension_visible;
    compiled_with_file;
    outside;
    declared_types = Hashtbl.create 64;
  }

let order (t : t) (cls : Class_table.cls) :
    Class_table.cls Linearisation.linearisation =
  Class_table.order t.classes cls

let own_members (t : t) (cls : Class_table.cls) (name : string) :
    Func_info.t list =
  List.filter t.compiled_with_file (Class_table.own_members cls name)

let first_defining (t : t) (classes : Class_table.cls list) (name : string) :
    (Class_table.cls * Func_info.t list) option =
  List.find_map
    (fun (cls : Class_table.cls) ->
      match own_members t cls name with
      | [] -> None
      | defined -> Some (cls, defined))
    classes

let on_side (t : t) (side : Class_parents.side) (cls : Class_table.cls)
    (name : string) (func : Func_info.t) : bool =
  match (Lang_config.get t.lang).Lang_config.method_sets with
  | Lang_config.Shared_by_class_and_instance -> true
  | Lang_config.Separate_for_class_and_instance -> (
      let static = Receiver.is_static func.Func_info.entity in
      match side with
      | Class_parents.Instance_side -> not static
      | Class_parents.Class_side -> static || Class_table.exposes cls name)

let own_members_on (t : t) (side : Class_parents.side) (cls : Class_table.cls)
    (name : string) : Func_info.t list =
  List.filter (on_side t side cls name) (own_members t cls name)

let same_signature (t : t) (name : string) ~(nearer : Func_info.t)
    ~(farther : Func_info.t) : bool =
  let method_of (func : Func_info.t) : Structural_typing.method_ =
    {
      Structural_typing.name;
      entity = func.Func_info.entity;
      fdef = func.Func_info.fdef;
    }
  in
  Structural_typing.method_satisfies ~lang:t.lang
    ~equal_type:(Class_table.equal_type t.classes)
    ~required:(method_of farther) (method_of nearer)

let visible_overloads (t : t) (side : Class_parents.side)
    ~(nearer : Func_info.t list) (farther : Class_table.cls list)
    (name : string) : Func_info.t list =
  List.fold_left
    (fun (visible : Func_info.t list) (cls : Class_table.cls) ->
      visible
      @ List.filter
          (fun (inherited : Func_info.t) ->
            not
              (List.exists
                 (fun (seen : Func_info.t) ->
                   same_signature t name ~nearer:seen ~farther:inherited)
                 visible))
          (own_members_on t side cls name))
    nearer farther

let rec first_defining_on (t : t) (side : Class_parents.side)
    (classes : Class_table.cls list) (name : string) :
    (Class_table.cls * Func_info.t list) option =
  match classes with
  | [] -> None
  | cls :: farther -> (
      match own_members_on t side cls name with
      | [] -> first_defining_on t side farther name
      | defined when Lang_config.overloads_by_type t.lang ->
          Some (cls, visible_overloads t side ~nearer:defined farther name)
      | defined -> Some (cls, defined))

let after (cls : Class_table.cls) (classes : Class_table.cls list) :
    Class_table.cls list =
  let rec drop (remaining : Class_table.cls list) : Class_table.cls list =
    match remaining with
    | [] -> []
    | current :: rest ->
        if Class_table.same current cls then rest else drop rest
  in
  drop classes

let descendants (t : t) (cls : Class_table.cls) : Class_table.cls list =
  Class_table.descendants t.classes cls

let rec overridable (t : t) ~(visited : Class_table.cls list)
    (cls : Class_table.cls) (defined : Func_info.t list) (name : string) : bool
    =
  Class_table.is_abstraction cls
  || List.exists
       (fun (func : Func_info.t) ->
         let attrs =
           match func.Func_info.entity with
           | Some ent -> ent.G.attrs
           | None -> []
         in
         match Lang_config.method_overridable t.lang attrs with
         | Some answer -> answer
         | None -> (
             match
               first_defining t
                 (after cls (order t cls).Linearisation.order
                 |> List.filter (fun (ancestor : Class_table.cls) ->
                        not (List.exists (Class_table.same ancestor) visited)))
                 name
             with
             | Some (ancestor, inherited) ->
                 overridable t ~visited:(cls :: visited) ancestor inherited name
             | None -> false))
       defined

let dispatches (t : t) (cls : Class_table.cls) (defined : Func_info.t list)
    (name : string) : bool =
  match (Lang_config.get t.lang).Lang_config.method_dispatch with
  | Lang_config.Dynamic -> true
  | Lang_config.Static -> Class_table.is_abstraction cls
  | Lang_config.Dynamic_when_overridable ->
      overridable t ~visited:[] cls defined name

let root_members (t : t) (name : string) : Func_info.t list =
  if top_level_defs_are_methods_of_object t.lang then
    Option.value (Path_tbl.find_opt t.qualified_functions [ name ]) ~default:[]
  else []

let root_resolution (t : t) (name : string) : resolution =
  match root_members t name with
  | _ :: _ as found -> Defined found
  | [] when top_level_defs_are_methods_of_object t.lang -> External
  | [] -> Defined []

let extension_along (t : t) (classes : Class_table.cls list) (name : string) :
    resolution =
  match
    List.find_map
      (fun (cls : Class_table.cls) ->
        match
          List.filter (t.extension_visible name)
            (Class_table.extensions cls name)
        with
        | [] -> None
        | found -> Some found)
      classes
  with
  | Some found -> Defined found
  | None -> (
      match (Lang_config.get t.lang).Lang_config.receiver_parameter with
      | Lang_config.Declares_extension -> External
      | Lang_config.Declares_method -> Defined [])

let instance_fields_along (classes : Class_table.cls list) (path : string list)
    : Func_info.t list =
  List.concat_map
    (fun (ancestor : Class_table.cls) -> Class_table.instance_fields ancestor path)
    classes
  |> List_.uniq_by same_definition

let trait_impl_members (t : t) (cls : Class_table.cls) (name : string) :
    Func_info.t list =
  (order t cls).Linearisation.order
  |> List.filter Class_table.is_trait_impl
  |> List.concat_map (fun (impl : Class_table.cls) ->
         match
           first_defining_on t Class_parents.Instance_side
             (order t impl).Linearisation.order name
         with
         | Some (_, defined) -> defined
         | None -> [])
  |> List_.uniq_by same_definition

let selected_on_instance (t : t) (cls : Class_table.cls)
    (definer : Class_table.cls) (defined : Func_info.t list) (name : string) :
    Func_info.t list =
  if Class_table.same definer cls then defined
  else
    match trait_impl_members t cls name with
    | [] -> defined
    | from_impls -> from_impls

let overrides (t : t) (cls : Class_table.cls) (name : string) :
    Func_info.t list =
  List.concat_map
    (fun (sub : Class_table.cls) ->
      match
        first_defining_on t Class_parents.Instance_side
          (order t sub).Linearisation.order name
      with
      | Some (definer, defined) -> selected_on_instance t sub definer defined name
      | None -> [])
    (descendants t cls)
  |> List_.uniq_by same_definition

let found_in_descendants (t : t) (cls : Class_table.cls) : bool =
  Class_table.is_abstraction cls
  ||
  match (Lang_config.get t.lang).Lang_config.method_dispatch with
  | Lang_config.Dynamic -> true
  | Lang_config.Static
  | Lang_config.Dynamic_when_overridable ->
      false

(* The fields and root methods found are definitions of this file that
   shadow whatever an ancestor the file does not hold defines. *)
let select_on_instance (t : t) (cls : Class_table.cls) ~(dispatch : bool)
    (name : string) : resolution =
  let linearisation = order t cls in
  let classes = linearisation.Linearisation.order in
  let fields = instance_fields_along classes [ name ] in
  match first_defining_on t Class_parents.Instance_side classes name with
  | Some (definer, defined) ->
      let defined = selected_on_instance t cls definer defined name in
      let overriding =
        if dispatch && dispatches t definer defined name then
          overrides t cls name
        else []
      in
      Defined (List_.uniq_by same_definition (defined @ overriding @ fields))
  | None when not linearisation.Linearisation.complete -> (
      match fields with
      | [] -> External
      | _ :: _ -> Defined (List_.uniq_by same_definition fields))
  | None -> (
      match
        if dispatch && found_in_descendants t cls then overrides t cls name
        else []
      with
      | _ :: _ as inherited ->
          Defined (List_.uniq_by same_definition (fields @ inherited))
      | [] -> (
          match fields @ root_members t name with
          | _ :: _ as found -> Defined (List_.uniq_by same_definition found)
          | [] -> (
              match root_resolution t name with
              | Defined _ -> extension_along t classes name
              | External -> External)))

(* Along the order, each class's own class-side members, then the instance
   members of the modules that class extends, the last extended first. *)
let select_on_class_side (t : t) (cls : Class_table.cls) (name : string) :
    resolution =
  let linearisation = order t cls in
  let rec along (classes : Class_table.cls list) : resolution =
    match classes with
    | [] ->
        if linearisation.Linearisation.complete then root_resolution t name
        else External
    | current :: rest -> (
        match own_members_on t Class_parents.Class_side current name with
        | _ :: _ as defined -> Defined defined
        | [] ->
            extended (Class_table.class_side_parents t.classes current) rest)
  and extended (parents : Class_table.cls option list)
      (rest : Class_table.cls list) : resolution =
    match parents with
    | [] -> along rest
    | None :: _ -> External
    | Some mixin :: others -> (
        let mixin_order = order t mixin in
        match
          first_defining_on t Class_parents.Instance_side
            mixin_order.Linearisation.order name
        with
        | Some (_, defined) -> Defined defined
        | None when mixin_order.Linearisation.complete -> extended others rest
        | None -> External)
  in
  let object_fields = Class_table.object_fields cls [ name ] in
  match (along linearisation.Linearisation.order, object_fields) with
  | Defined defined, _ ->
      Defined (List_.uniq_by same_definition (defined @ object_fields))
  | External, [] -> External
  | External, (_ :: _ as found) -> Defined found

let fields_else_external (fields : Func_info.t list) : resolution =
  match fields with
  | [] -> External
  | _ :: _ -> Defined fields

let node_of_function (func : Func_info.t) : Function_id.t option =
  Option.map Function_id.of_il_name (Func_info.bare_name func.Func_info.fn_id)

let member_type (t : t) (receiver : receiver_class)
    (lookup : Class_table.cls -> Class_table.cls option) :
    Class_table.cls option =
  match receiver with
  | Class cls
  | Exact cls
  | Class_object cls
  | Object_of { path = []; held_class = Of_class cls; _ } ->
      List.find_map lookup (order t cls).Linearisation.order
  | Ancestors_of cls ->
      List.find_map lookup (after cls (order t cls).Linearisation.order)
  | Object_of _
  | External_class
  | Root
  | Unknown ->
      None

let field_type (t : t) (receiver : receiver_class) (field : string) :
    Class_table.cls option =
  member_type t receiver (fun (owner : Class_table.cls) ->
      Type_state.field t.types owner field)

(* The functions a field path read from a receiver holds: a member for a
   single field, else the functions stored at that path of the object. *)
let rec resolve_path (t : t) (receiver : receiver_class) (path : string list) :
    resolution =
  let through_field (stored : Func_info.t list) : resolution =
    match (stored, path) with
    | [], field :: (_ :: _ as rest) -> (
        match field_type t receiver field with
        | Some cls -> resolve_path t (Class cls) rest
        | None -> Defined [])
    | _ -> Defined stored
  in
  match (receiver, path) with
  | Class cls, [ name ] -> select_on_instance t cls ~dispatch:true name
  | Exact cls, [ name ] -> select_on_instance t cls ~dispatch:false name
  | (Class cls | Exact cls), _ ->
      through_field (instance_fields_along (order t cls).Linearisation.order path)
  | Class_object cls, [ name ] -> select_on_class_side t cls name
  | Class_object cls, _ -> Defined (Class_table.object_fields cls path)
  | Ancestors_of cls, [ name ] -> (
      let linearisation = order t cls in
      match
        first_defining_on t Class_parents.Instance_side
          (after cls linearisation.Linearisation.order)
          name
      with
      | Some (_, defined) -> Defined defined
      | None when linearisation.Linearisation.complete ->
          root_resolution t name
      | None -> External)
  | Ancestors_of _, _ -> Defined []
  | Object_of held, _ -> (
      let fields =
        By_binding.at_path t.function_fields held.holder (held.path @ path)
      in
      match (held.path, path, held.held_class) with
      | [], [ name ], Of_class cls -> (
          match select_on_instance t cls ~dispatch:true name with
          | Defined defined ->
              Defined (List_.uniq_by same_definition (fields @ defined))
          | External -> fields_else_external fields)
      | [], [ _ ], (Of_external_class | Of_unknown_class) ->
          fields_else_external fields
      | [], _, Of_class cls ->
          through_field
            (List_.uniq_by same_definition
               (fields
               @ instance_fields_along (order t cls).Linearisation.order path))
      | _, _, (Of_class _ | Of_external_class | Of_unknown_class) ->
          Defined fields)
  | External_class, _ -> External
  | Root, [ name ] -> root_resolution t name
  | Root, _
  | Unknown, _ ->
      Defined []

let resolve_member (t : t) (receiver : receiver_class) (name : string) :
    resolution =
  resolve_path t receiver [ name ]

let function_of_node (t : t) (node : Function_id.t) : Func_info.t option =
  Node_tbl.find_opt t.by_node node

let self_type_of_caller (t : t) (caller : Function_id.t option) :
    self_type option =
  Option.bind caller (fun (node : Function_id.t) ->
      Option.bind (function_of_node t node) (fun (func : Func_info.t) ->
          Fdef_tbl.find_opt t.selves func.Func_info.fdef))

let self_scope (t : t) ~(caller : Function_id.t option) : scope_id option =
  match self_type_of_caller t caller with
  | Some (Instance_of id) -> Some id
  | Some (Instance_of_type _ | Class_of _ | Held_by _)
  | None ->
      None

let rec class_of_aliased_type (t : t) ~(context : scope_id option)
    ~(visited : G.SId.t list) (ty : G.type_) : receiver_class =
  match name_of_type ty with
  | Some name -> (
      let aliased =
        Option.bind (binding_of_id_info (id_info_of_name name))
          (fun (sid : G.SId.t) ->
            if List.exists (G.SId.equal sid) visited then None
            else
              Option.map
                (fun (target : G.type_) -> (sid, target))
                (SId_tbl.find_opt t.type_aliases sid))
      in
      match aliased with
      | Some (sid, target) ->
          class_of_aliased_type t ~context ~visited:(sid :: visited) target
      | None -> (
          match
            Class_table.class_of_name t.classes
              ~position:Class_table.Type_position ~context name
          with
          | Some cls -> Class cls
          | None ->
              if defined_elsewhere (id_info_of_name name) then External_class
              else Unknown))
  | None -> (
      match Class_table.path_of_type ty with
      | Some ((_, _ :: _) as path) -> (
          match
            Class_table.class_of_path t.classes
              ~position:Class_table.Type_position ~context path
          with
          | Some cls -> Class cls
          | None -> Unknown)
      | Some (_, [])
      | None ->
          Unknown)

let class_of_type (t : t) ~(context : scope_id option) (ty : G.type_) :
    receiver_class =
  let site =
    match ty.G.t with
    | G.TyExpr { G.e = G.DotAccess (_, _, G.FN (G.Id _ as last)); _ } ->
        Class_table.site_of_name last
    | _ -> Option.bind (name_of_type ty) Class_table.site_of_name
  in
  match site with
  | Some (site : G.SId.t) -> (
      let key =
        (site, Option.map (fun (id : scope_id) -> id.scope_binding) context)
      in
      match Hashtbl.find_opt t.declared_types key with
      | Some found -> found
      | None ->
          let found = class_of_aliased_type t ~context ~visited:[] ty in
          Hashtbl.replace t.declared_types key found;
          found)
  | None -> class_of_aliased_type t ~context ~visited:[] ty

let of_self_type (t : t) (self_type : self_type) : receiver_class =
  match self_type with
  | Instance_of id -> (
      match Class_table.class_of_scope t.classes id with
      | Some cls -> Class cls
      | None -> Unknown)
  | Instance_of_type ty -> class_of_type t ~context:None ty
  | Class_of id -> (
      match Class_table.class_of_scope t.classes id with
      | Some cls -> Class_object cls
      | None -> Unknown)
  | Held_by (holder, path) ->
      Object_of { holder; path; held_class = Of_unknown_class }

let self_receiver (t : t) ~(caller : Function_id.t option) : receiver_class =
  match self_type_of_caller t caller with
  | Some self_type -> of_self_type t self_type
  | None when top_level_defs_are_methods_of_object t.lang -> Root
  | None -> Unknown

let ancestors_of_self (t : t) ~(caller : Function_id.t option) :
    receiver_class =
  match self_receiver t ~caller with
  | Class cls -> Ancestors_of cls
  | External_class -> External_class
  | Exact _
  | Class_object _
  | Ancestors_of _
  | Object_of _
  | Root
  | Unknown ->
      Unknown

let method_class (t : t) (func : Func_info.t) : Class_table.cls option =
  match
    Option.map (of_self_type t) (Fdef_tbl.find_opt t.selves func.Func_info.fdef)
  with
  | Some (Class cls) -> Some cls
  | Some
      ( Exact _ | Class_object _ | Ancestors_of _ | Object_of _ | External_class
      | Root | Unknown )
  | None ->
      None

let with_overrides (t : t) (defined : Func_info.t list) : Func_info.t list =
  List_.uniq_by same_definition
    (defined
    @ List.concat_map
        (fun (func : Func_info.t) ->
          match (method_class t func, member_name func) with
          | Some cls, Some name
            when List.exists (same_definition func)
                   (Class_table.own_members cls name)
                 && dispatches t cls [ func ] name ->
              overrides t cls name
          | _ -> [])
        defined)

let classes_at (t : t) (path : string list) : Class_table.cls list =
  Path_tbl.find_all t.qualified_classes path
  |> List.filter_map (Class_table.class_of_scope t.classes)
  |> List_.uniq_by Class_table.same

let external_or_outside (t : t) ~(context : scope_id option) (name : G.name) :
    receiver_class =
  match
    Class_table.class_of_name t.classes ~position:Class_table.Term_position
      ~context name
  with
  | Some cls -> Class_object cls
  | None -> External_class

(* Module level assignments run in order before any function of the module
   runs (JavaScript, Python and Ruby modules, C and C++ static
   initialisers), so a use inside a function sees the last of them unless a
   function assigns the variable too. *)
let values_in_force (t : t) ~(caller : Function_id.t option) (sid : G.SId.t) :
    G.expr list =
  let assigned = Option.value (SId_tbl.find_opt t.values sid) ~default:[] in
  let in_functions, at_module_level =
    List.partition (fun (assigned : assigned_value) -> assigned.in_function)
      assigned
  in
  match (caller, in_functions, List.rev at_module_level) with
  | _, _, [] -> []
  | Some _, [], last :: _ -> [ last.value ]
  | _ -> List.map (fun (assigned : assigned_value) -> assigned.value) assigned

let rec receiver_of_name_from (t : t) ~(caller : Function_id.t option)
    ~(visited : G.SId.t list) (name : G.name) : receiver_class =
  let context = self_scope t ~caller in
  let info = id_info_of_name name in
  match binding_of_id_info info with
  | Some sid -> (
      match Class_table.object_of_binding t.classes sid with
      | Some cls -> (
          match !(info.G.id_resolved) with
          | Some (G.TypeName, _) when Lang_config.type_name_value_is_instance t.lang
            ->
              Exact cls
          | Some _
          | None ->
              Class_object cls)
      | None -> (
          match SId_tbl.find_opt t.receivers sid with
          | Some self_type -> of_self_type t self_type
          | None when defined_elsewhere info ->
              external_or_outside t ~context name
          | None -> (
              match
                Option.map (class_of_type t ~context)
                  (Ty_bare_name.instance_or_declared_type info)
              with
              | Some (Class cls) ->
                  Object_of { holder = sid; path = []; held_class = Of_class cls }
              | Some External_class ->
                  Object_of
                    { holder = sid; path = []; held_class = Of_external_class }
              | Some
                  ( Unknown | Exact _ | Class_object _ | Ancestors_of _
                  | Object_of _ | Root ) ->
                  Object_of
                    { holder = sid; path = []; held_class = Of_unknown_class }
              | None ->
                  Object_of
                    {
                      holder = sid;
                      path = [];
                      held_class =
                        held_by_values t ~caller ~visited:(sid :: visited) sid
                          info;
                    })))
  | None -> (
      match name with
      | G.IdQualified { G.name_top = Some _; _ } -> (
          match classes_at t (qualified_path name) with
          | [ cls ] -> Class_object cls
          | [] | _ :: _ :: _ -> external_or_outside t ~context name)
      | G.Id _
      | G.IdQualified _ ->
          if
            top_level_defs_are_methods_of_object t.lang
            && not (String_.is_capitalized (fst (last_ident_of_name name)))
          then Unknown
          else external_or_outside t ~context name)

and held_by_values (t : t) ~(caller : Function_id.t option)
    ~(visited : G.SId.t list) (sid : G.SId.t) (info : G.id_info) : held_class =
  let values =
    match (values_in_force t ~caller sid, !(info.G.id_svalue)) with
    | (_ :: _ as assigned), _ -> assigned
    | [], Some (G.Sym value) -> [ value ]
    | [], (Some _ | None) -> []
  in
  let class_of_value (value : G.expr) : Class_table.cls option =
    let repeated (value_name : G.name) : bool =
      match binding_of_id_info (id_info_of_name value_name) with
      | Some bound -> List.exists (G.SId.equal bound) visited
      | None -> false
    in
    match value.G.e with
    | G.N value_name when not (repeated value_name) -> (
        match receiver_of_name_from t ~caller ~visited value_name with
        | Exact cls
        | Class cls
        | Object_of { path = []; held_class = Of_class cls; _ } ->
            Some cls
        | Class_object _
        | Ancestors_of _
        | Object_of _
        | External_class
        | Root
        | Unknown ->
            None)
    | _ -> None
  in
  match List.map class_of_value values with
  | Some cls :: others
    when List.for_all
           (Option.fold ~none:false ~some:(Class_table.same cls))
           others ->
      Of_class cls
  | _ -> Of_unknown_class

let receiver_of_name (t : t) ~(caller : Function_id.t option) (name : G.name) :
    receiver_class =
  receiver_of_name_from t ~caller ~visited:[] name

let exact (receiver : receiver_class) : receiver_class =
  match receiver with
  | Class cls -> Exact cls
  | Exact _
  | Class_object _
  | Ancestors_of _
  | Object_of _
  | External_class
  | Root
  | Unknown ->
      receiver

let constructs_by_method (t : t) (name : string) : bool =
  Option.equal String.equal (Lang_config.construction_method t.lang) (Some name)

let rec reads_own_class (t : t) (e : G.expr) : bool =
  match e.G.e with
  | G.Call (callee, (_, [], _)) -> reads_own_class t callee
  | G.DotAccess ({ G.e = G.IdSpecial ((G.This | G.Self), _); _ }, _, G.FN name)
    ->
      List.exists
        (String.equal (fst (last_ident_of_name name)))
        (Lang_config.get t.lang).Lang_config.class_accessor_methods
  | _ -> false

let calls_builtin_super (t : t) (callee : G.expr) : bool =
  match callee.G.e with
  | G.N (G.Id (("super", _), info)) ->
      Lang_config.super_is_builtin_call t.lang
      && Option.is_none (binding_of_id_info info)
  | _ -> false

let constructs_by_member (t : t) (e : G.expr) : bool =
  match e.G.e with
  | G.DotAccess (_, _, G.FN name) ->
      constructs_by_method t (fst (last_ident_of_name name))
  | _ -> false

let member_access (t : t) (e : G.expr) : (G.expr * string) option =
  match e.G.e with
  | G.DotAccess (receiver, _, G.FN name) ->
      Some (receiver, fst (last_ident_of_name name))
  | G.ArrayAccess
      (receiver, (_, { G.e = G.L (G.String (_, (member, _), _)); _ }, _))
    when Lang_config.bracket_member_access t.lang ->
      Some (receiver, member)
  | _ -> None

let rec receiver_chain (t : t) (e : G.expr) : G.expr * string list =
  match (e.G.e, member_access t e) with
  | _, Some (inner, member)
    when not (reads_own_class t e || constructs_by_member t e) ->
      let root, path = receiver_chain t inner in
      (root, path @ [ member ])
  | G.DeRef (_, inner), _ -> receiver_chain t inner
  | _ -> (e, [])

(* The constructors of the first class in the resolution order that has one:
   the functions its binding holds (a constructor written with the class's
   name, a JavaScript function called with [new]) and its members with the
   language's constructor names. *)
let constructors_along (t : t) (classes : Class_table.cls list)
    ~(complete : bool) : resolution =
  let constructor_names = (Lang_config.get t.lang).Lang_config.constructor_names in
  match
    List.find_map
      (fun (ancestor : Class_table.cls) ->
        match
          Class_table.bound_functions ancestor
          @ List.concat_map (Class_table.own_members ancestor) constructor_names
        with
        | [] -> None
        | found -> Some found)
      classes
  with
  | Some found -> Defined found
  | None when complete -> Defined []
  | None -> External

let constructors_of_class (t : t) (cls : Class_table.cls) : resolution =
  let linearisation = order t cls in
  constructors_along t linearisation.Linearisation.order
    ~complete:linearisation.Linearisation.complete

let constructors (t : t) (scope : class_scope) : resolution =
  match Class_table.class_of_scope t.classes (scope_id_of scope) with
  | Some cls -> constructors_of_class t cls
  | None -> Defined []

let is_constructor_reference (t : t) (name : string) : bool =
  List.exists (String.equal name)
    (Lang_config.get t.lang).Lang_config.constructor_reference_names
  || constructs_by_method t name

let constructs (t : t) (use : use) : bool =
  match use with
  | Called -> Lang_config.constructs_by_bare_call t.lang
  | Referenced -> (Lang_config.get t.lang).Lang_config.class_is_callable_value

let joined_resolutions (resolutions : resolution list) : resolution =
  List.fold_left
    (fun (joined : resolution) (resolution : resolution) ->
      match (joined, resolution) with
      | External, _
      | _, External ->
          External
      | Defined left, Defined right ->
          Defined (List_.uniq_by same_definition (left @ right)))
    (Defined []) resolutions

let rec resolve_name (t : t) ~(caller : Function_id.t option) ~(use : use)
    ~(visited : G.SId.t list) (name : G.name) : resolution =
  let info = id_info_of_name name in
  match !(info.G.id_resolved) with
  | Some (G.TypeName, sid) -> (
      match Class_table.class_of_binding t.classes sid with
      | Some cls
        when constructs t use || Lang_config.is_callable_reference t.lang name
        ->
          constructors_of_class t cls
      | Some _
      | None ->
          Defined [])
  | Some ((G.Global | G.LocalVar | G.Parameter | G.EnclosedVar | G.Macro), sid)
    when names_class t.class_sites sid -> (
      match Class_table.class_of_binding t.classes sid with
      | Some cls when constructs t use -> constructors_of_class t cls
      | Some _
      | None ->
          Defined [])
  | Some ((G.Global | G.LocalVar | G.Parameter | G.EnclosedVar | G.Macro), sid)
    -> (
      match functions_of_binding t sid with
      | _ :: _ as defined -> Defined (with_overrides t defined)
      | [] when List.exists (G.SId.equal sid) visited -> Defined []
      | [] -> (
          match (values_in_force t ~caller sid, !(info.G.id_svalue)) with
          | (_ :: _ as assigned), _ ->
              joined_resolutions
                (List.map
                   (resolve_expr t ~caller ~use ~visited:(sid :: visited))
                   assigned)
          | [], Some (G.Sym value) ->
              resolve_expr t ~caller ~use ~visited:(sid :: visited) value
          | [], (Some _ | None) ->
              Defined []))
  | Some (G.EnumConstant, _) -> Defined []
  | Some ((G.ImportedEntity _ | G.ImportedModule _ | G.GlobalName _), _) ->
      External
  | None when top_level_defs_are_methods_of_object t.lang ->
      resolve_member t (self_receiver t ~caller)
        (fst (last_ident_of_name name))
  | None -> External

and resolve_expr (t : t) ~(caller : Function_id.t option) ~(use : use)
    ~(visited : G.SId.t list) (e : G.expr) : resolution =
  match (e.G.e, member_access t e) with
  | (G.N name | G.Ref (_, { G.e = G.N name; _ })), _ ->
      resolve_name t ~caller ~use ~visited name
  | _, Some (receiver, member) ->
      let root, prefix = receiver_chain t receiver in
      resolve_member_access t ~receiver ~member ~prefix
        ~root_class:(lazy (receiver_class t ~caller root))
  | G.ArrayAccess (indexed, _), None ->
      resolve_expr t ~caller ~use ~visited indexed
  | G.IdSpecial (G.Super, _), None -> (
      match self_receiver t ~caller with
      | Class cls ->
          let linearisation = order t cls in
          constructors_along t
            (after cls linearisation.Linearisation.order)
            ~complete:linearisation.Linearisation.complete
      | External_class -> External
      | Exact _
      | Class_object _
      | Ancestors_of _
      | Object_of _
      | Root
      | Unknown ->
          Defined [])
  | _ -> Defined []

and resolve_member_access (t : t) ~(receiver : G.expr) ~(member : string)
    ~(prefix : string list) ~(root_class : receiver_class Lazy.t) : resolution
    =
  match
    Option.bind (unbound_chain_path receiver) (fun (path : string list) ->
        Path_tbl.find_opt t.qualified_functions (path @ [ member ]))
  with
  | Some (_ :: _ as defined) -> Defined defined
  | Some []
  | None -> (
      match (Lazy.force root_class, prefix) with
      | Class_object cls, []
        when is_constructor_reference t member
             && Option.is_none
                  (first_defining_on t Class_parents.Class_side
                     (order t cls).Linearisation.order member) ->
          constructors_of_class t cls
      | root_receiver, _ -> resolve_path t root_receiver (prefix @ [ member ]))

(* The class an expression denotes as a receiver, when this file knows it. *)
and receiver_class (t : t) ~(caller : Function_id.t option) (e : G.expr) :
    receiver_class =
  match e.G.e with
  | G.DeRef (_, inner) -> receiver_class t ~caller inner
  | G.IdSpecial (G.Self, _) when Lang_config.self_is_defining_class t.lang ->
      exact (self_receiver t ~caller)
  | G.IdSpecial ((G.This | G.Self | G.LateStatic), _) -> self_receiver t ~caller
  | G.IdSpecial ((G.Super | G.Parent), _)
  | G.Call ({ G.e = G.IdSpecial ((G.Super | G.Parent), _); _ }, _) ->
      ancestors_of_self t ~caller
  | G.Call (callee, _) when calls_builtin_super t callee ->
      ancestors_of_self t ~caller
  | (G.DotAccess _ | G.Call _) when reads_own_class t e -> (
      match self_receiver t ~caller with
      | Class cls -> Class_object cls
      | External_class -> External_class
      | Exact _
      | Class_object _
      | Ancestors_of _
      | Object_of _
      | Root
      | Unknown ->
          Unknown)
  | G.N name -> receiver_of_name t ~caller name
  | G.Await (_, inner) -> receiver_class t ~caller inner
  | G.Cast (ty, _, _) -> class_of_type t ~context:(self_scope t ~caller) ty
  | G.New (_, ty, _, _) ->
      exact (class_of_type t ~context:(self_scope t ~caller) ty)
  | G.Call (({ G.e = G.N name; _ } as callee), _)
    when Lang_config.constructs_by_bare_call t.lang -> (
      match
        Class_table.class_of_name t.classes ~position:Class_table.Term_position
          ~context:(self_scope t ~caller) name
      with
      | Some cls -> Exact cls
      | None -> returned_by t ~caller callee)
  | G.Call ({ G.e = G.DotAccess (receiver, _, G.FN name); _ }, _)
  | G.DotAccess (receiver, _, G.FN name)
    when constructs_by_method t (fst (last_ident_of_name name)) -> (
      match receiver_class t ~caller receiver with
      | Class_object cls -> Exact cls
      | External_class -> External_class
      | Class _
      | Exact _
      | Ancestors_of _
      | Object_of _
      | Root
      | Unknown ->
          Unknown)
  | G.Call (callee, _) -> returned_by t ~caller callee
  | G.DotAccess _
  | G.ArrayAccess _ -> (
      match member_access t e with
      | Some (inner, field) ->
          member_receiver t (receiver_class t ~caller inner) field
      | None -> Unknown)
  | _ -> Unknown

and member_receiver (t : t) (receiver : receiver_class) (field : string) :
    receiver_class =
  match (field_type t receiver field, receiver) with
  | Some cls, _ -> Class cls
  | None, Object_of held ->
      Object_of
        { held with path = held.path @ [ field ]; held_class = Of_unknown_class }
  | None, External_class -> External_class
  | ( None,
      (Class _ | Exact _ | Class_object _ | Ancestors_of _ | Root | Unknown) ) ->
      Unknown

and member_call (t : t) ~(caller : Function_id.t option) (callee : G.expr) :
    (receiver_class * string * resolution Lazy.t) option =
  match (callee.G.e, member_access t callee) with
  | G.DotAccess _, Some (receiver, member) ->
      let root, prefix = receiver_chain t receiver in
      let root_class = receiver_class t ~caller root in
      Some
        ( List.fold_left (member_receiver t) root_class prefix,
          member,
          lazy
            (resolve_member_access t ~receiver ~member ~prefix
               ~root_class:(Lazy.from_val root_class)) )
  | _ -> None

and returned_by (t : t) ~(caller : Function_id.t option) (callee : G.expr) :
    receiver_class =
  let declared, resolved =
    match member_call t ~caller callee with
    | Some (receiver, member, resolved) ->
        ( member_type t receiver (fun (owner : Class_table.cls) ->
              Type_state.method_return t.types owner member),
          resolved )
    | None -> (None, lazy (resolve_expr t ~caller ~use:Called ~visited:[] callee))
  in
  match declared with
  | Some cls -> Class cls
  | None -> (
      match Lazy.force resolved with
      | Defined (funcs : Func_info.t list) -> returned_by_functions t funcs
      | External -> (
          match t.outside t ~caller callee with
          | Defined (funcs : Func_info.t list) -> returned_by_functions t funcs
          | External -> Unknown))

and returned_by_functions (t : t) (funcs : Func_info.t list) : receiver_class =
  match
    List_.uniq_by Class_table.same
      (List.filter_map
         (fun (func : Func_info.t) ->
           Option.bind (node_of_function func)
             (Type_state.function_return t.types))
         funcs)
  with
  | [ cls ] -> Class cls
  | []
  | _ :: _ :: _ ->
      Unknown

let resolve_callee (t : t) ~(caller : Function_id.t option) (e : G.expr) :
    resolution =
  resolve_expr t ~caller ~use:Called ~visited:[] e

let resolve_reference (t : t) ~(caller : Function_id.t option) (e : G.expr) :
    resolution =
  resolve_expr t ~caller ~use:Referenced ~visited:[] e

(* The constructors [new T(...)] reaches: those of the class [T] binds, or
   the function [T] binds, called as a constructor. *)
let resolve_construction (t : t) (ty : G.type_) : resolution =
  match name_of_type ty with
  | Some name -> (
      let info = id_info_of_name name in
      match
        ( binding_of_id_info info,
          Class_table.class_of_name t.classes
            ~position:Class_table.Type_position ~context:None name )
      with
      | _, Some cls -> constructors_of_class t cls
      | Some sid, None -> (
          match functions_of_binding t sid with
          | _ :: _ as defined -> Defined defined
          | [] -> if defined_elsewhere info then External else Defined [])
      | None, None -> External)
  | None -> (
      match Class_table.path_of_type ty with
      | Some ((_, _ :: _) as path) -> (
          match
            Class_table.class_of_path t.classes
              ~position:Class_table.Type_position ~context:None path
          with
          | Some cls -> constructors_of_class t cls
          | None -> External)
      | Some (_, [])
      | None ->
          Defined [])

let resolve_qualified (t : t) (name : G.name) : resolution =
  let path = qualified_path name in
  match Path_tbl.find_opt t.qualified_functions path with
  | Some (_ :: _ as defined) -> Defined defined
  | Some []
  | None -> (
      match List.rev path with
      | member :: (_ :: _ as class_path) -> (
          match classes_at t (List.rev class_path) with
          | [ cls ] -> resolve_member t (Class_object cls) member
          | [] | _ :: _ :: _ -> External)
      | [ _ ]
      | [] ->
          External)

let with_types (t : t) (types : Type_state.t) : t = { t with types }

let class_definitions (t : t) : (scope_id * G.definition_kind) list =
  t.class_definitions

let class_of_receiver (receiver : receiver_class) : Class_table.cls option =
  match receiver with
  | Class cls
  | Exact cls
  | Object_of { path = []; held_class = Of_class cls; _ } ->
      Some cls
  | Class_object _
  | Ancestors_of _
  | Object_of _
  | External_class
  | Root
  | Unknown ->
      None

let class_of_expr (t : t) ~(caller : Function_id.t option) (e : G.expr) :
    Class_table.cls option =
  class_of_receiver (receiver_class t ~caller e)

let class_of_declared_type (t : t) ~(context : scope_id option) (ty : G.type_)
    : Class_table.cls option =
  class_of_receiver (class_of_type t ~context ty)

let class_of_function (t : t) (func : Func_info.t) : Class_table.cls option =
  match
    Option.map (of_self_type t) (Fdef_tbl.find_opt t.selves func.Func_info.fdef)
  with
  | Some (Class cls | Exact cls | Class_object cls) -> Some cls
  | Some
      ( Ancestors_of _ | Object_of _ | External_class | Root | Unknown )
  | None ->
      None

let or_outside (t : t) ~(caller : Function_id.t option) (e : G.expr)
    (resolved : resolution) : resolution =
  match resolved with
  | Defined _ -> resolved
  | External -> t.outside t ~caller e

let resolve_call (t : t) ~(caller : Function_id.t option) (e : G.expr) :
    resolution =
  or_outside t ~caller e (resolve_callee t ~caller e)

let class_of_member_call (t : t) ~(caller : Function_id.t option)
    (callee : G.expr) : (Class_table.cls option * resolution Lazy.t) option =
  Option.map
    (fun ((receiver : receiver_class), (_ : string),
          (resolved : resolution Lazy.t)) ->
      ( class_of_receiver receiver,
        lazy (or_outside t ~caller callee (Lazy.force resolved)) ))
    (member_call t ~caller callee)
