module G = AST_generic

type side =
  | Instance_side
  | Class_side

type t = {
  written : G.type_;
  relation : Member_lookup.relation;
  side : side;
  arguments : G.arguments option;
  delegate : G.expr option;
}

type mixin_calls = {
  inclusions : t list;
  extends : G.type_ list;
}

let instance_parent (relation : Member_lookup.relation) (written : G.type_) : t
    =
  { written; relation; side = Instance_side; arguments = None; delegate = None }

let class_side (written : G.type_) : t =
  {
    written;
    relation = Member_lookup.Included;
    side = Class_side;
    arguments = None;
    delegate = None;
  }

let extended (parent : G.class_parent) : t =
  {
    written = parent.G.cp_type;
    relation =
      Member_lookup.Extends
        {
          constructed = Option.is_some parent.G.cp_args;
          virtual_base =
            AST_generic_helpers.has_keyword_attr G.Virtual
              parent.G.cp_type.G.t_attrs;
        };
    side = Instance_side;
    arguments = parent.G.cp_args;
    delegate = parent.G.cp_delegate;
  }

let type_of_expr (e : G.expr) : G.type_ = G.TyExpr e |> G.t

let rec expression_statements (stmt : G.stmt) : G.expr list =
  match stmt.G.s with
  | G.ExprStmt (e, _) -> [ e ]
  | G.Block (_, stmts, _) -> List.concat_map expression_statements stmts
  | _ -> []

let body_of_fields (fields : G.field list) : G.stmt list =
  List.map (fun (G.F stmt) -> stmt) fields

let embedded (body : G.stmt list) : t list =
  body
  |> List.concat_map expression_statements
  |> List.filter_map (fun (e : G.expr) ->
         match e.G.e with
         | G.Call
             ({ G.e = G.IdSpecial (G.Spread, _); _ }, (_, [ G.Arg embedded ], _))
           ->
             Some (instance_parent Member_lookup.Embedded (type_of_expr embedded))
         | _ -> None)

let mixes_in_by_call (lang : Lang.t) : bool =
  match lang with
  | Lang.Ruby
  | Lang.Crystal ->
      true
  | _ -> false

let mixin_arguments (args : G.argument list) : G.type_ list =
  List.filter_map
    (fun (arg : G.argument) ->
      match arg with
      | G.Arg mixin -> Some (type_of_expr mixin)
      | _ -> None)
    args

let no_mixin_calls : mixin_calls = { inclusions = []; extends = [] }

let mixin_calls (lang : Lang.t) (body : G.stmt list) : mixin_calls =
  if mixes_in_by_call lang then
    body
    |> List.concat_map expression_statements
    |> List.fold_left
         (fun (calls : mixin_calls) (e : G.expr) ->
           match e.G.e with
           | G.Call ({ G.e = G.N (G.Id (("prepend", _), _)); _ }, (_, args, _))
             ->
               {
                 calls with
                 inclusions =
                   calls.inclusions
                   @ List.rev_map
                       (instance_parent Member_lookup.Prepended)
                       (mixin_arguments args);
               }
           | G.Call ({ G.e = G.N (G.Id (("include", _), _)); _ }, (_, args, _))
             ->
               {
                 calls with
                 inclusions =
                   calls.inclusions
                   @ List.rev_map
                       (instance_parent Member_lookup.Included)
                       (mixin_arguments args);
               }
           | G.Call ({ G.e = G.N (G.Id (("extend", _), _)); _ }, (_, args, _))
             ->
               { calls with extends = calls.extends @ mixin_arguments args }
           | _ -> calls)
         no_mixin_calls
  else no_mixin_calls

let of_body (lang : Lang.t) ~(written : t list) (body : G.stmt list) : t list =
  let calls = mixin_calls lang body in
  written @ calls.inclusions @ embedded body
  @ List.rev_map class_side calls.extends

let definition_body (def : G.definition_kind) : G.stmt list =
  match def with
  | G.ClassDef cdef -> body_of_fields (Tok.unbracket cdef.G.cbody)
  | G.ModuleDef { G.mbody = G.ModuleStruct (_, items) } -> items
  | G.TypeDef
      { G.tbody = G.NewType { G.t = G.TyRecordAnon (_, (_, fields, _)); _ } } ->
      body_of_fields fields
  | _ -> []

let member_imports (def : G.definition_kind) : G.member_import list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DirectiveStmt { G.d = G.MemberImport import; _ } -> Some import
      | _ -> None)
    (definition_body def)

let type_members (def : G.definition_kind) : (string * G.type_) list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt
          ( { G.name = G.EN (G.Id ((name, _), _)); _ },
            G.TypeDef { G.tbody = G.AliasType aliased } ) ->
          Some (name, aliased)
      | _ -> None)
    (definition_body def)

let declared_members (def : G.definition_kind) : string list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt ({ G.name = G.EN (G.Id ((name, _), _)); _ }, _) -> Some name
      | _ -> None)
    (definition_body def)

let of_definition (lang : Lang.t) (def : G.definition_kind) : t list =
  match def with
  | G.ClassDef cdef ->
      of_body lang
        ~written:
          (List.map extended cdef.G.cextends
          @ List.map (instance_parent Member_lookup.Mixin) cdef.G.cmixins
          @ List.map (instance_parent Member_lookup.Implements)
              cdef.G.cimplements)
        (definition_body def)
  | G.ModuleDef { G.mbody = G.ModuleStruct _ }
  | G.TypeDef { G.tbody = G.NewType { G.t = G.TyRecordAnon _; _ } } ->
      of_body lang ~written:[] (definition_body def)
  | _ -> []

type module_functions =
  | No_module_functions
  | All_module_functions
  | Module_functions of string list

let module_functions_of_call (e : G.expr) : module_functions =
  match e.G.e with
  | G.Call
      ( { G.e = G.N (G.Id (("extend", _), _)); _ },
        (_, [ G.Arg { G.e = G.IdSpecial (G.Self, _); _ } ], _) )
  | G.Call ({ G.e = G.N (G.Id (("module_function", _), _)); _ }, (_, [], _)) ->
      All_module_functions
  | G.Call
      ({ G.e = G.N (G.Id (("module_function", _), _)); _ }, (_, args, _)) ->
      Module_functions
        (List.filter_map
           (fun (arg : G.argument) ->
             match arg with
             | G.Arg { G.e = G.L (G.Atom (_, (name, _))); _ } -> Some name
             | _ -> None)
           args)
  | _ -> No_module_functions

let joined_module_functions (functions : module_functions)
    (found : module_functions) : module_functions =
  match (functions, found) with
  | All_module_functions, _
  | _, All_module_functions ->
      All_module_functions
  | No_module_functions, _ -> found
  | _, No_module_functions -> functions
  | Module_functions earlier, Module_functions later ->
      Module_functions (earlier @ later)

let module_functions (lang : Lang.t) (def : G.definition_kind) :
    module_functions =
  if mixes_in_by_call lang then
    definition_body def
    |> List.concat_map expression_statements
    |> List.map module_functions_of_call
    |> List.fold_left joined_module_functions No_module_functions
  else No_module_functions

let has_modifier (modifier : string) (ent : G.entity) : bool =
  List.exists
    (fun (attr : G.attribute) ->
      match attr with
      | G.NamedAttr (_, G.Id ((found, _), _), _) -> String.equal found modifier
      | _ -> false)
    ent.G.attrs

(* The type an extension adds members to, when the extension carries it
   (Dart 'extension E on T'); a Swift extension's entity is the type. *)
let extended_type (def : G.definition_kind) : G.type_ option =
  match def with
  | G.ClassDef { G.ckind = G.Extension extended, _; _ } -> extended
  | _ -> None

let reopens (lang : Lang.t) (ent : G.entity) (def : G.definition_kind) : bool =
  match (lang, def) with
  | (Lang.Ruby | Lang.Crystal), (G.ClassDef _ | G.ModuleDef _)
  | (Lang.C | Lang.Cpp), G.ClassDef _ ->
      true
  | _, G.ClassDef { G.ckind = G.Extension _, _; _ } -> true
  | Lang.Csharp, G.ClassDef _ -> has_modifier "partial" ent
  | _ -> false

let is_module_function (functions : module_functions) (name : string) : bool =
  match functions with
  | No_module_functions -> false
  | All_module_functions -> true
  | Module_functions names -> List.exists (String.equal name) names

type metatable_fact =
  | Metatable_set of {
      table : G.name;
      metatable : G.expr;
    }
  | Index_assigned of {
      table : G.name;
      index : G.expr;
    }

let index_field (metatable : Lang_config.metatable) (fields : G.expr list) :
    G.expr list =
  List.filter_map
    (fun (field : G.expr) ->
      match field.G.e with
      | G.Assign ({ G.e = G.N (G.Id ((key, _), _)); _ }, _, index)
        when String.equal key metatable.Lang_config.index_key ->
          Some index
      | _ -> None)
    fields

let metatable_facts (metatable : Lang_config.metatable) (program : G.program) :
    metatable_fact list =
  let setmetatable_call (e : G.expr) : (G.expr * G.expr) option =
    match e.G.e with
    | G.Call
        ( { G.e = G.N (G.Id ((callee, _), info)); _ },
          (_, [ G.Arg target; G.Arg table ], _) )
      when String.equal callee metatable.Lang_config.set_metatable
           && Option.is_none !(info.G.id_resolved) ->
        Some (target, table)
    | _ -> None
  in
  (* The iter visitor returns unit: its callbacks gather the facts here. *)
  let facts = ref [] in
  let add (fact : metatable_fact) : unit = facts := fact :: !facts in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_expr env e =
        (match e.G.e with
        | G.Assign ({ G.e = G.N target; _ }, _, value) ->
            Option.iter
              (fun ((_ : G.expr), (metatable_arg : G.expr)) ->
                add (Metatable_set { table = target; metatable = metatable_arg }))
              (setmetatable_call value)
        | G.Assign
            ( {
                G.e =
                  G.DotAccess
                    ({ G.e = G.N table; _ }, _, G.FN (G.Id ((key, _), _)));
                _;
              },
              _,
              index )
          when String.equal key metatable.Lang_config.index_key ->
            add (Index_assigned { table; index })
        | _ -> (
            match setmetatable_call e with
            | Some ({ G.e = G.N target; _ }, metatable_arg) ->
                add (Metatable_set { table = target; metatable = metatable_arg })
            | Some _
            | None ->
                ()));
        super#visit_expr env e

      method! visit_definition env ((ent, def) as definition) =
        (match (ent.G.name, def) with
        | G.EN target, G.VarDef { G.vinit = Some value; _ } -> (
            Option.iter
              (fun ((_ : G.expr), (metatable_arg : G.expr)) ->
                add (Metatable_set { table = target; metatable = metatable_arg }))
              (setmetatable_call value);
            match value.G.e with
            | G.Container (G.Dict, (_, fields, _)) ->
                List.iter
                  (fun (index : G.expr) ->
                    add (Index_assigned { table = target; index }))
                  (index_field metatable fields)
            | _ -> ())
        | _ -> ());
        super#visit_definition env definition
    end
  in
  visitor#visit_program () program;
  List.rev !facts
