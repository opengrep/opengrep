module G = AST_generic

type side =
  | Instance_side
  | Class_side

type t = {
  written : G.type_;
  placement : Linearisation.placement;
  side : side;
}

type mixin_calls = {
  prepends : G.type_ list;
  includes : G.type_ list;
  extends : G.type_ list;
}

let appended (written : G.type_) : t =
  { written; placement = Linearisation.Appended; side = Instance_side }

let prepended (written : G.type_) : t =
  { written; placement = Linearisation.Prepended; side = Instance_side }

let class_side (written : G.type_) : t =
  { written; placement = Linearisation.Appended; side = Class_side }

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
             Some (appended (type_of_expr embedded))
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

let no_mixin_calls : mixin_calls = { prepends = []; includes = []; extends = [] }

let mixin_calls (lang : Lang.t) (body : G.stmt list) : mixin_calls =
  if mixes_in_by_call lang then
    body
    |> List.concat_map expression_statements
    |> List.fold_left
         (fun (calls : mixin_calls) (e : G.expr) ->
           match e.G.e with
           | G.Call ({ G.e = G.N (G.Id (("prepend", _), _)); _ }, (_, args, _))
             ->
               { calls with prepends = calls.prepends @ mixin_arguments args }
           | G.Call ({ G.e = G.N (G.Id (("include", _), _)); _ }, (_, args, _))
             ->
               { calls with includes = calls.includes @ mixin_arguments args }
           | G.Call ({ G.e = G.N (G.Id (("extend", _), _)); _ }, (_, args, _))
             ->
               { calls with extends = calls.extends @ mixin_arguments args }
           | _ -> calls)
         no_mixin_calls
  else no_mixin_calls

let of_body (lang : Lang.t) ~(mixins : G.type_ list) ~(extends : G.type_ list)
    (body : G.stmt list) : t list =
  let calls = mixin_calls lang body in
  List.rev_map prepended calls.prepends
  @ List.rev_map appended (mixins @ calls.includes)
  @ List.map appended extends
  @ embedded body
  @ List.rev_map class_side calls.extends

let definition_body (def : G.definition_kind) : G.stmt list =
  match def with
  | G.ClassDef cdef -> body_of_fields (Tok.unbracket cdef.G.cbody)
  | G.ModuleDef { G.mbody = G.ModuleStruct (_, items) } -> items
  | G.TypeDef
      { G.tbody = G.NewType { G.t = G.TyRecordAnon (_, (_, fields, _)); _ } } ->
      body_of_fields fields
  | _ -> []

let of_definition (lang : Lang.t) (def : G.definition_kind) : t list =
  match def with
  | G.ClassDef cdef ->
      of_body lang ~mixins:cdef.G.cmixins
        ~extends:(List.map fst cdef.G.cextends @ cdef.G.cimplements)
        (definition_body def)
  | G.ModuleDef { G.mbody = G.ModuleStruct _ }
  | G.TypeDef { G.tbody = G.NewType { G.t = G.TyRecordAnon _; _ } } ->
      of_body lang ~mixins:[] ~extends:[] (definition_body def)
  | _ -> []

type singleton_exposure =
  | No_singleton_exposure
  | Every_method_is_a_singleton
  | Named_singleton_methods of string list

let exposure_of_call (e : G.expr) : singleton_exposure =
  match e.G.e with
  | G.Call
      ( { G.e = G.N (G.Id (("extend", _), _)); _ },
        (_, [ G.Arg { G.e = G.IdSpecial (G.Self, _); _ } ], _) )
  | G.Call ({ G.e = G.N (G.Id (("module_function", _), _)); _ }, (_, [], _)) ->
      Every_method_is_a_singleton
  | G.Call
      ({ G.e = G.N (G.Id (("module_function", _), _)); _ }, (_, args, _)) ->
      Named_singleton_methods
        (List.filter_map
           (fun (arg : G.argument) ->
             match arg with
             | G.Arg { G.e = G.L (G.Atom (_, (name, _))); _ } -> Some name
             | _ -> None)
           args)
  | _ -> No_singleton_exposure

let joined_exposure (exposure : singleton_exposure)
    (found : singleton_exposure) : singleton_exposure =
  match (exposure, found) with
  | Every_method_is_a_singleton, _
  | _, Every_method_is_a_singleton ->
      Every_method_is_a_singleton
  | No_singleton_exposure, _ -> found
  | _, No_singleton_exposure -> exposure
  | Named_singleton_methods earlier, Named_singleton_methods later ->
      Named_singleton_methods (earlier @ later)

let singleton_exposure (lang : Lang.t) (def : G.definition_kind) :
    singleton_exposure =
  if mixes_in_by_call lang then
    definition_body def
    |> List.concat_map expression_statements
    |> List.map exposure_of_call
    |> List.fold_left joined_exposure No_singleton_exposure
  else No_singleton_exposure

let has_keyword_attribute (keyword : string) (ent : G.entity) : bool =
  List.exists
    (fun (attr : G.attribute) ->
      match attr with
      | G.NamedAttr (_, G.Id ((found, _), _), _) -> String.equal found keyword
      | _ -> false)
    ent.G.attrs

let reopens (lang : Lang.t) (ent : G.entity) (def : G.definition_kind) : bool =
  match (lang, def) with
  | (Lang.Ruby | Lang.Crystal), (G.ClassDef _ | G.ModuleDef _)
  | (Lang.C | Lang.Cpp), G.ClassDef _ ->
      true
  | Lang.Swift, G.ClassDef { G.ckind = _, keyword; _ } ->
      String.equal (Tok.content_of_tok keyword) "extension"
  | Lang.Csharp, G.ClassDef _ -> has_keyword_attribute "partial" ent
  | _ -> false

let exposes (exposure : singleton_exposure) (name : string) : bool =
  match exposure with
  | No_singleton_exposure -> false
  | Every_method_is_a_singleton -> true
  | Named_singleton_methods names -> List.exists (String.equal name) names
