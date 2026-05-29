(* Object Initialization Detection for All Languages
 *
 * This module provides comprehensive object initialization detection
 * for all languages supported by Semgrep, enabling sophisticated
 * taint analysis across object constructors and method calls.
 *)

module G = AST_generic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Object mapping: variable -> class *)
type object_mapping = G.name * G.name

type constructor_param_field_mapping = {
  class_name : string;
  field_name : G.name;
  param_index : int;
}

(* Matcher type: extracts class name from constructor expression *)
type matcher = G.expr -> G.name list -> G.name option

(*****************************************************************************)
(* Common Matchers *)
(*****************************************************************************)

(* Check if a name is in the known classes list *)
let is_known_class (name : G.name) (class_names : G.name list) : bool =
  List.exists
    (fun class_name ->
      match (name, class_name) with
      | G.Id ((str1, _), _), G.Id ((str2, _), _) -> str1 = str2
      | _ -> false)
    class_names

(* Check if string starts with uppercase *)
let is_uppercase_start str =
  String.length str > 0 && Char.uppercase_ascii str.[0] = str.[0]

let string_of_name = function
  | G.Id ((name, _), _) -> Some name
  | G.IdQualified { name_last = ((name, _), _); _ } -> Some name

let is_class_like_name name =
  match string_of_name name with
  | Some name -> is_uppercase_start name
  | None -> false

let same_name name1 name2 =
  match (string_of_name name1, string_of_name name2) with
  | Some str1, Some str2 -> String.equal str1 str2
  | _ -> false

let same_resolved_name name1 name2 =
  match (name1, name2) with
  | G.Id ((str1, _), id_info1), G.Id ((str2, _), id_info2) ->
      String.equal str1 str2
      &&
      (match (!(id_info1.G.id_resolved), !(id_info2.G.id_resolved)) with
      | Some (_, sid1), Some (_, sid2) -> G.SId.equal sid1 sid2
      | _ -> true)
  | _ -> same_name name1 name2

(* Matcher: new ClassName(args) - basic form *)
let match_new_basic rval_expr class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | _ -> None)
  | _ -> None

(* Matcher: new ClassName(args) - with TyExpr fallback *)
let match_new_with_tyexpr rval_expr class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | G.TyExpr expr -> (
          match expr.G.e with
          | G.N (G.Id ((_, _), _) as name) when is_known_class name class_names ->
              Some name
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Matcher: ClassName(args) - call with uppercase class name *)
let match_call_uppercase rval_expr class_names =
  match rval_expr.G.e with
  | G.Call (class_expr, _) -> (
      match class_expr.G.e with
      | G.N (G.Id ((str, _), _) as name)
        when is_uppercase_start str && is_known_class name class_names ->
          Some name
      | _ -> None)
  | _ -> None

(* Matcher: ClassName.new(args) - Ruby/Rust style *)
let match_dot_new rval_expr class_names =
  match rval_expr.G.e with
  | G.Call (dot_access, _) -> (
      match dot_access.G.e with
      | G.DotAccess (class_expr, _, G.FN (G.Id (("new", _), _))) -> (
          match class_expr.G.e with
          | G.N name when is_known_class name class_names -> Some name
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Matcher: Go &Struct{} or Struct{} *)
let match_go_struct rval_expr class_names =
  match rval_expr.G.e with
  | G.Ref (_, struct_expr) -> (
      match struct_expr.G.e with
      | G.New (_, struct_type, _, _) -> (
          match struct_type.G.t with
          | G.TyN name when is_known_class name class_names -> Some name
          | _ -> None)
      | _ -> None)
  | G.New (_, struct_type, _, _) -> (
      match struct_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | _ -> None)
  | _ -> None

(* Combine multiple matchers - try each until one succeeds *)
let combine matchers rval_expr class_names =
  List.find_map (fun m -> m rval_expr class_names) matchers

(* Apex: doesn't validate against class_names *)
let match_apex rval_expr _class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name -> Some name
      | _ -> None)
  | _ -> None

(*****************************************************************************)
(* Language Matcher Lookup *)
(*****************************************************************************)

let get_matcher (lang : Lang.t) : matcher option =
  match lang with
  | Lang.Java | Lang.Csharp | Lang.Vb -> Some match_new_basic
  | Lang.Php -> Some match_new_with_tyexpr
  | Lang.Python | Lang.Python2 | Lang.Python3 | Lang.Swift -> Some match_call_uppercase
  | Lang.Ruby | Lang.Rust -> Some match_dot_new
  | Lang.Go -> Some match_go_struct
  | Lang.Apex -> Some match_apex
  | Lang.Kotlin | Lang.Cpp -> Some (combine [match_new_basic; match_call_uppercase])
  | Lang.Scala | Lang.Js | Lang.Ts | Lang.Dart -> Some (combine [match_new_with_tyexpr; match_call_uppercase])
  | _ -> None

(*****************************************************************************)
(* Class Detection *)
(*****************************************************************************)

(* Collect all class names from the AST *)
let collect_class_names (ast : G.program) : G.name list =
  let class_names = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.ClassDef _ -> (
            match entity.G.name with
            | G.EN name -> class_names := name :: !class_names
            | _ -> ())
        (* Handle Go struct definitions - TypeDef with TyRecordAnon *)
        | entity, G.TypeDef type_def -> (
            match (entity.G.name, type_def.G.tbody) with
            | G.EN name, G.NewType { G.t = G.TyRecordAnon ((G.Class, _), _); _ }
              ->
                class_names := name :: !class_names
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in

  List.iter
    (fun item ->
      match item.G.s with
      | G.DefStmt def -> visitor#visit_definition () def
      | _ -> ())
    ast;
  !class_names

(*****************************************************************************)
(* Object Initialization Detection *)
(*****************************************************************************)

(* Extract class name from constructor call expression *)
let extract_class_name_from_constructor (rval_expr : G.expr) (lang : Lang.t)
    (class_names : G.name list) : G.name option =
  match get_matcher lang with
  | Some matcher -> matcher rval_expr class_names
  | None -> None

(* Object initialization detection for different languages *)
let detect_object_initialization (ast : G.program) (lang : Lang.t) :
    object_mapping list =
  let class_names = collect_class_names ast in
  let object_mappings = ref [] in
  let object_property_mappings = ref [] in
  let object_property_alias_mappings = ref [] in
  let injected_field_mappings = ref [] in
  let constructor_param_field_mappings = ref [] in
  let provider_array_mappings = ref [] in
  let function_return_mappings = ref [] in
  let function_return_param_mappings = ref [] in
  let function_return_object_property_mappings = ref [] in
  let function_alias_mappings = ref [] in
  let object_property_factory_return_mappings = ref [] in
  let object_property_function_mappings = ref [] in
  let string_constant_mappings = ref [] in
  let class_name_from_type (type_ : G.type_) =
    match type_.G.t with
    | G.TyN name when is_known_class name class_names -> Some name
    | G.TyExpr { G.e = G.N (G.Id _ as name); _ }
      when is_known_class name class_names ->
        Some name
    | _ -> None
  in
  let is_parameter_property_attr = function
    | G.KeywordAttr ((G.Public | G.Private | G.Protected | G.Const), _) -> true
    | _ -> false
  in
  let is_constructor_name func_name class_name =
    let config = Lang_config.get lang in
    List.mem func_name config.constructor_names || func_name = class_name
  in
  let constructor_arg_expr args index =
    let positional_args =
      args
      |> List.filter_map (function
           | G.Arg expr -> Some expr
           | _ -> None)
    in
    List.nth_opt positional_args index
  in
  let name_from_mapping mappings expr =
    match expr.G.e with
    | G.N name ->
        !mappings
        |> List.find_opt (fun (mapped_name, _resolved_name) ->
               same_resolved_name name mapped_name)
        |> Option.map snd
    | _ -> None
  in
  let name_from_called_function mappings func_expr =
    match func_expr.G.e with
    | G.Call ({ e = G.N name; _ }, _) ->
        !mappings
        |> List.find_opt (fun (mapped_name, _resolved_name) ->
               same_resolved_name name mapped_name)
        |> Option.map snd
    | _ -> None
  in
  let name_from_name_mapping mappings name =
    !mappings
    |> List.find_opt (fun (mapped_name, _resolved_name) ->
           same_resolved_name name mapped_name)
    |> Option.map snd
  in
  let same_resolved_path path1 path2 =
    List.length path1 = List.length path2
    && List.for_all2 same_resolved_name path1 path2
  in
  let name_from_static_string_value value =
    G.Id ((value, Tok.unsafe_fake_tok value), G.empty_id_info ())
  in
  let dynamic_property_key_name_component = function
    | G.Id ((name, _), id_info) ->
        let sid_suffix =
          match !(id_info.G.id_resolved) with
          | Some (_, sid) when not (G.SId.is_unsafe_default sid) ->
              ":" ^ G.SId.show sid
          | _ -> ""
        in
        Some ("var:" ^ String.escaped name ^ sid_suffix)
    | _ -> None
  in
  let name_from_dynamic_property_key_name = function
    | (G.Id ((_, _), id_info) as key_name) -> (
        match dynamic_property_key_name_component key_name with
        | Some component ->
            let dynamic_name = "__opengrep_dynamic_key:" ^ component in
            Some (G.Id ((dynamic_name, Tok.unsafe_fake_tok dynamic_name), id_info))
        | None -> None)
    | _ -> None
  in
  let rec dynamic_property_key_expr_component expr =
    match expr.G.e with
    | G.L (G.String (_, (part, _), _)) -> Some ("str:" ^ String.escaped part)
    | G.N key_name -> dynamic_property_key_name_component key_name
    | G.Call
        ( { e = G.IdSpecial (G.InterpolatedElement, _); _ },
          (_, [ G.Arg inner_expr ], _) ) ->
        dynamic_property_key_expr_component inner_expr
    | G.Call ({ e = G.IdSpecial (G.Op G.Plus, _); _ }, (_, args, _))
    | G.Call ({ e = G.IdSpecial (G.ConcatString _, _); _ }, (_, args, _)) ->
        dynamic_property_key_args_component args
    | _ -> None
  and dynamic_property_key_args_component args =
    let rec parts = function
      | [] -> Some []
      | G.Arg arg_expr :: rest -> (
          match (dynamic_property_key_expr_component arg_expr, parts rest) with
          | Some part, Some rest -> Some (part :: rest)
          | _ -> None)
      | _ -> None
    in
    match parts args with
    | Some parts -> Some ("expr:" ^ String.concat "|" parts)
    | None -> None
  in
  let rec name_from_static_string_expr expr =
    match expr.G.e with
    | G.L (G.String (_, (field_name, tok), _)) ->
        Some (G.Id ((field_name, tok), G.empty_id_info ()))
    | G.N key_name -> name_from_name_mapping string_constant_mappings key_name
    | G.Call ({ e = G.IdSpecial (G.Op G.Plus, _); _ }, (_, args, _))
    | G.Call ({ e = G.IdSpecial (G.ConcatString _, _); _ }, (_, args, _))
      ->
        name_from_static_string_args args
    | G.Call
        ( { e = G.IdSpecial (G.InterpolatedElement, _); _ },
          (_, [ G.Arg inner_expr ], _) ) ->
        name_from_static_string_expr inner_expr
    | _ -> None
  and name_from_static_string_args args =
    let rec string_parts = function
      | [] -> Some []
      | G.Arg arg_expr :: rest -> (
          match (name_from_static_string_expr arg_expr, string_parts rest) with
          | Some name, Some parts -> (
              match string_of_name name with
              | Some part -> Some (part :: parts)
              | None -> None)
          | _ -> None)
      | _ -> None
    in
    match string_parts args with
    | Some parts -> Some (name_from_static_string_value (String.concat "" parts))
    | None -> None
  in
  let name_from_dynamic_property_key_expr expr =
    match expr.G.e with
    | G.N key_name -> name_from_dynamic_property_key_name key_name
    | _ -> (
        match dynamic_property_key_expr_component expr with
        | Some component ->
            let dynamic_name = "__opengrep_dynamic_expr:" ^ component in
            Some
              (G.Id
                 ((dynamic_name, Tok.unsafe_fake_tok dynamic_name),
                  G.empty_id_info ()))
        | None -> None)
  in
  let name_from_property_key_expr expr =
    match name_from_static_string_expr expr with
    | Some _ as static_key -> static_key
    | None -> name_from_dynamic_property_key_expr expr
  in
  let field_name_from_entity entity =
    match entity.G.name with
    | G.EN field_name -> Some field_name
    | G.EDynamic field_expr -> name_from_property_key_expr field_expr
    | _ -> None
  in
  let method_name_string = function
    | G.Id ((name, _), _) -> Some name
    | _ -> None
  in
  let method_name_matches names method_name =
    match method_name_string method_name with
    | Some name -> List.exists (String.equal name) names
    | _ -> false
  in
  let name_from_lambda_returned_name fdef =
    match fdef.G.fbody with
    | G.FBExpr { G.e = G.N name; _ } -> Some name
    | _ ->
        let returned_name = ref None in
        let visitor =
          object
            inherit [_] G.iter as super

            method! visit_stmt () stmt =
              (match (!returned_name, stmt.G.s) with
              | None, G.Return (_, Some { G.e = G.N name; _ }, _) ->
                  returned_name := Some name
              | _ -> ());
              super#visit_stmt () stmt
          end
        in
        visitor#visit_stmt () (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody);
        !returned_name
  in
  let name_from_deferred_ref_expr expr =
    match expr.G.e with
    | G.Call
        ( { G.e = G.N deferred_ref_name; _ },
          (_, [ G.Arg { G.e = G.Lambda fdef; _ } ], _) )
      when method_name_matches [ "forwardRef"; "delay" ] deferred_ref_name ->
        name_from_lambda_returned_name fdef
    | _ -> None
  in
  let is_object_property_get_method_name =
    method_name_matches
      [ "get"; "getAsync"; "resolve"; "resolveAsync"; "lookup" ]
  in
  let is_object_property_set_method_name =
    method_name_matches [ "set"; "register"; "bind"; "provide" ]
  in
  let is_object_property_provider_method_name =
    method_name_matches
      [ "to"; "toConstantValue"; "toDynamicValue"; "useClass"; "useValue";
        "useFactory"; "asClass"; "asValue"; "asFunction"; "useExisting";
        "toService"; "aliasTo" ]
  in
  let is_object_property_provider_lifecycle_method_name =
    method_name_matches
      [ "inSingletonScope"; "inTransientScope"; "inRequestScope"; "singleton";
        "transient"; "scoped"; "proxy"; "classic"; "when"; "whenTargetNamed";
        "whenTargetTagged"; "whenInjectedInto"; "whenAnyAncestorIs";
        "whenNoAncestorIs" ]
  in
  let is_object_child_container_method_name =
    method_name_matches
      [ "createChild"; "createChildContainer"; "createScope";
        "createRequestScope" ]
  in
  let is_inject_attribute_name =
    method_name_matches [ "Inject"; "inject"; "Autowired" ]
  in
  let is_dependency_metadata_attribute_name =
    method_name_matches
      [ "Inject"; "Autowired"; "Optional"; "Self"; "SkipSelf"; "Host" ]
  in
  let is_injectable_attribute_name =
    method_name_matches
      [ "Injectable"; "Component"; "Controller"; "Service"; "Directive";
        "Resolver" ]
  in
  let has_dependency_metadata_attribute attrs =
    attrs
    |> List.exists (function
         | G.NamedAttr (_, attr_name, _) ->
             is_dependency_metadata_attribute_name attr_name
         | _ -> false)
  in
  let has_injectable_attribute attrs =
    attrs
    |> List.exists (function
         | G.NamedAttr (_, attr_name, _) ->
             is_injectable_attribute_name attr_name
         | _ -> false)
  in
  let injected_key_from_attrs attrs =
    attrs
    |> List.find_map (function
         | G.NamedAttr (_, attr_name, attr_args)
           when is_inject_attribute_name attr_name -> (
             match Tok.unbracket attr_args with
             | [ G.Arg { G.e = G.N class_token; _ } ]
               when is_class_like_name class_token ->
                 Some class_token
             | [ G.Arg key_expr ] -> (
                 match name_from_deferred_ref_expr key_expr with
                 | Some class_token when is_class_like_name class_token ->
                     Some class_token
                 | _ -> name_from_property_key_expr key_expr)
             | _ -> None)
         | _ -> None)
  in
  let rec object_property_path_from_base obj_expr field_name =
    match object_property_path_from_expr obj_expr with
    | Some (obj_name, field_path) -> Some (obj_name, field_path @ [ field_name ])
    | None -> (
        match obj_expr.G.e with
        | G.N obj_name -> Some (obj_name, [ field_name ])
        | _ -> None)
  and object_property_path_from_expr expr =
    match expr.G.e with
    | G.DotAccess (obj_expr, _, G.FN field_name) ->
        object_property_path_from_base obj_expr field_name
    | G.ArrayAccess (obj_expr, (_, field_expr, _)) -> (
        match name_from_property_key_expr field_expr with
        | Some field_name -> object_property_path_from_base obj_expr field_name
        | None -> None)
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg key_expr ], _) ) -> (
        match
          ( is_object_property_get_method_name method_name,
            name_from_property_key_expr key_expr )
        with
        | true, Some field_name -> object_property_path_from_base obj_expr field_name
        | _ -> None)
    | _ -> None
  in
  let object_property_set_call_from_expr expr =
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg key_expr; G.Arg value_expr ], _) ) -> (
        match
          ( is_object_property_set_method_name method_name,
            name_from_property_key_expr key_expr )
        with
        | true, Some field_name -> (
            match object_property_path_from_base obj_expr field_name with
            | Some (obj_name, field_path) -> Some (obj_name, field_path, value_expr)
            | None -> None)
        | _ -> None)
    | _ -> None
  in
  let rec unwrap_provider_lifecycle_expr expr =
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (provider_expr, _, G.FN lifecycle_method_name); _ },
          _ )
      when is_object_property_provider_lifecycle_method_name
             lifecycle_method_name ->
        unwrap_provider_lifecycle_expr provider_expr
    | _ -> expr
  in
  let object_property_provider_call_from_expr expr =
    let expr = unwrap_provider_lifecycle_expr expr in
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (bind_expr, _, G.FN provider_method_name); _ },
          (_, [ G.Arg provider_expr ], _) ) -> (
        match
          ( is_object_property_provider_method_name provider_method_name,
            method_name_string provider_method_name,
            bind_expr.G.e )
        with
        | ( true,
            Some provider_method_name,
            G.Call
              ( { e = G.DotAccess (obj_expr, _, G.FN bind_method_name); _ },
                (_, [ G.Arg key_expr ], _) ) ) -> (
            match
              ( is_object_property_set_method_name bind_method_name,
                name_from_property_key_expr key_expr )
            with
            | true, Some field_name -> (
                match object_property_path_from_base obj_expr field_name with
                | Some (obj_name, field_path) ->
                    Some (obj_name, field_path, provider_method_name, provider_expr)
                | None -> None)
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let object_name_from_child_container_expr expr =
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess ({ e = G.N obj_name; _ }, _, G.FN method_name); _ },
          _ )
      when is_object_child_container_method_name method_name ->
        Some obj_name
    | _ -> None
  in
  let rec object_property_set_chain_entries_from_expr expr =
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg key_expr; G.Arg value_expr ], _) ) -> (
        let previous_entries =
          object_property_set_chain_entries_from_expr obj_expr
        in
        match
          ( is_object_property_set_method_name method_name,
            name_from_property_key_expr key_expr )
        with
        | true, Some field_name -> previous_entries @ [ (field_name, value_expr) ]
        | _ -> previous_entries)
    | _ -> []
  in
  let value_expr_from_chained_object_property_get expr =
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg key_expr ], _) ) -> (
        match
          ( is_object_property_get_method_name method_name,
            name_from_property_key_expr key_expr )
        with
        | true, Some field_name ->
            object_property_set_chain_entries_from_expr obj_expr
            |> List.rev
            |> List.find_opt (fun (mapped_field_name, _value_expr) ->
                   same_resolved_name field_name mapped_field_name)
            |> Option.map snd
        | _ -> None)
    | _ -> None
  in
  let class_name_from_object_mapping expr =
    name_from_mapping object_mappings expr
  in
  let object_property_path_from_alias alias_name =
    let rec aux seen alias_name =
      if List.exists (same_resolved_name alias_name) seen then None
      else
        !object_property_alias_mappings
        |> List.find_opt (fun (mapped_alias, _obj_name, _field_path) ->
               same_resolved_name alias_name mapped_alias)
        |> Option.map (fun (_mapped_alias, obj_name, field_path) ->
               match aux (alias_name :: seen) obj_name with
               | Some (root_obj_name, root_field_path) ->
                   (root_obj_name, root_field_path @ field_path)
               | None -> (obj_name, field_path))
    in
    aux [] alias_name
  in
  let class_name_from_object_property_mapping expr =
    let class_name_from_path obj_name field_path =
      !object_property_mappings
      |> List.find_opt (fun (mapped_obj, mapped_path, _class_name) ->
             same_resolved_name obj_name mapped_obj
             && same_resolved_path field_path mapped_path)
      |> Option.map (fun (_obj_name, _field_path, class_name) -> class_name)
    in
    match object_property_path_from_expr expr with
    | Some (obj_name, field_path) -> (
        match class_name_from_path obj_name field_path with
        | Some _ as class_name -> class_name
        | None -> (
            match object_property_path_from_alias obj_name with
            | Some (root_obj_name, root_field_path) ->
                class_name_from_path root_obj_name
                  (root_field_path @ field_path)
            | None -> None))
    | None -> None
  in
  let class_name_from_injected_provider_key field_name =
    !object_property_mappings
    |> List.find_opt (fun (_mapped_obj, mapped_path, _class_name) ->
           same_resolved_path [ field_name ] mapped_path
           ||
           match mapped_path with
           | [ mapped_name ] -> same_name field_name mapped_name
           | _ -> false)
    |> Option.map (fun (_mapped_obj, _mapped_path, class_name) -> class_name)
  in
  let class_name_from_provider_key_or_self class_name =
    match class_name_from_injected_provider_key class_name with
    | Some provider_class_name -> provider_class_name
    | None
      when is_known_class class_name class_names -> (
        !object_property_mappings
        |> List.find_opt (fun (_mapped_obj, mapped_path, _class_name) ->
               match mapped_path with
               | [ mapped_name ] -> same_name class_name mapped_name
               | _ -> false)
        |> Option.map (fun (_mapped_obj, _mapped_path, provider_class_name) ->
               provider_class_name)
        |> Option.value ~default:class_name)
    | None -> class_name
  in
  let normalized_injected_field_name field_name =
    match string_of_name field_name with
    | Some field_name -> name_from_static_string_value field_name
    | None -> field_name
  in
  let record_injected_field_mapping field_name injected_key =
    let field_name = normalized_injected_field_name field_name in
    injected_field_mappings :=
      (field_name, injected_key) :: !injected_field_mappings;
    match class_name_from_injected_provider_key injected_key with
    | Some class_name ->
        object_mappings := (field_name, class_name) :: !object_mappings
    | None -> ()
  in
  let record_injected_metadata_class_mapping field_name class_name =
    object_mappings :=
      (normalized_injected_field_name field_name, class_name) :: !object_mappings
  in
  let class_name_from_direct_object_property_factory obj_name field_path =
    !object_property_factory_return_mappings
    |> List.find_opt (fun (mapped_obj, mapped_path, _class_name) ->
           same_resolved_name obj_name mapped_obj
           && same_resolved_path field_path mapped_path)
    |> Option.map (fun (_obj_name, _field_path, class_name) -> class_name)
  in
  let function_name_from_object_property_factory obj_name field_path =
    !object_property_function_mappings
    |> List.find_opt (fun (mapped_obj, mapped_path, _func_name) ->
           same_resolved_name obj_name mapped_obj
           && same_resolved_path field_path mapped_path)
    |> Option.map (fun (_obj_name, _field_path, func_name) -> func_name)
  in
  let function_has_returned_object_properties func_name =
    !function_return_object_property_mappings
    |> List.exists (fun (mapped_func_name, _mapped_path, _class_name) ->
           same_resolved_name func_name mapped_func_name)
  in
  let rec returned_object_property_function_names seen func_name =
    if List.exists (same_resolved_name func_name) seen then []
    else
      let direct_names =
        if function_has_returned_object_properties func_name then [ func_name ]
        else []
      in
      let alias_names =
        match name_from_name_mapping function_alias_mappings func_name with
        | Some aliased_func ->
            returned_object_property_function_names (func_name :: seen)
              aliased_func
        | None -> []
      in
      direct_names @ alias_names
  in
  let returned_object_property_function_names_from_callee callee_expr =
    let function_names_from_name name =
      returned_object_property_function_names [] name
    in
    match callee_expr.G.e with
    | G.N func_name -> function_names_from_name func_name
    | _ -> (
        match object_property_path_from_expr callee_expr with
        | Some (obj_name, field_path) -> (
            match function_name_from_object_property_factory obj_name field_path with
            | Some func_name -> function_names_from_name func_name
            | None -> [])
        | None -> [])
  in
  let class_name_from_direct_object_property_factory_expr expr =
    match object_property_path_from_expr expr with
    | Some (obj_name, field_path) ->
        class_name_from_direct_object_property_factory obj_name field_path
    | None -> None
  in
  let class_name_from_object_property_function_mapping callee_expr =
    match object_property_path_from_expr callee_expr with
    | Some (obj_name, field_path) ->
        (match class_name_from_direct_object_property_factory obj_name field_path with
        | Some _ as class_name -> class_name
        | None -> (
            match function_name_from_object_property_factory obj_name field_path with
            | Some func_name ->
                name_from_name_mapping function_return_mappings func_name
            | None -> None))
    | None -> None
  in
  let class_name_from_function_return expr =
    let class_name_from_function_alias () =
      match name_from_called_function function_alias_mappings expr with
      | Some returned_func ->
          name_from_name_mapping function_return_mappings returned_func
      | None -> (
          match expr.G.e with
          | G.Call (callee_expr, _) -> (
              match name_from_called_function function_alias_mappings callee_expr with
              | Some returned_func ->
                  name_from_name_mapping function_return_mappings returned_func
              | None -> None)
          | _ -> None)
    in
    match name_from_called_function function_return_mappings expr with
    | Some _ as class_name -> class_name
    | None -> (
        match expr.G.e with
        | G.Call (callee_expr, _) -> (
            match class_name_from_object_property_function_mapping callee_expr with
            | Some _ as class_name -> class_name
            | None -> class_name_from_function_alias ())
        | _ -> class_name_from_function_alias ())
  in
  let function_alias_from_expr expr =
    match name_from_called_function function_alias_mappings expr with
    | Some _ as returned_func -> returned_func
    | None -> (
        match expr.G.e with
        | G.N name -> (
            match name_from_name_mapping function_alias_mappings name with
            | Some _ as returned_func -> returned_func
            | None -> (
                match name_from_name_mapping function_return_mappings name with
                | Some _ -> Some name
                | None ->
                    if function_has_returned_object_properties name then Some name
                    else None))
        | _ -> (
            match object_property_path_from_expr expr with
            | Some (obj_name, field_path) ->
                function_name_from_object_property_factory obj_name field_path
            | None -> None))
  in
  let record_function_alias_mapping alias_name init_expr =
    match function_alias_from_expr init_expr with
    | Some returned_func ->
        function_alias_mappings :=
          (alias_name, returned_func) :: !function_alias_mappings
    | None -> (
        match class_name_from_direct_object_property_factory_expr init_expr with
        | Some class_name ->
            function_return_mappings :=
              (alias_name, class_name) :: !function_return_mappings
        | None -> ())
  in
  let rec class_name_from_expr expr =
    match expr.G.e with
    | G.Await (_, awaited_expr) -> class_name_from_expr awaited_expr
    | _ -> (
        match extract_class_name_from_constructor expr lang class_names with
        | Some _ as class_name -> class_name
        | None -> (
            match class_name_from_object_mapping expr with
            | Some _ as class_name -> class_name
            | None -> (
                match class_name_from_object_property_mapping expr with
                | Some _ as class_name -> class_name
                | None -> (
                    match value_expr_from_chained_object_property_get expr with
                    | Some value_expr -> class_name_from_expr value_expr
                    | None -> (
                        match class_name_from_function_return expr with
                        | Some _ as class_name -> class_name
                        | None -> class_name_from_conditional_expr expr)))))
  and class_name_from_conditional_expr expr =
    match expr.G.e with
    | G.Conditional (_condition, then_expr, else_expr) -> (
        match (class_name_from_expr then_expr, class_name_from_expr else_expr) with
        | Some then_class, Some else_class when same_name then_class else_class
          ->
            Some then_class
        | _ -> None)
    | _ -> None
  in
  let record_constructor_param_field_mappings class_name class_attrs fdef =
    let use_typed_constructor_metadata =
      has_injectable_attribute class_attrs
    in
    let param_indexes =
      Tok.unbracket fdef.G.fparams
      |> List.mapi (fun index -> function
           | G.Param { G.pname = Some ((param_name, _)); _ } ->
               Some (param_name, index)
           | _ -> None)
      |> List.filter_map Fun.id
    in
    let injected_param_keys =
      Tok.unbracket fdef.G.fparams
      |> List.filter_map (function
           | G.Param { G.pname = Some ((param_name, _)); pattrs; pinfo; _ } -> (
               match injected_key_from_attrs pattrs with
               | Some injected_key -> Some (param_name, pinfo, injected_key)
               | None -> None)
           | _ -> None)
    in
    let injected_param_classes =
      Tok.unbracket fdef.G.fparams
      |> List.filter_map (function
           | G.Param
               {
                 G.pname = Some ((param_name, _));
                 ptype = Some param_type;
                 pattrs;
                 pinfo;
                 _;
               }
             when (has_dependency_metadata_attribute pattrs
                  || use_typed_constructor_metadata)
                  && Option.is_none (injected_key_from_attrs pattrs) -> (
               match class_name_from_type param_type with
               | Some class_name ->
                   Some
                     ( param_name,
                       pinfo,
                       class_name_from_provider_key_or_self class_name )
               | None -> None)
           | _ -> None)
    in
    let param_index param_name = List.assoc_opt param_name param_indexes in
    let injected_param_key param_name =
      injected_param_keys
      |> List.find_opt (fun (mapped_param_name, _pinfo, _injected_key) ->
             String.equal mapped_param_name param_name)
      |> Option.map (fun (_param_name, _pinfo, injected_key) -> injected_key)
    in
    let injected_param_class param_name =
      injected_param_classes
      |> List.find_opt (fun (mapped_param_name, _pinfo, _class_name) ->
             String.equal mapped_param_name param_name)
      |> Option.map (fun (_param_name, _pinfo, class_name) -> class_name)
    in
    injected_param_keys
    |> List.iter (fun (param_name, pinfo, injected_key) ->
           match class_name_from_injected_provider_key injected_key with
           | Some class_name ->
               object_mappings :=
                 (G.Id ((param_name, Tok.unsafe_fake_tok param_name), pinfo),
                  class_name)
                 :: !object_mappings
           | None -> ());
    injected_param_classes
    |> List.iter (fun (param_name, pinfo, class_name) ->
           object_mappings :=
             (G.Id ((param_name, Tok.unsafe_fake_tok param_name), pinfo),
              class_name)
             :: !object_mappings);
    let visitor =
      object
        inherit [_] G.iter as super

        method! visit_expr () expr =
          (match expr.G.e with
          | G.Assign
              ( { e =
                    G.DotAccess
                      ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
                        _,
                        G.FN (G.Id _ as field_name) );
                  _ },
                _,
                { e = G.N (G.Id ((param_name, _), _)); _ } ) -> (
              match param_index param_name with
              | Some index ->
                  constructor_param_field_mappings :=
                    { class_name; field_name; param_index = index }
                    :: !constructor_param_field_mappings;
                  (match injected_param_key param_name with
                  | Some injected_key ->
                      record_injected_field_mapping field_name injected_key
                  | None -> ());
                  (match injected_param_class param_name with
                  | Some class_name ->
                      record_injected_metadata_class_mapping field_name
                        class_name
                  | None -> ())
              | None -> ())
          | _ -> ());
          super#visit_expr () expr
      end
    in
    visitor#visit_function_definition () fdef
  in
  let record_class_constructor_param_field_mappings class_name class_attrs
      class_def =
    let class_name_str = string_of_name class_name in
    match class_name_str with
    | None -> ()
    | Some class_name_str ->
        Tok.unbracket class_def.G.cbody
        |> List.iter (function
             | G.F { G.s = G.DefStmt (entity, G.FuncDef fdef); _ } -> (
                 match entity.G.name with
                 | G.EN (G.Id ((func_name, _), _))
                   when is_constructor_name func_name class_name_str ->
                     record_constructor_param_field_mappings class_name_str
                       class_attrs fdef
                 | _ -> ())
             | _ -> ())
  in
  let record_constructor_argument_mappings class_name args =
    match string_of_name class_name with
    | None -> ()
    | Some class_name_str ->
        !constructor_param_field_mappings
        |> List.iter (fun mapping ->
               if mapping.class_name = class_name_str then
                 match constructor_arg_expr args mapping.param_index with
                 | Some arg_expr -> (
                     match class_name_from_expr arg_expr with
                     | Some arg_class ->
                         object_mappings :=
                           (mapping.field_name, arg_class) :: !object_mappings
                     | None -> ())
                 | None -> ())
  in
  let class_name_from_lambda_return fdef =
    let class_name = ref None in
    let visitor =
      object
        inherit [_] G.iter as super

        method! visit_stmt () stmt =
          (match (!class_name, stmt.G.s) with
          | None, G.Return (_, Some return_expr, _) ->
              class_name := class_name_from_expr return_expr
          | _ -> ());
          super#visit_stmt () stmt
      end
    in
    visitor#visit_stmt () (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody);
    !class_name
  in
  let record_object_property_mappings obj_name init_expr =
    match init_expr.G.e with
    | G.Call (callee_expr, _) ->
        let returned_property_func_names =
          returned_object_property_function_names_from_callee callee_expr
        in
        !function_return_object_property_mappings
        |> List.iter
             (fun (mapped_func_name, mapped_path, class_name) ->
               if
                 List.exists
                   (fun func_name ->
                     same_resolved_name func_name mapped_func_name)
                   returned_property_func_names
               then
                 object_property_mappings :=
                   (obj_name, mapped_path, class_name)
                   :: !object_property_mappings)
    | G.Record (_, fields, _) ->
        let copy_spread_properties source_name =
          !object_property_mappings
          |> List.iter (fun (mapped_obj, mapped_path, class_name) ->
                 if same_resolved_name source_name mapped_obj then
                   object_property_mappings :=
                     (obj_name, mapped_path, class_name)
                     :: !object_property_mappings)
        in
        let rec record_fields field_path fields =
          fields
          |> List.iter (function
             | G.F
                 {
                   G.s =
                     G.DefStmt
                       ( field_entity,
                         G.FieldDefColon { G.vinit = Some field_init; _ } );
                   _;
                 } -> (
                 match field_name_from_entity field_entity with
                 | Some field_name -> (
                     let field_path = field_path @ [ field_name ] in
                     match class_name_from_expr field_init with
                     | Some class_name ->
                         object_property_mappings :=
                           (obj_name, field_path, class_name)
                           :: !object_property_mappings
                     | None -> ();
                     (match function_alias_from_expr field_init with
                     | Some func_name ->
                         object_property_function_mappings :=
                           (obj_name, field_path, func_name)
                           :: !object_property_function_mappings
                     | None -> ());
                     (match
                        class_name_from_direct_object_property_factory_expr
                          field_init
                      with
                     | Some class_name ->
                         object_property_factory_return_mappings :=
                           (obj_name, field_path, class_name)
                           :: !object_property_factory_return_mappings
                     | None -> ());
                     (match field_init.G.e with
                     | G.Lambda fdef -> (
                         match class_name_from_lambda_return fdef with
                         | Some class_name ->
                             object_property_factory_return_mappings :=
                               (obj_name, field_path, class_name)
                               :: !object_property_factory_return_mappings
                         | None -> ())
                     | _ -> ());
                     match field_init.G.e with
                     | G.Record (_, nested_fields, _) ->
                         record_fields field_path nested_fields
                     | _ -> ())
                 | _ -> ())
             | G.F
                 {
                   G.s =
                     G.ExprStmt
                       ( {
                           G.e =
                             G.Call
                               ( { G.e = G.IdSpecial (G.Spread, _); _ },
                                 ( _,
                                   [ G.Arg { G.e = G.N source_name; _ } ],
                                   _ ) );
                           _;
                         },
                         _ );
                   _;
                 } ->
                 copy_spread_properties source_name
             | _ -> ())
        in
        record_fields [] fields
    | _ -> ()
  in
  let record_object_property_class_mapping obj_name field_path class_name =
    let record_for_alias add_mapping obj_name field_path value =
      match object_property_path_from_alias obj_name with
      | Some (root_obj_name, root_field_path) ->
          add_mapping root_obj_name (root_field_path @ field_path) value
      | None -> ()
    in
    let add_object_mapping obj_name field_path class_name =
      object_property_mappings :=
        (obj_name, field_path, class_name) :: !object_property_mappings
    in
    add_object_mapping obj_name field_path class_name;
    record_for_alias add_object_mapping obj_name field_path class_name;
    !injected_field_mappings
    |> List.iter (fun (field_name, injected_key) ->
           if same_resolved_path [ injected_key ] field_path then
             object_mappings :=
               (field_name, class_name) :: !object_mappings)
  in
  let class_name_from_class_reference expr =
    match expr.G.e with
    | G.N name when is_known_class name class_names -> Some name
    | _ -> (
        match name_from_deferred_ref_expr expr with
        | Some name when is_known_class name class_names -> Some name
        | _ -> None)
  in
  let provider_key_names_from_expr key_expr =
    let add_unique key_name key_names =
      if List.exists (same_name key_name) key_names then key_names
      else key_name :: key_names
    in
    let key_names =
      match name_from_property_key_expr key_expr with
      | Some key_name -> [ key_name ]
      | None -> []
    in
    let key_names =
      match key_expr.G.e with
      | G.N class_token when is_known_class class_token class_names ->
          add_unique class_token key_names
      | _ -> key_names
    in
    match name_from_deferred_ref_expr key_expr with
    | Some class_token when is_known_class class_token class_names ->
        add_unique class_token key_names
    | _ -> key_names
  in
  let class_name_from_provider_expr provider_method_name provider_expr =
    match provider_method_name with
    | "to" | "useClass" | "asClass" -> class_name_from_class_reference provider_expr
    | "toConstantValue" | "useValue" | "asValue" ->
        class_name_from_expr provider_expr
    | "toDynamicValue" | "useFactory" | "asFunction" -> (
        match provider_expr.G.e with
        | G.Lambda fdef -> class_name_from_lambda_return fdef
        | _ -> class_name_from_function_return provider_expr)
    | "useExisting" | "toService" | "aliasTo" -> (
        match
          provider_key_names_from_expr provider_expr
          |> List.find_map class_name_from_injected_provider_key
        with
        | Some _ as class_name -> class_name
        | None -> class_name_from_class_reference provider_expr)
    | _ -> None
  in
  let class_name_from_provider_spec_expr expr =
    let expr = unwrap_provider_lifecycle_expr expr in
    match expr.G.e with
    | G.Call ({ e = G.N provider_name; _ }, (_, [ G.Arg provider_expr ], _)) -> (
        match method_name_string provider_name with
        | Some provider_method_name
          when is_object_property_provider_method_name provider_name ->
            class_name_from_provider_expr provider_method_name provider_expr
        | _ -> None)
    | _ -> None
  in
  let record_object_property_value_mapping obj_name field_path value_expr =
    let record_for_alias add_mapping obj_name field_path value =
      match object_property_path_from_alias obj_name with
      | Some (root_obj_name, root_field_path) ->
          add_mapping root_obj_name (root_field_path @ field_path) value
      | None -> ()
    in
    let add_function_mapping obj_name field_path func_name =
      object_property_function_mappings :=
        (obj_name, field_path, func_name) :: !object_property_function_mappings
    in
    let add_factory_return_mapping obj_name field_path class_name =
      object_property_factory_return_mappings :=
        (obj_name, field_path, class_name)
        :: !object_property_factory_return_mappings
    in
    let class_name_from_factory_expr expr =
      match class_name_from_direct_object_property_factory_expr expr with
      | Some _ as class_name -> class_name
      | None -> (
          match expr.G.e with
          | G.Lambda fdef -> class_name_from_lambda_return fdef
          | _ -> None)
    in
    (match
       match class_name_from_provider_spec_expr value_expr with
       | Some _ as class_name -> class_name
       | None -> class_name_from_expr value_expr
     with
    | Some class_name ->
        record_object_property_class_mapping obj_name field_path class_name
    | None -> ());
    (match function_alias_from_expr value_expr with
    | Some func_name ->
        add_function_mapping obj_name field_path func_name;
        record_for_alias add_function_mapping obj_name field_path func_name
    | None -> ());
    match class_name_from_factory_expr value_expr with
    | Some class_name ->
        add_factory_return_mapping obj_name field_path class_name;
        record_for_alias add_factory_return_mapping obj_name field_path class_name
    | None -> ()
  in
  let record_object_property_provider_mapping expr =
    let record_to_self_mapping expr =
      let expr = unwrap_provider_lifecycle_expr expr in
      match expr.G.e with
      | G.Call
          ( { e = G.DotAccess (bind_expr, _, G.FN self_method_name); _ },
            (_, [], _) )
        when method_name_matches [ "toSelf" ] self_method_name -> (
          match bind_expr.G.e with
          | G.Call
              ( { e = G.DotAccess (obj_expr, _, G.FN bind_method_name); _ },
                (_, [ G.Arg key_expr ], _) ) -> (
              match
                ( is_object_property_set_method_name bind_method_name,
                  name_from_property_key_expr key_expr,
                  class_name_from_class_reference key_expr )
              with
              | true, Some field_name, Some class_name -> (
                  match object_property_path_from_base obj_expr field_name with
                  | Some (obj_name, field_path) ->
                      record_object_property_class_mapping obj_name field_path
                        class_name
                  | None -> ())
              | _ -> ())
          | _ -> ())
      | _ -> ()
    in
    (match object_property_provider_call_from_expr expr with
    | Some (obj_name, field_path, provider_method_name, provider_expr) -> (
        match class_name_from_provider_expr provider_method_name provider_expr with
        | Some class_name ->
            record_object_property_class_mapping obj_name field_path class_name
        | None -> ())
    | None -> ());
    record_to_self_mapping expr
  in
  let field_init_named names fields =
    fields
    |> List.find_map (function
         | G.F
             {
               G.s =
                 G.DefStmt
                   ( field_entity,
                     G.FieldDefColon { G.vinit = Some field_init; _ } );
               _;
             } -> (
             match field_name_from_entity field_entity with
             | Some field_name when method_name_matches names field_name ->
                 Some field_init
             | _ -> None)
         | _ -> None)
  in
  let provider_field_entry fields =
    fields
    |> List.find_map (function
         | G.F
             {
               G.s =
                 G.DefStmt
                   ( field_entity,
                     G.FieldDefColon { G.vinit = Some provider_expr; _ } );
               _;
             } -> (
             match field_name_from_entity field_entity with
             | Some field_name -> (
                 match method_name_string field_name with
                 | Some provider_method_name
                   when is_object_property_provider_method_name field_name ->
                     Some (provider_method_name, provider_expr)
                 | _ -> None)
             | None -> None)
         | _ -> None)
  in
  let class_name_from_provider_dep_expr dep_expr =
    match
      provider_key_names_from_expr dep_expr
      |> List.find_map class_name_from_injected_provider_key
    with
    | Some _ as class_name -> class_name
    | None -> (
        match class_name_from_class_reference dep_expr with
        | Some class_name -> Some (class_name_from_provider_key_or_self class_name)
        | None -> None)
  in
  let param_names_from_function fdef =
    Tok.unbracket fdef.G.fparams
    |> List.filter_map (function
         | G.Param { G.pname = Some ((param_name, _)); pinfo; _ } ->
             Some (G.Id ((param_name, Tok.unsafe_fake_tok param_name), pinfo))
         | _ -> None)
  in
  let class_name_from_provider_factory_deps provider_expr deps_expr =
    let returned_param_index =
      match provider_expr.G.e with
      | G.Lambda fdef -> (
          match name_from_lambda_returned_name fdef with
          | Some returned_name ->
              param_names_from_function fdef
              |> List.mapi (fun index param_name -> (index, param_name))
              |> List.find_opt (fun (_index, param_name) ->
                     same_resolved_name returned_name param_name)
              |> Option.map fst
          | None -> None)
      | G.N factory_name ->
          !function_return_param_mappings
          |> List.find_opt (fun (mapped_factory_name, _index) ->
                 same_resolved_name factory_name mapped_factory_name)
          |> Option.map snd
      | _ -> None
    in
    match (returned_param_index, deps_expr.G.e) with
    | ( Some index,
        G.Container (G.Array, (_, dependency_exprs, _)) ) -> (
        match List.nth_opt dependency_exprs index with
        | Some dependency_expr -> class_name_from_provider_dep_expr dependency_expr
        | None -> None)
    | _ -> None
  in
  let class_name_from_provider_object_fields fields provider_method_name
      provider_expr =
    match class_name_from_provider_expr provider_method_name provider_expr with
    | Some _ as class_name -> class_name
    | None -> (
        match
          ( provider_method_name,
            field_init_named [ "deps"; "inject" ] fields )
        with
        | ("toDynamicValue" | "useFactory" | "asFunction"), Some deps_expr ->
            class_name_from_provider_factory_deps provider_expr deps_expr
        | _ -> None)
  in
  let class_mapping_from_provider_object_fields fields =
    match
      ( field_init_named [ "provide"; "token"; "name" ] fields,
        provider_field_entry fields )
    with
    | Some key_expr, Some (provider_method_name, provider_expr) -> (
        match
          ( provider_key_names_from_expr key_expr,
            class_name_from_provider_object_fields fields provider_method_name
              provider_expr )
        with
        | field_name :: _, Some class_name -> Some (field_name, class_name)
        | _ -> None)
    | _ -> None
  in
  let class_mappings_from_provider_object_fields fields =
    match
      ( field_init_named [ "provide"; "token"; "name" ] fields,
        provider_field_entry fields )
    with
    | Some key_expr, Some (provider_method_name, provider_expr) -> (
        match
          class_name_from_provider_object_fields fields provider_method_name
            provider_expr
        with
        | Some class_name ->
            let key_names = provider_key_names_from_expr key_expr in
            key_names |> List.map (fun key_name -> (key_name, class_name))
        | None -> [])
    | _ -> []
  in
  let record_provider_object obj_expr fields =
    class_mappings_from_provider_object_fields fields
    |> List.iter (fun (field_name, class_name) ->
           match object_property_path_from_base obj_expr field_name with
           | Some (obj_name, field_path) ->
               record_object_property_class_mapping obj_name field_path
                 class_name
           | None -> ())
  in
  let record_provider_metadata_object fields =
    class_mappings_from_provider_object_fields fields
    |> List.iter (fun (field_name, class_name) ->
           object_property_mappings :=
             ( name_from_static_string_value "__opengrep_provider_metadata",
               [ field_name ],
               class_name )
             :: !object_property_mappings)
  in
  let is_provider_array_wrapper_name =
    method_name_matches [ "makeEnvironmentProviders" ]
  in
  let rec provider_array_exprs_from_expr expr =
    match expr.G.e with
    | G.Container (G.Array, (_, provider_exprs, _)) -> Some provider_exprs
    | G.N provider_name -> provider_array_exprs_from_name provider_name
    | G.Call ({ e = G.N wrapper_name; _ }, (_, [ G.Arg providers_expr ], _))
      when is_provider_array_wrapper_name wrapper_name ->
        provider_array_exprs_from_expr providers_expr
    | G.Call
        ( { e = G.DotAccess (_, _, G.FN wrapper_name); _ },
          (_, [ G.Arg providers_expr ], _) )
      when is_provider_array_wrapper_name wrapper_name ->
        provider_array_exprs_from_expr providers_expr
    | _ -> None
  and provider_array_exprs_from_name provider_name =
    !provider_array_mappings
    |> List.find_opt (fun (mapped_name, _provider_exprs) ->
           same_resolved_name provider_name mapped_name
           || same_name provider_name mapped_name)
    |> Option.map snd
  in
  let class_name_from_provider_shorthand_expr expr =
    match expr.G.e with
    | G.N class_name when is_class_like_name class_name -> Some class_name
    | _ -> (
        match name_from_deferred_ref_expr expr with
        | Some class_name when is_known_class class_name class_names ->
            Some class_name
        | _ -> None)
  in
  let rec provider_array_exprs_have_provider_entry provider_exprs =
    provider_exprs
    |> List.exists (function
         | { G.e = G.Record (_, fields, _); _ } ->
             Option.is_some (class_mapping_from_provider_object_fields fields)
         | provider_expr
           when Option.is_some
                  (class_name_from_provider_shorthand_expr provider_expr) ->
             true
         | { G.e = G.Container (G.Array, (_, nested_provider_exprs, _)); _ } ->
             provider_array_exprs_have_provider_entry nested_provider_exprs
         | provider_expr -> (
             match provider_array_exprs_from_expr provider_expr with
             | Some provider_exprs ->
                 provider_array_exprs_have_provider_entry provider_exprs
             | None -> false))
  in
  let record_provider_array_mapping provider_name expr =
    match provider_array_exprs_from_expr expr with
    | Some provider_exprs
      when provider_array_exprs_have_provider_entry provider_exprs ->
        provider_array_mappings :=
          (provider_name, provider_exprs) :: !provider_array_mappings
    | _ -> ()
  in
  let rec record_provider_metadata_mapping expr =
    let record_provider_shorthand_class_mapping class_name =
      object_property_mappings :=
        ( name_from_static_string_value "__opengrep_provider_metadata",
          [ class_name ],
          class_name )
        :: !object_property_mappings
    in
    let rec record_provider_array_exprs ?(allow_shorthand = true) provider_exprs =
      let record_provider_array_exprs_once provider_exprs =
        provider_exprs
        |> List.iter (function
             | { G.e = G.Record (_, fields, _); _ } ->
                 record_provider_metadata_object fields
             | { G.e = G.Container (G.Array, (_, nested_provider_exprs, _)); _ }
               ->
                 record_provider_array_exprs ~allow_shorthand
                   nested_provider_exprs
             | {
                 G.e =
                   G.Call
                     ( { G.e = G.IdSpecial (G.Spread, _); _ },
                       (_, [ G.Arg provider_expr ], _) );
                 _;
               } -> (
                 match provider_array_exprs_from_expr provider_expr with
                 | Some provider_exprs ->
                     record_provider_array_exprs ~allow_shorthand provider_exprs
                 | None -> ())
             | provider_expr -> (
                 match provider_array_exprs_from_expr provider_expr with
                 | Some provider_exprs ->
                     record_provider_array_exprs ~allow_shorthand provider_exprs
                 | None -> (
                     match
                       class_name_from_provider_shorthand_expr provider_expr
                     with
                     | Some class_name when allow_shorthand ->
                         record_provider_shorthand_class_mapping class_name
                     | Some _ -> ()
                     | None -> ())))
      in
      provider_exprs
      |> List.iter (fun _ -> record_provider_array_exprs_once provider_exprs)
    in
    match expr.G.e with
    | G.Container (G.Array, (_, provider_exprs, _)) ->
        record_provider_array_exprs ~allow_shorthand:false provider_exprs
    | G.Record (_, fields, _) ->
        fields
        |> List.iter (function
             | G.F
                 {
                   G.s =
                     G.DefStmt
                       ( field_entity,
                         G.FieldDefColon { G.vinit = Some providers_expr; _ }
                       );
                   _;
                 } -> (
                 match
                   ( field_name_from_entity field_entity,
                     provider_array_exprs_from_expr providers_expr )
                 with
                 | ( Some field_name,
                     Some provider_exprs )
                   when method_name_matches [ "providers" ] field_name ->
                     record_provider_array_exprs provider_exprs
                 | _ -> ())
             | _ -> ())
    | G.Call (_callee_expr, (_, args, _)) ->
        args
        |> List.iter (function
             | G.Arg arg_expr -> record_provider_metadata_mapping arg_expr
             | _ -> ())
    | _ -> ()
  in
  let record_provider_metadata_attrs attrs =
    attrs
    |> List.iter (function
         | G.NamedAttr (_, _attr_name, attr_args) ->
             Tok.unbracket attr_args
             |> List.iter (function
                  | G.Arg arg_expr -> record_provider_metadata_mapping arg_expr
                  | _ -> ())
         | _ -> ())
  in
  let record_object_property_registration_map_mapping expr =
    let record_registration_field obj_expr field_entity provider_spec_expr =
      match
        ( field_name_from_entity field_entity,
          class_name_from_provider_spec_expr provider_spec_expr )
      with
      | Some field_name, Some class_name -> (
          match object_property_path_from_base obj_expr field_name with
          | Some (obj_name, field_path) ->
              record_object_property_class_mapping obj_name field_path class_name
          | None -> ())
      | _ -> ()
    in
    let record_registration_tuple obj_expr provider_tuple_exprs =
      match provider_tuple_exprs with
      | [ key_expr; provider_spec_expr ] -> (
          match
            ( name_from_property_key_expr key_expr,
              class_name_from_provider_spec_expr provider_spec_expr )
          with
          | Some field_name, Some class_name -> (
              match object_property_path_from_base obj_expr field_name with
              | Some (obj_name, field_path) ->
                  record_object_property_class_mapping obj_name field_path
                    class_name
              | None -> ())
          | _ -> ())
      | _ -> ()
    in
    match expr.G.e with
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg { e = G.Record (_, fields, _); _ } ], _) )
      when method_name_matches [ "register" ] method_name ->
        fields
        |> List.iter (function
             | G.F
                 {
                   G.s =
                     G.DefStmt
                       ( field_entity,
                         G.FieldDefColon { G.vinit = Some provider_spec_expr; _ } );
                   _;
                 } ->
                 record_registration_field obj_expr field_entity provider_spec_expr
             | _ -> ())
    | G.Call
        ( { e = G.DotAccess (obj_expr, _, G.FN method_name); _ },
          (_, [ G.Arg { e = G.Container (G.Array, (_, provider_exprs, _)); _ } ], _)
        )
      when method_name_matches [ "register" ] method_name ->
        provider_exprs
        |> List.iter (function
             | { G.e = G.Record (_, fields, _); _ } ->
                 record_provider_object obj_expr fields
             | { G.e = G.Container (G.Array, (_, provider_tuple_exprs, _)); _ }
               ->
                 record_registration_tuple obj_expr provider_tuple_exprs
             | _ -> ())
    | _ -> ()
  in
  let record_injected_property_mapping entity vtype =
    match (entity.G.name, injected_key_from_attrs entity.G.attrs) with
    | G.EN field_name, Some injected_key ->
        record_injected_field_mapping field_name injected_key
    | G.EN field_name, None when has_dependency_metadata_attribute entity.G.attrs
      -> (
        match vtype with
        | Some field_type -> (
            match class_name_from_type field_type with
            | Some class_name ->
                record_injected_metadata_class_mapping field_name
                  (class_name_from_provider_key_or_self class_name)
            | None -> ())
        | None -> ())
    | _ -> ()
  in
  let record_object_property_assignment_mapping lval_expr rval_expr =
    match object_property_path_from_expr lval_expr with
    | Some (obj_name, field_path) ->
        record_object_property_value_mapping obj_name field_path rval_expr
    | None -> ()
  in
  let record_object_property_method_mapping expr =
    match object_property_set_call_from_expr expr with
    | Some (obj_name, field_path, value_expr) ->
        record_object_property_value_mapping obj_name field_path value_expr
    | None -> ()
  in
  let record_object_property_set_chain_mapping obj_name init_expr =
    object_property_set_chain_entries_from_expr init_expr
    |> List.iter (fun (field_name, value_expr) ->
           record_object_property_value_mapping obj_name [ field_name ]
             value_expr)
  in
  let record_object_property_alias_mapping alias_name init_expr =
    match object_property_path_from_expr init_expr with
    | Some (obj_name, field_path) ->
        object_property_alias_mappings :=
          (alias_name, obj_name, field_path) :: !object_property_alias_mappings
    | None -> ()
  in
  let record_object_container_alias_mapping alias_name init_expr =
    match object_name_from_child_container_expr init_expr with
    | Some obj_name ->
        object_property_alias_mappings :=
          (alias_name, obj_name, []) :: !object_property_alias_mappings
    | None -> ()
  in
  let record_destructured_object_property_mappings init_expr =
    let local_name_from_field_init field_name = function
      | Some { G.e = G.N local_name; _ } -> Some local_name
      | Some _ -> None
      | None -> Some field_name
    in
    let top_level_field_in excluded_fields = function
      | field_name :: _ ->
          List.exists (same_resolved_name field_name) excluded_fields
      | [] -> false
    in
    let object_property_entries_from_expr source_expr =
      match source_expr.G.e with
      | G.N obj_name ->
          !object_property_mappings
          |> List.filter_map
               (fun (mapped_obj, mapped_path, class_name) ->
                 if same_resolved_name obj_name mapped_obj then
                   Some (mapped_path, class_name)
                 else None)
      | G.Call (callee_expr, _) ->
          let returned_property_func_names =
            returned_object_property_function_names_from_callee callee_expr
          in
          !function_return_object_property_mappings
          |> List.filter_map
               (fun (mapped_func_name, mapped_path, class_name) ->
                 if
                   List.exists
                     (fun func_name ->
                       same_resolved_name func_name mapped_func_name)
                     returned_property_func_names
                 then Some (mapped_path, class_name)
                 else None)
      | _ -> []
    in
    let class_name_from_source_object_property source_expr field_name =
      object_property_entries_from_expr source_expr
      |> List.find_opt (fun (mapped_path, _class_name) ->
             same_resolved_path [ field_name ] mapped_path)
      |> Option.map snd
    in
    let copy_rest_properties source_expr rest_name excluded_fields =
      object_property_entries_from_expr source_expr
      |> List.iter (fun (mapped_path, class_name) ->
             if not (top_level_field_in excluded_fields mapped_path) then
               object_property_mappings :=
                 (rest_name, mapped_path, class_name)
                 :: !object_property_mappings)
    in
    match init_expr.G.e with
    | G.Assign ({ e = G.Record (_, fields, _); _ }, _, source_expr)
      ->
        let destructured_fields =
          fields
          |> List.filter_map (function
               | G.F
                   {
                     G.s =
                       G.DefStmt
                         ( field_entity,
                           G.FieldDefColon { G.vinit = _; _ } );
                     _;
                   } -> (
                   field_name_from_entity field_entity)
               | _ -> None)
        in
        fields
        |> List.iter (function
             | G.F
                 {
                   G.s =
                     G.DefStmt
                       ( field_entity,
                         G.FieldDefColon { G.vinit = field_init; _ } );
                   _;
                 } -> (
                 match field_name_from_entity field_entity with
                 | Some field_name -> (
                     match
                       ( local_name_from_field_init field_name field_init,
                         class_name_from_source_object_property source_expr
                           field_name
                       )
                     with
                     | Some local_name, Some class_name ->
                         object_mappings :=
                           (local_name, class_name) :: !object_mappings
                     | _ -> ())
                 | _ -> ())
             | G.F
                 {
                   G.s =
                     G.ExprStmt
                       ( {
                           G.e =
                             G.Call
                               ( { G.e = G.IdSpecial (G.Spread, _); _ },
                                 ( _,
                                   [ G.Arg { G.e = G.N rest_name; _ } ],
                                   _ ) );
                           _;
                         },
                         _ );
                   _;
                 } ->
                 copy_rest_properties source_expr rest_name destructured_fields
             | _ -> ())
    | _ -> ()
  in
  let record_function_return_mapping func_name fdef =
    let local_object_mappings = ref [] in
    let local_object_property_mappings = ref [] in
    let parameter_names =
      Tok.unbracket fdef.G.fparams
      |> List.filter_map (function
           | G.Param { G.pname = Some ((param_name, _)); pinfo; _ } ->
               Some (G.Id ((param_name, Tok.unsafe_fake_tok param_name), pinfo))
           | _ -> None)
    in
    let returned_param_index returned_name =
      parameter_names
      |> List.mapi (fun index param_name -> (index, param_name))
      |> List.find_opt (fun (_index, param_name) ->
             same_resolved_name returned_name param_name)
      |> Option.map fst
    in
    let record_returned_param_mapping returned_name =
      match returned_param_index returned_name with
      | Some index ->
          function_return_param_mappings :=
            (func_name, index) :: !function_return_param_mappings;
          true
      | None -> false
    in
    let rec class_name_from_return_expr expr =
      match expr.G.e with
      | G.Await (_, awaited_expr) -> class_name_from_return_expr awaited_expr
      | _ -> (
          match extract_class_name_from_constructor expr lang class_names with
          | Some _ as class_name -> class_name
          | None -> (
              match name_from_mapping local_object_mappings expr with
              | Some _ as class_name -> class_name
              | None -> (
                  match class_name_from_function_return expr with
                  | Some _ as class_name -> class_name
                  | None -> class_name_from_return_conditional_expr expr)))
    and class_name_from_return_conditional_expr expr =
      match expr.G.e with
      | G.Conditional (_condition, then_expr, else_expr) -> (
          match
            ( class_name_from_return_expr then_expr,
              class_name_from_return_expr else_expr )
          with
          | Some then_class, Some else_class when same_name then_class else_class
            ->
              Some then_class
          | _ -> None)
      | _ -> None
    in
    let record_local_object_mapping var_name init_expr =
      match class_name_from_return_expr init_expr with
      | Some class_name ->
          local_object_mappings := (var_name, class_name) :: !local_object_mappings
      | None -> ()
    in
    let rec record_returned_record_fields field_path fields =
      fields
      |> List.iter (function
           | G.F
               {
                 G.s =
                   G.DefStmt
                     ( field_entity,
                       G.FieldDefColon { G.vinit = Some field_init; _ } );
                 _;
               } -> (
               match field_name_from_entity field_entity with
               | Some field_name ->
                   let field_path = field_path @ [ field_name ] in
                   (match class_name_from_return_expr field_init with
                   | Some class_name ->
                       function_return_object_property_mappings :=
                         (func_name, field_path, class_name)
                         :: !function_return_object_property_mappings
                   | None -> ());
                   (match field_init.G.e with
                   | G.Record (_, nested_fields, _) ->
                       record_returned_record_fields field_path nested_fields
                   | _ -> ())
               | _ -> ())
           | _ -> ())
    in
    let object_property_entries_from_return_call callee_expr =
      let returned_property_func_names =
        returned_object_property_function_names_from_callee callee_expr
      in
      !function_return_object_property_mappings
      |> List.filter_map
           (fun (mapped_func_name, mapped_path, class_name) ->
             if
               List.exists
                 (fun func_name -> same_resolved_name func_name mapped_func_name)
                 returned_property_func_names
             then Some (mapped_path, class_name)
             else None)
    in
    let object_property_entries_from_local local_name =
      !local_object_property_mappings
      |> List.filter_map
           (fun (mapped_local_name, mapped_path, class_name) ->
             if same_resolved_name local_name mapped_local_name then
               Some (mapped_path, class_name)
             else None)
    in
    let record_returned_object_properties return_expr =
      match return_expr.G.e with
      | G.Record (_, fields, _) -> record_returned_record_fields [] fields
      | G.N local_name ->
          object_property_entries_from_local local_name
          |> List.iter (fun (mapped_path, class_name) ->
                 function_return_object_property_mappings :=
                   (func_name, mapped_path, class_name)
                   :: !function_return_object_property_mappings)
      | G.Call (callee_expr, _) ->
          object_property_set_chain_entries_from_expr return_expr
          |> List.iter (fun (field_name, value_expr) ->
                 match class_name_from_return_expr value_expr with
                 | Some class_name ->
                     function_return_object_property_mappings :=
                       (func_name, [ field_name ], class_name)
                       :: !function_return_object_property_mappings
                 | None -> ());
          object_property_entries_from_return_call callee_expr
          |> List.iter (fun (mapped_path, class_name) ->
                 function_return_object_property_mappings :=
                   (func_name, mapped_path, class_name)
                   :: !function_return_object_property_mappings)
      | _ -> ()
    in
    let record_local_object_property_mappings var_name init_expr =
      match init_expr.G.e with
      | G.Record (_, fields, _) ->
          let rec record_fields field_path fields =
            fields
            |> List.iter (function
                 | G.F
                     {
                       G.s =
                         G.DefStmt
                           ( field_entity,
                             G.FieldDefColon { G.vinit = Some field_init; _ } );
                       _;
                     } -> (
                     match field_name_from_entity field_entity with
                     | Some field_name ->
                         let field_path = field_path @ [ field_name ] in
                         (match class_name_from_return_expr field_init with
                         | Some class_name ->
                             local_object_property_mappings :=
                               (var_name, field_path, class_name)
                               :: !local_object_property_mappings
                         | None -> ());
                         (match field_init.G.e with
                         | G.Record (_, nested_fields, _) ->
                             record_fields field_path nested_fields
                         | _ -> ())
                     | _ -> ())
                 | _ -> ())
          in
          record_fields [] fields
      | G.Call (callee_expr, _) ->
          object_property_entries_from_return_call callee_expr
          |> List.iter (fun (mapped_path, class_name) ->
                 local_object_property_mappings :=
                   (var_name, mapped_path, class_name)
                   :: !local_object_property_mappings);
          object_property_set_chain_entries_from_expr init_expr
          |> List.iter (fun (field_name, value_expr) ->
                 match class_name_from_return_expr value_expr with
                 | Some class_name ->
                     local_object_property_mappings :=
                       (var_name, [ field_name ], class_name)
                       :: !local_object_property_mappings
                 | None -> ())
      | G.N local_name ->
          object_property_entries_from_local local_name
          |> List.iter (fun (mapped_path, class_name) ->
                 local_object_property_mappings :=
                   (var_name, mapped_path, class_name)
                   :: !local_object_property_mappings)
      | _ -> ()
    in
    let record_local_object_property_method_mapping expr =
      match object_property_set_call_from_expr expr with
      | Some (obj_name, field_path, value_expr) -> (
          match class_name_from_return_expr value_expr with
          | Some class_name ->
              local_object_property_mappings :=
                (obj_name, field_path, class_name)
                :: !local_object_property_mappings
          | None -> ())
      | None -> ()
    in
    let visitor =
      object
        inherit [_] G.iter as super

        method! visit_stmt () stmt =
          (match stmt.G.s with
          | G.DefStmt (entity, G.VarDef { G.vinit = Some init_expr; _ }) -> (
              match entity.G.name with
              | G.EN var_name ->
                  record_local_object_mapping var_name init_expr;
                  record_local_object_property_mappings var_name init_expr
              | _ -> ())
          | G.ExprStmt (expr, _) -> (
              record_local_object_property_method_mapping expr;
              match expr.G.e with
              | G.Assign ({ G.e = G.N var_name; _ }, _, rval_expr) ->
                  record_local_object_mapping var_name rval_expr;
                  record_local_object_property_mappings var_name rval_expr
              | _ -> ())
          | G.Return (_, Some return_expr, _) -> (
              record_returned_object_properties return_expr;
              match class_name_from_return_expr return_expr with
              | Some class_name ->
                  function_return_mappings :=
                    (func_name, class_name) :: !function_return_mappings
              | None -> (
                  match return_expr.G.e with
                  | G.N returned_name ->
                      if not (record_returned_param_mapping returned_name) then
                        function_alias_mappings :=
                          (func_name, returned_name) :: !function_alias_mappings
                  | _ -> ()))
          | _ -> ());
          super#visit_stmt () stmt
      end
    in
    (match fdef.G.fbody with
    | G.FBExpr { G.e = G.N returned_name; _ } ->
        ignore (record_returned_param_mapping returned_name)
    | _ -> ());
    visitor#visit_function_definition () fdef
  in
  let function_return_visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.FuncDef fdef -> (
            match entity.G.name with
            | G.EN func_name -> record_function_return_mapping func_name fdef
            | _ -> ())
        | entity, G.VarDef { G.vinit = Some { G.e = G.Lambda fdef; _ }; _ }
          -> (
            match entity.G.name with
            | G.EN func_name -> record_function_return_mapping func_name fdef
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in
  let record_string_constant_mapping var_name init_expr =
    match name_from_static_string_expr init_expr with
    | Some field_name ->
        string_constant_mappings :=
          (var_name, field_name) :: !string_constant_mappings
    | None -> ()
  in
  let string_constant_visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.VarDef { G.vinit = Some init_expr; _ } -> (
            match entity.G.name with
            | G.EN var_name -> record_string_constant_mapping var_name init_expr
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in
  let provider_array_visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.VarDef { G.vinit = Some init_expr; _ } -> (
            match entity.G.name with
            | G.EN provider_name ->
                record_provider_array_mapping provider_name init_expr
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in
  let provider_metadata_visitor =
    object
      inherit [_] G.iter as super

      method! visit_expr () expr =
        record_provider_metadata_mapping expr;
        super#visit_expr () expr

      method! visit_definition () def =
        (match def with
        | entity, G.ClassDef _ -> record_provider_metadata_attrs entity.G.attrs
        | _ -> ());
        super#visit_definition () def
    end
  in

  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_expr () expr =
        record_provider_metadata_mapping expr;
        (match expr.G.e with
        | G.New (_, class_type, _, args) -> (
            match class_type.G.t with
            | G.TyN class_name
            | G.TyExpr { G.e = G.N class_name; _ } ->
                record_constructor_argument_mappings class_name
                  (Tok.unbracket args)
            | _ -> ())
        | _ -> ());
        super#visit_expr () expr

      method! visit_stmt () stmt =
        (match stmt.G.s with
        | G.DefStmt (entity, def_kind) -> (
            match def_kind with
            | G.VarDef var_def -> (
                match (entity.G.name, var_def.G.vinit) with
                | G.EN var_name, Some init_expr -> (
                    record_injected_property_mapping entity var_def.G.vtype;
                    record_object_property_mappings var_name init_expr;
                    record_object_property_set_chain_mapping var_name init_expr;
                    record_object_property_provider_mapping init_expr;
                    record_object_property_registration_map_mapping init_expr;
                    record_object_property_alias_mapping var_name init_expr;
                    record_object_container_alias_mapping var_name init_expr;
                    record_destructured_object_property_mappings init_expr;
                    record_provider_array_mapping var_name init_expr;
                    record_function_alias_mapping var_name init_expr;
                    let class_name = class_name_from_expr init_expr in
                    let class_name =
                      match (class_name, lang) with
                      | Some cls, _ -> Some cls
                      | None, Lang.Cpp -> (
                          match var_def.G.vtype with
                          | Some var_type -> (
                              match var_type.G.t with
                              | G.TyN name when is_known_class name class_names
                                ->
                                  Some name
                              | _ -> None)
                          | None -> None)
                      | None, _ -> None
                    in
                    match class_name with
                    | Some cls ->
                        object_mappings := (var_name, cls) :: !object_mappings
                    | _ -> ())
                | G.EN var_name, None when lang = Lang.Cpp -> (
                    match var_def.G.vtype with
                    | Some var_type -> (
                        match var_type.G.t with
                        | G.TyN name when is_known_class name class_names ->
                            object_mappings :=
                              (var_name, name) :: !object_mappings
                        | G.TyFun (_, return_type) -> (
                            match return_type.G.t with
                            | G.TyN name when is_known_class name class_names ->
                                object_mappings :=
                                  (var_name, name) :: !object_mappings
                            | _ -> ())
                        | _ -> ())
                    | None -> ())
                | _ -> ())
            | _ -> ())
        | G.ExprStmt (expr, _) -> (
            record_object_property_method_mapping expr;
            record_object_property_provider_mapping expr;
            record_object_property_registration_map_mapping expr;
            match expr.G.e with
            | G.Assign (lval_expr, _, rval_expr) -> (
                record_object_property_assignment_mapping lval_expr rval_expr;
                (match lval_expr.G.e with
                | G.N alias_name ->
                    record_object_property_mappings alias_name rval_expr;
                    record_object_property_alias_mapping alias_name rval_expr;
                    record_object_container_alias_mapping alias_name rval_expr;
                    record_function_alias_mapping alias_name rval_expr
                | _ -> ());
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  | G.DotAccess
                      ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
                        _,
                        G.FN (G.Id _ as name) ) ->
                      Some name
                  | G.DotAccess (obj_expr, _, G.FN _) when lang = Lang.Go -> (
                      match obj_expr.G.e with
                      | G.N obj_name -> (
                          let existing_mapping =
                            List.find_opt
                              (fun (var, _) ->
                                match (var, obj_name) with
                                | G.Id ((str1, _), _), G.Id ((str2, _), _) ->
                                    str1 = str2
                                | _ -> false)
                              !object_mappings
                          in
                          match existing_mapping with
                          | Some (_, _) -> None
                          | None -> Some obj_name)
                      | _ -> None)
                  | _ -> None
                in
                let class_name =
                  class_name_from_expr rval_expr
                in
                match (var_name, class_name) with
                | Some var, Some cls ->
                    object_mappings := (var, cls) :: !object_mappings
                | _ -> ())
            | G.AssignOp (lval_expr, _, rval_expr) -> (
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  | _ -> None
                in
                let class_name =
                  extract_class_name_from_constructor rval_expr lang class_names
                in
                match (var_name, class_name) with
                | Some var, Some cls ->
                    object_mappings := (var, cls) :: !object_mappings
                | _ -> ())
            | _ -> ())
        | _ -> ());
        super#visit_stmt () stmt

      method! visit_definition () def =
        (match def with
        | entity, G.ClassDef class_def -> (
            match entity.G.name with
            | G.EN class_name ->
                record_provider_metadata_attrs entity.G.attrs;
                record_class_constructor_param_field_mappings class_name
                  entity.G.attrs class_def
            | _ -> ())
        | entity, G.VarDef var_def -> (
            match (entity.G.name, var_def.G.vinit) with
            | G.EN var_name, Some init_expr -> (
                record_object_property_mappings var_name init_expr;
                record_object_property_set_chain_mapping var_name init_expr;
                record_object_property_alias_mapping var_name init_expr;
                record_object_container_alias_mapping var_name init_expr;
                record_destructured_object_property_mappings init_expr;
                record_provider_array_mapping var_name init_expr;
                record_function_alias_mapping var_name init_expr;
                let class_name = class_name_from_expr init_expr in
                match class_name with
                | Some cls ->
                    object_mappings := (var_name, cls) :: !object_mappings
                | _ -> ())
            | G.EN _, None -> record_injected_property_mapping entity var_def.G.vtype
            | _ -> ())
        | _, G.FuncDef fdef ->
            Tok.unbracket fdef.G.fparams
            |> List.iter (function
                 | G.Param
                     {
                       G.pname = Some ((param_name, param_tok));
                       ptype = Some param_type;
                       pattrs;
                       pinfo;
                       _;
                     }
                   when List.exists is_parameter_property_attr pattrs -> (
                     match class_name_from_type param_type with
                     | Some cls ->
                         let field_name =
                           G.Id ((param_name, param_tok), pinfo)
                         in
                         object_mappings := (field_name, cls) :: !object_mappings
                     | None -> ())
                 | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in

  string_constant_visitor#visit_program () ast;
  function_return_visitor#visit_program () ast;
  provider_array_visitor#visit_program () ast;
  provider_metadata_visitor#visit_program () ast;
  visitor#visit_program () ast;
  !object_mappings

(*****************************************************************************)
(* Constructor Detection Utilities *)
(*****************************************************************************)

(* Check if a function is a constructor for the given language *)
let is_constructor (lang : Lang.t) (func_name : string)
    (class_name_opt : string option) : bool =
  let config = Lang_config.get lang in
  List.mem func_name config.constructor_names
  ||
  match class_name_opt with
  | Some class_name ->
      func_name = class_name
  | None -> false

(* Get all constructor method names for a language *)
let get_constructor_names (lang : Lang.t) : string list =
  (Lang_config.get lang).constructor_names

(* Check if language uses 'new' keyword *)
let uses_new_keyword (lang : Lang.t) : bool =
  (Lang_config.get lang).uses_new_keyword

(*****************************************************************************)
(* Unified Constructor Execution *)
(*****************************************************************************)

let execute_unified_constructor constructor_exp args args_taints
    check_function_call_fn env =
  match check_function_call_fn env constructor_exp args args_taints with
  | Some (call_taints, shape, updated_lval_env) ->
      Some (call_taints, shape, updated_lval_env)
  | None -> None

let execute_constructor_call lang constructor_name class_name args =
  if is_constructor lang constructor_name class_name then
    Some (constructor_name, class_name, args)
  else None

(*****************************************************************************)
(* C++ Constructor Statement Detection *)
(*****************************************************************************)

let detect_cpp_constructor_defstmt stmt class_names =
  match stmt.G.s with
  | G.DefStmt (ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) -> (
      match (ent.name, ty.G.t) with
      | G.EN (G.Id ((var_name, _), _)), G.TyFun (params, return_type) -> (
          match return_type.G.t with
          | G.TyN (G.Id ((class_name, _), _) as name)
            when is_known_class name class_names ->
              Some (var_name, class_name, params)
          | _ -> None)
      | _ -> None)
  | _ -> None

(*****************************************************************************)
(* Debugging and Display *)
(*****************************************************************************)

let show_object_mapping (var_name, class_name) =
  let var_str =
    match var_name with
    | G.Id ((str, _), _) -> str
    | _ -> "???"
  in
  let class_str =
    match class_name with
    | G.Id ((str, _), _) -> str
    | _ -> "???"
  in
  Printf.sprintf "%s -> %s" var_str class_str

let show_object_mappings (mappings : object_mapping list) =
  mappings
  |> List.map show_object_mapping
  |> String.concat "; " |> Printf.sprintf "[%s]"
