module G = AST_generic

open Types

let named_function (init : G.expr) : string option =
  match init.G.e with
  | G.N (G.Id (((name : string), _), _)) -> Some name
  | G.Ref (_, { G.e = G.N (G.Id (((name : string), _), _)); _ }) -> Some name
  | _ -> None

let file_scope_values (ast : G.program) : (string * string option) list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt
          ({ G.name = G.EN (G.Id (((name : string), _), _)); _ },
           G.VarDef { G.vinit = (init : G.expr option); _ }) ->
        Some (name, Option.bind init named_function)
      | G.ExprStmt
          ({ G.e =
               G.Assign
                 ({ G.e = G.N (G.Id (((name : string), _), _)); _ }, _,
                  (rhs : G.expr));
             _ }, _) -> Some (name, named_function rhs)
      | _ -> None)
    ast

let function_pointer_aliases (ast : G.program) : (string * string) list =
  let values = file_scope_values ast in
  let bindings_per_name =
    List.fold_left
      (fun (counts : int Common.SMap.t) ((name : string), _) ->
        Common.SMap.update name
          (fun (earlier : int option) ->
            Some (1 + Option.value earlier ~default:0))
          counts)
      Common.SMap.empty values
  in
  List.filter_map
    (fun ((name : string), (target : string option)) ->
      match target with
      | None -> None
      | Some (target : string) ->
        if
          Int.equal
            (Option.value (Common.SMap.find_opt name bindings_per_name)
               ~default:0)
            1
        then Some (name, target)
        else None)
    values

let bind_alias (bound : Func_lookup.scope_entry list Common.SMap.t)
    ((name : string), (target : string))
    : Func_lookup.scope_entry list Common.SMap.t =
  match
    Func_lookup.functions_of_entries
      (Option.value (Common.SMap.find_opt target bound) ~default:[])
  with
  | [] -> bound
  | (_ :: _) as funcs ->
    Common.SMap.add name
      (List.map
         (fun (func : Func_info.t) ->
           { Func_lookup.kind = Func_lookup.Scope_function func;
             parent_path = [] })
         funcs)
      bound

type using_clauses = {
  uc_directives : (Names.Module_qn.t * Tok.t) list;
  uc_declarations : (string * Names.Module_qn.t * Tok.t) list;
}

let qn_of_dots (dots : G.dotted_ident) : Names.Module_qn.t =
  Names.Module_qn.of_parts (List.map fst dots)

let using_clauses_of_file (ast : G.program) : using_clauses =
  let directives, declarations =
    Walker.fold_stmts_in_program ~skip_nested_fdefs:true
      (fun ((directives : (Names.Module_qn.t * Tok.t) list),
            (declarations : (string * Names.Module_qn.t * Tok.t) list))
           (stmt : G.stmt) ->
        match stmt.G.s with
        | G.DirectiveStmt
            { G.d =
                G.ImportAll ((tok : Tok.t),
                             G.DottedName (dots : G.dotted_ident), _);
              _ } -> ((qn_of_dots dots, tok) :: directives, declarations)
        | G.DirectiveStmt
            { G.d =
                G.ImportFrom ((tok : Tok.t),
                              G.DottedName (dots : G.dotted_ident), names);
              _ } ->
          ( directives,
            List.fold_left
              (fun (declarations :
                      (string * Names.Module_qn.t * Tok.t) list)
                   ((((name : string), _), (alias : G.alias option))) ->
                let local =
                  match alias with
                  | Some (((local : string), _), _) -> local
                  | None -> name
                in
                (local, Names.Module_qn.concat (qn_of_dots dots) name, tok)
                :: declarations)
              declarations names )
        | _ -> (directives, declarations))
      ([], []) ast
  in
  { uc_directives = List.rev directives;
    uc_declarations = List.rev declarations }

let is_global_definition ~(definitions_by_qn : definition Common.SMap.t)
    (name : string) (func : Func_info.t) : bool =
  match Common.SMap.find_opt name definitions_by_qn with
  | Some (Function_definitions (funcs : Func_info.t list)) ->
    List.exists
      (fun (candidate : Func_info.t) ->
        Func_info.equal_fn_id candidate.Func_info.fn_id func.Func_info.fn_id)
      funcs
  | Some (Class_definition _)
  | None -> false

let global_bindings_of_includes
    ~(definitions_by_qn : definition Common.SMap.t)
    (bindings : Scope_binding.positioned_binding list)
    : Scope_binding.positioned_binding list =
  List.filter_map
    (fun (binding : Scope_binding.positioned_binding) ->
      match
        List.filter
          (fun (kind : Func_lookup.scope_kind) ->
            match kind with
            | Func_lookup.Scope_function (func : Func_info.t) ->
              is_global_definition ~definitions_by_qn
                binding.Scope_binding.pb_name func
            | Func_lookup.Scope_class _
            | Func_lookup.Scope_companion _
            | Func_lookup.Scope_object _
            | Func_lookup.Scope_local_value
            | Func_lookup.Scope_extension _ -> false)
          binding.Scope_binding.pb_kinds
      with
      | [] -> None
      | (_ :: _) as kinds ->
        Some { binding with Scope_binding.pb_kinds = kinds })
    bindings

let rec enclosing_namespaces (region : Names.Module_qn.t)
    : Names.Module_qn.t list =
  match Names.Module_qn.split_last region with
  | None -> []
  | Some ((enclosing : Names.Module_qn.t), _) ->
    enclosing :: enclosing_namespaces enclosing

let open_namespaces (fi : file_info) : Names.Module_qn.t list =
  List.concat_map
    (fun (region : Names.Module_qn.t) ->
      region :: enclosing_namespaces region)
    fi.fi_module_regions
  |> List.filter (fun (region : Names.Module_qn.t) ->
       not (Names.Module_qn.is_empty region))
  |> List.sort_uniq Names.Module_qn.compare

let build
    ~(tier_rank : Index_lang_rules.tier -> int)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    ~(region_bindings : Scope_binding.region_bindings Common.SMap.t)
    ~(include_bindings : Scope_binding.positioned_binding list)
    ~(included_files : string list)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info) : Func_lookup.scope_entry list Common.SMap.t =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let using = using_clauses_of_file fi.fi_ast in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let included_classes =
    List.concat_map
      (fun (included : string) ->
        Option.value (Common.SMap.find_opt included classes_by_file)
          ~default:[])
      included_files
  in
  let own_class_by_qn = Scope_binding.classes_by_qn own_classes in
  let regions : unit Common.SMap.t =
    List.fold_left
      (fun (regions : unit Common.SMap.t) (region : Names.Module_qn.t) ->
        Common.SMap.add (Names.Module_qn.to_string region) () regions)
      Common.SMap.empty fi.fi_module_regions
  in
  let bindings_of_namespace ~(pos : Pos.t option) (target : Names.Module_qn.t)
      : Scope_binding.positioned_binding list =
    match
      Common.SMap.find_opt (Names.Module_qn.to_string target) region_bindings
    with
    | Some (bound : Scope_binding.region_bindings) when Option.is_none pos ->
      Scope_binding.bindings_in_region bound
    | Some _
    | None ->
      Scope_binding.bindings_of_every_attribute ~pos
        (Func_lookup.attributes_of_module attributes_by_module target)
  in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let alias_bindings =
    Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  in
  let type_bindings =
    Scope_binding.own_class_bindings ~companion:Scope_binding.no_companion
      ~class_parent_paths
      ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
        Common.SMap.mem (Names.Class_qn.to_string owner) regions)
      ~scope_of_owner:(fun (owner : Names.Class_qn.t) ->
        Option.map
          (fun (ci : class_info) ->
            [ Some (Scope_binding.class_il_name_of ci) ])
          (Common.SMap.find_opt (Names.Class_qn.to_string owner)
             own_class_by_qn))
      own_classes
  in
  let member_bindings =
    Scope_binding.class_member_bindings
      ~members_of:
        (Scope_package.members_along_order ~resolution_orders ~methods_by_class)
      own_classes
  in
  let own_namespace_bindings =
    List.concat_map
      (fun (region : Names.Module_qn.t) ->
        bindings_of_namespace ~pos:None region)
      (open_namespaces fi)
  in
  let imported =
    List.concat_map
      (fun (((local : string), (target : Names.Module_qn.t), (tok : Tok.t))) ->
        let pos = Scope_binding.position_of_tok tok in
        match
          Common.SMap.find_opt (Names.Module_qn.to_string target)
            definitions_by_qn
        with
        | Some (Function_definitions (funcs : Func_info.t list)) ->
          Scope_binding.function_binding_of ~pos ~parent_path:[] local funcs
        | Some (Class_definition { class_qn; _ }) ->
          [ Scope_binding.class_binding_of ~pos ~parent_path:[] local class_qn ]
        | None -> [])
      using.uc_declarations
  in
  let included_type_bindings =
    List.filter_map
      (fun (ci : class_info) ->
        Option.map
          (fun (((_ : Names.Class_qn.t), (name : string))) ->
            Scope_binding.class_binding_of ~pos:None ~parent_path:[] name
              ci.ci_qn)
          (Names.Class_qn.split_last ci.ci_qn))
      included_classes
  in
  let on_demand =
    global_bindings_of_includes ~definitions_by_qn include_bindings
    @ included_type_bindings
    @ Scope_package.unambiguous_on_demand
        (List.concat_map
           (fun (((target : Names.Module_qn.t), (tok : Tok.t))) ->
             bindings_of_namespace ~pos:(Scope_binding.position_of_tok tok)
               target)
           using.uc_directives)
  in
  let bindings =
    Scope_package.at_tier Index_lang_rules.On_demand on_demand
    @ Scope_package.at_tier Index_lang_rules.Own_scope
        (own_namespace_bindings @ function_bindings @ alias_bindings
         @ type_bindings @ member_bindings)
    @ Scope_package.at_tier Index_lang_rules.Single_import imported
  in
  List.fold_left bind_alias
    (Scope_binding.bindings_of_positioned
       (Scope_package.keep_strongest ~tier_rank bindings))
    (function_pointer_aliases fi.fi_ast)
