open Types

type tier =
  | On_demand
  | Single_import
  | Own_scope

let shadowing_order (lang : Lang.t) : tier list =
  match lang with
  | Lang.Java
  | Lang.Scala -> [ On_demand; Own_scope; Single_import ]
  | Lang.Kotlin
  | Lang.Csharp
  | Lang.Vb
  | Lang.Cpp
  | Lang.C -> [ On_demand; Single_import; Own_scope ]
  | _ -> [ On_demand; Single_import; Own_scope ]

let namespaces_nest (lang : Lang.t) : bool =
  match lang with
  | Lang.Csharp
  | Lang.Vb -> true
  | Lang.Java
  | Lang.Kotlin
  | Lang.Scala -> false
  | _ -> false

type tiered = {
  tier : tier;
  binding : Scope_binding.positioned_binding;
}

let at_tier (tier : tier) (bindings : Scope_binding.positioned_binding list)
    : tiered list =
  List.map
    (fun (binding : Scope_binding.positioned_binding) -> { tier; binding })
    bindings

let unambiguous_on_demand
    (bindings : Scope_binding.positioned_binding list)
    : Scope_binding.positioned_binding list =
  let sources =
    List.fold_left
      (fun (sources : Pos.t option list Common.SMap.t)
           (binding : Scope_binding.positioned_binding) ->
        Common.SMap.update binding.Scope_binding.pb_name
          (function
            | None -> Some [ binding.Scope_binding.pb_pos ]
            | Some (earlier : Pos.t option list) ->
              Some (binding.Scope_binding.pb_pos :: earlier))
          sources)
      Common.SMap.empty bindings
  in
  List.filter
    (fun (binding : Scope_binding.positioned_binding) ->
      match Common.SMap.find_opt binding.Scope_binding.pb_name sources with
      | None -> true
      | Some (positions : Pos.t option list) ->
        Int.equal
          (List.length (List.sort_uniq (Option.compare Pos.compare) positions))
          1)
    bindings

let is_extension_binding (binding : Scope_binding.positioned_binding) : bool =
  List.for_all
    (fun (kind : Func_lookup.scope_kind) ->
      match kind with
      | Func_lookup.Scope_extension _ -> true
      | Func_lookup.Scope_function _
      | Func_lookup.Scope_object _
      | Func_lookup.Scope_local_value
      | Func_lookup.Scope_class _ -> false)
    binding.Scope_binding.pb_kinds

let equal_tier (first : tier) (second : tier) : bool =
  match (first, second) with
  | On_demand, On_demand
  | Single_import, Single_import
  | Own_scope, Own_scope -> true
  | (On_demand | Single_import | Own_scope), _ -> false

let keep_strongest (lang : Lang.t) (bindings : tiered list)
    : Scope_binding.positioned_binding list =
  let at_file_scope (entry : tiered) : bool =
    (match entry.binding.Scope_binding.pb_parent_path with
     | [] -> true
     | _ :: _ -> false)
    && not (is_extension_binding entry.binding)
  in
  let strongest =
    List.fold_left
      (fun (strongest : tier Common.SMap.t) (entry : tiered) ->
        if not (at_file_scope entry) then strongest
        else
          let name = entry.binding.Scope_binding.pb_name in
          Common.SMap.add name entry.tier strongest)
      Common.SMap.empty
      (List.filter at_file_scope bindings
       |> List.stable_sort (fun (first : tiered) (second : tiered) ->
              let rank (entry : tiered) : int =
                let rec index (position : int) (order : tier list) : int =
                  match order with
                  | [] -> position
                  | candidate :: rest ->
                    if equal_tier candidate entry.tier then position
                    else index (position + 1) rest
                in
                index 0 (shadowing_order lang)
              in
              Int.compare (rank first) (rank second)))
  in
  List.filter_map
    (fun (entry : tiered) ->
      if not (at_file_scope entry) then Some entry.binding
      else
        match
          Common.SMap.find_opt entry.binding.Scope_binding.pb_name strongest
        with
        | Some (winner : tier) when equal_tier winner entry.tier ->
          Some entry.binding
        | Some _
        | None -> None)
    bindings

let members_along_order
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    (class_qn : Names.Class_qn.t) : (string * Func_info.t list) list =
  let order =
    Option.value
      (Common.SMap.find_opt (Names.Class_qn.to_string class_qn)
         resolution_orders)
      ~default:[]
  in
  let bound_on (ancestor : Names.Class_qn.t)
    : Func_info.t list Common.SMap.t =
    Option.value
      (Func_lookup.Class_qn_map.find_opt ancestor methods_by_class)
      ~default:Common.SMap.empty
  in
  List.fold_left
    (fun (members : Func_info.t list Common.SMap.t)
         (ancestor : Names.Class_qn.t) ->
      Common.SMap.fold
        (fun (name : string) (funcs : Func_info.t list)
             (members : Func_info.t list Common.SMap.t) ->
          if Common.SMap.mem name members then members
          else Common.SMap.add name funcs members)
        (bound_on ancestor) members)
    Common.SMap.empty order
  |> Common.SMap.bindings

let build
    ~(lang : Lang.t)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    ~(extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t)
    ~(nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t)
    ~(global_imports : import list)
    ~(namespace_object_members :
        Scope_binding.positioned_binding list Common.SMap.t)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t
      * (Names.Class_name.t * Fpath.t) list =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let regions : unit Common.SMap.t =
    List.fold_left
      (fun (regions : unit Common.SMap.t) (region : Names.Module_qn.t) ->
        Common.SMap.add (Names.Module_qn.to_string region) () regions)
      Common.SMap.empty fi.fi_module_regions
  in
  let own_class_by_qn : class_info Common.SMap.t =
    List.fold_left
      (fun (by_qn : class_info Common.SMap.t) (ci : class_info) ->
        Common.SMap.add (Names.Class_qn.to_string ci.ci_qn) ci by_qn)
      Common.SMap.empty own_classes
  in
  let type_bindings =
    Scope_binding.own_class_bindings ~class_parent_paths
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
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let alias_bindings =
    Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  in
  let member_bindings =
    List.concat_map
      (fun (ci : class_info) ->
        let parent_path = [ Some (Scope_binding.class_il_name_of ci) ] in
        let pos = Scope_binding.position_of_tok (Function_id.tok ci.ci_id) in
        List.concat_map
          (fun ((name : string), (funcs : Func_info.t list)) ->
            Scope_binding.function_binding_of ~pos ~parent_path name funcs)
          (members_along_order ~resolution_orders ~methods_by_class ci.ci_qn))
      own_classes
  in
  let bindings_of_class_members ~(pos : Pos.t option)
      (class_qn : Names.Class_qn.t)
      : Scope_binding.positioned_binding list =
    List.concat_map
      (fun ((name : string), (funcs : Func_info.t list)) ->
        Scope_binding.function_binding_of ~pos ~parent_path:[] name funcs)
      (members_along_order ~resolution_orders ~methods_by_class class_qn)
  in
  let bindings_of_module ~(pos : Pos.t option) (target : Names.Module_qn.t)
      : Scope_binding.positioned_binding list =
    let attributes =
      Func_lookup.attributes_of_module attributes_by_module target
    in
    let bound = Scope_binding.bindings_of_every_attribute ~pos attributes in
    match
      Common.SMap.find_opt (Names.Module_qn.to_string target)
        namespace_object_members
    with
    | None -> bound
    | Some (lifted : Scope_binding.positioned_binding list) -> bound @ lifted
  in
  let bindings_of_nested_types ~(pos : Pos.t option)
      (class_qn : Names.Class_qn.t)
      : Scope_binding.positioned_binding list =
    Common.SMap.fold
      (fun (name : string) (nested_qn : Names.Class_qn.t)
           (bindings : Scope_binding.positioned_binding list) ->
        Scope_binding.class_binding_of ~pos ~parent_path:[] name nested_qn
        :: bindings)
      (Option.value
         (Common.SMap.find_opt (Names.Class_qn.to_string class_qn)
            nested_types_by_class)
         ~default:Common.SMap.empty)
      []
  in
  let class_target (target : Names.Module_qn.t) : Names.Class_qn.t option =
    match
      Common.SMap.find_opt (Names.Module_qn.to_string target) definitions_by_qn
    with
    | Some (Class_definition { class_qn; _ }) -> Some class_qn
    | Some (Function_definitions _)
    | None -> None
  in
  let on_demand_bindings (imp : import) (target : Names.Module_qn.t)
      : Scope_binding.positioned_binding list =
    match class_target target with
    | Some (class_qn : Names.Class_qn.t) ->
      if imp.im_static then
        bindings_of_class_members
          ~pos:(Scope_binding.position_of_tok imp.im_tok) class_qn
      else
        bindings_of_nested_types
          ~pos:(Scope_binding.position_of_tok imp.im_tok) class_qn
    | None ->
      bindings_of_module ~pos:(Scope_binding.position_of_tok imp.im_tok)
        target
  in
  let imported, on_demand, bound_class_files, hidden =
    List.fold_left
      (fun ((imported : Scope_binding.positioned_binding list),
            (on_demand : Scope_binding.positioned_binding list),
            (bound_class_files : (Names.Class_name.t * Fpath.t) list),
            (hidden : unit Common.SMap.t))
           (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t) ->
          (imported, on_demand_bindings imp target @ on_demand,
           bound_class_files, hidden)
        | Imports.Named_binding { local = "_"; target } ->
          ( imported, on_demand, bound_class_files,
            Common.SMap.add (Names.Module_qn.bare_name target) () hidden )
        | Imports.Named_binding { local; target } -> (
          match
            Common.SMap.find_opt (Names.Module_qn.to_string target)
              definitions_by_qn
          with
          | Some (Function_definitions (funcs : Func_info.t list)) ->
            ( Scope_binding.function_binding_of ~pos:(Scope_binding.position_of_tok imp.im_tok) ~parent_path:[]
                local funcs
              @ imported,
              on_demand, bound_class_files, hidden )
          | Some (Class_definition { class_file; class_qn }) ->
            ( Scope_binding.class_binding_of ~pos:(Scope_binding.position_of_tok imp.im_tok) ~parent_path:[]
                local class_qn
              :: imported,
              on_demand,
              (Names.Class_name.of_string (Names.Module_qn.bare_name target),
               class_file)
              :: bound_class_files,
              hidden )
          | None -> (imported, on_demand, bound_class_files, hidden)))
      ([], [], [], Common.SMap.empty) fi.fi_imports
  in
  let on_demand =
    List.filter
      (fun (binding : Scope_binding.positioned_binding) ->
        not (Common.SMap.mem binding.Scope_binding.pb_name hidden))
      on_demand
  in
  let global_bindings =
    List.concat_map
      (fun (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t) ->
          on_demand_bindings imp target
        | Imports.Named_binding _ -> [])
      global_imports
  in
  let rec enclosing_namespaces (region : Names.Module_qn.t)
      : Names.Module_qn.t list =
    match Names.Module_qn.split_last region with
    | None -> []
    | Some ((enclosing : Names.Module_qn.t), _) ->
      enclosing :: enclosing_namespaces enclosing
  in
  let own_namespaces : Names.Module_qn.t list =
    (if namespaces_nest lang then
       List.concat_map
         (fun (region : Names.Module_qn.t) ->
           region :: enclosing_namespaces region)
         fi.fi_module_regions
     else fi.fi_module_regions)
    |> List.sort_uniq Names.Module_qn.compare
  in
  let own_region_bindings =
    List.concat_map
      (fun (region : Names.Module_qn.t) -> bindings_of_module ~pos:None region)
      own_namespaces
  in
  let extension_namespaces : Names.Module_qn.t list =
    own_namespaces
    @ List.filter_map
        (fun (imp : import) ->
          match Imports.binding_of imp with
          | Imports.Wildcard_from (target : Names.Module_qn.t)
            when Option.is_none (class_target target) -> Some target
          | Imports.Wildcard_from _
          | Imports.Named_binding _ -> None)
        (fi.fi_imports @ global_imports)
    |> List.sort_uniq Names.Module_qn.compare
  in
  let extension_bindings =
    List.concat_map
      (fun (namespace : Names.Module_qn.t) ->
        Common.SMap.fold
          (fun (name : string) (funcs : Func_info.t list)
               (bindings : Scope_binding.positioned_binding list) ->
            { Scope_binding.pb_pos = None; pb_name = name; pb_parent_path = [];
              pb_kinds =
                List.map
                  (fun (func : Func_info.t) ->
                    Func_lookup.Scope_extension func)
                  funcs }
            :: bindings)
          (Option.value
             (Common.SMap.find_opt (Names.Module_qn.to_string namespace)
                extensions_by_module)
             ~default:Common.SMap.empty)
          [])
      extension_namespaces
  in
  let bindings =
    at_tier On_demand
      (unambiguous_on_demand (global_bindings @ List.rev on_demand)
       @ extension_bindings)
    @ at_tier Own_scope
        (own_region_bindings @ function_bindings @ alias_bindings
       @ type_bindings @ member_bindings)
    @ at_tier Single_import (List.rev imported)
  in
  (Scope_binding.bindings_of_positioned (keep_strongest lang bindings),
   bound_class_files)
