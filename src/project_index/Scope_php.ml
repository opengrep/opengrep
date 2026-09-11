open Types

let own_regions (fi : file_info) : Names.Module_qn.t list =
  List.sort_uniq Names.Module_qn.compare fi.fi_module_regions

let region_keys (regions : Names.Module_qn.t list) : unit Common.SMap.t =
  List.fold_left
    (fun (keys : unit Common.SMap.t) (region : Names.Module_qn.t) ->
      Common.SMap.add (Names.Module_qn.to_string region) () keys)
    Common.SMap.empty regions

let bindings_of_attributes ~(keep : Func_lookup.module_attribute -> bool)
    (attributes : Func_lookup.module_attribute Common.SMap.t)
    : Scope_binding.positioned_binding list =
  Common.SMap.fold
    (fun (name : string) (attribute : Func_lookup.module_attribute)
         (bindings : Scope_binding.positioned_binding list) ->
      if not (keep attribute) then bindings
      else
        match attribute with
        | Func_lookup.Attr_functions (funcs : Func_info.t list) ->
          Scope_binding.function_binding_of ~pos:None ~parent_path:[] name funcs
          @ bindings
        | Func_lookup.Attr_class (class_qn : Names.Class_qn.t) ->
          Scope_binding.class_binding_of ~pos:None ~parent_path:[] name class_qn
          :: bindings
        | Func_lookup.Attr_module _ -> bindings)
    attributes []

let is_function_attribute (attribute : Func_lookup.module_attribute) : bool =
  match attribute with
  | Func_lookup.Attr_functions _ -> true
  | Func_lookup.Attr_class _
  | Func_lookup.Attr_module _ -> false

let every_attribute (_ : Func_lookup.module_attribute) : bool = true

type region_bindings = {
  rb_in_region : Scope_binding.positioned_binding list;
  rb_names : unit Common.SMap.t;
}

let build_region_bindings
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(file_infos : file_info list) : region_bindings Common.SMap.t =
  let of_region (region : Names.Module_qn.t) : region_bindings =
    let in_region =
      bindings_of_attributes ~keep:every_attribute
        (Func_lookup.attributes_of_module attributes_by_module region)
    in
    { rb_in_region = in_region;
      rb_names =
        List.fold_left
          (fun (names : unit Common.SMap.t)
               (binding : Scope_binding.positioned_binding) ->
            Common.SMap.add binding.Scope_binding.pb_name () names)
          Common.SMap.empty in_region }
  in
  List.fold_left
    (fun (by_region : region_bindings Common.SMap.t) (fi : file_info) ->
      List.fold_left
        (fun (by_region : region_bindings Common.SMap.t)
             (region : Names.Module_qn.t) ->
          let key = Names.Module_qn.to_string region in
          if Common.SMap.mem key by_region then by_region
          else Common.SMap.add key (of_region region) by_region)
        by_region fi.fi_module_regions)
    Common.SMap.empty file_infos

let global_function_bindings
    ~(attributes_by_module : Func_lookup.module_attributes)
    : Scope_binding.positioned_binding list =
  bindings_of_attributes ~keep:is_function_attribute
    (Func_lookup.attributes_of_module attributes_by_module
       Names.Module_qn.empty)

let import_binds_kind (imp : import)
    (definition : definition) : bool =
  match (imp.im_binds, definition) with
  | Types.Binds_any, Class_definition _
  | Types.Binds_function, Function_definitions _ -> true
  | Types.Binds_any, Function_definitions _
  | Types.Binds_function, Class_definition _
  | Types.Binds_constant, _
  | Types.Binds_type, _
  | Types.Binds_module, _ -> false

let build
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(region_bindings : region_bindings Common.SMap.t)
    ~(global_bindings : Scope_binding.positioned_binding list)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t * Names.Module_qn.t list =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let regions = own_regions fi in
  let keys = region_keys regions in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let type_bindings =
    Scope_binding.own_class_bindings ~class_parent_paths
      ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
        Common.SMap.mem (Names.Class_qn.to_string owner) keys)
      ~scope_of_owner:(fun _ -> None)
      own_classes
  in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let own_regions_bindings : region_bindings list =
    List.filter_map
      (fun (region : Names.Module_qn.t) ->
        Common.SMap.find_opt (Names.Module_qn.to_string region)
          region_bindings)
      regions
  in
  let in_regions =
    List.concat_map
      (fun (bound : region_bindings) -> bound.rb_in_region)
      own_regions_bindings
  in
  let bound_by_regions (name : string) : bool =
    List.exists
      (fun (bound : region_bindings) -> Common.SMap.mem name bound.rb_names)
      own_regions_bindings
  in
  let from_global =
    if Common.SMap.mem (Names.Module_qn.to_string Names.Module_qn.empty) keys
    then []
    else
      List.filter
        (fun (binding : Scope_binding.positioned_binding) ->
          not (bound_by_regions binding.Scope_binding.pb_name))
        global_bindings
  in
  let imported =
    List.concat_map
      (fun (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from _ -> []
        | Imports.Named_binding { local; target } -> (
          match
            Common.SMap.find_opt (Names.Module_qn.to_string target)
              definitions_by_qn
          with
          | None -> []
          | Some (definition : definition) ->
            if not (import_binds_kind imp definition) then []
            else
              let pos = Scope_binding.position_of_tok imp.im_tok in
              match definition with
              | Function_definitions (funcs : Func_info.t list) ->
                Scope_binding.function_binding_of ~pos ~parent_path:[] local
                  funcs
              | Class_definition { class_qn; _ } ->
                [ Scope_binding.class_binding_of ~pos ~parent_path:[] local
                    class_qn ]))
      fi.fi_imports
  in
  ( Scope_binding.bindings_of_positioned
      (from_global @ in_regions @ function_bindings @ type_bindings @ imported),
    regions )
