open Types

let self_type_name : string = "Self"

let imported_bindings ~(definitions_by_qn : definition Common.SMap.t)
    ~(attributes_by_module : Func_lookup.module_attributes) (fi : file_info)
    : Scope_binding.positioned_binding list =
  List.fold_left
    (fun (bindings : Scope_binding.positioned_binding list) (imp : import) ->
      let pos = Scope_binding.position_of_tok imp.im_tok in
      match Imports.binding_of imp with
      | Imports.Wildcard_from (target : Names.Module_qn.t) ->
        Scope_binding.bindings_of_attributes ~pos
          ~keep:(fun _ _ -> true)
          (Func_lookup.attributes_of_module attributes_by_module target)
        @ bindings
      | Imports.Named_binding { local; target } -> (
        match
          Common.SMap.find_opt (Names.Module_qn.to_string target)
            definitions_by_qn
        with
        | None -> bindings
        | Some (Function_definitions (funcs : Func_info.t list)) ->
          Scope_binding.function_binding_of ~pos ~parent_path:[] local funcs
          @ bindings
        | Some (Class_definition { class_qn; _ }) ->
          Scope_binding.class_binding_of ~pos ~parent_path:[] local class_qn
          :: bindings))
    [] fi.fi_imports

let module_aliases ~(cfg : Index_lang_rules.t)
    ~(import_aliases : (string, Names.Module_qn.t) Hashtbl.t option)
    (fi : file_info) : (string, Names.Module_qn.t) Hashtbl.t =
  let aliases =
    match import_aliases with
    | Some (aliases : (string, Names.Module_qn.t) Hashtbl.t) ->
      Hashtbl.copy aliases
    | None -> Hashtbl.create (List.length cfg.Index_lang_rules.relative_module_names)
  in
  List.iter
    (fun ((name : string), (relative : Index_lang_rules.relative_module)) ->
      Hashtbl.replace aliases name
        (Module_paths.relative_module_qn ~current:fi.fi_module_path relative))
    cfg.Index_lang_rules.relative_module_names;
  aliases

let build
    ~(cfg : Index_lang_rules.t)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(import_aliases : (string, Names.Module_qn.t) Hashtbl.t option)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t
      * (string, Names.Module_qn.t) Hashtbl.t =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let own_by_qn = Scope_binding.classes_by_qn own_classes in
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
        String.equal (Names.Class_qn.to_string owner)
          (Names.Module_qn.to_string fi.fi_module_path))
      ~scope_of_owner:(fun (owner : Names.Class_qn.t) ->
        Option.map
          (fun (ci : entry) ->
            [ Some (Scope_binding.class_il_name_of ci) ])
          (Common.SMap.find_opt (Names.Class_qn.to_string owner) own_by_qn))
      own_classes
  in
  let self_bindings =
    List.concat_map
      (fun (ci : entry) ->
        Scope_binding.bindings_in_class ci (fun ~pos ~parent_path ->
          [ Scope_binding.class_binding_of ~pos ~parent_path self_type_name
              (Scope_binding.class_qn_of_entry ci) ]))
      own_classes
  in
  let imported =
    imported_bindings ~definitions_by_qn ~attributes_by_module fi
  in
  ( Scope_binding.bindings_of_positioned
      (self_bindings @ List.rev imported @ type_bindings
       @ function_bindings @ alias_bindings),
    module_aliases ~cfg ~import_aliases fi )
