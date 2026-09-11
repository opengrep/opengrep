module G = AST_generic
module FA = Graph_from_AST

let build_by_package
    ~(cfg : Index_lang_rules.t)
    (all_funcs : FA.func_info list)
  : (string, FA.func_info list) Hashtbl.t =
  (* Keyed by directory basename: bounded by the function count. *)
  let h : (string, FA.func_info list) Hashtbl.t =
    Hashtbl.create (List.length all_funcs) in
  if cfg.Index_lang_rules.unqualified_scope <> `Per_directory then h
  else begin
    List.iter (fun (func : FA.func_info) ->
      if Option.is_some (Func_info.as_free func.FA.fn_id) then
        match Func_info.def_file_opt func with
        | Some file ->
          (* Key is directory basename, deliberately non-unique; [identify_callee]
             narrows by same-file/dir. Widen to full package path if that goes. *)
          (* [Fpath.basename] is "" only for a file directly under the
             filesystem root; lookups use source package names, so that
             bucket is unreachable and needs no sentinel. *)
          let pkg = Fpath.parent file |> Fpath.basename in
          let cur =
            Option.value (Hashtbl.find_opt h pkg) ~default:[]
          in
          Hashtbl.replace h pkg (func :: cur)
        | None -> ()
    ) all_funcs;
    h
  end

let build_by_module
    ~(cfg : Index_lang_rules.t)
    ~(file_infos : Types.file_info list)
    (all_funcs : FA.func_info list)
  : (Names.Module_qn.t, FA.func_info list) Hashtbl.t =
  (* One module per file. *)
  let h : (Names.Module_qn.t, FA.func_info list) Hashtbl.t =
    Hashtbl.create (List.length file_infos)
  in
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_file | `Per_directory -> begin
    let file_to_module : (string, Names.Module_qn.t) Hashtbl.t =
      Hashtbl.create (List.length file_infos)
    in
    List.iter (fun (fi : Types.file_info) ->
      Hashtbl.replace file_to_module
        (Fpath.to_string fi.fi_file) fi.fi_module_path
    ) file_infos;
    List.iter (fun (func : FA.func_info) ->
      if Option.is_some (Func_info.as_free func.FA.fn_id) then
        match Option.map Fpath.to_string (Func_info.def_file_opt func) with
        | Some file ->
          (match Hashtbl.find_opt file_to_module file with
           | None -> ()
           | Some mp ->
             let cur =
               Option.value (Hashtbl.find_opt h mp) ~default:[]
             in
             Hashtbl.replace h mp (func :: cur))
        | None -> ()
    ) all_funcs;
    h
  end
  | _ -> h

let build_attributes_by_module
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(definitions_by_qn : Types.definition Common.SMap.t)
    ~(file_infos : Types.file_info list)
  : Func_lookup.module_attributes =
  let module_qns : unit Common.SMap.t =
    List.fold_left
      (fun (qns : unit Common.SMap.t) (fi : Types.file_info) ->
        Common.SMap.add
          (Names.Module_qn.to_string fi.Types.fi_module_path) () qns)
      Common.SMap.empty file_infos
  in
  let attribute_of (definition : Types.definition)
    : Func_lookup.module_attribute =
    match definition with
    | Types.Class_definition { class_qn; _ } -> Func_lookup.Attr_class class_qn
    | Types.Function_definitions (funcs : FA.func_info list) ->
      Func_lookup.Attr_functions funcs
  in
  let add (module_key : string) (name : string)
      (attribute : Func_lookup.module_attribute)
      (attributes : Func_lookup.module_attributes)
    : Func_lookup.module_attributes =
    Common.SMap.update module_key
      (function
        | None -> Some (Common.SMap.singleton name attribute)
        | Some (bound : Func_lookup.module_attribute Common.SMap.t) ->
          Some (Common.SMap.add name attribute bound))
      attributes
  in
  let per_file (attributes : Func_lookup.module_attributes)
      (on_binding :
         string -> Func_lookup.module_attributes -> Types.import ->
         Func_lookup.module_attributes)
    : Func_lookup.module_attributes =
    List.fold_left
      (fun (attributes : Func_lookup.module_attributes)
           (fi : Types.file_info) ->
        let module_key = Names.Module_qn.to_string fi.Types.fi_module_path in
        List.fold_left (on_binding module_key) attributes fi.Types.fi_imports)
      attributes file_infos
  in
  let imported =
    per_file
      (Common.SMap.map (fun () -> Common.SMap.empty) module_qns)
      (fun (module_key : string)
           (attributes : Func_lookup.module_attributes)
           (imp : Types.import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from _ -> attributes
        | Imports.Named_binding { local; target } ->
          let target_key = Names.Module_qn.to_string target in
          if Common.SMap.mem target_key module_qns then
            add module_key local (Func_lookup.Attr_module target) attributes
          else
            match Common.SMap.find_opt target_key definitions_by_qn with
            | None -> attributes
            | Some (definition : Types.definition) ->
              add module_key local (attribute_of definition) attributes)
  in
  let with_own_definitions =
    Common.SMap.fold
      (fun (qn : string) (definition : Types.definition)
           (attributes : Func_lookup.module_attributes) ->
        match Names.Def_qn.split_last (Names.Def_qn.of_string qn) with
        | Some ((parent : Names.Def_qn.t), (name : string))
          when Common.SMap.mem (Names.Def_qn.to_string parent) module_qns ->
          add (Names.Def_qn.to_string parent) name
            (attribute_of definition) attributes
        | Some _
        | None -> attributes)
      definitions_by_qn imported
  in
  let star_targets : (string * Names.Module_qn.t) list =
    List.concat_map
      (fun (fi : Types.file_info) ->
        List.filter_map
          (fun (imp : Types.import) ->
            match Imports.binding_of imp with
            | Imports.Named_binding _ -> None
            | Imports.Wildcard_from (target : Names.Module_qn.t) ->
              Some (Names.Module_qn.to_string fi.Types.fi_module_path, target))
          fi.Types.fi_imports)
      file_infos
  in
  let one_star_pass (attributes : Func_lookup.module_attributes)
    : Func_lookup.module_attributes * int =
    List.fold_left
      (fun ((attributes : Func_lookup.module_attributes), (added : int))
           ((module_key : string), (target : Names.Module_qn.t)) ->
        Common.SMap.fold
          (fun (name : string) (attribute : Func_lookup.module_attribute)
               ((attributes : Func_lookup.module_attributes), (added : int)) ->
            if not (Reexports.star_exported ~dunder_all target name) then
              (attributes, added)
            else
              let bound = Common.SMap.find_opt module_key attributes in
              if
                Option.fold ~none:false
                  ~some:(fun (names : Func_lookup.module_attribute Common.SMap.t) ->
                    Common.SMap.mem name names)
                  bound
              then (attributes, added)
              else (add module_key name attribute attributes, added + 1))
          (Func_lookup.attributes_of_module attributes target)
          (attributes, added))
      (attributes, 0) star_targets
  in
  let rec fixpoint (attributes : Func_lookup.module_attributes)
    : Func_lookup.module_attributes =
    match one_star_pass attributes with
    | attributes, 0 -> attributes
    | attributes, _ -> fixpoint attributes
  in
  fixpoint with_own_definitions
