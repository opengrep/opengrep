module Log = Log_projidx.Log
module FA = Graph_from_AST

let collect_def_names
    ~(cfg : Index_lang_rules.t) (fi : Types.file_info) : string list =
  List.fold_left (fun acc obs ->
    match obs with
    | Walker.Observation.Func_def { opt_ent = Some ent; _ } ->
      (match Index_lang_rules.entity_simple_name ent with
       | Some name -> name :: acc
       | None -> acc)
    | Walker.Observation.Class_def { ent; cdef } ->
      let acc =
        match Index_lang_rules.entity_simple_name ent with
        | Some name -> name :: acc
        | None -> acc
      in
      List.fold_left (fun acc (name, _tok) -> name :: acc) acc
        (cfg.Index_lang_rules.class_body_synth_methods cdef)
    | _ -> acc
  ) [] fi.Types.fi_observations

let build_dir_index
    ~(cfg : Index_lang_rules.t)
    (file_infos : Types.file_info list)
  : (string, (string, unit) Hashtbl.t) Hashtbl.t =
  let dir_index : (string, (string, unit) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 1024
  in
  let scope_key_opt (fi : Types.file_info) : string option =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_directory -> Some (Fpath.parent fi.Types.fi_file |> Fpath.to_string)
    | `Per_package -> Some (Names.Module_qn.to_string fi.Types.fi_module_path)
    | `Per_file -> None
  in
  List.iter (fun (fi : Types.file_info) ->
    match scope_key_opt fi with
    | None -> ()
    | Some key ->
      let names_set =
        match Hashtbl.find_opt dir_index key with
        | Some set -> set
        | None ->
          let set = Hashtbl.create 64 in
          Hashtbl.replace dir_index key set; set
      in
      List.iter (fun name -> Hashtbl.replace names_set name ())
        (collect_def_names ~cfg fi)
  ) file_infos;
  (match cfg.Index_lang_rules.unqualified_scope with
   | `Per_directory ->
     Log.info (fun m -> m "Per-directory scope: %d directories indexed"
       (Hashtbl.length dir_index))
   | `Per_package ->
     Log.info (fun m -> m "Per-package scope: %d packages indexed"
       (Hashtbl.length dir_index))
   | `Per_file -> ());
  dir_index

let for_file
    ~(cfg : Index_lang_rules.t)
    ~(dir_visible_names : (string, (string, unit) Hashtbl.t) Hashtbl.t)
    ~(project_funcs_by_module : (Names.Module_qn.t, FA.func_info list) Hashtbl.t)
    (fi : Types.file_info) : (string, unit) Hashtbl.t =
  let visible : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  (* key must match [build_dir_index]. *)
  let scope_key_opt =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_directory -> Some (Fpath.parent fi.Types.fi_file |> Fpath.to_string)
    | `Per_package -> Some (Names.Module_qn.to_string fi.Types.fi_module_path)
    | `Per_file -> None
  in
  (match scope_key_opt with
   | None -> ()
   | Some key ->
     match Hashtbl.find_opt dir_visible_names key with
     | Some pkg_names ->
       Hashtbl.iter (fun name () -> Hashtbl.replace visible name ()) pkg_names
     | None -> ());
  List.iter (fun (local, target) ->
    if String.equal local "*" then
      (* ImportAll: target's free funcs visible by bare name, else callee is
         pruned. Deliberately keeps [_]-prefixed names (unlike [Reexports]):
         visibility only prunes candidates, so over-approximating is safe. *)
      (match Hashtbl.find_opt project_funcs_by_module target with
       | None -> ()
       | Some funcs ->
         List.iter (fun (func : FA.func_info) ->
           match Func_info.as_free func.FA.fn_id with
           | Some name -> Hashtbl.replace visible (fst name.IL.ident) ()
           | None -> ()) funcs)
    else
      Hashtbl.replace visible local ()
  ) fi.Types.fi_imports;
  let add_name ent =
    match Index_lang_rules.entity_simple_name ent with
    | Some name -> Hashtbl.replace visible name ()
    | None -> ()
  in
  List.iter (fun obs ->
    match obs with
    (* [Func_def] also covers lambda-VarDefs ([const X = () => ...]) for callbacks. *)
    | Walker.Observation.Func_def { opt_ent = Some ent; _ } ->
      add_name ent
    | Walker.Observation.Type_def { ent; _ } ->
      add_name ent
    | Walker.Observation.Class_def { ent; cdef } ->
      add_name ent;
      List.iter (fun (name, _tok) -> Hashtbl.replace visible name ())
        (cfg.Index_lang_rules.class_body_synth_methods cdef)
    | _ -> ()
  ) fi.Types.fi_observations;
  visible
