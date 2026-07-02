module Log = Log_projidx.Log
module FA = Graph_from_AST

let collect_def_names
    ~(cfg : Index_lang_rules.t) (fi : Types.file_info) : string list =
  List.fold_left (fun acc obs ->
    match obs with
    | Walker.Observation.Func_def { opt_ent = Some ent; _ } ->
      (match Index_lang_rules.entity_simple_name ent with
       | Some n -> n :: acc
       | None -> acc)
    | Walker.Observation.Class_def { ent; cdef } ->
      let acc =
        match Index_lang_rules.entity_simple_name ent with
        | Some n -> n :: acc
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
  let h : (string, (string, unit) Hashtbl.t) Hashtbl.t =
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
        match Hashtbl.find_opt h key with
        | Some s -> s
        | None ->
          let s = Hashtbl.create 64 in
          Hashtbl.replace h key s; s
      in
      List.iter (fun n -> Hashtbl.replace names_set n ())
        (collect_def_names ~cfg fi)
  ) file_infos;
  (match cfg.Index_lang_rules.unqualified_scope with
   | `Per_directory ->
     Log.info (fun m -> m "Per-directory scope: %d directories indexed"
       (Hashtbl.length h))
   | `Per_package ->
     Log.info (fun m -> m "Per-package scope: %d packages indexed"
       (Hashtbl.length h))
   | `Per_file -> ());
  h

let for_file
    ~(cfg : Index_lang_rules.t)
    ~(dir_visible_names : (string, (string, unit) Hashtbl.t) Hashtbl.t)
    ~(project_funcs_by_module : (Names.Module_qn.t, FA.func_info list) Hashtbl.t)
    (fi : Types.file_info) : (string, unit) Hashtbl.t =
  let h : (string, unit) Hashtbl.t = Hashtbl.create 64 in
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
       Hashtbl.iter (fun n () -> Hashtbl.replace h n ()) pkg_names
     | None -> ());
  List.iter (fun (local, target) ->
    if String.equal local "*" then
      (* ImportAll: target's free funcs visible by bare name, else callee is
         pruned. Deliberately keeps [_]-prefixed names (unlike [Reexports]):
         visibility only prunes candidates, so over-approximating is safe. *)
      (match Hashtbl.find_opt project_funcs_by_module target with
       | None -> ()
       | Some funcs ->
         List.iter (fun (f : FA.func_info) ->
           match Func_info.as_free f.FA.fn_id with
           | Some n -> Hashtbl.replace h (fst n.IL.ident) ()
           | None -> ()) funcs)
    else
      Hashtbl.replace h local ()
  ) fi.Types.fi_imports;
  let add_name ent =
    match Index_lang_rules.entity_simple_name ent with
    | Some n -> Hashtbl.replace h n ()
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
      List.iter (fun (name, _tok) -> Hashtbl.replace h name ())
        (cfg.Index_lang_rules.class_body_synth_methods cdef)
    | _ -> ()
  ) fi.Types.fi_observations;
  h
