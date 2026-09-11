module G = AST_generic

let module_qn_of_file ~(cfg : Index_lang_rules.t)
    ~(go_modules : Go_modules.t) ~(project_root : Fpath.t)
    ~(ast : G.program option) (file : Fpath.t) : Names.Module_qn.t =
  match Go_modules.import_path_of_dir go_modules (Fpath.parent file) with
  | Some import_path -> Names.Module_qn.of_string import_path
  | None -> (
    match Option.bind ast cfg.Index_lang_rules.module_path_from_ast with
    | Some module_str -> Names.Module_qn.of_string module_str
    | None ->
      let rel = Discover.relative_to ~project_root file in
      let path_str = Fpath.rem_ext rel |> Fpath.normalize |> Fpath.to_string in
      let path_str = cfg.Index_lang_rules.rewrite_module_path path_str in
      Names.Module_qn.of_string
        (String.concat "." (Fpath.segs (Fpath.v path_str))))

let module_name_string ~(cfg : Index_lang_rules.t)
    ~(current_module_path : Names.Module_qn.t)
    ~(is_init_file : bool)
    (mn : G.module_name) : Names.Module_qn.t =
  match mn with
  | G.FileName (spec, _) ->
    Names.Module_qn.of_string (cfg.Index_lang_rules.normalize_import_specifier spec)
  | G.DottedName parts ->
    let prefix_segs, real_parts =
      let rec split acc = function
        | ((part_str, _) as seg) :: rest
          when String.equal part_str "." || String.equal part_str ".." ->
          split (seg :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      split [] parts
    in
    let real_strs = List.map fst real_parts in
    if prefix_segs = [] then Names.Module_qn.of_parts real_strs
    else begin
      (* For a relative import, an [__init__.py] file is itself the package,
         so the last segment is not dropped. Each extra [.] removes one
         further level. *)
      let init_offset = if is_init_file then 0 else 1 in
      let extra_dotdots =
        List.fold_left (fun acc (part_str, _) ->
          if String.equal part_str ".." then acc + 1 else acc
        ) 0 prefix_segs
      in
      let drops = init_offset + extra_dotdots in
      let pkg_parts =
        if Names.Module_qn.is_empty current_module_path then []
        else Names.Module_qn.parts current_module_path
      in
      let n_keep = max 0 (List.length pkg_parts - drops) in
      let kept = List.filteri (fun i _ -> i < n_keep) pkg_parts in
      Names.Module_qn.of_parts (kept @ real_strs)
    end

let enclosing_package ~(cfg : Index_lang_rules.t) ~(file : Fpath.t)
    (module_qn : Names.Module_qn.t) : Names.Module_qn.t =
  if cfg.Index_lang_rules.is_init_file file then module_qn
  else
    match Names.Module_qn.split_last module_qn with
    | Some (parent, _) -> parent
    | None -> Names.Module_qn.empty
