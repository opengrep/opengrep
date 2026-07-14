module G = AST_generic
module FA = Graph_from_AST

let build_by_package
    ~(cfg : Index_lang_rules.t)
    (all_funcs : FA.func_info list)
  : (string, FA.func_info list) Hashtbl.t =
  let h : (string, FA.func_info list) Hashtbl.t = Hashtbl.create 1024 in
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
  let h : (Names.Module_qn.t, FA.func_info list) Hashtbl.t =
    Hashtbl.create 1024
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
