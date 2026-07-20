module G = AST_generic
module FA = Graph_from_AST
module Log = Log_projidx.Log

(* Re-export a free function under a different bound name (Python
   [from .m import f as g]): expose it as [alias] at the target's own
   position/sid, so name-based lookup finds [alias] while the graph node
   and signature stamp (via fn_id_to_node / resolved_name_of_fn_id in
   the tainting layer) resolve to the target's real definition — same
   same-position/different-name convention as Ts_class_aliases.
   [entity = None] keeps the target's original name from also being
   exposed under this module. *)
let expose_free_as ~(alias : string) (func : FA.func_info) : FA.func_info =
  match Func_info.leaf_name func.FA.fn_id with
  | None -> func
  | Some (tname : IL.name) ->
    let alias_ii = G.empty_id_info () in
    alias_ii.G.id_resolved :=
      (match !(tname.IL.id_info.G.id_resolved) with
       | Some _ as resolved -> resolved
       | None -> Some (G.Global, tname.IL.sid));
    let alias_name : IL.name =
      { ident = (alias, snd tname.IL.ident);
        sid = tname.IL.sid;
        id_info = alias_ii }
    in
    { func with FA.fn_id = Func_info.free_id alias_name; FA.entity = None }

(* [bound -> target] for every name an [__init__]-style package file
   re-exports: importing [pkg.local] resolves to the defining module. *)
let build_reexport_map ~(cfg : Index_lang_rules.t)
    (file_infos : Types.file_info list)
  : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t =
  (* Re-exports only come from package files: bounded by the file count. *)
  let reexport_map = Hashtbl.create (List.length file_infos) in
  if not cfg.Index_lang_rules.has_reexports then reexport_map
  else begin
    List.iter (fun (fi : Types.file_info) ->
      if cfg.Index_lang_rules.is_init_file fi.fi_file then
        let pkg = fi.fi_module_path in
        List.iter (fun (local, target) ->
          let bound =
            if Names.Module_qn.is_empty pkg
            then Names.Module_qn.of_string local
            else Names.Module_qn.concat pkg local
          in
          if not (Names.Module_qn.equal bound target) then
            Hashtbl.replace reexport_map bound target
        ) fi.fi_imports
    ) file_infos;
    reexport_map
  end

(* Dedup so wildcard re-export doesn't double-add a func an explicit import brought in. *)
let merge_dedup ~cur ~newcomers =
  let key (func : FA.func_info) =
    match Func_info.as_free func.FA.fn_id with
    | Some name ->
      let file =
        match Func_info.def_file_opt func with
        | Some file -> Fpath.to_string file
        | None -> ""
      in
      Some (fst name.IL.ident, file)
    | None -> None
  in
  let seen : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun func ->
    match key func with Some k -> Hashtbl.replace seen k () | None -> ())
    cur;
  let added = List.filter (fun func ->
    match key func with
    | None -> true
    | Some k ->
      if Hashtbl.mem seen k then false
      else begin Hashtbl.replace seen k (); true end
  ) newcomers in
  (added @ cur, List.length added)

module MQMap = Map.Make (struct
  type t = Names.Module_qn.t
  let compare = Names.Module_qn.compare
end)

let resolve_into_module_index
    ~(project_funcs_by_module
      : (Names.Module_qn.t, FA.func_info list) Hashtbl.t)
    (file_infos : Types.file_info list)
    : (Names.Module_qn.t * FA.func_info list) list =
  (* Additions as a Map overlay so chained re-exports see prior additions;
     the base table is only read. *)
  let lookup overlay qn =
    match MQMap.find_opt qn overlay with
    | Some fs -> fs
    | None ->
      Option.value (Hashtbl.find_opt project_funcs_by_module qn) ~default:[]
  in
  let overlay, n_added, n_wildcard =
    List.fold_left (fun acc (fi : Types.file_info) ->
      List.fold_left (fun ((overlay, n_added, n_wildcard) as acc)
                        (local, target_qn) ->
        if String.equal local "*" then
          (* [*] doesn't import names starting with [_] (Python). *)
          let public = List.filter (fun (func : FA.func_info) ->
            match Func_info.as_free func.FA.fn_id with
            | Some name ->
              let name_str = fst name.IL.ident in
              String.length name_str > 0 && Char.equal name_str.[0] '_' = false
            | None -> false
          ) (lookup overlay target_qn) in
          if public = [] then acc
          else
            let cur = lookup overlay fi.fi_module_path in
            let merged, n = merge_dedup ~cur ~newcomers:public in
            (MQMap.add fi.fi_module_path merged overlay,
             n_added, n_wildcard + n)
        else
        match Names.Module_qn.split_last target_qn with
        | Some (target_mod, target_name)
          when not (Names.Module_qn.is_empty target_mod) ->
          let matches = List.filter (fun (func : FA.func_info) ->
            match Func_info.as_free func.FA.fn_id with
            | Some name -> String.equal (fst name.IL.ident) target_name
            | None -> false
          ) (lookup overlay target_mod) in
          if matches = [] then acc
          else
            (* [from .m import f] injects [f]; [from .m import f as g]
               injects it exposed as [g] at [f]'s identity. *)
            let exposed =
              if String.equal local target_name then matches
              else List.map (expose_free_as ~alias:local) matches
            in
            let cur = lookup overlay fi.fi_module_path in
            (MQMap.add fi.fi_module_path (exposed @ cur) overlay,
             n_added + List.length exposed, n_wildcard)
        | _ -> acc
      ) acc fi.fi_imports
    ) (MQMap.empty, 0, 0) file_infos
  in
  Log.info (fun m ->
    m "Re-exports: %d funcs added to module index (+%d via wildcard)"
      n_added n_wildcard);
  MQMap.bindings overlay
