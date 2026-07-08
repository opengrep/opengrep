module G = AST_generic
module FA = Graph_from_AST

let emit_dispatch_edges
    ~(cfg : Index_lang_rules.t)
    ~(type_state : Type_state.t)
    ~(func_def_file : FA.func_info -> string option)
    ~(class_infos : Types.class_info list)
    ~(graph : Call_graph.G.t) : int =
  (* Go binds receivers in any file of the package, so collect across [ci]'s whole directory. Memoized per (ci_qn, file). *)
  let methods_in_file_cache : (string * string, FA.func_info list) Hashtbl.t =
    Hashtbl.create 1024
  in
  let methods_in_file (ci : Types.class_info) : FA.func_info list =
    let key =
      (Names.Class_qn.to_string ci.ci_qn, Fpath.to_string ci.ci_file)
    in
    match Hashtbl.find_opt methods_in_file_cache key with
    | Some r -> r
    | None ->
      let leaf = Names.Class_qn.leaf ci.ci_qn in
      let cands =
        Option.value
          (Type_state.get_methods type_state
             (Names.Class_name.of_string leaf))
          ~default:[]
      in
      let ci_dir_str = Fpath.parent ci.ci_file |> Fpath.to_string in
      let r = List.filter (fun f ->
        match Func_info.as_method f.FA.fn_id with
        | Some (c, _) when String.equal (fst c.IL.ident) leaf ->
          (match func_def_file f with
           | Some df ->
             String.equal
               (Fpath.parent (Fpath.v df) |> Fpath.to_string) ci_dir_str
           | None -> false)
        | _ -> false
      ) cands
      in
      Hashtbl.add methods_in_file_cache key r;
      r
  in
  let method_name (f : FA.func_info) : string option =
    Option.map (fun (_, m) -> fst m.IL.ident)
      (Func_info.as_method f.FA.fn_id)
  in
  let def_id_info (f : FA.func_info) : G.id_info option =
    match f.FA.entity with
    | Some { G.name = G.EN (G.Id (_, ii)); _ } -> Some ii
    | _ -> None
  in
  (* Mutates the shared AST: records [impl] on [i_m]'s [id_resolved_alternatives], deduped by sid. *)
  let record_impl_alternative (i_m : FA.func_info) (impl : FA.func_info) : unit =
    match def_id_info i_m, FA.resolved_name_of_fn_id impl.FA.fn_id with
    | Some ii, Some ((_, sid) as rn) ->
      let alts = ii.G.id_resolved_alternatives in
      if not (List.exists (fun (_, s) -> G.SId.equal s sid) !alts) then
        alts := rn :: !alts
    | _ -> ()
  in
  let method_arity (f : FA.func_info) : int =
    let _, params, _ = f.FA.fdef.G.fparams in
    let raw = List.length params in
    (* Subtract Go receivers ([ParamReceiver], present on impls but not interface decls) so arities match. *)
    let has_receiver =
      match params with
      | G.ParamReceiver _ :: _ -> true
      | _ -> false
    in
    if has_receiver then raw - 1 else raw
  in
  let method_name_arity (f : FA.func_info) : (string * int) option =
    match method_name f with
    | Some n -> Some (n, method_arity f)
    | None -> None
  in
  let interfaces, concretes =
    List.partition (fun (ci : Types.class_info) ->
      match ci.ci_class_kind with G.Interface -> true | _ -> false)
      class_infos
  in
  let package_key (ci : Types.class_info) : string =
    Fpath.parent ci.ci_file |> Fpath.to_string
  in
  let is_exported_method (f : FA.func_info) : bool =
    match method_name f with
    | Some name when String.length name > 0 ->
      let c = name.[0] in
      c >= 'A' && c <= 'Z'
    | _ -> false
  in
  let concrete_metas
    : (Types.class_info * FA.func_info list
       * (string * int) list * string) list =
    List.filter_map (fun (c_ci : Types.class_info) ->
      let c_methods = methods_in_file c_ci in
      if c_methods = [] then None
      else
        let c_methods_na =
          List.filter_map method_name_arity c_methods
          |> List.sort_uniq compare
        in
        Some (c_ci, c_methods, c_methods_na, package_key c_ci)
    ) concretes
  in
  let by_method_na : ((string * int), _ list) Hashtbl.t =
    Hashtbl.create 1024 in
  List.iter (fun ((_, _, methods_na, _) as cm) ->
    List.iter (fun na ->
      let prev = Option.value (Hashtbl.find_opt by_method_na na) ~default:[] in
      Hashtbl.replace by_method_na na (cm :: prev)
    ) methods_na
  ) concrete_metas;
  let emit_dispatch_edge (i_m : FA.func_info) (c_methods : FA.func_info list)
    : int =
    match method_name i_m with
    | None -> 0
    | Some name ->
      match
        List.find_opt (fun (c_m : FA.func_info) ->
          match method_name c_m with
          | Some n -> String.equal n name
          | None -> false) c_methods
      with
      | None -> 0
      | Some c_m ->
        (match FA.fn_id_to_node c_m.FA.fn_id,
               FA.fn_id_to_node i_m.FA.fn_id with
         | Some src, Some dst ->
           (* Impl method's NAME token as call_site, matching scip-go's Dispatch convention. *)
           let call_tok =
             match c_m.FA.fn_id with
             | [_; Some m_il] -> snd m_il.IL.ident
             | _ -> snd c_m.fdef.G.fkind
           in
           Call_graph.add_edge ~kind:Call_graph.Dispatch
             graph ~src ~dst ~call_tok;
           record_impl_alternative i_m c_m;
           1
         | _ -> 0)
  in
  List.fold_left (fun n (i_ci : Types.class_info) ->
    let i_methods = methods_in_file i_ci in
    if i_methods = [] then n
    else
      let i_methods_na =
        List.filter_map method_name_arity i_methods
        |> List.sort_uniq compare
      in
      if i_methods_na = [] then n
      else
        (* An interface with any unexported method is package-private; all-exported ones accept any package. *)
        let same_package_required =
          cfg.Index_lang_rules.interface_dispatch_uses_export_visibility
          && not (List.for_all is_exported_method i_methods)
        in
        let i_pkg = if same_package_required then package_key i_ci else "" in
        let candidates =
          List.fold_left (fun best na ->
            let cands =
              Option.value (Hashtbl.find_opt by_method_na na) ~default:[]
            in
            match best with
            | None -> Some cands
            | Some prev when List.length cands < List.length prev -> Some cands
            | _ -> best
          ) None i_methods_na
          |> Option.value ~default:[]
        in
        List.fold_left (fun n (_c_ci, c_methods, c_methods_na, c_pkg) ->
          if same_package_required && not (String.equal c_pkg i_pkg) then n
          else if not
            (List.for_all (fun na -> List.mem na c_methods_na) i_methods_na)
          then n
          else
            List.fold_left (fun n i_m ->
              n + emit_dispatch_edge i_m c_methods
            ) n i_methods
        ) n candidates
  ) 0 interfaces
