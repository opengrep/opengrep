module G = AST_generic
module FA = Graph_from_AST

(* TODO: [emit_dispatch_edges] below is very large and hard to read; split it
   into named helpers (candidate selection, package-visibility gating, edge
   emission) so it can be reviewed and tested in pieces. *)
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
    | Some cached_methods -> cached_methods
    | None ->
      let leaf = Names.Class_qn.leaf ci.ci_qn in
      let cands =
        Option.value
          (Type_state.get_methods type_state
             (Names.Class_name.of_string leaf))
          ~default:[]
      in
      let ci_dir_str = Fpath.parent ci.ci_file |> Fpath.to_string in
      let filtered_methods = List.filter (fun func ->
        match Func_info.as_method func.FA.fn_id with
        | Some (cls, _) when String.equal (fst cls.IL.ident) leaf ->
          (match func_def_file func with
           | Some df ->
             String.equal
               (Fpath.parent (Fpath.v df) |> Fpath.to_string) ci_dir_str
           | None -> false)
        | _ -> false
      ) cands
      in
      Hashtbl.add methods_in_file_cache key filtered_methods;
      filtered_methods
  in
  let method_name (func : FA.func_info) : string option =
    Option.map (fun (_, meth) -> fst meth.IL.ident)
      (Func_info.as_method func.FA.fn_id)
  in
  let def_id_info (func : FA.func_info) : G.id_info option =
    match func.FA.entity with
    | Some { G.name = G.EN (G.Id (_, ii)); _ } -> Some ii
    | _ -> None
  in
  (* Mutates the shared AST: records [impl] on [i_m]'s [id_resolved_alternatives], deduped by sid. *)
  let record_impl_alternative (i_m : FA.func_info) (impl : FA.func_info) : unit =
    match def_id_info i_m, FA.resolved_name_of_fn_id impl.FA.fn_id with
    | Some ii, Some ((_, sid) as rn) ->
      let alts = ii.G.id_resolved_alternatives in
      if not (List.exists (fun (_, other_sid) -> G.SId.equal other_sid sid) !alts) then
        alts := rn :: !alts
    | _ -> ()
  in
  let method_arity (func : FA.func_info) : int =
    let _, params, _ = func.FA.fdef.G.fparams in
    let raw = List.length params in
    (* Subtract Go receivers ([ParamReceiver], present on impls but not interface decls) so arities match. *)
    let has_receiver =
      match params with
      | G.ParamReceiver _ :: _ -> true
      | _ -> false
    in
    if has_receiver then raw - 1 else raw
  in
  let method_name_arity (func : FA.func_info) : (string * int) option =
    match method_name func with
    | Some name -> Some (name, method_arity func)
    | None -> None
  in
  (* Single return type leaf, or [None] when there is no return or it
     isn't a simple named type (Go multi-returns, func types, etc.).

     We compare RETURN types, not parameters: tree-sitter-go misparses an
     unnamed-param interface decl ([Group(string, func(RouteRegister),
     ...)]) by shifting the [name type] pairing — it reads [string] as
     the parameter NAME and the following token as its type — so a
     genuine implementation's params disagree with its own interface's
     garbled params. A return position is always a bare type (no
     [name type] ambiguity), so its leaf is trustworthy on both the decl
     and the impl. *)
  let rettype_leaf (func : FA.func_info) : string option =
    match func.FA.fdef.G.frettype with
    | Some ty -> Option.bind (Ty_leaf.class_name_of_ty ty) Ty_leaf.leaf_of_name
    | None -> None
  in
  (* Only a definite return-type mismatch (both sides a simple named type,
     different leaves) rejects; either side unknown stays compatible, so
     the match degrades to name+arity where returns are absent/complex —
     never losing an edge the old code emitted. Leaf comparison ignores
     qualification ([pkg.Err] vs imported [Err]). *)
  let types_compatible (a : string option) (b : string option) : bool =
    match a, b with
    | Some x, Some y -> String.equal x y
    | _ -> true
  in
  (* [iface_m] is satisfied by [concrete_m]: same name, same arity, and no
     definite return-type mismatch. *)
  let method_satisfies (iface_m : FA.func_info) (concrete_m : FA.func_info)
    : bool =
    match method_name iface_m, method_name concrete_m with
    | Some i_name, Some c_name when String.equal i_name c_name ->
      Int.equal (method_arity iface_m) (method_arity concrete_m)
      && types_compatible (rettype_leaf iface_m) (rettype_leaf concrete_m)
    | _ -> false
  in
  let interfaces, concretes =
    List.partition (fun (ci : Types.class_info) ->
      match ci.ci_class_kind with G.Interface -> true | _ -> false)
      class_infos
  in
  let package_key (ci : Types.class_info) : string =
    Fpath.parent ci.ci_file |> Fpath.to_string
  in
  let is_exported_method (func : FA.func_info) : bool =
    match method_name func with
    | Some name when String.length name > 0 ->
      let first_ch = name.[0] in
      first_ch >= 'A' && first_ch <= 'Z'
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
    | Some _ ->
      match List.find_opt (method_satisfies i_m) c_methods with
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
          (* Class-membership gate stays name+arity: a genuine multi-method
             interface must keep qualifying even if type extraction is
             imperfect on one method (an all-or-nothing type gate here
             drops the whole interface, losing valid edges). The parameter-
             type precision is applied per-edge in [emit_dispatch_edge], so
             a concrete that only accidentally shares a method name+arity
             (e.g. an unrelated [Validate(ctx, runtime.Object)] against
             [Validator.Validate(ctx, *plugins.Plugin)]) passes this gate
             but emits no edge. *)
          else if not
            (List.for_all (fun na -> List.mem na c_methods_na) i_methods_na)
          then n
          else
            List.fold_left (fun n i_m ->
              n + emit_dispatch_edge i_m c_methods
            ) n i_methods
        ) n candidates
  ) 0 interfaces
