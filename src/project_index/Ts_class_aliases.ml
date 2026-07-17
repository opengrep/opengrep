module G = AST_generic
module FA = Graph_from_AST

let lang_applies (lang : Lang.t) : bool =
  Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js

let add_class_body_aliases
    ~(lang : Lang.t)
    ~(project_funcs_by_name : (string, FA.func_info list) Hashtbl.t)
    (file_infos : Types.file_info list)
    (type_state : Type_state.t) : Type_state.t =
  if not (lang_applies lang) then type_state
  else
    let leaf_of_init_expr (expr : G.expr) : string option =
      match expr.G.e with
      | G.N (G.Id ((name, _), _)) -> Some name
      | _ -> None
    in
    let free_named (name : string) : FA.func_info list =
      Option.value (Hashtbl.find_opt project_funcs_by_name name) ~default:[]
      |> List.filter (fun (func : FA.func_info) ->
             Option.is_some (Func_info.as_free func.FA.fn_id))
    in
    (* The referenced name resolves like any identifier in the file:
       a same-file def first, else the file's imports (which also map an
       import alias back to the original name). Never a project-wide
       name match — with homonyms that binds an arbitrary, input-order
       dependent function. *)
    let find_target_fn (fi : Types.file_info) (name : string)
      : FA.func_info option =
      let defined_in (file : Fpath.t) (func : FA.func_info) =
        match Func_info.def_file_opt func with
        | Some def_file -> Fpath.equal def_file file
        | None -> false
      in
      match List.find_opt (defined_in fi.Types.fi_file) (free_named name) with
      | Some _ as same_file -> same_file
      | None ->
        (match List.assoc_opt name fi.Types.fi_imports with
         | None -> None
         | Some target_qn ->
           (match Names.Module_qn.split_last target_qn with
            | None -> None
            | Some (target_mod, target_name) ->
              (* TS import entries keep the raw specifier as the module
                 part; resolve it to candidate files the same way the
                 rest of the TS handling does. Bare (package) specifiers
                 need the path-suffix index, which is built later — an
                 alias to one stays unresolved rather than guessed. *)
              let target_files =
                Ts_modules.resolve_specifier
                  ~current_file:fi.Types.fi_file
                  (Names.Module_qn.to_string target_mod)
              in
              free_named target_name
              |> List.find_opt (fun (func : FA.func_info) ->
                     match Func_info.def_file_opt func with
                     | Some def_file ->
                       List.exists
                         (String.equal (Fpath.to_string def_file))
                         target_files
                     | None -> false)))
    in
    let class_aliases_in_fields fi cls_name fields =
      List.filter_map (fun (G.F stmt) ->
        match stmt.G.s with
        | G.DefStmt (alias_ent,
                     G.VarDef { G.vinit = Some init; G.vtype = None; _ }) ->
          (match Index_lang_rules.entity_simple_name alias_ent, leaf_of_init_expr init with
           | Some alias_name, Some target_name ->
             Option.bind (find_target_fn fi target_name) (fun target ->
               Option.map (fun (tname : IL.name) ->
                 let cls_il = IL.{
                   ident = (cls_name, Tok.unsafe_fake_tok cls_name);
                   sid = G.SId.unsafe_default;
                   id_info = G.empty_id_info ();
                 } in
                 (* The exposed [alias_name] at the TARGET's position and
                    sid: [fn_id_to_node] reads the same-position/
                    different-name signature back as "alias" and emits
                    the target's identity, where the def (and later its
                    signature) actually lives. *)
                 let alias_ii = G.empty_id_info () in
                 alias_ii.G.id_resolved :=
                   (match !(tname.IL.id_info.G.id_resolved) with
                    | Some _ as resolved -> resolved
                    | None -> Some (G.Global, tname.IL.sid));
                 let method_il =
                   IL.{ ident = (alias_name, snd tname.IL.ident);
                        sid = tname.IL.sid;
                        id_info = alias_ii }
                 in
                 let synthetic : FA.func_info = {
                   fn_id = Func_info.method_id ~cls:cls_il ~meth:method_il;
                   entity = target.FA.entity;
                   fdef = target.FA.fdef;
                 } in
                 (cls_name, synthetic)
               ) (Func_info.leaf_name target.FA.fn_id))
           | _ -> None)
        | _ -> None
      ) fields
    in
    let collected =
      List.concat_map (fun fi ->
        List.concat_map (fun obs ->
          match obs with
          | Walker.Observation.Class_def { ent; cdef } ->
            (match Index_lang_rules.entity_simple_name ent with
             | None -> []
             | Some cls_name ->
               let _, fields, _ = cdef.G.cbody in
               class_aliases_in_fields fi cls_name fields)
          | _ -> []
        ) fi.Types.fi_observations
      ) file_infos
    in
    List.fold_left (fun state (cls, synth) ->
      Type_state.add_method state (Names.Class_name.of_string cls) synth
    ) type_state collected
