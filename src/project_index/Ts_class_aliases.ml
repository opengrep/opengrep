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
    let find_target_fn (name : string) : FA.func_info option =
      match Hashtbl.find_opt project_funcs_by_name name with
      | None -> None
      | Some fs ->
        List.find_opt (fun (func : FA.func_info) ->
          Option.is_some (Func_info.as_free func.FA.fn_id))
          fs
    in
    let class_aliases_in_fields cls_name fields =
      List.filter_map (fun (G.F stmt) ->
        match stmt.G.s with
        | G.DefStmt (alias_ent,
                     G.VarDef { G.vinit = Some init; G.vtype = None; _ }) ->
          (match Index_lang_rules.entity_simple_name alias_ent, leaf_of_init_expr init with
           | Some alias_name, Some target_name ->
             Option.bind (find_target_fn target_name) (fun target ->
               let target_fn_name =
                 match target.FA.fn_id with
                 | [_; Some method_name] -> Some method_name
                 | _ -> None
               in
               Option.map (fun method_name ->
                 let cls_il = IL.{
                   ident = (cls_name, Tok.unsafe_fake_tok cls_name);
                   sid = G.SId.unsafe_default;
                   id_info = G.empty_id_info ();
                 } in
                 let method_il =
                   IL.{ method_name with ident = (alias_name, snd method_name.IL.ident); }
                 in
                 let synthetic : FA.func_info = {
                   fn_id = Func_info.method_id ~cls:cls_il ~meth:method_il;
                   entity = target.FA.entity;
                   fdef = target.FA.fdef;
                 } in
                 (cls_name, synthetic)
               ) target_fn_name)
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
               class_aliases_in_fields cls_name fields)
          | _ -> []
        ) fi.Types.fi_observations
      ) file_infos
    in
    List.fold_left (fun state (cls, synth) ->
      Type_state.add_method state (Names.Class_name.of_string cls) synth
    ) type_state collected
