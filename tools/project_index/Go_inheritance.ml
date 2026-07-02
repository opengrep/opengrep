module G = AST_generic
module FA = Graph_from_AST

let lift_embedded_interfaces
    ~(lang : Lang.t)
    (file_infos : Types.file_info list)
    (type_state : Type_state.t) : Type_state.t =
  if not (Lang.equal lang Lang.Go) then type_state
  else
  (* Embedding graph: [iface_name -> embedded iface names]. *)
  let embeds : (string, string list) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (fi : Types.file_info) ->
    List.iter (fun obs ->
      match obs with
      | Walker.Observation.Type_def { ent; tdef } ->
        (match tdef with
         | { G.tbody = G.NewType
               { G.t = G.TyRecordAnon ((G.Interface, _),
                                          (_, fields, _)); _ } } ->
           let iname = Index_lang_rules.entity_simple_name ent in
           (match iname with
            | None -> ()
            | Some iname ->
              let embedded = List.filter_map (fun field ->
                match field with
                | G.F { G.s = G.ExprStmt (
                    { G.e = G.Call (
                        { G.e = G.IdSpecial (G.Spread, _); _ },
                        (_, [G.Arg { G.e = G.N name; _ }], _)); _ }, _); _ } ->
                  Ty_leaf.leaf_of_name name
                | _ -> None
              ) fields in
              if embedded <> [] then
                let cur =
                  Option.value (Hashtbl.find_opt embeds iname) ~default:[]
                in
                Hashtbl.replace embeds iname (List.rev_append embedded cur))
         | _ -> ())
      | _ -> ()
    ) fi.Types.fi_observations
  ) file_infos;
  if Hashtbl.length embeds = 0 then type_state
  else begin
    let lift_into (state : Type_state.t) (embedder : string) : Type_state.t =
      let visited = Hashtbl.create 8 in
      let rec bfs acc = function
        | [] -> acc
        | n :: rest when Hashtbl.mem visited n -> bfs acc rest
        | n :: rest ->
          Hashtbl.add visited n ();
          let acc =
            (* Read the pre-lift [type_state], not the accumulating [state]:
               the bfs itself walks [embeds] transitively, so lifted copies
               are never needed and the result is independent of the order
               the embedders are processed in. *)
            match Type_state.get_methods type_state
                    (Names.Class_name.of_string n) with
            | Some ms -> List.rev_append ms acc
            | None -> acc
          in
          let neighbours =
            Option.value (Hashtbl.find_opt embeds n) ~default:[]
          in
          bfs acc (neighbours @ rest)
      in
      let methods =
        bfs []
          (Option.value (Hashtbl.find_opt embeds embedder) ~default:[])
      in
      List.fold_left (fun s (m : FA.func_info) ->
        match Func_info.as_method m.FA.fn_id with
        | Some (_, m_il) ->
          let embedder_tok = Tok.unsafe_fake_tok embedder in
          let embedder_il = IL.{
            ident = (embedder, embedder_tok);
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          } in
          let rewritten_fn_id =
            Func_info.method_id ~cls:embedder_il ~meth:m_il
          in
          let m' = { m with FA.fn_id = rewritten_fn_id } in
          Type_state.add_method s
            (Names.Class_name.of_string embedder) m'
        | None -> s
      ) state methods
    in
    (* Sorted so the lifted-method list order is deterministic. *)
    Hashtbl.fold (fun embedder _ acc -> embedder :: acc) embeds []
    |> List.sort String.compare
    |> List.fold_left lift_into type_state
  end
