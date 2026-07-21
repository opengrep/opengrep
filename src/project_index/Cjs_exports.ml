module G = AST_generic
module FA = Graph_from_AST

let build_default_export_fn
    ~(lang : Lang.t)
    (file_infos : Types.file_info list)
  : (string, FA.func_info) Hashtbl.t =
  (* At most one default export per file. *)
  let default_exports : (string, FA.func_info) Hashtbl.t =
    Hashtbl.create (List.length file_infos) in
  if not (Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js) then default_exports
  else begin
    List.iter (fun (fi : Types.file_info) ->
      let file_str = Fpath.to_string fi.fi_file in
      (* Same-file top-level free functions by name, for the
         [module.exports = namedFn] re-export idiom. *)
      let free_fn_by_name : (string, FA.func_info) Hashtbl.t =
        Hashtbl.create 8
      in
      List.iter (fun obs ->
        match obs with
        | Walker.Observation.Func_def { opt_ent; parent_path; fdef } ->
          (match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
           | Some fn_id ->
             (match Func_info.as_free fn_id with
              | Some name ->
                Hashtbl.replace free_fn_by_name (fst name.IL.ident)
                  { FA.fn_id; entity = opt_ent; fdef }
              | None -> ())
           | None -> ())
        | _ -> ()
      ) fi.fi_observations;
      (* Only [module.exports] sets the default export. A bare
         [exports = fn] merely rebinds the local [exports] binding and
         does NOT change what [require()] returns, so it must not
         synthesise a default. *)
      let is_module_exports lhs =
        match lhs.G.e with
        | G.DotAccess ({ G.e = G.N (G.Id (("module", _), _)); _ }, _,
                       G.FN (G.Id (("exports", _), _))) -> true
        | _ -> false
      in
      let synth_from_rhs (rhs : G.expr) =
        match rhs.G.e with
        | G.Lambda fdef ->
          let tok = snd fdef.G.fkind in
          let synth_name = IL.{
            ident = ("_module_exports_default", tok);
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          } in
          Hashtbl.replace default_exports file_str
            { FA.fn_id = Func_info.free_id synth_name; entity = None; fdef }
        (* [module.exports = namedFn]: the default export IS the named
           same-file function. *)
        | G.N (G.Id ((name, _), _)) ->
          (match Hashtbl.find_opt free_fn_by_name name with
           | Some func -> Hashtbl.replace default_exports file_str func
           | None -> ())
        | _ -> ()
      in
      (* Module scope only: descend through top-level control flow but
         not into nested function bodies. A [module.exports =] buried in
         a nested function is not the file's default export. Traversal
         order gives last-assignment-wins, matching CJS runtime. *)
      Walker.fold_stmts_in_program ~skip_nested_fdefs:true
        (fun () stmt ->
          match stmt.G.s with
          | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _)
            when is_module_exports lhs ->
            synth_from_rhs rhs
          | _ -> ())
        () fi.fi_ast
    ) file_infos;
    default_exports
  end
