module G = AST_generic
module FA = Graph_from_AST

let build_default_export_fn
    ~(lang : Lang.t)
    (file_infos : Types.file_info list)
  : (string, FA.func_info) Hashtbl.t =
  let h : (string, FA.func_info) Hashtbl.t = Hashtbl.create 1024 in
  if not (Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js) then h
  else begin
    List.iter (fun (fi : Types.file_info) ->
      let file_str = Fpath.to_string fi.fi_file in
      let is_module_exports lhs =
        match lhs.G.e with
        | G.DotAccess ({ G.e = G.N (G.Id (("module", _), _)); _ }, _,
                       G.FN (G.Id (("exports", _), _))) -> true
        | G.N (G.Id (("exports", _), _)) -> true
        | _ -> false
      in
      let synth_from_lambda (lam_e : G.expr) =
        match lam_e.G.e with
        | G.Lambda fdef ->
          let tok = snd fdef.G.fkind in
          let synth_name = IL.{
            ident = ("_module_exports_default", tok);
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          } in
          let f : FA.func_info = {
            fn_id = Func_info.free_id synth_name;
            entity = None;
            fdef;
          } in
          Hashtbl.replace h file_str f
        | _ -> ()
      in
      let visitor = object
        inherit [_] G.iter_no_id_info as super
        method! visit_stmt () stmt =
          (match stmt.G.s with
           | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _)
             when is_module_exports lhs ->
             synth_from_lambda rhs
           | _ -> ());
          super#visit_stmt () stmt
      end in
      visitor#visit_program () fi.fi_ast
    ) file_infos;
    h
  end
