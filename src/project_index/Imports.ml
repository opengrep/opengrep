(* Import collection: the per-file (local name -> module) bindings and raw
   import specifiers that feed alias resolution and re-export handling. *)

module G = AST_generic

open Types

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
      (* Relative imports: [__init__.py] file IS the package (no leaf drop);
         each extra [.] walks one level up. *)
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

(* Clojure [(ns x (:require ...))] is one [OtherDirective("NsDirective")] whose
   requires the parser doesn't surface as imports; pull aliases/refers out here. *)
let collect_clojure_ns_form
    (st : (string * Names.Module_qn.t) list
        * (string * string * import_kind) list)
    (expr_arg : G.any)
  : (string * Names.Module_qn.t) list
    * (string * string * import_kind) list =
  let id_name (expr : G.expr) : string option =
    match expr.G.e with G.N name -> Ty_leaf.leaf_of_name name | _ -> None
  in
  let kwd_name (expr : G.expr) : string option =
    match expr.G.e with
    | G.OtherExpr (("Atom", _), [G.Name name]) ->
      (match List.rev (Index_lang_rules.name_to_path name) with
       | last :: _ -> Some last
       | [] -> None)
    | _ -> None
  in
  let is_kwd name expr = match kwd_name expr with Some str -> String.equal str name | None -> false in
  let add ((acc, specs) :
           (string * Names.Module_qn.t) list
           * (string * string * import_kind) list)
      (local : string) (target : Names.Module_qn.t) =
    ((local, target) :: acc, specs)
  in
  let walk_require_vector st vec_items =
    match vec_items with
    | ns_expr :: modifiers ->
      (match id_name ns_expr with
       | None -> st
       | Some ns_str ->
         let ns_qn = Names.Module_qn.of_string ns_str in
         let rec scan st = function
           | [] -> st
           | kw :: value :: tail when is_kwd ":as" kw ->
             (* wildcard [("*", ns_qn)] tells the re-export pass to copy ns_qn's
                free fns for [(h/handle ...)]. *)
             let st = match id_name value with
               | Some alias -> add (add st alias ns_qn) "*" ns_qn
               | None -> add st "*" ns_qn
             in
             scan st tail
           | kw :: { G.e = G.Container (G.Array, (_, refs, _)); _ } :: tail
             when is_kwd ":refer" kw ->
             let st = List.fold_left (fun st ref_expr ->
               match id_name ref_expr with
               | Some name -> add st name (Names.Module_qn.concat ns_qn name)
               | None -> st) st refs
             in
             scan st tail
           | _ :: tail -> scan st tail
         in
         scan st modifiers)
    | [] -> st
  in
  match expr_arg with
  | G.E { G.e = G.Call (callee, args); _ } when is_kwd ":require" callee ->
    Tok.unbracket args
    |> List.fold_left (fun st arg ->
         match arg with
         | G.Arg { G.e = G.Container (G.Array, (_, items, _)); _ } ->
           walk_require_vector st items
         | _ -> st) st
  | _ -> st

let collect_imports ~(cfg : Index_lang_rules.t)
    ~(current_module_path : Names.Module_qn.t)
    ~(is_init_file : bool)
    (ast : G.program) :
    (string * Names.Module_qn.t) list
    * (string * string * import_kind) list =
  let raw_specifier = function
    | G.FileName (spec, _) -> spec
    | G.DottedName _ -> ""
  in
  let add_spec (acc, specs) local mn kind =
    let spec = raw_specifier mn in
    if String.length spec > 0
    then (acc, (local, spec, kind) :: specs)
    else (acc, specs)
  in
  let add (acc, specs) local target = ((local, target) :: acc, specs) in
  let on_directive st (dir : G.directive) =
    match dir.G.d with
    | G.ImportAs (_, mn, alias_opt) ->
      let qn = module_name_string ~cfg ~current_module_path ~is_init_file mn in
      let local =
        match alias_opt with
        | Some ((alias, _), _) -> alias
        | None ->
          (match mn with
           | G.DottedName ((seg, _) :: _) -> seg
           | G.DottedName [] -> ""
           (* Unaliased path import: dir-scoped langs (Go) use the path's last
              segment as local; other langs keep the raw specifier. *)
           | G.FileName (spec, _) ->
             (match cfg.Index_lang_rules.unqualified_scope with
              | `Per_directory ->
                (match Fpath.of_string spec with
                 | Ok path -> Fpath.basename path
                 | Error _ -> spec)
              | _ -> spec))
      in
      if String.length local > 0 && not (Names.Module_qn.is_empty qn) then
        (* TS/JS default and namespace imports are indistinguishable here;
           treat both as [I_namespace]. *)
        add_spec (add st local qn) local mn I_namespace
      else st
    | G.ImportFrom (_, mn, names) ->
      let qn = module_name_string ~cfg ~current_module_path ~is_init_file mn in
      if Names.Module_qn.is_empty qn then st
      else
        List.fold_left (fun st ((name, _), alias_opt) ->
          let local =
            match alias_opt with
            | Some ((alias, _), _) -> alias
            | None -> name
          in
          let target = Names.Module_qn.concat qn name in
          let kind =
            if String.equal name "default" then I_default
            else I_named name
          in
          add_spec (add st local target) local mn kind
        ) st names
    (* sentinel [("*", M_qn)] tells the re-export pass to bulk-copy M's free funcs. *)
    | G.ImportAll (_, mn, _) ->
      let qn = module_name_string ~cfg ~current_module_path ~is_init_file mn in
      if Names.Module_qn.is_empty qn then st
      else add st "*" qn
    | G.OtherDirective (("NsDirective", _), exprs) ->
      List.fold_left collect_clojure_ns_form st exprs
    | _ -> st
  in
  let extract_require_spec (expr : G.expr) : string option =
    match expr.G.e with
    | G.Call ({ G.e = G.IdSpecial (G.Require, _); _ }, args) ->
      (match Tok.unbracket args with
       | [G.Arg { G.e = G.L (G.String (_, (spec, _), _)); _ }] -> Some spec
       | _ -> None)
    | _ -> None
  in
  let mk_filename_mn (spec : string) : G.module_name =
    G.FileName (spec, Tok.unsafe_fake_tok spec)
  in
  let qn_of_specifier spec : Names.Module_qn.t =
    (* No relative-path rewriting; [Ts_modules.resolve_specifier] uses the raw form. *)
    Names.Module_qn.of_string spec
  in
  let on_defstmt st (ent : G.entity) (vd : G.variable_definition) =
    match vd.G.vinit with
    | None -> st
    | Some rhs ->
      match extract_require_spec rhs, ent.G.name with
      | Some spec, G.EN (G.Id ((local, _), _))
        when String.length local > 0 ->
        let qn = qn_of_specifier spec in
        let st = add st local qn in
        let st = add_spec st local (mk_filename_mn spec) I_default in
        add_spec st local (mk_filename_mn spec) I_namespace
      | Some spec, G.EPattern (G.PatRecord (_, fields, _)) ->
        List.fold_left (fun st (pat_field : G.dotted_ident * G.pattern) ->
          let dotted_name, value_pat = pat_field in
          let key_name = match dotted_name with
            | (seg, _) :: _ -> Some seg
            | [] -> None
          in
          let local_name = match value_pat with
            | G.PatId ((id_str, _), _) -> Some id_str
            | _ -> key_name
          in
          match key_name, local_name with
          | Some key, Some local ->
            let target =
              Names.Module_qn.concat (Names.Module_qn.of_string spec) key
            in
            add_spec (add st local target) local (mk_filename_mn spec) (I_named key)
          | _ -> st
        ) st fields
      | _ -> st
  in
  let acc, specs =
    Walker.fold_stmts_in_program (fun st stmt ->
      match stmt.G.s with
      | G.DirectiveStmt dir -> on_directive st dir
      | G.DefStmt (ent, G.VarDef vd) -> on_defstmt st ent vd
      | _ -> st) ([], []) ast
  in
  (List.rev acc, List.rev specs)
