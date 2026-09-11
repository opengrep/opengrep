(* Import collection: the per-file (local name -> module) bindings and raw
   import specifiers that feed alias resolution and re-export handling. *)

module G = AST_generic

open Types

let wildcard_local : string = "*"

type binding =
  | Wildcard_from of Names.Module_qn.t
  | Named_binding of { local : string; target : Names.Module_qn.t }

let binding_of (imp : import) : binding =
  if String.equal imp.im_local wildcard_local then Wildcard_from imp.im_target
  else Named_binding { local = imp.im_local; target = imp.im_target }

(* Clojure [(ns x (:require ...))] is one [OtherDirective("NsDirective")] whose
   requires the parser doesn't surface as imports; pull aliases/refers out here. *)
let collect_clojure_ns_form ~(tok : Tok.t)
    (st : import list * (string * string * import_kind) list)
    (expr_arg : G.any)
  : import list * (string * string * import_kind) list =
  let id_name (expr : G.expr) : string option =
    match expr.G.e with G.N name -> Ty_bare_name.bare_name_of_name name | _ -> None
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
  let add ((acc, specs) : import list * (string * string * import_kind) list)
      (local : string) (target : Names.Module_qn.t) =
    ({ im_local = local; im_target = target; im_tok = tok } :: acc, specs)
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
               | Some alias -> add (add st alias ns_qn) wildcard_local ns_qn
               | None -> add st wildcard_local ns_qn
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
    import list * (string * string * import_kind) list =
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
  let add ~(tok : Tok.t) (acc, specs) local target =
    ({ im_local = local; im_target = target; im_tok = tok } :: acc, specs)
  in
  let on_directive st (dir : G.directive) =
    match dir.G.d with
    | G.ImportAs (tok, mn, alias_opt) ->
      let qn =
        Module_paths.module_name_string ~cfg ~current_module_path ~is_init_file
          mn
      in
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
        add_spec (add ~tok st local qn) local mn I_namespace
      else st
    | G.ImportFrom (tok, mn, names) ->
      let qn =
        Module_paths.module_name_string ~cfg ~current_module_path ~is_init_file
          mn
      in
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
          add_spec (add ~tok st local target) local mn kind
        ) st names
    (* sentinel [("*", M_qn)] tells the re-export pass to bulk-copy M's free funcs.
       The raw specifier is kept under the same "*" sentinel so file-target
       narrowing can resolve a whole-file import (Ruby [require_relative]) to
       the file(s) it names ([add_spec] drops [DottedName] imports, whose
       specifier is empty). *)
    | G.ImportAll (tok, mn, _) ->
      let qn =
        Module_paths.module_name_string ~cfg ~current_module_path ~is_init_file
          mn
      in
      if Names.Module_qn.is_empty qn then st
      else add_spec (add ~tok st wildcard_local qn) wildcard_local mn I_namespace
    | G.OtherDirective (("NsDirective", tok), exprs) ->
      List.fold_left (collect_clojure_ns_form ~tok) st exprs
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
      | Some spec, G.EN (G.Id ((local, tok), _))
        when String.length local > 0 ->
        let qn = qn_of_specifier spec in
        let st = add ~tok st local qn in
        let st = add_spec st local (mk_filename_mn spec) I_default in
        add_spec st local (mk_filename_mn spec) I_namespace
      | Some spec, G.EPattern (G.PatRecord (_, fields, _)) ->
        List.fold_left (fun st (pat_field : G.dotted_ident * G.pattern) ->
          let dotted_name, value_pat = pat_field in
          let key_name = match dotted_name with
            | (seg, tok) :: _ -> Some (seg, tok)
            | [] -> None
          in
          let local_name = match value_pat, key_name with
            | G.PatId ((id_str, _), _), _ -> Some id_str
            | _, Some (seg, _) -> Some seg
            | _, None -> None
          in
          match key_name, local_name with
          | Some (key, tok), Some local ->
            let target =
              Names.Module_qn.concat (Names.Module_qn.of_string spec) key
            in
            add_spec (add ~tok st local target) local (mk_filename_mn spec)
              (I_named key)
          | _ -> st
        ) st fields
      | _ -> st
  in
  (* PHP [require]/[include] parse as calls to [__builtin__require*], not
     import directives; capture them as whole-file "*" imports like Ruby's
     [require_relative] so file-target narrowing sees them. *)
  let php_require_spec (expr : G.expr) : string option =
    match expr.G.e with
    | G.Call ({ G.e = G.N (G.Id ((callee_name, _), _)); _ }, args)
      when List.mem callee_name
             [ "__builtin__require"; "__builtin__require_once";
               "__builtin__include"; "__builtin__include_once" ] ->
      (match Tok.unbracket args with
       | [G.Arg { G.e = G.L (G.String (_, (spec, _), _)); _ }] -> Some spec
       | _ -> None)
    | _ -> None
  in
  let on_exprstmt st (expr : G.expr) =
    match php_require_spec expr with
    (* Spec only — no [("*", qn)] binding, which would opt the file into the
       re-export bulk-copy pass. *)
    | Some spec when String.length spec > 0 ->
      add_spec st wildcard_local (mk_filename_mn spec) I_namespace
    | _ -> st
  in
  let acc, specs =
    Walker.fold_stmts_in_program (fun st stmt ->
      match stmt.G.s with
      | G.DirectiveStmt dir -> on_directive st dir
      | G.DefStmt (ent, G.VarDef vd) -> on_defstmt st ent vd
      | G.ExprStmt (expr, _) -> on_exprstmt st expr
      | _ -> st) ([], []) ast
  in
  (List.rev acc, List.rev specs)
