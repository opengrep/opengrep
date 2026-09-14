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

let record_field_names (e : G.expr) : (G.ident * string) list =
  match e.G.e with
  | G.Record (_, fields, _) ->
    List.filter_map
      (fun (field : G.field) ->
        match field with
        | G.F { G.s = G.DefStmt ((ent : G.entity),
                                 (G.FieldDefColon vd | G.VarDef vd)); _ } -> (
          match (ent.G.name, vd.G.vinit) with
          | G.EN (G.Id ((key : G.ident), _)),
            Some { G.e = G.N (G.Id ((value, _), _)); _ } -> Some (key, value)
          | G.EN (G.Id ((key : G.ident), _)), _ -> Some (key, fst key)
          | _ -> None)
        | _ -> None)
      fields
  | _ -> []

(* Clojure [(ns x (:require ...))] is one [OtherDirective("NsDirective")] whose
   requires the parser doesn't surface as imports; pull aliases/refers out here. *)
let collect_clojure_ns_form ~(tok : Tok.t) (st : import list)
    (expr_arg : G.any)
  : import list =
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
  let add (acc : import list) (local : string) (target : Names.Module_qn.t) =
    { im_local = local; im_alias = None; im_target = target; im_tok = tok;
      im_static = false; im_global = false; im_binds = Binds_any;
      im_role = Role_binds }
    :: acc
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
           | kw :: all :: tail when is_kwd ":refer" kw && is_kwd ":all" all ->
             scan (add st wildcard_local ns_qn) tail
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
  | G.E { G.e = G.RawExpr
                  (Raw_tree.Case
                     ("Quot_lit",
                      Raw_tree.Tuple
                        [ _;
                          Raw_tree.Any
                            (G.E { G.e = G.Container (G.Array, (_, items, _));
                                   _ }) ]));
          _ } ->
    walk_require_vector st items
  | _ -> st

let collect_imports ~(cfg : Index_lang_rules.t)
    ~(resolution : Module_paths.specifier_resolution)
    ~(current_file : Fpath.t)
    ~(current_module_path : Names.Module_qn.t)
    ~(is_init_file : bool)
    (ast : G.program) : import list =
  let add ~(tok : Tok.t) ~(static : bool) ~(global : bool)
      ~(binds : import_binds) ~(role : import_role)
      ~(alias : string option) (acc : import list) local target =
    { im_local = local; im_alias = alias; im_target = target; im_tok = tok;
      im_static = static; im_global = global; im_binds = binds;
      im_role = role }
    :: acc
  in
  let is_static_attr (attr : G.attribute) : bool =
    match attr with
    | G.KeywordAttr (G.Static, _) -> true
    | _ -> false
  in
  let is_global_attr (attr : G.attribute) : bool =
    match attr with
    | G.KeywordAttr (G.GlobalScope, _) -> true
    | _ -> false
  in
  let has_keyword (keyword : G.keyword_attribute) (attrs : G.attribute list)
      : bool =
    List.exists (fun (attr : G.attribute) ->
      match attr with
      | G.KeywordAttr (kw, _) -> G.equal_keyword_attribute kw keyword
      | _ -> false)
      attrs
  in
  let marks_reexport (attrs : G.attribute list) : bool =
    match cfg.Index_lang_rules.reexport_source with
    | Index_lang_rules.Reexports_from_init_file -> false
    | Index_lang_rules.Reexports_from_public_directives ->
      has_keyword G.Public attrs
  in
  let role_of_attrs (attrs : G.attribute list) : import_role =
    if has_keyword G.Reexport attrs || marks_reexport attrs then Role_reexports
    else Role_binds
  in
  let binds_of_attrs (attrs : G.attribute list) : import_binds =
    if has_keyword G.TypeOnly attrs then Binds_type
    else if has_keyword G.Callable attrs then Binds_function
    else if has_keyword G.Const attrs then Binds_constant
    else Binds_any
  in
  let own_module_names : unit Common.SMap.t =
    if not cfg.Index_lang_rules.import_head_may_be_own_module then
      Common.SMap.empty
    else
      List.fold_left
        (fun (names : unit Common.SMap.t) (stmt : G.stmt) ->
          match stmt.G.s with
          | G.DefStmt ({ G.name = G.EN (G.Id (((name : string), _), _)); _ },
                       G.ModuleDef { G.mbody = G.ModuleStruct _ }) ->
            Common.SMap.add name () names
          | _ -> names)
        Common.SMap.empty ast
  in
  let module_name_of (mn : G.module_name) : Names.Module_qn.t option =
    match
      Module_paths.module_name_string ~cfg ~resolution ~current_file
        ~current_module_path ~own_module_names ~is_init_file mn
    with
    | Some (qn : Names.Module_qn.t) when not (Names.Module_qn.is_empty qn) ->
      Some qn
    | Some _
    | None -> None
  in
  let on_directive st (dir : G.directive) =
    let add ~(binds : import_binds) ~(alias : string option) =
      add ~static:(List.exists is_static_attr dir.G.d_attrs)
        ~global:(List.exists is_global_attr dir.G.d_attrs)
        ~binds ~alias
        ~role:(role_of_attrs dir.G.d_attrs)
    in
    let attr_binds = binds_of_attrs dir.G.d_attrs in
    match dir.G.d with
    | G.ImportAs (tok, mn, alias_opt) ->
      let alias = Option.map (fun ((name, _), _) -> name) alias_opt in
      let local =
        match alias with
        | Some (alias : string) -> alias
        | None ->
          (match mn with
           | G.DottedName (((first_seg : string), _) :: _ as segs) -> (
             match cfg.Index_lang_rules.unaliased_import_binds with
             | Index_lang_rules.First_segment_binds -> first_seg
             | Index_lang_rules.Last_segment_binds ->
               (match List.rev segs with
                | ((last_seg : string), _) :: _ -> last_seg
                | [] -> first_seg))
           | G.DottedName [] -> ""
           (* Unaliased path import: dir-scoped langs (Go) use the path's last
              segment as local; other langs keep the raw specifier. *)
           | G.FileName (spec, _) ->
             (match cfg.Index_lang_rules.unqualified_scope with
              | `Per_directory
              | `Per_go_package ->
                (match Fpath.of_string spec with
                 | Ok path -> Fpath.basename path
                 | Error _ -> spec)
              | `Per_module -> ""
              | `Per_file
              | `Per_crate
              | `Per_constant_path
              | `Per_package
              | `Per_namespace
              | `Per_translation_unit
              | `Per_project -> spec))
      in
      let binds =
        match (attr_binds, cfg.Index_lang_rules.unqualified_scope) with
        | Binds_type, _ -> Binds_type
        | (Binds_any | Binds_function | Binds_constant | Binds_module),
          `Per_module -> Binds_module
        | _, (`Per_file | `Per_crate | `Per_constant_path | `Per_directory
             | `Per_go_package | `Per_package | `Per_namespace
             | `Per_translation_unit | `Per_project) ->
          attr_binds
      in
      (match
         (module_name_of mn, Int.compare (String.length local) 0 > 0)
       with
       | Some (qn : Names.Module_qn.t), true ->
         add ~binds ~alias ~tok st local qn
       | Some _, false
       | None, _ -> st)
    | G.ImportFrom (tok, mn, names) -> (
      match module_name_of mn with
      | None -> st
      | Some (qn : Names.Module_qn.t) ->
        List.fold_left (fun st ((name, _), alias_opt) ->
          let alias = Option.map (fun ((name, _), _) -> name) alias_opt in
          let local = Option.value alias ~default:name in
          let target = Names.Module_qn.concat qn name in
          add ~binds:attr_binds ~alias ~tok st local target
        ) st names)
    (* sentinel [("*", M_qn)] tells the re-export pass to bulk-copy M's free funcs. *)
    | G.ImportAll (tok, mn, _) -> (
      match module_name_of mn with
      | None -> st
      | Some (qn : Names.Module_qn.t) ->
        add ~binds:attr_binds ~alias:None ~tok st wildcard_local qn)
    | G.OtherDirective (("NsDirective", tok), exprs)
    | G.OtherDirective (("RequireDirective", tok), exprs) ->
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
  let qn_of_specifier (spec : string) : Names.Module_qn.t option =
    module_name_of (mk_filename_mn spec)
  in
  let on_defstmt st (ent : G.entity) (vd : G.variable_definition) =
    match vd.G.vinit with
    | None -> st
    | Some rhs ->
      let bind_names (spec : string)
          (names : (G.ident * string) list) =
        match qn_of_specifier spec with
        | None -> st
        | Some (qn : Names.Module_qn.t) ->
          List.fold_left
            (fun st (((key : string), (tok : Tok.t)), (local : string)) ->
              add ~tok ~static:false ~global:false ~binds:Binds_any
                ~alias:(Some local) ~role:Role_binds st local
                (Names.Module_qn.concat qn key))
            st names
      in
      match (extract_require_spec rhs, ent.G.name) with
      | Some spec, G.EN (G.Id ((local, tok), _))
        when Int.compare (String.length local) 0 > 0 -> (
        match qn_of_specifier spec with
        | None -> st
        | Some (qn : Names.Module_qn.t) ->
          add ~tok ~static:false ~global:false ~binds:Binds_module
            ~alias:(Some local) ~role:Role_binds st local qn)
      | Some spec, G.EPattern (G.PatRecord (_, fields, _)) ->
        bind_names spec
          (List.filter_map
             (fun (((dotted_name : G.dotted_ident), (value_pat : G.pattern))) ->
               match (dotted_name, value_pat) with
               | (key : G.ident) :: _, G.PatId ((id_str, _), _) ->
                 Some (key, id_str)
               | (key : G.ident) :: _, _ -> Some (key, fst key)
               | [], _ -> None)
             fields)
      | None, _ -> (
        match rhs.G.e with
        | G.Assign (pattern, _, (inner : G.expr)) -> (
          match extract_require_spec inner with
          | None -> st
          | Some (spec : string) -> bind_names spec (record_field_names pattern))
        | _ -> st)
      | Some _, _ -> st
  in
  let acc =
    Walker.fold_stmts_in_program (fun st stmt ->
      match stmt.G.s with
      | G.DirectiveStmt dir -> on_directive st dir
      | G.DefStmt (ent, G.VarDef vd) -> on_defstmt st ent vd
      | _ -> st) [] ast
  in
  List.rev acc

let with_package_clause_locals ~(cfg : Index_lang_rules.t)
    ~(clause_of_module : Names.Module_qn.t -> string option)
    ((file_infos : file_info list), (class_infos : class_info list))
    : file_info list * class_info list =
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_file
  | `Per_crate
  | `Per_constant_path
  | `Per_directory
  | `Per_module
  | `Per_namespace
  | `Per_translation_unit
  | `Per_project
  | `Per_package -> (file_infos, class_infos)
  | `Per_go_package ->
    let of_import (imp : import) : import =
      if String.equal imp.im_local wildcard_local then imp
      else
        match imp.im_alias with
        | Some _ -> imp
        | None -> (
          match clause_of_module imp.im_target with
          | None -> imp
          | Some (clause : string) -> { imp with im_local = clause })
    in
    ( List.map
        (fun (fi : file_info) ->
          { fi with fi_imports = List.map of_import fi.fi_imports })
        file_infos,
      List.map
        (fun (ci : class_info) ->
          { ci with ci_imports = List.map of_import ci.ci_imports })
        class_infos )
