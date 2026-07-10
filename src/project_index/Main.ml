(* projidx: language-agnostic project-wide symbol walker. *)

module G = AST_generic

open Types

module Log = Log_projidx.Log

let run_visit ?context ~(on : Fpath.t) (visit : unit -> unit) : unit =
  try visit () with
  | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as exn ->
    raise exn
  | exn ->
    let where = match context with
      | None -> Fpath.to_string on
      | Some ctx -> Fpath.to_string on ^ ":" ^ ctx
    in
    Log.warn (fun m -> m "[skip] %s: visit failed: %s"
      where (Printexc.to_string exn))

let module_path ~(cfg : Index_lang_rules.t) ~(project_root : Fpath.t)
    ?(ast : G.program option) (file : Fpath.t) : Names.Module_qn.t =
  match Option.bind ast cfg.Index_lang_rules.module_path_from_ast with
  | Some mod_str -> Names.Module_qn.of_string mod_str
  | None ->
    let rel = Discover.relative_to ~project_root file in
    let path_str = Fpath.rem_ext rel |> Fpath.normalize |> Fpath.to_string in
    let path_str = cfg.Index_lang_rules.rewrite_module_path path_str in
    Names.Module_qn.of_string
      (String.map (fun ch -> if Char.equal ch '/' then '.' else ch) path_str)

let decorator_simple_name = Index_lang_rules.decorator_simple_name
let entity_simple_name = Index_lang_rules.entity_simple_name

let entity_decorator_names (ent : G.entity) : string list =
  List.filter_map decorator_simple_name ent.G.attrs

let entity_range (ent : G.entity) : Range.t option =
  match ent.G.name with
  | G.EN gname ->
    (match AST_generic_helpers.range_of_any_opt (G.E (G.N gname |> G.e)) with
     | Some (start_tok, end_tok) -> Some (Range.range_of_token_locations start_tok end_tok)
     | None -> None)
  | _ -> None

(* Entity name -> [Function_id.t], matching Graph_from_AST's identity. *)
let function_id_of_entity (ent : G.entity) : Function_id.t option =
  match ent.G.name with
  | G.EN gname ->
    Some (Function_id.of_il_name (AST_to_IL.var_of_name gname))
  | _ -> None

let synth_function_id (parent : Function_id.t) (name : string) : Function_id.t =
  Function_id.of_string_and_tok name (Function_id.tok parent)

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
    (* No relative-path rewriting; [resolve_ts_specifier] uses the raw form. *)
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

let build_path_suffix_index (file_paths : string list)
  : (string, string list) Hashtbl.t =
  let index : (string, string list) Hashtbl.t = Hashtbl.create 16384 in
  let strip_ext path =
    if Filename.check_suffix path ".tsx" then Filename.chop_suffix path ".tsx"
    else if Filename.check_suffix path ".ts" then Filename.chop_suffix path ".ts"
    else if Filename.check_suffix path ".jsx" then Filename.chop_suffix path ".jsx"
    else if Filename.check_suffix path ".js" then Filename.chop_suffix path ".js"
    else path
  in
  let strip_index path =
    if Filename.check_suffix path "/index" then
      Filename.chop_suffix path "/index"
    else path
  in
  List.iter (fun path ->
    let stripped = path |> strip_ext |> strip_index in
    let parts = String.split_on_char '/' stripped in
    let n = List.length parts in
    let arr = Array.of_list parts in
    for i = 0 to n - 1 do
      let suffix = String.concat "/"
        (Array.to_list (Array.sub arr i (n - i))) in
      let cur = Option.value (Hashtbl.find_opt index suffix) ~default:[] in
      Hashtbl.replace index suffix (path :: cur)
    done
  ) file_paths;
  index

let resolve_ts_specifier
    ?(path_suffix_index : (string, string list) Hashtbl.t option = None)
    ~(current_file : Fpath.t) (specifier : string) : string list =
  if String.length specifier = 0 then []
  else if specifier.[0] = '.' then begin
    let base =
      Fpath.append (Fpath.parent current_file) (Fpath.v specifier)
      |> Fpath.normalize |> Fpath.rem_empty_seg |> Fpath.to_string
    in
    [ base ^ ".ts"; base ^ ".tsx"; base ^ ".js"; base ^ ".jsx";
      base ^ "/index.ts"; base ^ "/index.tsx";
      base ^ "/index.js"; base ^ "/index.jsx" ]
  end
  else
    match path_suffix_index with
    | None -> []
    | Some idx ->
      (Option.value (Hashtbl.find_opt idx specifier) ~default:[])

let name_to_path = Index_lang_rules.name_to_path

let rec expr_to_path (expr : G.expr) : string list =
  match expr.G.e with
  | G.N name -> name_to_path name
  | G.DotAccess (lhs, _, G.FN name) ->
    expr_to_path lhs @ name_to_path name
  | _ -> []

let parent_path (parent_ty : G.type_) : string list =
  match parent_ty.G.t with
  | G.TyN name -> name_to_path name
  | G.TyExpr expr -> expr_to_path expr
  | _ -> []

type scope_kind =
  | Sc_class of { name : string; id : Function_id.t }
  | Sc_function of string

let qualified_name_of ~(module_path : Names.Module_qn.t)
    (outer_to_inner : scope_kind list) (leaf : string) : string =
  let buf = Buffer.create 64 in
  Buffer.add_string buf (Names.Module_qn.to_string module_path);
  let prev_was_fn =
    List.fold_left (fun prev_was_fn scope ->
      Buffer.add_char buf '.';
      if prev_was_fn then Buffer.add_string buf "<locals>.";
      (match scope with
       | Sc_class { name; _ } -> Buffer.add_string buf name
       | Sc_function fn_name -> Buffer.add_string buf fn_name);
      (match scope with Sc_function _ -> true | _ -> false)
    ) false outer_to_inner
  in
  Buffer.add_char buf '.';
  if prev_was_fn then Buffer.add_string buf "<locals>.";
  Buffer.add_string buf leaf;
  Buffer.contents buf

let immediate_enclosing_class_id (innermost_first : scope_kind list)
  : Function_id.t option =
  match innermost_first with
  | Sc_class { id; _ } :: _ -> Some id
  | _ -> None

let collect_in_ast ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(module_path : Names.Module_qn.t) ~(file : Fpath.t)
    (ast : G.program) : entry list * class_info list * file_info =
  let entries = ref [] in
  let class_infos = ref [] in
  let dc_wrappers = ref [] in
  let imports, import_specifiers =
    let is_init_file = cfg.Index_lang_rules.is_init_file file in
    collect_imports ~cfg ~current_module_path:module_path ~is_init_file ast
  in
  let mk_entry ~id ~name ~kind ~range ~defining_class_id =
    { id; name; kind; file; range; defining_class_id }
  in
  let emit_synth_dunder ~class_id ~range name =
    let m_id = synth_function_id class_id name in
    entries := mk_entry ~id:m_id ~name ~kind:K_method ~range
                 ~defining_class_id:(Some class_id) :: !entries
  in
  (* Scope stack threaded as the visitor's env (innermost first). *)
  let visitor = object
    inherit [_] G.iter_no_id_info as super
    method! visit_definition (scope : scope_kind list) (ent, def_kind) =
      let ent, def_kind =
        match cfg.Index_lang_rules.class_def_reshape ent def_kind with
        | Some pair -> pair
        | None -> (ent, def_kind)
      in
      match def_kind with
      | G.ClassDef cdef -> begin
          match entity_simple_name ent, function_id_of_entity ent with
          | None, _ | _, None -> super#visit_definition scope (ent, def_kind)
          | Some name, Some class_id ->
            let class_qn =
              qualified_name_of ~module_path (List.rev scope) name
            in
            let class_range = entity_range ent in
            let parent_class_id = immediate_enclosing_class_id scope in
            entries := mk_entry ~id:class_id ~name ~kind:K_class
                         ~range:class_range
                         ~defining_class_id:parent_class_id :: !entries;
            let synthesized =
              let from_dec = cfg.Index_lang_rules.class_dunders_from_decorators ent.G.attrs in
              let from_ext = cfg.Index_lang_rules.class_dunders_from_extends cdef in
              List.sort_uniq String.compare (from_dec @ from_ext)
            in
            List.iter (emit_synth_dunder ~class_id ~range:class_range)
              synthesized;
            (* Class-body macro methods (Ruby [attr_reader]); def-site tok from
               the symbol literal to match scip-ruby. *)
            List.iter (fun (m_name, m_tok) ->
              let m_id = Function_id.of_string_and_tok m_name m_tok in
              entries := mk_entry ~id:m_id ~name:m_name ~kind:K_method
                            ~range:class_range
                            ~defining_class_id:(Some class_id) :: !entries
            ) (cfg.Index_lang_rules.class_body_synth_methods cdef);
            List.iter (fun (parent_ty, _args) ->
              match parent_ty.G.t with
              | G.TyExpr expr ->
                (match cfg.Index_lang_rules.inner_class_from_call expr with
                 | None -> ()
                 | Some (inner_name, dunders) ->
                   let inner_id = synth_function_id class_id inner_name in
                   entries := mk_entry ~id:inner_id ~name:inner_name
                                 ~kind:K_class
                                 ~range:class_range
                                 ~defining_class_id:parent_class_id
                              :: !entries;
                   List.iter (fun dunder ->
                     let m_id = synth_function_id inner_id dunder in
                     entries := mk_entry ~id:m_id ~name:dunder ~kind:K_method
                                   ~range:class_range
                                   ~defining_class_id:(Some inner_id)
                                :: !entries
                   ) dunders)
              | _ -> ()
            ) cdef.G.cextends;
            let parent_paths =
              let from_extends =
                List.filter_map (fun (ty, _) ->
                  match parent_path ty with [] -> None | path -> Some path
                ) cdef.G.cextends
              in
              from_extends @ cfg.Index_lang_rules.class_body_extra_parents cdef
            in
            class_infos := { ci_id = class_id;
                             ci_qn = Names.Class_qn.of_string class_qn;
                             ci_class_kind = fst cdef.G.ckind;
                             ci_file = file;
                             ci_range = class_range;
                             ci_parent_paths = parent_paths;
                             ci_imports = imports;
                             ci_decorator_names = entity_decorator_names ent }
                           :: !class_infos;
            let scope' = Sc_class { name; id = class_id } :: scope in
            super#visit_definition scope' (ent, def_kind)
        end
      | G.FuncDef _
      | G.VarDef { G.vinit = Some { G.e = G.Lambda _; _ }; _ } -> begin
          (match cfg.Index_lang_rules.extract_wrapper ent with
           | Some wrapper -> dc_wrappers := wrapper :: !dc_wrappers
           | None -> ());
          (* Lambda defs: use the synth lambda name to match
             [Graph_from_AST.fn_id_of_entity]'s key, else it can't resolve. *)
          let fid_opt =
            match def_kind with
            | G.VarDef { G.vinit = Some { G.e = G.Lambda fdef; _ }; _ } ->
                Some (Function_id.of_il_name
                        (Visit_function_defs.synth_lambda_il_name fdef))
            | G.FuncDef fdef
              when (match fst fdef.G.fkind with
                    | G.LambdaKind | G.Arrow -> true
                    | _ -> false) ->
                Some (Function_id.of_il_name
                        (Visit_function_defs.synth_lambda_il_name fdef))
            | _ -> function_id_of_entity ent
          in
          match entity_simple_name ent, fid_opt with
          | None, _ | _, None -> super#visit_definition scope (ent, def_kind)
          | Some name, Some fn_id ->
            let defining_class_id = immediate_enclosing_class_id scope in
            let kind =
              if Option.is_some defining_class_id then K_method
              else K_function
            in
            entries := mk_entry ~id:fn_id ~name ~kind
                         ~range:(entity_range ent)
                         ~defining_class_id :: !entries;
            let scope' = Sc_function name :: scope in
            super#visit_definition scope' (ent, def_kind)
        end
      | G.VarDef { G.vinit = Some init; _ } -> begin
          (match cfg.Index_lang_rules.synth_call_dunders init,
                 entity_simple_name ent, function_id_of_entity ent with
           | Some dunders, Some lhs_name, Some class_id ->
             let parent_class_id = immediate_enclosing_class_id scope in
             entries := mk_entry ~id:class_id ~name:lhs_name ~kind:K_class
                          ~range:(entity_range ent)
                          ~defining_class_id:parent_class_id :: !entries;
             List.iter (emit_synth_dunder ~class_id
                          ~range:(entity_range ent)) dunders
           | _ -> ());
          super#visit_definition scope (ent, def_kind)
        end
      | _ -> super#visit_definition scope (ent, def_kind)

    (* Plain synth-call assignments [X = NewType(...)] aren't [definition]s,
       so handle them at the stmt level. *)
    method! visit_stmt (scope : scope_kind list) stmt =
      (match stmt.G.s with
       | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _) -> begin
           let lhs_name_id =
             match lhs.G.e with
             | G.N gname ->
               (match Ty_leaf.leaf_of_name gname with
                | None -> None
                | Some name ->
                  Some (name, Function_id.of_il_name
                              (AST_to_IL.var_of_name gname)))
             | _ -> None
           in
           match cfg.Index_lang_rules.synth_call_dunders rhs, lhs_name_id with
           | Some dunders, Some (lhs_name, class_id) ->
             let range =
               match AST_generic_helpers.range_of_any_opt (G.E lhs) with
               | Some (start_tok, end_tok) -> Some (Range.range_of_token_locations start_tok end_tok)
               | None -> None
             in
             let parent_class_id = immediate_enclosing_class_id scope in
             entries := mk_entry ~id:class_id ~name:lhs_name ~kind:K_class
                          ~range ~defining_class_id:parent_class_id
                        :: !entries;
             List.iter (emit_synth_dunder ~class_id ~range) dunders
           | _ -> ()
         end
       | _ -> ());
      super#visit_stmt scope stmt
  end in
  visitor#visit_program [] ast;
  let fi = { fi_file = file; fi_module_path = module_path;
             fi_imports = imports;
             fi_import_specifiers = import_specifiers;
             fi_dataclass_wrappers = !dc_wrappers;
             fi_ast = ast;
             fi_observations = Walker.walk_file ~lang ast } in
  (List.rev !entries, List.rev !class_infos, fi)

(* Per-class owned-method-name sets, seeded from [K_method] entries; the
   returned function materialises (and caches) a class's set. *)
let methods_by_class (entries : entry list)
    : Function_id.t -> (string, unit) Hashtbl.t =
  let tbl : (Function_id.t, (string, unit) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 8192
  in
  let ensure_set id =
    match Hashtbl.find_opt tbl id with
    | Some set -> set
    | None ->
      let set = Hashtbl.create 16 in Hashtbl.replace tbl id set; set
  in
  List.iter (fun entry ->
    if entry.kind = K_method then
      match entry.defining_class_id with
      | Some cls_id -> Hashtbl.replace (ensure_set cls_id) entry.name ()
      | None -> ()
  ) entries;
  ensure_set

let dataclass_wrapper_synth_entries ~(cfg : Index_lang_rules.t)
    ~(wrappers : (string, dataclass_wrapper) Hashtbl.t)
    (entries : entry list) (class_infos : class_info list) : entry list =
  let ensure_set = methods_by_class entries in
  List.fold_left (fun acc ci ->
    match List.find_map (fun dec -> Hashtbl.find_opt wrappers dec)
            ci.ci_decorator_names with
    | None -> acc
    | Some wrapper ->
      let owned = ensure_set ci.ci_id in
      List.fold_left (fun acc dunder ->
        if Hashtbl.mem owned dunder then acc
        else begin
          Hashtbl.replace owned dunder ();
          let m_id = synth_function_id ci.ci_id dunder in
          { id = m_id; name = dunder; kind = K_method;
            file = ci.ci_file; range = ci.ci_range;
            defining_class_id = Some ci.ci_id }
          :: acc
        end
      ) acc (cfg.Index_lang_rules.wrapper_dunders wrapper)
  ) [] class_infos

let build_reexport_map ~(cfg : Index_lang_rules.t) (file_infos : file_info list)
  : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t =
  let reexport_map = Hashtbl.create 4096 in
  if not cfg.Index_lang_rules.has_reexports then reexport_map
  else begin
    List.iter (fun fi ->
      if cfg.Index_lang_rules.is_init_file fi.fi_file then
        let pkg = fi.fi_module_path in
        List.iter (fun (local, target) ->
          let bound =
            if Names.Module_qn.is_empty pkg
            then Names.Module_qn.of_string local
            else Names.Module_qn.concat pkg local
          in
          if not (Names.Module_qn.equal bound target) then
            Hashtbl.replace reexport_map bound target
        ) fi.fi_imports
    ) file_infos;
    reexport_map
  end

let mro_inherited_entries
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(scope_resolution : bool)
    (entries : entry list) (class_infos : class_info list) : entry list =
  let by_qn : (Names.Class_qn.t, class_info) Hashtbl.t = Hashtbl.create 8192 in
  List.iter (fun ci -> Hashtbl.replace by_qn ci.ci_qn ci) class_infos;
  let known_class_qns : (Names.Class_qn.t, unit) Hashtbl.t =
    Hashtbl.create 8192
  in
  List.iter (fun ci -> Hashtbl.replace known_class_qns ci.ci_qn ())
    class_infos;
  let qns_by_leaf : (Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t =
    Hashtbl.create 8192
  in
  if scope_resolution then
    List.iter (fun ci ->
      let leaf = Names.Class_qn.leaf ci.ci_qn in
      if leaf <> "" then begin
        let leaf_key = Names.Class_name.of_string leaf in
        let cur = Option.value (Hashtbl.find_opt qns_by_leaf leaf_key) ~default:[] in
        Hashtbl.replace qns_by_leaf leaf_key (ci.ci_qn :: cur)
      end
    ) class_infos;
  let ensure_set = methods_by_class entries in
  List.fold_left (fun all_new ci ->
    let own = ensure_set ci.ci_id in
    let visited : (Function_id.t, unit) Hashtbl.t = Hashtbl.create 8 in
    let rec walk acc pci =
      if Hashtbl.mem visited pci.ci_id then acc
      else begin
        Hashtbl.replace visited pci.ci_id ();
        let acc =
          Hashtbl.fold (fun mname () acc ->
            if Hashtbl.mem own mname then acc
            else begin
              Hashtbl.replace own mname ();
              let m_id = synth_function_id ci.ci_id mname in
              { id = m_id; name = mname; kind = K_method;
                file = ci.ci_file; range = ci.ci_range;
                defining_class_id = Some ci.ci_id }
              :: acc
            end
          ) (ensure_set pci.ci_id) acc
        in
        List.fold_left (fun acc gp_path ->
          match
            (match Mro.resolve_parent_qn ~imports:pci.ci_imports
                     ~reexport_map ~known_class_qns gp_path with
             | Some _ as resolved -> resolved
             | None when scope_resolution ->
               Mro.resolve_parent_by_scope ~by_qn ~qns_by_leaf pci gp_path
             | None -> None)
          with
          | None -> acc
          | Some gp_qn ->
            (match Hashtbl.find_opt by_qn
                     (Names.Class_qn.of_string
                        (Names.Module_qn.to_string gp_qn)) with
             | Some gpci -> walk acc gpci
             | None -> acc)
        ) acc pci.ci_parent_paths
      end
    in
    List.fold_left (fun acc p_path ->
      match
        (match Mro.resolve_parent_qn ~imports:ci.ci_imports
                 ~reexport_map ~known_class_qns p_path with
         | Some _ as resolved -> resolved
         | None when scope_resolution ->
           Mro.resolve_parent_by_scope ~by_qn ~qns_by_leaf ci p_path
         | None -> None)
      with
      | None -> acc
      | Some pq ->
        (match Hashtbl.find_opt by_qn
                 (Names.Class_qn.of_string
                    (Names.Module_qn.to_string pq)) with
         | Some pci -> walk acc pci
         | None -> acc)
    ) all_new ci.ci_parent_paths
  ) [] class_infos

module FA = Graph_from_AST

(* Prefers the entity token: reshaped defs (Rust impl) carry a fake [fkind]
   but a real entity name token. *)
let func_def_file (func : FA.func_info) : string option =
  let try_tok tok =
    try Some (Fpath.to_string (Tok.file_of_tok tok))
    with Tok.NoTokenLocation _ -> None
  in
  let entity_file =
    match func.FA.entity with
    | Some { G.name = G.EN (G.Id ((_, tok), _)); _ }
    | Some { G.name = G.EN (G.IdQualified { name_last = ((_, tok), _); _ }); _ }
      -> try_tok tok
    | _ -> None
  in
  match entity_file with
  | Some _ as resolved -> resolved
  | None -> Option.map Fpath.to_string (Func_info.def_file_opt func)

(* Declared return types, in one pass over [all_funcs]:
   - free-function return (leaf key, [class_name_of_ty]);
   - method return ([(class, method)], [inner_class_name_of_ty], [this]/[self]
     resolving to the enclosing class);
   - tuple returns (Go [func F() (T, error)]) so [a, b := F()] splits into
     [(a, T)]/[(b, error)] — keyed by leaf and, for methods, by [(class, method)]. *)
let populate_returns_from_decls
    (state : Type_state.t) (all_funcs : FA.func_info list) : Type_state.t =
  let leaf_is_this_or_self name =
    match Ty_leaf.leaf_of_name name with
    | Some ("this" | "Self" | "self") -> true
    | _ -> false
  in
  List.fold_left (fun state (func : FA.func_info) ->
    let leaf = Func_info.leaf_name func.FA.fn_id in
    let method_ = Func_info.as_method func.FA.fn_id in
    let frettype = func.FA.fdef.G.frettype in
    let state =
      match leaf, Option.bind frettype Ty_leaf.class_name_of_ty with
      | Some name, Some ret_type ->
        Type_state.set_function_return state
          (Names.Method_name.of_string (fst name.IL.ident)) ret_type
      | _ -> state
    in
    let state =
      match method_ with
      | Some (cls, meth) ->
        let ret =
          match frettype with
          | Some ty ->
            (match Ty_leaf.inner_class_name_of_ty ty with
             | Some name when leaf_is_this_or_self name ->
               Some (G.Id (cls.IL.ident, G.empty_id_info ()))
             | other -> other)
          | None -> None
        in
        (match ret with
         | Some ret_type ->
           Type_state.set_method_return state
             (Names.Class_name.of_string (fst cls.IL.ident))
             (Names.Method_name.of_string (fst meth.IL.ident)) ret_type
         | None -> state)
      | None -> state
    in
    match frettype with
    | Some { G.t = G.TyTuple (_, ts, _); _ } ->
      let elems = List.map Ty_leaf.class_name_of_ty ts in
      let state =
        match leaf with
        | Some name ->
          Type_state.set_function_return_tuple state
            (Names.Method_name.of_string (fst name.IL.ident)) elems
        | None -> state
      in
      (match method_ with
       | Some (cls, meth) ->
         Type_state.set_method_return_tuple state
           (Names.Class_name.of_string (fst cls.IL.ident))
           (Names.Method_name.of_string (fst meth.IL.ident)) elems
       | None -> state)
    | _ -> state
  ) state all_funcs

(* Per-class field-type indexes: scalar ([obj.field] -> class) and slice-element
   ([for _, x := range obj.field] -> element type). *)
let build_fields_by_class_index
    ~(cfg : Index_lang_rules.t)
    (state : Type_state.t)
    (file_infos : file_info list)
  : Type_state.t * (string * string, G.name) Hashtbl.t =
  let collected = ref [] in
  let helems = Hashtbl.create 1024 in
  let add_field ~def_file cls field_name vtype =
    (match Ty_leaf.qualified_class_name_of_ty vtype with
     | Some name ->
       collected := (cls, field_name, def_file, name) :: !collected
     | None -> ());
    (match Type_infer.slice_element_of_ty vtype with
     | Some name -> Hashtbl.replace helems (cls, field_name) name
     | None -> ())
  in
  let harvest_ctor_synth_fields ~def_file cls (fdef : G.function_definition) =
    List.iter (fun (fname, fty) -> add_field ~def_file cls fname fty)
      (cfg.Index_lang_rules.class_constructor_synth_fields fdef)
  in
  let process_field_list ~def_file cls fields =
    List.iter (fun (G.F stmt) ->
      match stmt.G.s with
      | G.DefStmt (ent, G.VarDef { G.vtype = Some ty; _ }) ->
        (match entity_simple_name ent with
         | Some fname ->
           add_field ~def_file cls
             (cfg.Index_lang_rules.strip_field_sigil fname) ty
         | None -> ())
      | G.DefStmt (ent, G.FuncDef fdef)
        when (match entity_simple_name ent with
              | Some "constructor" -> true | _ -> false) ->
        harvest_ctor_synth_fields ~def_file cls fdef
      | _ -> ()
    ) fields
  in
  List.iter (fun fi ->
    List.iter (fun obs ->
      match obs with
      | Walker.Observation.Class_def { ent; cdef } ->
        (match entity_simple_name ent with
         | Some cls ->
           let _, fields, _ = cdef.G.cbody in
           process_field_list ~def_file:fi.fi_file cls fields
         | None -> ())
      | Walker.Observation.Type_def { ent;
          tdef = { G.tbody = G.NewType
              { G.t = G.TyRecordAnon (_, (_, fields, _)); _ }; _ } } ->
        (match entity_simple_name ent with
         | Some cls -> process_field_list ~def_file:fi.fi_file cls fields
         | None -> ())
      (* TS [type X = {...}] aliases parse as [OtherDef("typedef")]; index their
         fields like a class. *)
      | Walker.Observation.Other_def { ent; kind; anys }
        when String.equal kind "typedef" ->
        (match entity_simple_name ent with
         | Some cls ->
           List.iter (function
             | G.T { G.t = G.TyRecordAnon (_, (_, fields, _)); _ } ->
               process_field_list ~def_file:fi.fi_file cls fields
             | _ -> ()
           ) anys
         | None -> ())
      | _ -> ()
    ) fi.fi_observations
  ) file_infos;
  let state =
    List.fold_left (fun state (cls, field, def_file, ty) ->
      Type_state.set_field state
        (Names.Class_name.of_string cls)
        (Names.Field_name.of_string field)
        def_file
        ty
    ) state (List.rev !collected)
  in
  state, helems

(* TS-only: classes of [export]ed top-level names, so imported values keep
   their class for cross-file [x.method()] resolution. *)
let build_export_class_indexes ~(lang : Lang.t)
    ~(type_state : Type_state.t)
    (file_infos : file_info list)
  : (string, G.name) Hashtbl.t * (string * string, G.name) Hashtbl.t =
  let default_h = Hashtbl.create 1024 in
  let named_h = Hashtbl.create 4096 in
  if not (Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js) then (default_h, named_h)
  else begin
    List.iter (fun fi ->
      let file_str = Fpath.to_string fi.fi_file in
      let exported : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      let collect_exports = object
        inherit [_] G.iter_no_id_info as super
        method! visit_directive () dir =
          (match dir.G.d with
           | G.OtherDirective ((kind, _), anys) when String.equal kind "Export" ->
             List.iter (function
               | G.I (name, _) -> Hashtbl.replace exported name ()
               | _ -> ()
             ) anys
           | _ -> ());
          super#visit_directive () dir
      end in
      collect_exports#visit_program () fi.fi_ast;
      if Hashtbl.length exported > 0 then begin
        let mappings =
          Object_initialization.detect_object_initialization
            ~extra_class_names:[] fi.fi_ast lang
        in
        let lookup_local name =
          List.find_map (fun (var, cls) ->
            match var with
            | G.Id ((id_str, _), _) when String.equal id_str name -> Some cls
            | _ -> None
          ) mappings
        in
        let class_of_init (expr : G.expr) : G.name option =
          match expr.G.e with
          | G.New (_, ty, _, _) -> Ty_leaf.class_name_of_ty ty
          | G.N (G.Id ((id_str, _), _)) -> lookup_local id_str
          | G.Call ({ e = G.N (G.Id ((fname, _), _)); _ }, _) ->
            Type_state.get_function_return type_state
              (Names.Method_name.of_string fname)
          | _ -> None
        in
        let process_named_export ent cls_opt =
          match entity_simple_name ent with
          | Some name when Hashtbl.mem exported name ->
            (match cls_opt with
             | Some cls ->
               if String.equal name "default" then
                 Hashtbl.replace default_h file_str cls
               else
                 Hashtbl.replace named_h (file_str, name) cls
             | None -> ())
          | _ -> ()
        in
        List.iter (fun obs ->
          match obs with
          | Walker.Observation.Var_def { ent;
              vdef = { G.vinit = Some init; _ } } ->
            process_named_export ent (class_of_init init)
          | Walker.Observation.Class_def { ent; _ } ->
            let cls_opt = match ent.G.name with
              | G.EN name -> Some name
              | _ -> None
            in
            process_named_export ent cls_opt
          | _ -> ()
        ) fi.fi_observations
      end
    ) file_infos;
    (default_h, named_h)
  end

let build_file_funcs_index (all_funcs : FA.func_info list)
  : (string, FA.func_info list) Hashtbl.t =
  let index = Hashtbl.create 4096 in
  List.iter (fun (func : FA.func_info) ->
    let is_recognised =
      Option.is_some (Func_info.as_method func.FA.fn_id)
      || Option.is_some (Func_info.as_free func.FA.fn_id)
    in
    if is_recognised then
      match Func_info.def_file_opt func with
      | Some file ->
        let file = Fpath.to_string file in
        let cur = Option.value (Hashtbl.find_opt index file) ~default:[] in
        Hashtbl.replace index file (func :: cur)
      | None -> ()
  ) all_funcs;
  index


(* Infer return types from [return EXPR] bodies when no declared type exists;
   iterates to a fixpoint so chains like [return self.foo()] resolve. *)
let augment_return_types_from_bodies
    ~(uses_new_keyword : bool)
    ~(type_state : Type_state.t)
    (all_funcs : FA.func_info list) : Type_state.t =
  let collect_return_exprs (func : FA.func_info) : G.expr list =
    Nonfatal.catch ~default:[] (fun () ->
      let body_stmt = AST_generic_helpers.funcbody_to_stmt func.FA.fdef.G.fbody in
      Walker.fold_stmts_in_stmt ~skip_nested_fdefs:true (fun acc stmt ->
        match stmt.G.s with
        | G.Return (_, Some expr, _) -> expr :: acc
        | _ -> acc) [] body_stmt)
  in
  let leaf_fn_name (func : FA.func_info) =
    Option.map (fun name -> fst name.IL.ident) (Func_info.leaf_name func.FA.fn_id)
  in
  let class_method_of (func : FA.func_info) =
    Option.map (fun (cls, meth) -> (fst cls.IL.ident, fst meth.IL.ident))
      (Func_info.as_method func.FA.fn_id)
  in
  let step (state : Type_state.t) : Type_state.t =
    List.fold_left (fun state (func : FA.func_info) ->
      let already_known =
        match class_method_of func with
        | Some (cls, meth) ->
          Type_state.get_method_return state
            (Names.Class_name.of_string cls)
            (Names.Method_name.of_string meth)
          |> Option.is_some
        | None ->
          (match leaf_fn_name func with
           | Some name ->
             Type_state.get_function_return state
               (Names.Method_name.of_string name)
             |> Option.is_some
           | None -> true)
      in
      let has_decl =
        match func.FA.fdef.G.frettype with Some _ -> true | None -> false
      in
      if has_decl || already_known then state
      else
        let rets = collect_return_exprs func in
        let inferred =
          List.filter_map (fun expr ->
            Type_infer.infer_expr_type ~max_depth:6 ~uses_new_keyword
              ~type_state:state expr
          ) rets
        in
        match inferred with
        | [] -> state
        | ty :: _ ->
          (match class_method_of func with
           | Some (cls, meth) ->
             Type_state.set_method_return state
               (Names.Class_name.of_string cls)
               (Names.Method_name.of_string meth) ty
           | None ->
             (match leaf_fn_name func with
              | Some name ->
                Type_state.set_function_return state
                  (Names.Method_name.of_string name) ty
              | None -> state))
    ) state all_funcs
  in
  let final, _iters =
    Fixpoint.run ~equal:Type_state.equal ~step
      ~max_iterations:Limits_semgrep.projidx_RETURN_TYPES_MAX_ITERS type_state
  in
  final

(* [(callee_class, callee_method, arg_idx) -> type] of caller-supplied arg types,
   so [self.X = param] can be typed from what callers pass. *)
let build_caller_arg_types
    ~(uses_new_keyword : bool)
    ~(type_state : Type_state.t)
    (file_infos : file_info list)
  : (string * string * int, G.name) Hashtbl.t =
  let arg_types = Hashtbl.create 8192 in
  let infer expr =
    Type_infer.infer_expr_type ~max_depth:6 ~uses_new_keyword ~type_state expr
  in
  List.iter (fun fi ->
    let visitor = object
      inherit [_] G.iter_no_id_info as super
      method! visit_expr () expr =
        (match expr.G.e with
         | G.Call (callee, args) ->
           let callee_resolved : (string * string) option =
             match callee.G.e with
             | G.DotAccess _ ->
               Option.bind
                 (Type_infer.method_call_target ~type_recv:infer callee)
                 (fun (recv, meth) ->
                    Option.map (fun cls -> (cls, meth)) (Ty_leaf.leaf_of_name recv))
             (* Bare-name call [Cls(args)]: treat as ctor [(Cls, "__init__")]. *)
             | G.N (G.Id ((cls, _), _)) ->
               Some (cls, "__init__")
             | _ -> None
           in
           (match callee_resolved with
            | Some (cls, meth) ->
              List.iteri (fun i arg ->
                match arg with
                | G.Arg expr | G.ArgKwd (_, expr) | G.ArgKwdOptional (_, expr) ->
                  (match infer expr with
                   | Some ty ->
                     if not (Hashtbl.mem arg_types (cls, meth, i)) then
                       Hashtbl.replace arg_types (cls, meth, i) ty
                   | None -> ())
                | _ -> ()
              ) (Tok.unbracket args)
            | None -> ())
         | _ -> ());
        super#visit_expr () expr
    end in
    run_visit ~on:fi.fi_file (fun () -> visitor#visit_program () fi.fi_ast)
  ) file_infos;
  arg_types

(* Module-level singleton bindings keyed by full qn (module_qn + var_name):
   [x = SomeClass()] at module scope lets importers' [x.method()] resolve.
   The full-qn key matches [collect_imports]'s [fi_imports] targets. *)
let build_module_singleton_types
    ~(uses_new_keyword : bool)
    (state : Type_state.t)
    (file_infos : file_info list)
  : Type_state.t =
  let module_level_assigns_of_file (fi : file_info) =
    let mp = fi.fi_module_path in
    let collect acc stmt =
      match stmt.G.s with
      | G.DefStmt (ent, G.VarDef { G.vinit = Some rhs; _ }) ->
        (match ent.G.name with
         | G.EN (G.Id ((name, _), _)) -> (mp, name, rhs) :: acc
         | _ -> acc)
      | G.ExprStmt ({ G.e = G.Assign (
          { G.e = G.N (G.Id ((name, _), _)); _ }, _, rhs); _ }, _) ->
        (mp, name, rhs) :: acc
      | _ -> acc
    in
    Nonfatal.catch ~default:[] (fun () ->
      List.fold_left (fun acc top ->
        Walker.fold_stmts_in_stmt ~skip_nested_fdefs:true collect acc top
      ) [] fi.fi_ast
      |> List.rev)
  in
  let collected = List.concat_map module_level_assigns_of_file file_infos in
  List.fold_left (fun state (mp, name, rhs) ->
    match Type_infer.infer_expr_type ~max_depth:6 ~uses_new_keyword
            ~type_state:state rhs with
    | Some ty ->
      let qn = Names.Module_qn.concat mp name in
      Type_state.set_module_singleton state qn ty
    | None -> state
  ) state collected

(* Augment fields from [this.X = RHS] in class methods so [self.X.method()]
   chains resolve; [caller_arg_types] types [self.X = param] from callers;
   [ctor_param_promotion] (PHP 8) registers typed ctor params as fields. *)
let augment_fields_from_self_assignments
    ~(lang : Lang.t)
    ~(uses_new_keyword : bool)
    ~(caller_arg_types : (string * string * int, G.name) Hashtbl.t)
    ~(cfg : Index_lang_rules.t)
    ~(type_state : Type_state.t)
    (all_funcs : FA.func_info list) : Type_state.t =
  let strip = cfg.Index_lang_rules.strip_field_sigil in
  let seen : (string * string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let already_known cls field =
    Hashtbl.mem seen (cls, field) ||
    (Type_state.get_field type_state
       (Names.Class_name.of_string cls)
       (Names.Field_name.of_string field)
     |> Option.is_some)
  in
  let collected =
    List.fold_left (fun outer_acc (func : FA.func_info) ->
    match Func_info.as_method func.FA.fn_id with
    | Some (cls_il, meth_il) ->
      let cls = fst cls_il.IL.ident in
      let meth = fst meth_il.IL.ident in
      let def_file =
        Option.value (Func_info.def_file_opt func) ~default:(Fpath.v "<fake>")
      in
      let param_types : (string, G.name) Hashtbl.t = Hashtbl.create 4 in
      List.iteri (fun i param ->
        match param with
        | G.Param { pname = Some (pn, _); ptype = Some pty; _ }
        | G.ParamReceiver { pname = Some (pn, _); ptype = Some pty; _ } ->
          (match Ty_leaf.class_name_of_ty pty with
           | Some name -> Hashtbl.replace param_types pn name
           | None ->
             (match Hashtbl.find_opt caller_arg_types (cls, meth, i) with
              | Some name -> Hashtbl.replace param_types pn name
              | None -> ()))
        | G.Param { pname = Some (pn, _); ptype = None; _ } ->
          (match Hashtbl.find_opt caller_arg_types (cls, meth, i) with
           | Some name -> Hashtbl.replace param_types pn name
           | None -> ())
        | _ -> ()
      ) (Tok.unbracket func.FA.fdef.G.fparams);
      (* PHP 8 ctor property promotion: the parser drops the visibility
         modifier, so every typed ctor param is a candidate field. *)
      let outer_acc =
        if cfg.Index_lang_rules.ctor_param_promotion
           && Object_initialization.is_constructor lang meth (Some cls) then
          List.fold_left (fun acc param ->
            match param with
            | G.Param { G.pname = Some (pn, _); ptype = Some pty; _ }
            | G.ParamReceiver { G.pname = Some (pn, _); ptype = Some pty; _ } ->
              (match Ty_leaf.class_name_of_ty pty with
               | Some ty ->
                 let field = strip pn in
                 if already_known cls field then acc
                 else begin
                   Hashtbl.replace seen (cls, field) ();
                   (cls, field, def_file, ty) :: acc
                 end
               | None -> acc)
            | _ -> acc
          ) outer_acc (Tok.unbracket func.FA.fdef.G.fparams)
        else outer_acc
      in
      let body =
        Nonfatal.catch ~default:None (fun () ->
          Some (AST_generic_helpers.funcbody_to_stmt func.FA.fdef.G.fbody))
      in
      (match body with
       | None -> outer_acc
       | Some body_stmt ->
         (* Publish param classes onto the body's [id_type] so the typer
            resolves param-derived rhs exprs. *)
         let param_facts =
           Hashtbl.fold (fun pname ty acc ->
             (G.Id ((pname, Tok.unsafe_fake_tok pname), G.empty_id_info ()),
              ty) :: acc
           ) param_types []
         in
         Object_initialization.stamp_id_types param_facts [body_stmt];
         Nonfatal.catch ~default:outer_acc (fun () ->
           Walker.fold_exprs_in_stmt ~skip_nested_fdefs:true (fun acc expr ->
             match expr.G.e with
             | G.Assign (
                 { G.e = G.DotAccess (
                     { G.e = G.IdSpecial ((G.This | G.Self), _); _ }, _,
                     G.FN (G.Id ((field_name, _), _))); _ },
                 _, rhs)
               when not (already_known cls (strip field_name)) ->
               let field_name = strip field_name in
               let rhs_ty =
                 match rhs.G.e with
                 | G.N (G.Id ((vn, _), _)) ->
                   (match Hashtbl.find_opt param_types vn with
                    | Some _ as resolved -> resolved
                    | None ->
                      Type_infer.infer_expr_type ~max_depth:6 ~uses_new_keyword
                        ~type_state rhs)
                 | _ ->
                   Type_infer.infer_expr_type ~max_depth:6 ~uses_new_keyword
                     ~type_state rhs
               in
               (match rhs_ty with
                | Some ty ->
                  Hashtbl.replace seen (cls, field_name) ();
                  (cls, field_name, def_file, ty) :: acc
                | None -> acc)
             | _ -> acc) outer_acc body_stmt))
    | None -> outer_acc
  ) [] all_funcs
  in
  List.fold_left (fun state (cls, field, def_file, ty) ->
    Type_state.set_field state
      (Names.Class_name.of_string cls)
      (Names.Field_name.of_string field)
      def_file
      ty
  ) type_state (List.rev collected)

(* Infer var classes from assignment/def/range statements and stamp them onto
   [id_type]; iterate so one pass's stamps unlock the next pass's inferences
   (the typer reads receiver types off [id_type]). *)
let stamp_var_types_from_bodies
    ~(uses_new_keyword : bool)
    ~(type_state : Type_state.t)
    ~(slice_element_of_field : (string * string, G.name) Hashtbl.t)
    (ast : G.program) : unit =
  let pass () : (G.name * G.name) list =
    let known (name : G.name) = Type_infer.declared_class_of_name name <> None in
    let fact lhs rhs acc =
      if known lhs then acc
      else
        match Type_infer.infer_expr_type ~uses_new_keyword ~type_state rhs with
        | None -> acc
        | Some ty -> (lhs, ty) :: acc
    in
    let rec tuple_facts lhs_names elem_types acc =
      match lhs_names, elem_types with
      | lhs :: lrest, Some ty :: erest ->
        let acc = if known lhs then acc else (lhs, ty) :: acc in
        tuple_facts lrest erest acc
      | _ :: lrest, None :: erest -> tuple_facts lrest erest acc
      | _ -> acc
    in
    let call_tuple_facts (lhs_names : G.name list) (rhs : G.expr) acc =
      match rhs.G.e with
      | G.Call ({ G.e = G.N (G.Id ((fname, _), _)); _ }, _) ->
        (match Type_state.get_function_return_tuple type_state
                 (Names.Method_name.of_string fname) with
         | None -> acc
         | Some elem_types -> tuple_facts lhs_names elem_types acc)
      | G.Call ({ G.e = G.DotAccess (
          { G.e = G.N obj_name; _ }, _,
          G.FN (G.Id ((mname, _), _))); _ }, _) ->
        (match Option.bind (Type_infer.declared_class_of_name obj_name)
                 Ty_leaf.leaf_of_name with
         | None -> acc
         | Some cls ->
           (match Type_state.get_method_return_tuple type_state
                    (Names.Class_name.of_string cls)
                    (Names.Method_name.of_string mname) with
            | None -> acc
            | Some elem_types -> tuple_facts lhs_names elem_types acc))
      | _ -> acc
    in
    let extract_tuple_names (expr : G.expr) : G.name list option =
      match expr.G.e with
      | G.Container (G.Tuple, (_, items, _)) ->
        let names =
          List.filter_map (fun (it : G.expr) ->
            match it.G.e with
            | G.N name -> Some name
            | _ -> None) items
        in
        if names = [] then None else Some names
      | _ -> None
    in
    let range_facts pat range_expr acc =
      let iter_names =
        match pat with
        | G.PatTuple (_, items, _) ->
          List.filter_map (fun (it : G.pattern) ->
            match it with
            | G.PatId (id, info) -> Some (G.Id (id, info))
            | _ -> None
          ) items
        | G.PatId (id, info) -> [G.Id (id, info)]
        | _ -> []
      in
      let elem_class : G.name option =
        match range_expr.G.e with
        | G.DotAccess ({ G.e = G.N obj_name; _ }, _,
                       G.FN (G.Id ((field, _), _))) ->
          (match Option.bind (Type_infer.declared_class_of_name obj_name)
                   Ty_leaf.leaf_of_name with
           | Some cls -> Hashtbl.find_opt slice_element_of_field (cls, field)
           | None -> None)
        | _ -> None
      in
      (* Bind the LAST iter var (Go's value position for 1- and 2-var range). *)
      match elem_class, List.rev iter_names with
      | Some elem, last :: _ when not (known last) -> (last, elem) :: acc
      | _ -> acc
    in
    Walker.fold_stmts_in_program (fun acc stmt ->
      match stmt.G.s with
      (* Go's [s := f()] lowers to AssignOp(N, (Eq, _), _), not Assign. *)
      | G.ExprStmt ({ G.e = G.Assign ({ G.e = G.N lhs; _ }, _, rhs); _ }, _)
      | G.ExprStmt ({ G.e = G.AssignOp ({ G.e = G.N lhs; _ }, _, rhs); _ }, _) ->
        fact lhs rhs acc
      | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _)
      | G.ExprStmt ({ G.e = G.AssignOp (lhs, _, rhs); _ }, _) ->
        (match extract_tuple_names lhs with
         | Some ns -> call_tuple_facts ns rhs acc
         | None -> acc)
      | G.DefStmt (ent, G.VarDef { G.vinit = Some rhs; _ }) ->
        (match ent.G.name with
         | G.EN name -> fact name rhs acc
         (* Rust [let x = ...] parses as [EPattern(PatId)], not [EN(Id)]. *)
         | G.EPattern (G.PatId (id, info)) ->
           fact (G.Id (id, info)) rhs acc
         | _ -> acc)
      | G.For (_, G.ForEach (pat, _, range_expr), _) ->
        range_facts pat range_expr acc
      | _ -> acc
    ) [] ast
  in
  let rec loop i =
    if i >= Limits_semgrep.projidx_OBJECT_MAPPINGS_MAX_ITERS then ()
    else
      match pass () with
      | [] -> ()
      | facts ->
        Object_initialization.stamp_id_types (List.rev facts) ast;
        loop (i + 1)
  in
  loop 0

(* Maximum number of files processed per parallel work unit.  Batching
   amortises Domainslib dispatch overhead over many small per-file tasks
   while keeping [chunksize = 1] — one task per thread — so the
   [Memprof_limits]-based memory limit and timeout stay sound (see the
   warning on [Domainslib_.parmap]). *)
let per_file_batch_size = 500

(* Split a list into chunks of at most [n] elements. *)
let rec chunks (n : int) (xs : 'a list) : 'a list list =
  match xs with
  | [] -> []
  | _ ->
    let len = List.length xs in
    let take = min n len in
    let batch = List_.take_safe take xs in
    let rest = List_.drop take xs in
    batch :: chunks n rest

(* Run [fn] on each item, in parallel when [ncores > 1] and there is
   more than one batch, sequentially otherwise.  Each parallel work
   unit is a whole batch; inside a batch, a per-item failure becomes
   an [Error] for the caller to log and skip, while the fatal trio is
   re-raised. *)
let run_per_file (caps : < Cap.fork >) ~(ncores : int)
    (fn : 'a -> 'b) (items : 'a list)
    : ('b, string) Result.t list =
  (* [fn] fixes the type: one [Ok]/[Error] per item. *)
  let run_one item =
    try Ok (fn item)
    with
    | (Out_of_memory | Stack_overflow | Time_limit.Timeout _) as exn ->
      Exception.catch_and_reraise exn
    | exn -> Error (Printexc.to_string exn)
  in
  let batches = chunks per_file_batch_size items in
  let n = List.length batches in
  if ncores <= 1 || n <= 1 then List_.map run_one items
  else
    Domainslib_.parmap caps
      ~num_domains:(min ncores n) ~chunksize:1
      ~exception_handler:(fun _ exn ->
        match Exception.get_exn exn with
        | Out_of_memory | Stack_overflow | Time_limit.Timeout _ ->
          Exception.reraise exn
        | exn -> Printexc.to_string exn)
      (fun batch -> List_.map run_one batch)
      batches
    |> List.concat_map (function
        | Ok batch_results -> batch_results
        | Error msg -> [ Error msg ])

let build_project_call_graph (caps : < Cap.fork >)
    ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(ncores : int)
    ?(class_infos = [])
    ?(reexport_map = Hashtbl.create 0)
    (file_infos : file_info list) : Call_graph.G.t =
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
  (* [child_simple_name -> parent_simple_name] resolves [super().X()] to the
     parent's method (single-inheritance approximation: first parent's leaf). *)
  let type_state =
    List.fold_left (fun state ci ->
      let child =
        match Names.Class_qn.leaf ci.ci_qn with
        | "" -> None
        | leaf -> Some leaf
      in
      let parent =
        match ci.ci_parent_paths with
        | first :: _ ->
          (match List.rev first with
           | last :: _ -> Some last
           | [] -> None)
        | [] -> None
      in
      match child, parent with
      | Some child_name, Some parent_name ->
        Type_state.set_parent state
          (Names.Class_name.of_string child_name)
          (Names.Class_name.of_string parent_name)
      | _ -> state
    ) Type_state.empty class_infos
  in
  let graph = Call_graph.G.create () in
  (* Per-file synthetic [<top_level>] node for module-scope calls, so the dump
     keeps the caller's file/line. *)
  let top_level_nodes : (string, Function_id.t) Hashtbl.t = Hashtbl.create 4096 in
  let top_level_node_for (file : Fpath.t) : Function_id.t =
    Hashtbl.find top_level_nodes (Fpath.to_string file)
  in

  let placeholder_fdef tok : G.function_definition = {
    G.fkind = (G.Method, tok);
    fparams = Tok.unsafe_fake_bracket [];
    frettype = None;
    fbody = G.FBNothing;
  } in
  let synth_func_info_for_class
      (class_il : IL.name) (name, tok) : FA.func_info =
    let m_il = IL.{
      ident = (name, tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    } in
    { FA.fn_id = Func_info.method_id ~cls:class_il ~meth:m_il;
      entity = None;
      fdef = placeholder_fdef tok }
  in
  let phase1_per_file (fi : file_info) : FA.func_info list =
    let acc =
      List.fold_left (fun (acc : FA.func_info list) obs ->
        match obs with
        | Walker.Observation.Func_def { opt_ent; parent_path; fdef } ->
          if skip_anon opt_ent then acc
          else
            (match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
             | Some fn_id -> { FA.fn_id; entity = opt_ent; fdef } :: acc
             | None -> acc)
        | _ -> acc
      ) [] fi.fi_observations
    in
    (* Go interface methods come as a [TypeDef] with [TyRecordAnon(Interface)],
       not a [ClassDef], so attribute them to the interface. *)
    let acc =
      if not (Lang.equal lang Lang.Go) then acc
      else
        List.fold_left (fun acc obs ->
          match obs with
          | Walker.Observation.Type_def { ent; tdef } ->
            (match tdef with
             | { G.tbody = G.NewType
                   { G.t = G.TyRecordAnon ((G.Interface, _),
                                              (_, fields, _)); _ } } ->
               (match ent.G.name with
                | G.EN ((G.Id _ | G.IdQualified _) as iface_name) ->
                  let iface_il = AST_to_IL.var_of_name iface_name in
                  List.fold_left (fun acc field ->
                    match field with
                    | G.F { G.s = G.DefStmt (m_ent, G.FuncDef m_fdef); _ } ->
                      (match m_ent.G.name with
                       | G.EN ((G.Id _ | G.IdQualified _) as m_name) ->
                         let m_il = AST_to_IL.var_of_name m_name in
                         let fn_id = Func_info.method_id ~cls:iface_il ~meth:m_il in
                         { FA.fn_id;
                           entity = Some m_ent;
                           fdef = m_fdef } :: acc
                       | _ -> acc)
                    | _ -> acc
                  ) acc fields
                | _ -> acc)
             | _ -> acc)
          | _ -> acc
        ) acc fi.fi_observations
    in
    List.fold_left (fun acc obs ->
      match obs with
      | Walker.Observation.Class_def { ent; cdef } ->
        (match Visit_function_defs.entity_to_il_name ent with
         | Some class_il ->
           List.fold_left (fun acc pair ->
             synth_func_info_for_class class_il pair :: acc
           ) acc (cfg.Index_lang_rules.class_body_synth_methods cdef)
         | None -> acc)
      | _ -> acc
    ) acc fi.fi_observations
  in
  let per_file_funcs = run_per_file caps ~ncores phase1_per_file file_infos in
  let all_funcs =
    List.concat_map (function
      | Ok fs -> fs
      | Error msg ->
        Log.warn (fun m -> m "[skip] phase 1 worker failed: %s" msg);
        []) per_file_funcs
  in
  List.iter (fun (func : FA.func_info) ->
    match FA.fn_id_to_node func.FA.fn_id with
    | Some node -> Call_graph.G.add_vertex graph node
    | None -> ()
  ) all_funcs;

  let project_class_names : G.name list =
    let seen : (string, unit) Hashtbl.t = Hashtbl.create 8192 in
    List.fold_left (fun acc fi ->
      List.fold_left (fun acc name ->
        match name with
        | G.Id ((name_str, _), _) when not (Hashtbl.mem seen name_str) ->
          Hashtbl.replace seen name_str ();
          name :: acc
        | _ -> acc
      ) acc (Object_initialization.collect_class_names fi.fi_ast)
    ) [] file_infos
  in
  Log.debug (fun m -> m "Project class names: %d (interfile object_mappings)"
    (List.length project_class_names));

  let funcs_by_name : (string, FA.func_info list) Hashtbl.t =
    Hashtbl.create (List.length all_funcs * 2)
  in
  List.iter (fun (func : FA.func_info) ->
    let add_name name =
      let cur = Option.value (Hashtbl.find_opt funcs_by_name name) ~default:[] in
      Hashtbl.replace funcs_by_name name (func :: cur)
    in
    let leaf = Option.map (fun name -> fst name.IL.ident) (Func_info.leaf_name func.FA.fn_id) in
    Option.iter add_name leaf;
    (* Named lambdas have a synthetic [_tmp_lambda] leaf; also index under the
       binding var name so [handler(...)] resolves. *)
    (match func.FA.entity with
     | Some ent ->
       (match entity_simple_name ent with
        | Some entity_name when (match leaf with
                                 | Some leaf_name -> not (String.equal leaf_name entity_name)
                                 | None -> true) ->
            add_name entity_name
        | _ -> ())
     | None -> ())
  ) all_funcs;
  let project_funcs_by_name = funcs_by_name in

  (* Class -> methods index, so a file that knows a class can resolve [d.speak]
     even if "speak" never appears in it. *)
  let type_state =
    List.fold_left (fun state (func : FA.func_info) ->
      match Func_info.as_method func.FA.fn_id with
      | Some (cls, _) ->
        Type_state.add_method state
          (Names.Class_name.of_string (fst cls.IL.ident)) func
      | None -> state
    ) type_state all_funcs
  in
  let type_state =
    Go_inheritance.lift_embedded_interfaces ~lang file_infos type_state
  in
  let type_state =
    if cfg.Index_lang_rules.walks_inheritance then
      Mro.inherit_into_type_state ~reexport_map ~class_infos
        ~func_def_file type_state
    else type_state
  in

  (* TS/JS class-body aliases [class C { static foo = importedFn }]. *)
  let type_state =
    Ts_class_aliases.add_class_body_aliases
      ~lang ~project_funcs_by_name file_infos type_state
  in

  let type_state = populate_returns_from_decls type_state all_funcs in
  let type_state, slice_element_of_field =
    build_fields_by_class_index ~cfg type_state file_infos in
  (* Cross-type inference fixpoint: alternate body-return-types and
     self-assignment field-types until neither adds anything.  Rebuild
     [caller_arg_types] between passes so fresh return types feed the next;
     compare on [Type_state] only (the Hashtbl is derived). *)
  let uses_new_keyword = FA.uses_new_keyword lang in
  let outer_step (ts, _car) =
    let ts =
      augment_return_types_from_bodies ~uses_new_keyword ~type_state:ts all_funcs
    in
    let car =
      build_caller_arg_types ~uses_new_keyword ~type_state:ts file_infos
    in
    let ts =
      augment_fields_from_self_assignments ~lang ~uses_new_keyword
        ~caller_arg_types:car ~cfg ~type_state:ts all_funcs
    in
    (ts, car)
  in
  let outer_equal (a, _) (b, _) = Type_state.equal a b in
  let (type_state, caller_arg_types), outer_iters =
    Fixpoint.run
      ~equal:outer_equal
      ~step:outer_step
      ~max_iterations:Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES
      (type_state, Hashtbl.create 0)
  in
  Log.debug (fun m -> m "Body-inferred type fixpoint: %d outer passes, %d caller-arg-types"
    outer_iters (Hashtbl.length caller_arg_types));
  let type_state =
    build_module_singleton_types ~uses_new_keyword type_state file_infos
  in
  let default_export_class, named_export_classes =
    build_export_class_indexes ~lang ~type_state file_infos in

  (* CommonJS [module.exports = ...] default-export fn.  See [Cjs_exports]. *)
  let default_export_fn = Cjs_exports.build_default_export_fn ~lang file_infos in
  Log.debug (fun m -> m "default_export_fn: %d files"
    (Hashtbl.length default_export_fn));
  let file_funcs_index = build_file_funcs_index all_funcs in
  let path_suffix_index : (string, string list) Hashtbl.t option =
    if Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js then
      Some (build_path_suffix_index
              (List.map (fun fi -> Fpath.to_string fi.fi_file) file_infos))
    else None
  in
  Log.debug (fun m -> m "Exports: %d default / %d named.  File-funcs index: %d files.  Suffix index: %d entries"
    (Hashtbl.length default_export_class)
    (Hashtbl.length named_export_classes)
    (Hashtbl.length file_funcs_index)
    (match path_suffix_index with Some index -> Hashtbl.length index | None -> 0));

  (* Project-wide free-function indexes.  See [Func_index]. *)
  let project_funcs_by_package = Func_index.build_by_package ~cfg all_funcs in
  Log.debug (fun m -> m "Per-package func index: %d packages (Per_directory only)"
    (Hashtbl.length project_funcs_by_package));
  let project_funcs_by_module =
    Func_index.build_by_module ~cfg ~file_infos all_funcs
  in
  Log.debug (fun m -> m "Per-module func index: %d modules (Per_file only)"
    (Hashtbl.length project_funcs_by_module));

  (* Re-export pass for [`Per_file] languages.  See [Reexports]. *)
  if cfg.Index_lang_rules.unqualified_scope = `Per_file then
    Reexports.resolve_into_module_index
      ~project_funcs_by_module file_infos
    |> List.iter (fun (qn, funcs) ->
         Hashtbl.replace project_funcs_by_module qn funcs);

  (* Defining-file -> package module qn; disambiguates same-basename packages
     for method-homonym resolution. *)
  let file_module_qn : (string, Names.Module_qn.t) Hashtbl.t =
    let index = Hashtbl.create (List.length file_infos) in
    List.iter (fun (fi : file_info) ->
      Hashtbl.replace index (Fpath.to_string fi.fi_file) fi.fi_module_path
    ) file_infos;
    index
  in

  (* Directory and per-file visible-names sets.  See [Visibility]. *)
  let dir_visible_names =
    Visibility.build_dir_index ~cfg file_infos
  in
  let visible_names_for_file =
    Visibility.for_file ~cfg ~dir_visible_names ~project_funcs_by_module
  in

  (* Per-AST [extract_calls] into an edge list; graph mutated only in the
     merge step. *)
  let pipeline_ctx : Pipeline.ctx =
    { Pipeline.lang;
      cfg;
      type_state;
      all_funcs;
      project_funcs_by_name;
      project_funcs_by_module;
      file_module_qn;
      project_funcs_by_package;
      project_class_names;
      file_funcs_index;
      default_export_class;
      named_export_classes;
      default_export_fn;
      path_suffix_index;
      slice_element_of_field;
      top_level_node_for;
      visible_names_for_file;
      stamp_var_types =
        (fun ~type_state ~slice_element_of_field ast ->
          stamp_var_types_from_bodies ~uses_new_keyword
            ~type_state ~slice_element_of_field ast);
      resolve_ts_specifier =
        (fun ~path_suffix_index ~current_file specifier ->
          resolve_ts_specifier ~path_suffix_index ~current_file specifier);
    }
  in
  let edges_for_file fi = Pipeline.edges_for_file pipeline_ctx fi in
  (* Pre-populate [<top_level>] nodes BEFORE the parallel phase: the table and
     graph are read-only across domains after this. *)
  List.iter (fun (fi : file_info) ->
    let key = Fpath.to_string fi.fi_file in
    if not (Hashtbl.mem top_level_nodes key) then begin
      let node =
        Function_id.of_il_name (FA.top_level_name_of_ast fi.fi_ast)
      in
      Hashtbl.replace top_level_nodes key node;
      Call_graph.G.add_vertex graph node
    end
  ) file_infos;
  (* Largest-first so megafiles don't stall the tail on one worker; stat once
     (decorate-sort-undecorate) to keep the comparator pure. *)
  let file_size_of fi =
    Nonfatal.catch ~default:0 (fun () ->
      (Unix.stat (Fpath.to_string fi.fi_file)).Unix.st_size)
  in
  let file_infos =
    file_infos
    |> List_.map (fun fi -> (file_size_of fi, fi))
    |> List.sort (fun (a, _) (b, _) -> Int.compare b a)
    |> List_.map snd
  in
  let per_file_edges = run_per_file caps ~ncores edges_for_file file_infos in
  List.iter (function
    | Ok edges ->
      List.iter (fun (src, dst, call_tok) ->
        Call_graph.add_edge graph ~src ~dst ~call_tok)
        edges
    | Error msg ->
      Log.warn (fun m -> m "[skip] phase 2 worker failed: %s" msg))
    per_file_edges;
  (* Interface dispatch edges.  See [Structural_dispatch]. *)
  let n_dispatch =
    Structural_dispatch.emit_dispatch_edges
      ~cfg ~type_state ~func_def_file ~class_infos ~graph
  in
  if n_dispatch > 0 then
    Log.debug (fun m -> m "Interface dispatch: emitted %d Dispatch edges"
      n_dispatch);
  graph

let project_root_abs_of (project_root : Fpath.t) : Fpath.t =
  if Fpath.is_abs project_root then project_root
  else Fpath.(v (Sys.getcwd ()) // project_root) |> Fpath.normalize

let run_pipeline (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int * file_info list =
  let cfg = Index_lang_rules.for_lang lang in
  (* [discover_excludes] so the CLI and embedded engine index the same files. *)
  let excludes =
    excludes @ cfg.Index_lang_rules.discover_excludes ~project_root
  in
  let files = Discover.discover_files ~targeting_conf
    ~lang ~project_root ~includes ~excludes in
  let n_total = List.length files in
  Log.info (fun m -> m "Discovered %d %s files. Parsing with %d domain(s)..."
    n_total (Lang.to_string lang) ncores);
  (* Rust-only: rewrite [impl Foo {...}] ([OtherDef("Impl")]) into a [ClassDef]
     so the walkers see the methods. *)
  (* Rust only: impl methods must look like class methods to every later
     pass, so the STORED ast is reshaped too ([cfg.class_def_reshape]).
     Go/Ruby also wire the hook but reshape only the collector's view —
     their stored TypeDefs/ModuleDefs must survive for the embedding and
     module walks. *)
  let reshape_class_defs (ast : G.program) : G.program =
    if not (Lang.equal lang Lang.Rust) then ast
    else
      List.map (fun (stmt : G.stmt) ->
        match stmt.G.s with
        | G.DefStmt (ent, def_kind) ->
          (match cfg.Index_lang_rules.class_def_reshape ent def_kind with
           | Some (new_ent, new_kind) ->
             { stmt with G.s = G.DefStmt (new_ent, new_kind) }
           | None -> stmt)
        | _ -> stmt
      ) ast
  in
  (* Absolutize paths: interface dispatch's [family_key] needs consistent
     directory prefixes. *)
  let project_root_abs = project_root_abs_of project_root in
  let absolutize (file : Fpath.t) : Fpath.t =
    if Fpath.is_abs file then file
    else Fpath.(project_root_abs // file) |> Fpath.normalize
  in
  (* Go package identity from [go.mod]/[go.work]; empty for non-Go, so [mp]
     falls back to the path-derived default. *)
  let go_modules =
    if Lang.equal lang Lang.Go
    then Go_modules.discover ~project_root:project_root_abs
           (List.map absolutize files)
    else Go_modules.empty
  in
  let process file =
    let file = absolutize file in
    let ast =
      Parse_target.parse_and_resolve_name_warn_if_partial lang file
    in
    let ast = reshape_class_defs ast in
    let mp =
      match Go_modules.import_path_of_dir go_modules (Fpath.parent file) with
      | Some path -> Names.Module_qn.of_string path
      | None -> module_path ~cfg ~project_root ~ast file
    in
    collect_in_ast ~cfg ~lang ~module_path:mp ~file ast
  in
  let results =
    if ncores <= 1 then
      List.map (fun file ->
        try Ok (process file)
        with
        | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as exn ->
          raise exn
        | exn -> Error (file, Printexc.to_string exn)
      ) files
    else
      Domainslib_.parmap caps
        ~num_domains:ncores
        ~chunksize:1
        ~exception_handler:(fun file exc ->
          (match Exception.get_exn exc with
           | Out_of_memory | Stack_overflow | Time_limit.Timeout _ ->
             Exception.reraise exc
           | exn -> (file, Printexc.to_string exn)))
        process
        files
  in
  let scanned, skipped, all_entries, all_classes, all_files, _ =
    List.fold_left (fun (sc, sk, es, cs, fis, n_logged) -> function
      | Ok (entries, class_infos, fi) ->
        (sc + 1, sk,
         List.rev_append entries es,
         List.rev_append class_infos cs,
         fi :: fis,
         n_logged)
      | Error (file, msg) ->
        if n_logged < 5 then
          Log.warn (fun m -> m "[skip] %s: %s" (Fpath.to_string file) msg);
        (sc, sk + 1, es, cs, fis, n_logged + 1)
    ) (0, 0, [], [], [], 0) results
  in
  let reexport_map = build_reexport_map ~cfg all_files in
  Log.debug (fun m -> m "Re-export map: %d entries (lang has_reexports=%b)"
    (Hashtbl.length reexport_map) cfg.Index_lang_rules.has_reexports);
  let wrappers : (string, dataclass_wrapper) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun fi ->
    List.iter (fun (wrapper : Index_lang_rules.wrapper) ->
      Hashtbl.replace wrappers wrapper.w_simple_name wrapper
    ) fi.fi_dataclass_wrappers
  ) all_files;
  Log.debug (fun m -> m "Wrappers: %d" (Hashtbl.length wrappers));
  let synth_from_wrappers =
    dataclass_wrapper_synth_entries ~cfg ~wrappers all_entries all_classes
  in
  Log.debug (fun m -> m "Wrapper synthesis: %d dunders emitted"
    (List.length synth_from_wrappers));
  let entries_pre_mro = all_entries @ synth_from_wrappers in
  let inherited =
    if cfg.Index_lang_rules.walks_inheritance then
      mro_inherited_entries ~reexport_map
        ~scope_resolution:cfg.Index_lang_rules.mro_uses_scope_resolution
        entries_pre_mro all_classes
    else []
  in
  Log.debug (fun m -> m "Inheritance walk: attached %d inherited methods (lang walks_inheritance=%b)"
    (List.length inherited) cfg.Index_lang_rules.walks_inheritance);
  let final_entries = entries_pre_mro @ inherited in
  let graph =
    build_project_call_graph caps ~cfg ~lang ~ncores
      ~class_infos:all_classes ~reexport_map all_files
  in
  Log.info (fun m -> m "Call graph: %d vertices, %d edges"
    (Call_graph.G.nb_vertex graph) (Call_graph.G.nb_edges graph));
  (final_entries, graph, scanned, skipped, all_files)

let collect (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int =
  let (entries, graph, scanned, skipped, _all_files) =
    run_pipeline caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes ~excludes ()
  in
  (entries, graph, scanned, skipped)

let collect_resolved (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : Call_graph.G.t * (string, G.program) Hashtbl.t =
  let project_root_abs = project_root_abs_of project_root in
  let absnorm (file : Fpath.t) : string =
    (if Fpath.is_abs file then file else Fpath.(project_root_abs // file))
    |> Fpath.normalize |> Fpath.to_string
  in
  let (_entries, graph, _scanned, _skipped, all_files) =
    run_pipeline caps ~targeting_conf ~lang ~project_root:project_root_abs
      ~ncores ~includes ~excludes ()
  in
  let tbl = Hashtbl.create (List.length all_files) in
  List.iter (fun (fi : file_info) ->
    Hashtbl.replace tbl (absnorm fi.fi_file) fi.fi_ast)
    all_files;
  (graph, tbl)

let resolve_ast_for_file (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(target : Fpath.t) ()
  : G.program option =
  let project_root_abs = project_root_abs_of project_root in
  let target_key =
    (if Fpath.is_abs target then target else Fpath.(project_root_abs // target))
    |> Fpath.normalize |> Fpath.to_string
  in
  let _graph, asts =
    collect_resolved caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes:[] ~excludes:[] ()
  in
  Hashtbl.find_opt asts target_key
