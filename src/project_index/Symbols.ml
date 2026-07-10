(* Symbol collection: walk one file's AST and produce the entry list
   (functions, methods, classes, synthesised dunders), class_info records,
   and the file_info carried through the pipeline. *)

module G = AST_generic

open Types

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
    Imports.collect_imports ~cfg ~current_module_path:module_path ~is_init_file ast
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
