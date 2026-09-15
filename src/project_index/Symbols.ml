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
  (* A namespace/module (Ruby [module], TS [namespace], the [A::] prefix of a
     compact [class A::B]) is a pure qn qualifier: it contributes its name to
     enclosed classes' qns but never owns a method or nests a class
     ([immediate_enclosing_class_id] skips it). *)
  | Sc_namespace of string
  | Sc_function of string

let qualified_name_of ~(module_path : Names.Module_qn.t)
    (outer_to_inner : scope_kind list) (bare_name : string) : string =
  let buf = Buffer.create 64 in
  Buffer.add_string buf (Names.Module_qn.to_string module_path);
  let add_separator () : unit =
    if Buffer.length buf > 0 then Buffer.add_char buf '.'
  in
  let prev_was_fn =
    List.fold_left (fun prev_was_fn scope ->
      add_separator ();
      if prev_was_fn then Buffer.add_string buf "<locals>.";
      (match scope with
       | Sc_class { name; _ } -> Buffer.add_string buf name
       | Sc_namespace ns -> Buffer.add_string buf ns
       | Sc_function fn_name -> Buffer.add_string buf fn_name);
      (match scope with Sc_function _ -> true | _ -> false)
    ) false outer_to_inner
  in
  add_separator ();
  if prev_was_fn then Buffer.add_string buf "<locals>.";
  Buffer.add_string buf bare_name;
  Buffer.contents buf

let immediate_enclosing_class_id (innermost_first : scope_kind list)
  : Function_id.t option =
  match innermost_first with
  | Sc_class { id; _ } :: _ -> Some id
  | _ -> None

(* The namespace qualifier baked into a compact qualified name — the [A; B] of
   [class A::B::C], [Svc] of [class Svc::Base] — as outer-to-inner scope names.
   Empty for a plain [Id] entity. *)
let entity_qualifier_parts (ent : G.entity) : string list =
  match ent.G.name with
  | G.EN (G.IdQualified { G.name_middle = Some (G.QDots dots); _ }) ->
    List.map (fun ((s, _), _) -> s) dots
  | _ -> []

(* Extend a scope stack (innermost first) with the namespace scopes named by a
   qualified entity, so [class A::B]'s body sees [A] as an enclosing namespace
   and its qn becomes [...A.B]. *)
let push_qualifier_scopes (ent : G.entity) (scope : scope_kind list)
  : scope_kind list =
  List.rev_map (fun part -> Sc_namespace part) (entity_qualifier_parts ent)
  @ scope

(* A [class_definition] view of a module body, so the class-body hooks
   ([class_body_extra_parents] for a module-level [include], macro methods)
   apply to a module the same way they do to a class. *)
let cdef_of_module_items (items : G.stmt list) : G.class_definition =
  let fk = Tok.unsafe_fake_tok "module" in
  { G.ckind = (G.Class, fk);
    cextends = []; cimplements = []; cmixins = [];
    cparams = (fk, [], fk);
    cbody = (fk, List.map (fun stmt -> G.F stmt) items, fk) }

let collect_in_ast ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(resolution : Module_paths.specifier_resolution)
    ~(module_path : Names.Module_qn.t) ~(file : Fpath.t)
    (ast : G.program) : entry list * class_info list * file_info =
  let entries = ref [] in
  let class_infos = ref [] in
  let opened_module_scopes : Names.Module_qn.t list ref = ref [] in
  let dc_wrappers = ref [] in
  let imports =
    let is_init_file = cfg.Index_lang_rules.is_init_file file in
    Imports.collect_imports ~cfg ~resolution ~current_file:file
      ~current_module_path:module_path ~is_init_file ast
  in
  let mk_entry ~(id : Function_id.t) ~(name : string) ~(qn : Names.Def_qn.t)
      ~(kind : def_kind) ~(range : Range.t option)
      ~(defining_class_id : Function_id.t option) : entry =
    { id; name; qn; kind; file; range; defining_class_id }
  in
  let emit_synth_dunder ~(class_id : Function_id.t) ~(class_qn : Names.Def_qn.t)
      ~(range : Range.t option) (name : string) : unit =
    let m_id = synth_function_id class_id name in
    entries := mk_entry ~id:m_id ~name ~qn:(Names.Def_qn.concat class_qn name)
                 ~kind:K_method ~range
                 ~defining_class_id:(Some class_id) :: !entries
  in
  (* Scope stack threaded as the visitor's env (innermost first).  The
     visitor is built per top-level subtree with the [module_path] in force
     there — the file's for most files, but the open package namespace scope's for a
     class inside a [namespace] (see [walk_top_level]).  The shared
     accumulators are captured from the enclosing scope, so instances agree. *)
  let make_visitor (module_path : Names.Module_qn.t) = object
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
            (* A compact [class A::B] carries its namespace in the entity name;
               decompose it into namespace scopes so the qn matches the nested
               [module A; class B] form. *)
            let scope = push_qualifier_scopes ent scope in
            let is_companion =
              match
                (fst cdef.G.ckind,
                 cfg.Index_lang_rules.companion_object_has_own_name)
              with
              | G.Object, true -> true
              | (G.Object | G.Class | G.Interface | G.Trait), _ -> false
            in
            let qn_name = if is_companion then name ^ "$" else name in
            let class_qn =
              qualified_name_of ~module_path (List.rev scope) qn_name
            in
            let class_qn_def = Names.Def_qn.of_string class_qn in
            let class_range = entity_range ent in
            let parent_class_id = immediate_enclosing_class_id scope in
            entries := mk_entry ~id:class_id ~name ~qn:class_qn_def
                         ~kind:(if is_companion then K_companion else K_class)
                         ~range:class_range
                         ~defining_class_id:parent_class_id :: !entries;
            let synthesized =
              let from_dec = cfg.Index_lang_rules.class_dunders_from_decorators ent.G.attrs in
              let from_ext = cfg.Index_lang_rules.class_dunders_from_extends cdef in
              List.sort_uniq String.compare (from_dec @ from_ext)
            in
            List.iter
              (emit_synth_dunder ~class_id ~class_qn:class_qn_def
                 ~range:class_range)
              synthesized;
            (* Class-body macro methods (Ruby [attr_reader]); def-site tok from
               the symbol literal so each accessor has its own location. *)
            List.iter (fun (m_name, m_tok) ->
              let m_id = Function_id.of_string_and_tok m_name m_tok in
              entries := mk_entry ~id:m_id ~name:m_name
                            ~qn:(Names.Def_qn.concat class_qn_def m_name)
                            ~kind:K_method
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
                   let inner_qn =
                     Names.Def_qn.of_string
                       (qualified_name_of ~module_path (List.rev scope)
                          inner_name)
                   in
                   entries := mk_entry ~id:inner_id ~name:inner_name
                                 ~qn:inner_qn
                                 ~kind:K_class
                                 ~range:class_range
                                 ~defining_class_id:parent_class_id
                              :: !entries;
                   List.iter (fun dunder ->
                     let m_id = synth_function_id inner_id dunder in
                     entries := mk_entry ~id:m_id ~name:dunder
                                   ~qn:(Names.Def_qn.concat inner_qn dunder)
                                   ~kind:K_method
                                   ~range:class_range
                                   ~defining_class_id:(Some inner_id)
                                :: !entries
                   ) dunders)
              | _ -> ()
            ) cdef.G.cextends;
            let parent_paths =
              let from_extends =
                List.filter_map (fun (ty, _) ->
                  match parent_path ty with
                  | [] -> None
                  | path ->
                    Some { Index_lang_rules.cp_path = path;
                           cp_position = Index_lang_rules.Appended }
                ) cdef.G.cextends
              in
              let extra = cfg.Index_lang_rules.class_body_extra_parents cdef in
              match cfg.Index_lang_rules.superclass_position with
              | Index_lang_rules.Superclass_before_mixins -> from_extends @ extra
              | Index_lang_rules.Superclass_after_mixins -> extra @ from_extends
            in
            class_infos := { ci_id = class_id;
                             ci_qn = Names.Class_qn.of_string class_qn;
                             ci_name = name;
                             ci_class_kind = fst cdef.G.ckind;
                             ci_file = file;
                             ci_range = class_range;
                             ci_parent_paths = parent_paths;
                             ci_singleton_exposure =
                               cfg.Index_lang_rules.class_body_singleton_methods
                                 cdef;
                             ci_imports = imports;
                             ci_decorator_names = entity_decorator_names ent }
                           :: !class_infos;
            let scope' = Sc_class { name = qn_name; id = class_id } :: scope in
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
          let method_owner =
            match def_kind with
            | G.FuncDef (fdef : G.function_definition) ->
              cfg.Index_lang_rules.method_owner_of_funcdef fdef
            | _ -> None
          in
          match entity_simple_name ent, fid_opt with
          | None, _ | _, None -> super#visit_definition scope (ent, def_kind)
          | Some name, Some fn_id ->
            let defining_class_id = immediate_enclosing_class_id scope in
            let kind =
              if Option.is_some defining_class_id || Option.is_some method_owner
              then K_method
              else K_function
            in
            let qualified_scope = push_qualifier_scopes ent scope in
            let owner_qn =
              Names.Def_qn.of_string
                (qualified_name_of ~module_path (List.rev qualified_scope)
                   (Option.value method_owner ~default:name))
            in
            let qn =
              match method_owner with
              | Some _ -> Names.Def_qn.concat owner_qn name
              | None -> owner_qn
            in
            entries := mk_entry ~id:fn_id ~name ~qn
                         ~kind
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
             let lhs_qn =
               Names.Def_qn.of_string
                 (qualified_name_of ~module_path (List.rev scope) lhs_name)
             in
             entries := mk_entry ~id:class_id ~name:lhs_name ~qn:lhs_qn
                          ~kind:K_class
                          ~range:(entity_range ent)
                          ~defining_class_id:parent_class_id :: !entries;
             List.iter (emit_synth_dunder ~class_id ~class_qn:lhs_qn
                          ~range:(entity_range ent)) dunders
           | _ -> ());
          super#visit_definition scope (ent, def_kind)
        end
      (* A [module]/[namespace] scope: a pure qn qualifier.  Nested classes
         inherit its name in their qn but are not owned by it (their
         [defining_class_id] stays [None]).  On mixin-capable languages the
         module is also emitted as a [class_info] so [include M] resolves it as
         a parent — the module's instance methods reach the includer through
         the MRO ([Type_state] already attributes them to [M] via
         [Visit_function_defs]'s module-as-class-scope handling).  A module
         emitted as a class is also what a path segment binds to, so
         [a::handle] in Rust reaches the definitions of the module [a]. *)
      | G.ModuleDef { G.mbody = G.ModuleStruct (_, items); _ } -> begin
          match entity_simple_name ent, function_id_of_entity ent with
          | None, _ | _, None -> super#visit_definition scope (ent, def_kind)
          | Some name, Some ns_id ->
            let scope = push_qualifier_scopes ent scope in
            let ns_range = entity_range ent in
            if cfg.Index_lang_rules.walks_inheritance then begin
              let ns_qn = qualified_name_of ~module_path (List.rev scope) name in
              let ns_qn_def = Names.Def_qn.of_string ns_qn in
              let cdef = cdef_of_module_items items in
              (* A module-level [include N] ([include_module_includes_module])
                 makes N a parent of the module. *)
              let parent_paths = cfg.Index_lang_rules.class_body_extra_parents cdef in
              entries := mk_entry ~id:ns_id ~name ~qn:ns_qn_def ~kind:K_class
                           ~range:ns_range
                           ~defining_class_id:(immediate_enclosing_class_id scope)
                         :: !entries;
              List.iter (fun (m_name, m_tok) ->
                let m_id = Function_id.of_string_and_tok m_name m_tok in
                entries := mk_entry ~id:m_id ~name:m_name
                             ~qn:(Names.Def_qn.concat ns_qn_def m_name)
                             ~kind:K_method
                             ~range:ns_range ~defining_class_id:(Some ns_id)
                           :: !entries
              ) (cfg.Index_lang_rules.class_body_synth_methods cdef);
              class_infos := { ci_id = ns_id;
                               ci_qn = Names.Class_qn.of_string ns_qn;
                               ci_name = name;
                               ci_class_kind = G.Class;
                               ci_file = file;
                               ci_range = ns_range;
                               ci_parent_paths = parent_paths;
                               ci_singleton_exposure =
                                 cfg.Index_lang_rules
                                   .class_body_singleton_methods cdef;
                               ci_imports = imports;
                               ci_decorator_names = [] } :: !class_infos
            end;
            let scope' = Sc_namespace name :: scope in
            if cfg.Index_lang_rules.module_definition_is_namespace then begin
              let parts : string list =
                List.rev
                  (List.filter_map
                     (fun (sc : scope_kind) ->
                       match sc with
                       | Sc_namespace (ns : string) -> Some ns
                       | Sc_class _
                       | Sc_function _ -> None)
                     scope')
              in
              opened_module_scopes :=
                Names.Module_qn.of_parts parts :: !opened_module_scopes
            end;
            super#visit_definition scope' (ent, def_kind)
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
               (match Ty_bare_name.bare_name_of_name gname with
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
             let lhs_qn =
               Names.Def_qn.of_string
                 (qualified_name_of ~module_path (List.rev scope) lhs_name)
             in
             entries := mk_entry ~id:class_id ~name:lhs_name ~qn:lhs_qn
                          ~kind:K_class
                          ~range ~defining_class_id:parent_class_id
                        :: !entries;
             List.iter (emit_synth_dunder ~class_id ~class_qn:lhs_qn ~range)
               dunders
           | _ -> ()
         end
       | _ -> ());
      super#visit_stmt scope stmt
  end in
  (* Package-scoped languages (Java/Kotlin/C#/C++) delimit namespaces with
     [Package]/[PackageEnd] directives rather than nested [ModuleDef]s, and a
     file may open several ([namespace a {..} namespace b {..}]) or nest them.
     Walk the top level threading the open-package stack functionally: each
     class is qualified by the namespace scope in force at its definition, not the
     file's first package.  Other languages (Go's [package] included) carry
     package identity in the module path, so the stack stays empty and the
     file [module_path] is used unchanged. *)
  let package_scoped = cfg.Index_lang_rules.package_directive_is_namespace in
  (* The prefix for a class qn.  Empty for constant-path languages (Ruby), so a
     class reopened across files shares one qn and parent resolution scores on
     the lexical constant path, not the filename.  [fi_module_path] keeps the
     real [module_path] for require-relative / indexing. *)
  let root_namespace_qn =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_package
    | `Per_namespace
    | `Per_translation_unit
    | `Per_project -> true
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_go_package
    | `Per_module -> cfg.Index_lang_rules.class_identity_is_constant_path
  in
  let qn_module_path =
    if root_namespace_qn then Names.Module_qn.empty else module_path
  in
  let namespace_scope_prefix : string list =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_module ->
      if Names.Module_qn.is_empty module_path then []
      else Names.Module_qn.parts module_path
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_go_package
    | `Per_package
    | `Per_namespace
    | `Per_translation_unit
    | `Per_project -> []
  in
  let module_path_of_namespace_scopes (namespace_scopes : string list list) : Names.Module_qn.t =
    match List.concat (List.rev namespace_scopes) with
    | [] -> qn_module_path
    | parts -> Names.Module_qn.of_parts (namespace_scope_prefix @ parts)
  in
  let rec walk_top_level
      ((namespace_scopes : string list list), (opened : Names.Module_qn.t list))
      (stmts : G.stmt list)
    : string list list * Names.Module_qn.t list =
    match stmts with
    | [] -> (namespace_scopes, opened)
    | stmt :: rest ->
      (* Visit under the namespace scope open at this statement, then update the stack
         for the following siblings — [Package]/[PackageEnd]/def are flat
         siblings, so the namespace scope a class sees is the one active when reached. *)
      let namespace_scopes, opened =
        match stmt.G.s with
        (* A braced namespace (PHP [namespace A { .. }]) wraps its
           [Package]/[PackageEnd] and defs in a [Block]; descend so the namespace scope
           is tracked around the classes inside (the flat statement form
           [namespace A;] needs no unwrapping). *)
        | G.Block (_, inner, _) -> walk_top_level (namespace_scopes, opened) inner
        | G.DirectiveStmt { G.d = G.Package (_, parts); _ } when package_scoped ->
          (make_visitor (module_path_of_namespace_scopes namespace_scopes))#visit_stmt [] stmt;
          let namespace_scopes = List.map fst parts :: namespace_scopes in
          (namespace_scopes, module_path_of_namespace_scopes namespace_scopes :: opened)
        | G.DirectiveStmt { G.d = G.PackageEnd _; _ } when package_scoped ->
          (make_visitor (module_path_of_namespace_scopes namespace_scopes))#visit_stmt [] stmt;
          ((match namespace_scopes with _ :: outer -> outer | [] -> []), opened)
        | _ ->
          (make_visitor (module_path_of_namespace_scopes namespace_scopes))#visit_stmt [] stmt;
          (namespace_scopes, opened)
      in
      walk_top_level (namespace_scopes, opened) rest
  in
  (* Non-package languages never open namespace scopes, so one visitor over the whole
     program suffices; only package languages need the per-top-level walk. *)
  let opened_namespace_scopes =
    if package_scoped then snd (walk_top_level ([], []) ast)
    else begin
      (make_visitor qn_module_path)#visit_program [] ast;
      !opened_module_scopes
    end
  in
  let namespace_scopes =
    let opened = List.sort_uniq Names.Module_qn.compare opened_namespace_scopes in
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_module -> module_path :: opened
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_go_package
    | `Per_package
    | `Per_namespace
    | `Per_translation_unit
    | `Per_project -> (
      match opened with
      | [] -> [ (if package_scoped then qn_module_path else module_path) ]
      | namespace_scopes -> namespace_scopes)
  in
  let fi = { fi_file = file; fi_module_path = module_path;
             fi_package_clause = cfg.Index_lang_rules.package_clause_of_ast ast;
             fi_namespace_scopes = namespace_scopes;
             fi_imports = imports;
             fi_dataclass_wrappers = !dc_wrappers;
             fi_ast = ast;
             fi_observations = Walker.walk_file ~lang ast } in
  (List.rev !entries, List.rev !class_infos, fi)

(* Per-class owned-method-name sets, seeded from [K_method] entries; the
   returned function materialises (and caches) a class's set. *)
let methods_by_class (entries : entry list)
    : Function_id.t -> (string, unit) Hashtbl.t =
  (* One set per class, populated lazily; the class count is not known
     here and [entries] would overshoot (it includes every function). *)
  let tbl : (Function_id.t, (string, unit) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 1024
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
          { id = m_id; name = dunder;
            qn = Names.Def_qn.concat
                   (Names.Def_qn.of_string (Names.Class_qn.to_string ci.ci_qn))
                   dunder;
            kind = K_method;
            file = ci.ci_file; range = ci.ci_range;
            defining_class_id = Some ci.ci_id }
          :: acc
        end
      ) acc (cfg.Index_lang_rules.wrapper_dunders wrapper)
  ) [] class_infos
