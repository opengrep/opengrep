(* Project-wide type augmentation: derive return types, field types,
   caller-argument types, module-singleton types, and variable classes
   from declarations and function bodies, extending the [Type_state.t]
   lattice that callee resolution reads.  Driven to a fixpoint by
   [Project_index.build_project_call_graph]. *)

module G = AST_generic
module Log = Log_projidx.Log

open Types

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
        (match Index_lang_rules.entity_simple_name ent with
         | Some fname ->
           add_field ~def_file cls
             (cfg.Index_lang_rules.strip_field_sigil fname) ty
         | None -> ())
      | G.DefStmt (ent, G.FuncDef fdef)
        when (match Index_lang_rules.entity_simple_name ent with
              | Some "constructor" -> true | _ -> false) ->
        harvest_ctor_synth_fields ~def_file cls fdef
      | _ -> ()
    ) fields
  in
  List.iter (fun fi ->
    List.iter (fun obs ->
      match obs with
      | Walker.Observation.Class_def { ent; cdef } ->
        (match Index_lang_rules.entity_simple_name ent with
         | Some cls ->
           let _, fields, _ = cdef.G.cbody in
           process_field_list ~def_file:fi.fi_file cls fields
         | None -> ())
      | Walker.Observation.Type_def { ent;
          tdef = { G.tbody = G.NewType
              { G.t = G.TyRecordAnon (_, (_, fields, _)); _ }; _ } } ->
        (match Index_lang_rules.entity_simple_name ent with
         | Some cls -> process_field_list ~def_file:fi.fi_file cls fields
         | None -> ())
      (* TS [type X = {...}] aliases parse as [OtherDef("typedef")]; index their
         fields like a class. *)
      | Walker.Observation.Other_def { ent; kind; anys }
        when String.equal kind "typedef" ->
        (match Index_lang_rules.entity_simple_name ent with
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
          match Index_lang_rules.entity_simple_name ent with
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
  (* Keyed by def file: bounded by the function count. *)
  let index = Hashtbl.create (List.length all_funcs) in
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
  let final, iters =
    Fixpoint.run ~equal:Type_state.equal ~step
      ~max_iterations:Limits_semgrep.projidx_RETURN_TYPES_MAX_ITERS type_state
  in
  (* [Fixpoint.run] returns [i = max_iterations] only on the cap branch. *)
  if iters >= Limits_semgrep.projidx_RETURN_TYPES_MAX_ITERS then
    Log.warn (fun m ->
        m "Return-type fixpoint hit the %d-iteration cap without \
           converging; inferred return types may be incomplete"
          Limits_semgrep.projidx_RETURN_TYPES_MAX_ITERS);
  final

(* [(callee_class, callee_method, arg_idx) -> type] of caller-supplied arg types,
   so [self.X = param] can be typed from what callers pass. *)
let build_caller_arg_types
    ~(uses_new_keyword : bool)
    ~(type_state : Type_state.t)
    (file_infos : file_info list)
  : (string * string * int, G.name) Hashtbl.t =
  (* Small: only known-class candidate types are stored (zero entries on
     the reference corpora); the table grows if a project really passes
     class instances to constructors. *)
  let arg_types = Hashtbl.create 64 in
  (* Only types naming a class the index knows are stored: the table
     feeds [augment_fields_from_self_assignments] -> [Type_state.set_field],
     whose stored type is read back solely to resolve [self.field.m()] to
     a project class's methods — any other type is dead weight (measured:
     ALL entries on lemur and gitlab; 72 and 4,802 keys respectively).
     A key whose callers disagree on the class is dropped below: any
     single winner would be wrong at the other call sites, and which one
     won used to depend on the path-sorted file order (same
     missed-over-wrong bias as [Graph_from_AST.try_unique_by_distinct_key]).
     The precise semantics — the field's type is per construction site —
     needs per-call-site instantiation, not this global table; see the
     ctor-arg-conflict notes in the interfile task list. *)
  let candidate_leaves : (string * string * int, string list) Hashtbl.t =
    Hashtbl.create 64
  in
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
                     (match Ty_leaf.leaf_of_name ty with
                      | Some leaf when Type_state.has_class type_state leaf ->
                        let prev =
                          Option.value ~default:[]
                            (Hashtbl.find_opt candidate_leaves (cls, meth, i))
                        in
                        if not (List.mem leaf prev) then
                          Hashtbl.replace candidate_leaves (cls, meth, i)
                            (leaf :: prev);
                        if not (Hashtbl.mem arg_types (cls, meth, i)) then
                          Hashtbl.replace arg_types (cls, meth, i) ty
                      | _ -> ())
                   | None -> ())
                | _ -> ()
              ) (Tok.unbracket args)
            | None -> ())
         | _ -> ());
        super#visit_expr () expr
    end in
    Nonfatal.catch ~on:fi.fi_file ~default:()
      (fun () -> visitor#visit_program () fi.fi_ast)
  ) file_infos;
  let conflicted_keys =
    Hashtbl.fold
      (fun key leaves acc ->
        if List.length leaves > 1 then key :: acc else acc)
      candidate_leaves []
  in
  List.iter (Hashtbl.remove arg_types) conflicted_keys;
  if not (List_.null conflicted_keys) then
    Log_projidx.Log.debug (fun m ->
        m
          "build_caller_arg_types: dropped %d arg keys with conflicting \
           caller classes (%d kept)"
          (List.length conflicted_keys)
          (Hashtbl.length arg_types));
  arg_types

(* Module-level singleton bindings keyed by full qn (module_qn + var_name):
   [x = SomeClass()] at module scope lets importers' [x.method()] resolve.
   The full-qn key matches [Imports.collect_imports]'s [fi_imports] targets. *)
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
      let params = Tok.unbracket func.FA.fdef.G.fparams in
      (* [caller_arg_types] is keyed by CALL-argument index, which does
         not count the receiver; an explicit receiver param ([self]/[cls]
         in Python, [ParamReceiver] in Go) shifts every later param by
         one. *)
      let receiver_offset =
        match params with
        | G.ParamReceiver _ :: _ -> 1
        | G.Param { pname = Some (("self" | "cls"), _); _ } :: _ -> 1
        | _ -> 0
      in
      let caller_arg_type i =
        Hashtbl.find_opt caller_arg_types (cls, meth, i - receiver_offset)
      in
      List.iteri (fun i param ->
        match param with
        | G.Param { pname = Some (pn, _); ptype = Some pty; _ }
        | G.ParamReceiver { pname = Some (pn, _); ptype = Some pty; _ } ->
          (match Ty_leaf.class_name_of_ty pty with
           | Some name -> Hashtbl.replace param_types pn name
           | None ->
             (match caller_arg_type i with
              | Some name -> Hashtbl.replace param_types pn name
              | None -> ()))
        | G.Param { pname = Some (pn, _); ptype = None; _ } ->
          (match caller_arg_type i with
           | Some name -> Hashtbl.replace param_types pn name
           | None -> ())
        | _ -> ()
      ) params;
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
