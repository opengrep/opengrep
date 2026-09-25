(* Project-wide type augmentation: derive return types, field types,
   caller-argument types, module-singleton types, and variable classes
   from declarations and function bodies, extending the [Type_state.t]
   lattice that callee resolution reads.  Driven to a fixpoint by
   [Project_index.build_project_call_graph]. *)

module G = AST_generic
module Log = Log_projidx.Log

open Types

module FA = Graph_from_AST

type table_of_file = Fpath.t -> Symbol_table.t option

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

let table_of_func ~(table_of_file : table_of_file)
    ~(type_state : Type_state.t) (func : FA.func_info) : Symbol_table.t option =
  Option.map
    (fun (table : Symbol_table.t) -> Symbol_table.with_types table type_state)
    (Option.bind (Func_info.def_file_opt func) table_of_file)

let method_name (func : FA.func_info) : string option =
  Option.map (fun ((_ : IL.name), (meth : IL.name)) -> fst meth.IL.ident)
    (Func_info.as_method func.FA.fn_id)

let along (table : Symbol_table.t) (cls : Class_table.cls)
    (lookup : Class_table.cls -> 'found option) : 'found option =
  List.find_map lookup
    (Class_table.order (Symbol_table.class_table table) cls)
      .Linearisation.order

let names_self_type (ty : G.type_) : bool =
  match (Ty_bare_name.inner_named_type ty).G.t with
  | G.TyN (G.Id ((("this" | "Self" | "self"), _), info)) ->
      Option.is_none (Class_table.binding_of_id_info info)
  | _ -> false

let declared_class (table : Symbol_table.t) ~(owner : Class_table.cls option)
    (ty : G.type_) : Class_table.cls option =
  if names_self_type ty then owner
  else
    Symbol_table.class_of_declared_type table ~context:None
      (Ty_bare_name.inner_named_type ty)

(* Declared return types, in one pass over [all_funcs]:
   - free-function return (bare-name key, [class_name_of_ty]);
   - method return ([(class, method)], [inner_class_name_of_ty], [this]/[self]
     resolving to the enclosing class);
   - tuple returns (Go [func F() (T, error)]) so [a, b := F()] splits into
     [(a, T)]/[(b, error)], keyed by bare name and, for methods, by
     [(class, method)]. *)
let populate_returns_from_decls ~(table_of_file : table_of_file)
    (state : Type_state.t) (all_funcs : FA.func_info list) : Type_state.t =
  List.fold_left (fun state (func : FA.func_info) ->
    match
      ( table_of_func ~table_of_file ~type_state:state func,
        func.FA.fdef.G.frettype,
        Symbol_table.node_of_function func )
    with
    | Some table, Some (ret : G.type_), Some (node : Function_id.t) -> (
      let owner = Symbol_table.class_of_function table func in
      let in_owner (set : Type_state.t -> Class_table.cls -> string -> Type_state.t)
          (state : Type_state.t) : Type_state.t =
        match (owner, method_name func) with
        | Some (cls : Class_table.cls), Some (meth : string) -> set state cls meth
        | _ -> state
      in
      match ret.G.t with
      | G.TyTuple (_, (elements : G.type_ list), _) ->
        let keys = List.map (declared_class table ~owner) elements in
        in_owner
          (fun state cls meth ->
            Type_state.set_method_return_tuple state cls meth keys)
          (Type_state.set_function_return_tuple state node keys)
      | _ -> (
        match declared_class table ~owner ret with
        | Some (key : Class_table.cls) ->
          in_owner
            (fun state cls meth -> Type_state.set_method_return state cls meth key)
            (Type_state.set_function_return state node key)
        | None -> state))
    | _ -> state
  ) state all_funcs

(* Per-class field-type indexes: scalar ([obj.field] -> class) and slice-element
   ([for _, x := range obj.field] -> element type). *)
let build_fields_by_class_index
    ~(cfg : Index_lang_rules.t)
    ~(table_of_file : table_of_file)
    (state : Type_state.t)
    (file_infos : file_info list)
  : Type_state.t =
  let add_field (table : Symbol_table.t) (cls : Class_table.cls)
      (field_name : string) (vtype : G.type_) (state : Type_state.t)
      : Type_state.t =
    let state =
      match declared_class table ~owner:(Some cls) vtype with
      | Some (key : Class_table.cls) ->
        Type_state.set_field state cls field_name key
      | None -> state
    in
    match
      Option.bind (Type_infer.slice_element_of_ty vtype)
        (declared_class table ~owner:(Some cls))
    with
    | Some (element : Class_table.cls) ->
      Type_state.set_field_element state cls field_name element
    | None -> state
  in
  let process_field_list (table : Symbol_table.t) (cls : Class_table.cls)
      (state : Type_state.t) (body : G.stmt list) : Type_state.t =
    List.fold_left (fun state (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt (ent, G.VarDef { G.vtype = Some ty; _ }) ->
        (match Index_lang_rules.entity_simple_name ent with
         | Some fname ->
           add_field table cls (cfg.Index_lang_rules.strip_field_sigil fname) ty
             state
         | None -> state)
      | G.DefStmt (ent, G.FuncDef fdef)
        when (match Index_lang_rules.entity_simple_name ent with
              | Some "constructor" -> true | _ -> false) ->
        List.fold_left
          (fun state ((fname : string), (fty : G.type_)) ->
            add_field table cls fname fty state)
          state
          (cfg.Index_lang_rules.class_constructor_synth_fields fdef)
      | _ -> state
    ) state body
  in
  List.fold_left (fun state (fi : file_info) ->
    match table_of_file fi.fi_file with
    | None -> state
    | Some (table : Symbol_table.t) ->
      List.fold_left
        (fun state ((scope : Class_table.scope_id), (def : G.definition_kind)) ->
          match
            Class_table.class_of_scope (Symbol_table.class_table table) scope
          with
          | Some (cls : Class_table.cls) ->
            process_field_list table cls state
              (Class_parents.definition_body def)
          | None -> state)
        state
        (Symbol_table.class_definitions table)
  ) state file_infos

let build_file_funcs_index (all_funcs : FA.func_info list)
  : (string, FA.func_info list) Hashtbl.t =
  (* Keyed by def file: bounded by the function count. Every named function
     of the file, nested ones included: the same-file lookup finds nested
     callbacks through it; consumers resolving imports keep methods and free
     functions only. *)
  let index = Hashtbl.create (List.length all_funcs) in
  List.iter (fun (func : FA.func_info) ->
    if Option.is_some (Func_info.bare_name func.FA.fn_id) then
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
    ~(table_of_file : table_of_file)
    ~(type_state : Type_state.t)
    (all_funcs : FA.func_info list) : Type_state.t =
  let collect_return_exprs (func : FA.func_info) : G.expr list =
    Nonfatal.catch ?on:(func_def_file func |> Option.map Fpath.v) ~default:[]
      (fun () ->
      let body_stmt = AST_generic_helpers.funcbody_to_stmt func.FA.fdef.G.fbody in
      Walker.fold_stmts_in_stmt ~skip_nested_fdefs:true (fun acc stmt ->
        match stmt.G.s with
        | G.Return (_, Some expr, _) -> expr :: acc
        | _ -> acc) [] body_stmt)
  in
  let undeclared =
    List.filter_map (fun (func : FA.func_info) ->
      match
        ( func.FA.fdef.G.frettype,
          Option.bind (Func_info.def_file_opt func) table_of_file,
          Symbol_table.node_of_function func )
      with
      | None, Some (table : Symbol_table.t), Some (node : Function_id.t) -> (
        match collect_return_exprs func with
        | [] -> None
        | (returned : G.expr list) ->
          Some
            ( func, table, node, Symbol_table.class_of_function table func,
              returned ))
      | _ -> None)
      all_funcs
  in
  let step (state : Type_state.t) : Type_state.t =
    List.fold_left
      (fun state
           ((func : FA.func_info), (table : Symbol_table.t),
            (node : Function_id.t), (owner : Class_table.cls option),
            (returned : G.expr list)) ->
        let already_known =
          (* known for this class, the one of this file: a same-named
             class elsewhere does not stand in for it *)
          Option.is_some (Type_state.function_return state node)
        in
        if already_known then state
        else
          let table = Symbol_table.with_types table state in
          let inferred =
            List.filter_map
              (Symbol_table.class_of_expr table ~caller:(Some node))
              returned
          in
          (match inferred with
           | [] -> state
           | ty :: _ ->
             let state = Type_state.set_function_return state node ty in
             (match (owner, method_name func) with
              | Some (cls : Class_table.cls), Some (meth : string) ->
                Type_state.set_method_return state cls meth ty
              | _ -> state))
    ) state undeclared
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

let fold_calls_of_file ~(table_of_file : table_of_file)
    ~(type_state : Type_state.t)
    ~(funcs_by_file : (string, FA.func_info list) Hashtbl.t)
    (fold :
      'acc ->
      Symbol_table.t ->
      caller:Function_id.t option ->
      G.expr ->
      G.argument list ->
      'acc)
    (acc : 'acc) (fi : file_info) : 'acc =
  match table_of_file fi.fi_file with
  | None -> acc
  | Some table ->
    let table = Symbol_table.with_types table type_state in
    let calls ~(caller : Function_id.t option) (acc : 'acc) (stmt : G.stmt)
        : 'acc =
      Walker.fold_exprs_in_stmt ~skip_nested_fdefs:true
        (fun acc (expr : G.expr) ->
          match expr.G.e with
          | G.Call (callee, args) ->
            fold acc table ~caller callee (Tok.unbracket args)
          | _ -> acc)
        acc stmt
    in
    let acc =
      Nonfatal.catch ~on:fi.fi_file ~default:acc (fun () ->
        List.fold_left (calls ~caller:None) acc fi.fi_ast)
    in
    List.fold_left
      (fun acc (func : FA.func_info) ->
        Nonfatal.catch ~on:fi.fi_file ~default:acc (fun () ->
          calls ~caller:(Symbol_table.node_of_function func) acc
            (AST_generic_helpers.funcbody_to_stmt func.FA.fdef.G.fbody)))
      acc
      (Option.value ~default:[]
         (Hashtbl.find_opt funcs_by_file (Fpath.to_string fi.fi_file)))

(* [(callee_class, callee_method, arg_idx) -> type] of caller-supplied arg types,
   so [self.X = param] can be typed from what callers pass. *)
let build_caller_arg_types
    ~(table_of_file : table_of_file)
    ~(type_state : Type_state.t)
    ~(funcs_by_file : (string, FA.func_info list) Hashtbl.t)
    (file_infos : file_info list)
  : (Function_id.t * int, Class_table.cls) Hashtbl.t =
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
  let candidates : (Function_id.t * int, Class_table.cls list) Hashtbl.t =
    Hashtbl.create 64
  in
  let record (table : Symbol_table.t) ~(caller : Function_id.t option)
      (callee : G.expr) (args : G.argument list) : unit =
    let typed =
      List.filter_map (fun ((i : int), (arg : G.argument)) ->
        match arg with
        | G.Arg expr | G.ArgKwd (_, expr) | G.ArgKwdOptional (_, expr) ->
          Option.map (fun (cls : Class_table.cls) -> (i, cls))
            (Symbol_table.class_of_expr table ~caller expr)
        | _ -> None)
        (List.mapi (fun (i : int) (arg : G.argument) -> (i, arg)) args)
    in
    match typed with
    | [] -> ()
    | _ :: _ -> (
      match Symbol_table.resolve_call table ~caller callee with
      | Symbol_table.External -> ()
      | Symbol_table.Defined (funcs : Func_info.t list) ->
        List.iter (fun ((i : int), (cls : Class_table.cls)) ->
          List.iter
            (fun (func : Func_info.t) ->
              match Symbol_table.node_of_function func with
              | Some (node : Function_id.t) ->
                let prev =
                  Option.value ~default:[]
                    (Hashtbl.find_opt candidates (node, i))
                in
                if not (List.exists (Class_table.same cls) prev) then
                  Hashtbl.replace candidates (node, i) (cls :: prev)
              | None -> ())
            funcs)
          typed)
  in
  List.iter
    (fold_calls_of_file ~table_of_file ~type_state ~funcs_by_file
       (fun () table ~caller callee args -> record table ~caller callee args)
       ())
    file_infos;
  let conflicted = ref 0 in
  Hashtbl.iter
    (fun (key : Function_id.t * int) (classes : Class_table.cls list) ->
      match classes with
      | [ (cls : Class_table.cls) ] -> Hashtbl.replace arg_types key cls
      | _ -> incr conflicted)
    candidates;
  if !conflicted > 0 then
    Log_projidx.Log.debug (fun m ->
        m
          "build_caller_arg_types: dropped %d arg keys with conflicting \
           caller classes (%d kept)"
          !conflicted
          (Hashtbl.length arg_types));
  arg_types

(* Module-level singleton bindings keyed by full qn (module_qn + var_name):
   [x = SomeClass()] at module scope lets importers' [x.method()] resolve.
   The full-qn key matches [Imports.collect_imports]'s [fi_imports] targets. *)
let build_module_singleton_types
    ~(table_of_file : table_of_file)
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
    Nonfatal.catch ~on:fi.fi_file ~default:[] (fun () ->
      List.fold_left (fun acc top ->
        Walker.fold_stmts_in_stmt ~skip_nested_fdefs:true collect acc top
      ) [] fi.fi_ast
      |> List.rev)
  in
  List.fold_left (fun state (fi : file_info) ->
    match table_of_file fi.fi_file with
    | None -> state
    | Some table ->
      List.fold_left (fun state (mp, name, rhs) ->
        match
          Symbol_table.class_of_expr (Symbol_table.with_types table state)
            ~caller:None rhs
        with
        | Some (cls : Class_table.cls) ->
          Type_state.set_module_singleton state (Names.Module_qn.concat mp name)
            cls
        | None -> state
      ) state (module_level_assigns_of_file fi)
  ) state file_infos

let stamped_name (table : Symbol_table.t) (cls : Class_table.cls)
    : G.name option =
  Class_table.name_of_class (Symbol_table.class_table table) cls

(* Augment fields from [this.X = RHS] in class methods so [self.X.method()]
   chains resolve; [caller_arg_types] types [self.X = param] from callers;
   [ctor_param_promotion] (PHP 8) registers typed ctor params as fields. *)
let augment_fields_from_self_assignments
    ~(lang : Lang.t)
    ~(caller_arg_types : (Function_id.t * int, Class_table.cls) Hashtbl.t)
    ~(cfg : Index_lang_rules.t)
    ~(table_of_file : table_of_file)
    ~(type_state : Type_state.t)
    (all_funcs : FA.func_info list) : Type_state.t =
  let strip = cfg.Index_lang_rules.strip_field_sigil in
  List.fold_left (fun state (func : FA.func_info) ->
    match
      ( table_of_func ~table_of_file ~type_state:state func,
        Symbol_table.node_of_function func )
    with
    | Some table, Some (node : Function_id.t) -> (
      match
        (Func_info.as_method func.FA.fn_id,
         Symbol_table.class_of_function table func)
      with
      | Some _, Some (cls : Class_table.cls) ->
        let already_known (field : string) (state : Type_state.t) : bool =
          Option.is_some (Type_state.field state cls field)
        in
        let set_new (field : string) (ty : Class_table.cls)
            (state : Type_state.t) : Type_state.t =
          if already_known field state then state
          else Type_state.set_field state cls field ty
        in
        let param_types : (string, Class_table.cls) Hashtbl.t =
          Hashtbl.create 4
        in
        let params = Tok.unbracket func.FA.fdef.G.fparams in
        (* [caller_arg_types] is keyed by CALL-argument index, which does
           not count the receiver; an explicit receiver param ([self]/[cls]
           in Python, [ParamReceiver] in Go) shifts every later param by
           one. *)
        let receiver_offset =
          List.length params
          - Receiver.arity lang ~is_method:(Receiver.is_method func.FA.fdef)
              ~is_static:(Receiver.is_static func.FA.entity) params
        in
        let caller_arg_type (i : int) : Class_table.cls option =
          Hashtbl.find_opt caller_arg_types (node, i - receiver_offset)
        in
        List.iteri (fun (i : int) (param : G.parameter) ->
          match param with
          | G.Param { pname = Some (pn, _); ptype; _ }
          | G.ParamReceiver { pname = Some (pn, _); ptype; _ } -> (
            let declared =
              Option.bind ptype (declared_class table ~owner:(Some cls))
            in
            match declared with
            | Some (ty : Class_table.cls) -> Hashtbl.replace param_types pn ty
            | None -> (
              match caller_arg_type i with
              | Some (ty : Class_table.cls) -> Hashtbl.replace param_types pn ty
              | None -> ()))
          | _ -> ()
        ) params;
        (* PHP 8 ctor property promotion: the parser drops the visibility
           modifier, so every typed ctor param is a candidate field. *)
        let is_constructor () : bool =
          match Symbol_table.constructors_of_class table cls with
          | Symbol_table.Defined (constructors : Func_info.t list) ->
            List.exists
              (fun (constructor : Func_info.t) ->
                constructor.Func_info.fdef == func.FA.fdef)
              constructors
          | Symbol_table.External -> false
        in
        let state =
          if cfg.Index_lang_rules.ctor_param_promotion && is_constructor ()
          then
            List.fold_left (fun state (param : G.parameter) ->
              match param with
              | G.Param { G.pname = Some (pn, _); ptype = Some pty; _ }
              | G.ParamReceiver { G.pname = Some (pn, _); ptype = Some pty; _ } ->
                (match declared_class table ~owner:(Some cls) pty with
                 | Some (ty : Class_table.cls) -> set_new (strip pn) ty state
                 | None -> state)
              | _ -> state
            ) state params
          else state
        in
        let def_file_opt = func_def_file func |> Option.map Fpath.v in
        let body =
          Nonfatal.catch ?on:def_file_opt ~default:None (fun () ->
            Some (AST_generic_helpers.funcbody_to_stmt func.FA.fdef.G.fbody))
        in
        (match body with
         | None -> state
         | Some body_stmt ->
           (* Publish parameter classes onto the body's [id_instance_type] so
              [Type_infer] resolves right-hand-side expressions derived from a
              parameter. *)
           let param_facts =
             Hashtbl.fold (fun pname (ty : Class_table.cls) acc ->
               match stamped_name table ty with
               | Some (name : G.name) ->
                 (G.Id ((pname, Tok.unsafe_fake_tok pname), G.empty_id_info ()),
                  name) :: acc
               | None -> acc
             ) param_types []
           in
           Object_initialization.stamp_id_types param_facts [body_stmt];
           Nonfatal.catch ?on:def_file_opt ~default:state (fun () ->
             Walker.fold_exprs_in_stmt ~skip_nested_fdefs:true (fun state expr ->
               match expr.G.e with
               | G.Assign (
                   { G.e = G.DotAccess (
                       { G.e = G.IdSpecial ((G.This | G.Self), _); _ }, _,
                       G.FN (G.Id ((field_name, _), _))); _ },
                   _, rhs)
                 when not (already_known (strip field_name) state) ->
                 let field_name = strip field_name in
                 let rhs_ty =
                   match rhs.G.e with
                   | G.N (G.Id ((vn, _), _)) ->
                     (match Hashtbl.find_opt param_types vn with
                      | Some _ as resolved -> resolved
                      | None ->
                        Symbol_table.class_of_expr table ~caller:(Some node) rhs)
                   | _ -> Symbol_table.class_of_expr table ~caller:(Some node) rhs
                 in
                 (match rhs_ty with
                  | Some (ty : Class_table.cls) -> set_new field_name ty state
                  | None -> state)
               | _ -> state) state body_stmt))
      | _ -> state)
    | _ -> state
  ) type_state all_funcs

let is_value_class ~(lang : Lang.t) (cls : Class_table.cls) : bool =
  List.exists
    (fun (scope : Class_table.class_scope) ->
      match scope.Class_table.kind with
      | Class_table.Class_kind G.Struct -> true
      | Class_table.Class_kind G.Class ->
        Object_initialization.classes_are_value_types lang
      | Class_table.Class_kind (G.Interface | G.Trait | G.Object)
      | Class_table.Module_kind -> false)
    (Class_table.scopes cls)

let add_value_type_sites ~(lang : Lang.t) ~(table_of_file : table_of_file)
    (state : Type_state.t) (all_funcs : FA.func_info list) : Type_state.t =
  List.fold_left (fun state (func : FA.func_info) ->
    match table_of_func ~table_of_file ~type_state:state func with
    | None -> state
    | Some table ->
      List.fold_left (fun state (param : G.parameter) ->
        match param with
        | G.Param { ptype = Some ({ G.t = G.TyN (name : G.name); _ } as pty); _ }
        | G.ParamReceiver
            { ptype = Some ({ G.t = G.TyN (name : G.name); _ } as pty); _ } -> (
          match
            ( Symbol_table.class_of_declared_type table ~context:None pty,
              Class_table.site_of_name name )
          with
          | Some (cls : Class_table.cls), Some (site : G.SId.t)
            when is_value_class ~lang cls ->
            Type_state.add_value_type_site state site
          | _ -> state)
        | _ -> state)
        state (Tok.unbracket func.FA.fdef.G.fparams)
  ) state all_funcs

(* Infer variable classes from assignment/def/range statements and stamp them
   onto [id_instance_type]; iterate so one pass's stamps unlock the next
   pass's inferences ([Type_infer] reads a receiver's class off
   [id_instance_type], else off [id_type]). *)
let stamp_var_types_from_bodies
    ~(table : Symbol_table.t)
    ~(type_state : Type_state.t)
    ~(caller : Function_id.t option)
    (ast : G.program) : unit =
  let table = Symbol_table.with_types table type_state in
  let class_of (e : G.expr) : Class_table.cls option =
    Symbol_table.class_of_expr table ~caller e
  in
  let pass () : (G.name * G.name) list =
    let known (name : G.name) =
      Option.is_some
        (Option.bind
           (Ty_bare_name.instance_or_declared_type
              (Class_table.id_info_of_name name))
           Ty_bare_name.qualified_class_name_of_ty)
    in
    let with_class (lhs : G.name) (cls : Class_table.cls) acc =
      match stamped_name table cls with
      | Some (name : G.name) -> (lhs, name) :: acc
      | None -> acc
    in
    let fact lhs rhs acc =
      if known lhs then acc
      else
        match class_of rhs with
        | None -> acc
        | Some cls -> with_class lhs cls acc
    in
    let rec tuple_facts lhs_names elem_types acc =
      match lhs_names, elem_types with
      | lhs :: lrest, Some ty :: erest ->
        let acc = if known lhs then acc else with_class lhs ty acc in
        tuple_facts lrest erest acc
      | _ :: lrest, None :: erest -> tuple_facts lrest erest acc
      | _ -> acc
    in
    let returned_tuple (callee : G.expr) : Class_table.cls option list option =
      let member_call = Symbol_table.class_of_member_call table ~caller callee in
      let declared =
        match (callee.G.e, member_call) with
        | G.DotAccess (_, _, G.FN (G.Id ((mname, _), _))),
          Some (Some (cls : Class_table.cls), _) ->
            along table cls (fun (owner : Class_table.cls) ->
              Type_state.method_return_tuple type_state owner mname)
        | _ -> None
      in
      match declared with
      | Some _ -> declared
      | None -> (
        match
          match member_call with
          | Some (_, (resolved : Symbol_table.resolution Lazy.t)) ->
            Lazy.force resolved
          | None -> Symbol_table.resolve_call table ~caller callee
        with
        | Symbol_table.Defined (funcs : Func_info.t list) -> (
          match
            List_.uniq_by (List.equal (Option.equal Class_table.same))
              (List.filter_map
                 (fun (func : Func_info.t) ->
                   Option.bind (Symbol_table.node_of_function func)
                     (Type_state.function_return_tuple type_state))
                 funcs)
          with
          | [ elements ] -> Some elements
          | [] | _ :: _ :: _ -> None)
        | Symbol_table.External -> None)
    in
    let call_tuple_facts (lhs_names : G.name list) (rhs : G.expr) acc =
      match rhs.G.e with
      | G.Call (callee, _) -> (
        match returned_tuple callee with
        | None -> acc
        | Some elem_types -> tuple_facts lhs_names elem_types acc)
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
      let elem_class : Class_table.cls option =
        match range_expr.G.e with
        | G.DotAccess (obj, _, G.FN (G.Id ((field, _), _))) ->
          Option.bind (class_of obj) (fun (cls : Class_table.cls) ->
            along table cls (fun (owner : Class_table.cls) ->
              Type_state.field_element type_state owner field))
        | _ -> None
      in
      (* Bind the LAST iter var (Go's value position for 1- and 2-var range). *)
      match elem_class, List.rev iter_names with
      | Some elem, last :: _ when not (known last) -> with_class last elem acc
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
