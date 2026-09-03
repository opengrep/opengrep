open Common
module G = AST_generic
module Log = Log_call_graph.Log
module Reachable = Graph_reachability
open Callee_resolution
open Callback_extraction

(* Facade re-exports: the definitions live in Callee_resolution and
   Callback_extraction after the split, but they remain part of this
   module public interface for external consumers. *)
type fn_id = Func_info.fn_id

type func_info = Func_info.t = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

let fn_id_to_node = fn_id_to_node
let uses_new_keyword = uses_new_keyword
let resolved_name_of_fn_id = resolved_name_of_fn_id

let canonical_callee_key (e : G.expr) : string option =
  let rec key e =
    match e.G.e with
    | G.N (G.Id ((s, _), _)) -> Some s
    | G.DotAccess (sub, _, G.FN (G.Id ((s, _), _))) ->
      (match key sub with
       | Some k -> Some (k ^ "." ^ s)
       | None -> None)
    | _ -> None
  in
  key e

(* Extract Go receiver type from method *)
let extract_go_receiver_type (fdef : G.function_definition) : string option =
  let params = Tok.unbracket fdef.fparams in
  match params with
  (* Non-pointer receiver: func (r Type) ... *)
  | G.ParamReceiver { ptype = Some { t = G.TyN (G.Id ((name, _), _)); _ }; _ }
    :: _ ->
      Some name
  (* Pointer receiver: func (r *Type) ... *)
  | G.ParamReceiver
      {
        ptype =
          Some
            { t = G.TyPointer (_, { t = G.TyN (G.Id ((name, _), _)); _ }); _ };
        _;
      }
    :: _ ->
      Some name
  | _ -> None

(* Build fn_id from entity, or generate _tmp name for anonymous functions *)
let fn_id_of_entity ~(lang : Lang.t) (opt_ent : G.entity option)
    (parent_path : IL.name option list) (fdef : G.function_definition) : fn_id option =
  (* Ensure parent_path starts with [None] for top-level functions *)
  let normalized_parent_path =
    match parent_path with
    | [] -> [None]  (* Top-level: empty path becomes [None] *)
    | path -> path
  in
  let is_lambda =
    match fst fdef.fkind with
    | G.LambdaKind | G.Arrow -> true
    | _ -> false
  in
  match opt_ent with
  | _ when is_lambda ->
      (* All lambdas (named [cb = lambda x: ...] or anonymous) share one
         identity scheme: the lambda's own definition position. The binding
         variable is treated as an alias, not a distinct callable. *)
      Some (normalized_parent_path @ [Some (Visit_function_defs.synth_lambda_il_name fdef)])
  | None ->
      Log.warn (fun m ->
          m "fn_id_of_entity: anonymous non-lambda function definition \
             at %s; falling back to lambda-style identity"
            (Tok.stringpos_of_tok (snd fdef.fkind)));
      Some (normalized_parent_path @ [Some (Visit_function_defs.synth_lambda_il_name fdef)])
  | Some ent ->
      (match AST_to_IL.name_of_entity ent with
      | Some name ->
          (* For Go methods, extract receiver type as class name *)
          let go_receiver_il =
            match lang with
            | Lang.Go -> (
                match extract_go_receiver_type fdef with
                | Some recv_name ->
                    let fake_tok = Tok.unsafe_fake_tok recv_name in
                    Some
                      IL.
                        {
                          ident = (recv_name, fake_tok);
                          sid = AST_generic.SId.unsafe_default;
                          id_info = AST_generic.empty_id_info ();
                        }
                | None -> None)
            | _ -> None
          in
          (* If we have a Go receiver and parent_path is [None], replace with receiver *)
          let adjusted_parent_path =
            match (go_receiver_il, normalized_parent_path) with
            | Some recv, [None] -> [Some recv]
            | Some recv, None :: rest -> Some recv :: rest
            | _, path -> path
          in
          Some (adjusted_parent_path @ [Some name])
      | None -> None)

let dedup_fn_ids (ids : (fn_id * Tok.t) list) : (fn_id * Tok.t) list =
  ids |>
  List.sort_uniq (fun (f1, t1) (f2, t2) ->
    let cmp = compare_fn_id f1 f2 in
    if cmp <> 0 then cmp else Tok.compare t1 t2)

(* Anchored at the AST's first real token so [Function_id.key] embeds the
   file path; the engine checker and projidx must produce identical
   [<top_level>] ids or cross-file edges can't resolve. *)
let top_level_name_of_ast (ast : G.program) : IL.name =
  let fake_tok =
    try
      Tok.fake_tok
        (AST_generic_helpers.first_info_of_any (G.Pr ast)) "<top_level>"
    with Tok.NoTokenLocation _ -> Tok.unsafe_fake_tok "<top_level>"
  in
  IL.{ ident = ("<top_level>", fake_tok); sid = G.SId.unsafe_default;
       id_info = G.empty_id_info () }

let class_init_il_name (class_str : string) : IL.name =
  IL.{ ident = ("Class:" ^ class_str, Tok.unsafe_fake_tok ("Class:" ^ class_str));
       sid = G.SId.unsafe_default; id_info = G.empty_id_info () }

type fdef_edges = {
  calls : (fn_id * Tok.t) list;
  callbacks : (fn_id * Tok.t * IL.name option) list;
  unresolved_call_sites : int;
}

(* Tables are per-fdef / per-file top-level, so callee shape + arity suffice. *)
type callee_memo = (string * int, Func_info.fn_id option) Hashtbl.t

let callee_leaf_id_info (callee : G.expr) : G.id_info option =
  match callee.G.e with
  | G.N (G.Id (_, ii)) -> Some ii
  | G.DotAccess (_, _, G.FN (G.Id (_, ii))) -> Some ii
  (* Generic/qualified callees ([customMap<T>(...)], [pkg::f(...)]):
     the qualified name's own [id_info] is what AST_to_IL threads onto
     the IL callee. *)
  | G.N (G.IdQualified { name_info = ii; _ }) -> Some ii
  | G.DotAccess (_, _, G.FN (G.IdQualified { name_info = ii; _ })) ->
      Some ii
  | _ -> None

let write_back_callee_resolved (callee : G.expr) (fn_id : fn_id) : unit =
  match callee_leaf_id_info callee with
  | Some ii -> set_id_resolved_to_def ~allow_located_fake:true ii fn_id
  | None -> ()

(* Non-memoisable callee shapes bypass the cache; sole AST write-back chokepoint. *)
let memo_lookup_or_compute (memo_tbl : callee_memo)
    ~(call_arity : int) (callee : G.expr)
    (compute : unit -> fn_id option) : fn_id option =
  let result =
    match canonical_callee_key callee with
    | None -> compute ()
    | Some k ->
      let key = (k, call_arity) in
      (match Hashtbl.find_opt memo_tbl key with
       | Some r -> r
       | None ->
         let r = compute () in
         Hashtbl.add memo_tbl key r;
         r)
  in
  (match result with
   | Some fn_id -> write_back_callee_resolved callee fn_id
   | None -> ());
  result

let extract_calls ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(type_state : Type_state.t = Type_state.empty)
    ?(caller_parent_path = [])
    (fdef : G.function_definition) : fdef_edges =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for function");
  let method_hofs = Lang_config.hof_method_names lang in
  let function_hofs = Lang_config.hof_function_specs lang in
  let collect_callbacks (outer_e : G.expr_kind) (callee : G.expr)
      (args : G.arguments)
      : (fn_id * Tok.t * IL.name option) list =
    match outer_e with
    | G.Call ({ e = G.Call (inner_callee, inner_args); _ },
              (_, ([ G.Arg { G.e = G.Lambda _; _ } ] as outer_arg), _))
      when Lang.(lang =*= Ruby || lang =*= Crystal || lang =*= Scala) ->
      let merged =
        Tok.unsafe_fake_bracket (Tok.unbracket inner_args @ outer_arg)
      in
      extract_hof_callbacks_from_call
        ~lang ~method_hofs ~function_hofs ~all_funcs ~func_lookup
        ~caller_parent_path inner_callee merged
    | _ ->
      extract_hof_callbacks_from_call
        ~lang ~method_hofs ~function_hofs ~all_funcs ~func_lookup
        ~caller_parent_path callee args
  in
  let local_imports : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let imp_visitor = object
    inherit [_] G.iter_no_id_info as super
    method! visit_directive () dir =
      (match dir.G.d with
       | G.ImportFrom (_, _, names) ->
         List.iter (fun ((name, _), alias_opt) ->
           let local = match alias_opt with
             | Some ((s, _), _) -> s
             | None -> name
           in
           if String.length local > 0 then
             Hashtbl.replace local_imports local ()
         ) names
       | G.ImportAs (_, mn, alias_opt) ->
         let local = match alias_opt with
           | Some ((s, _), _) -> s
           | None ->
             (match mn with
              | G.DottedName ((s, _) :: _) -> s
              | G.DottedName [] -> ""
              | G.FileName (s, _) -> s)
         in
         if String.length local > 0 then
           Hashtbl.replace local_imports local ()
       | _ -> ());
      super#visit_directive () dir
  end in
  imp_visitor#visit_function_definition () fdef;
  let local_imports =
    if Int.equal (Hashtbl.length local_imports) 0 then None
    else Some local_imports
  in
  let func_lookup =
    Func_lookup.with_local_imports func_lookup
      (Option.map Func_lookup.name_set_of_hashtbl local_imports)
  in
  (* Ruby: [foo(bar)] with method [bar] means [foo(bar())]; treat an unresolved Id arg as a call. *)
  let unresolved_arg_call acc arg =
    match arg with
    | G.Arg ({ G.e = G.N (G.Id ((_, tok), id_info)); _ } as arg_expr)
      when Option.is_none !(id_info.G.id_resolved) ->
      (* [allow_constructor:false]: this treats a bare-identifier argument
         as a possible call (Ruby [foo(bar)] ≡ [foo(bar())]), but a class
         name passed as an argument ([api.url_for(Google)]) is not a
         construction, and [super(Cls, self)] must not give [Cls.__init__]
         a self-loop. *)
      (match identify_callee ~lang
               ~all_funcs ~func_lookup ~type_state ~caller_parent_path
               ~allow_constructor:false
               arg_expr with
       | Some fn_id ->
         Log.debug (fun m ->
           m "CALL_EXTRACT: Found unresolved Id that is a function, adding as implicit call");
         (fn_id, tok) :: acc
       | None -> acc)
    | _ -> acc
  in
  let memo_tbl : callee_memo = Hashtbl.create 64 in
  let identify_callee_cached ~(call_arity : int) (callee : G.expr) : fn_id option =
    memo_lookup_or_compute memo_tbl ~call_arity callee (fun () ->
        identify_callee ~lang ~all_funcs
          ~func_lookup ~type_state ~caller_parent_path ~call_arity callee)
  in
  (* Lang-gated: off where nested lambdas need the enclosing scope. *)
  let skip_nested =
    (Lang_config.get lang).Lang_config.skip_nested_in_extract_calls
  in
  let invoke_methods = (Lang_config.get lang).Lang_config.invoke_methods in
  let calls, callbacks =
    Walker.fold_exprs_in_fdef ~skip_nested_fdefs:skip_nested
      (fun ((calls, callbacks) as acc) e ->
        match e.G.e with
        | G.Call ({ e = G.IdSpecial (G.Op _, _); _ }, _) -> acc
        | G.Call (callee, args) ->
          let (_, args_list, _) = args in
          let cbs = collect_callbacks e.G.e callee args in
          let callbacks = cbs @ callbacks in
          (* Ruby/Crystal/Scala [f(args) do..end] parses as [Call(Call(f,args),[block])]; resolve the inner call. *)
          let call_callee, call_args_list =
            match callee.G.e, args with
            | G.Call (inner_callee, (_, inner_args, _)),
              (_, [ G.Arg { G.e = G.Lambda _; _ } ], _)
              when Lang.(lang =*= Ruby || lang =*= Crystal || lang =*= Scala) ->
              (inner_callee, inner_args)
            | _ -> (callee, args_list)
          in
          let call_arity = List.length call_args_list in
          let calls =
            match identify_callee_cached ~call_arity call_callee with
            | Some fn_id ->
              (* Ruby/Crystal [ClassName.new()] uses the class-name token, not the method token. *)
              let tok =
                match callee.G.e with
                | G.DotAccess (_, _, G.FN (G.Id (("new", _), _)))
                  when Lang.(lang =*= Ruby || lang =*= Crystal) ->
                  (match AST_generic_helpers.ii_of_any (G.E e) with
                   | tok :: _ -> tok
                   | [] -> Tok.unsafe_fake_tok "")
                | G.DotAccess (_, _, G.FN (G.Id ((_, method_tok), _))) ->
                  method_tok
                | _ ->
                  (match AST_generic_helpers.ii_of_any (G.E e) with
                   | tok :: _ -> tok
                   | [] -> Tok.unsafe_fake_tok "")
              in
              (fn_id, tok) :: calls
            | None ->
              (match callee.G.e with
               | G.DotAccess ({ e = G.N (G.Id ((var_name, _), _)); _ }, _,
                              G.FN (G.Id ((method_name, method_tok), _)))
                 when List.mem method_name invoke_methods ->
                 (match find_func_in_scope all_funcs caller_parent_path
                          var_name with
                  | Some f -> (f.fn_id, method_tok) :: calls
                  | None -> calls)
               | _ -> calls)
          in
          let calls = List.fold_left unresolved_arg_call calls call_args_list in
          (calls, callbacks)
        | G.New (_tok, ty, id_info, (_, args_list, _)) ->
          (* Use the class-name token to match class_construction's eorig. *)
          let calls =
            match resolve_constructor_from_type ~lang ~all_funcs ty with
            | Some fn_id ->
              (* [AST_to_IL.mk_class_constructor_name] threads this exact
                 [id_info] onto the IL ctor callee, so the stamp is what
                 the engine's signature lookup reads for [new Cls(...)]. *)
              set_id_resolved_to_def ~allow_located_fake:true id_info fn_id;
              let tok =
                match AST_generic_helpers.ii_of_any (G.T ty) with
                | tok :: _ -> tok
                | [] -> Tok.unsafe_fake_tok ""
              in
              (fn_id, tok) :: calls
            | None -> calls
          in
          let calls = List.fold_left unresolved_arg_call calls args_list in
          (calls, callbacks)
        | G.Xml { G.xml_kind = (G.XmlClassic (_, name, _, _)
                              | G.XmlSingleton (_, name, _)); _ } ->
          let synth =
            G.{ e = G.N name; e_id = -1; e_range = None;
                is_implicit_return = false; facts = [] }
          in
          (match identify_callee ~lang
                   ~all_funcs ~func_lookup ~type_state
                   ~caller_parent_path synth with
           | Some fn_id ->
             let tok = match name with
               | G.Id ((_, t), _) -> t
               | G.IdQualified { name_last = ((_, t), _); _ } -> t
             in
             ((fn_id, tok) :: calls, callbacks)
           | None -> acc)
        | _ -> acc) ([], []) fdef
  in
  let resolved = calls |> dedup_fn_ids in
  (* Same fold scope as [calls] so [total]/[resolved] are commensurable. *)
  let total =
    Walker.fold_exprs_in_fdef ~skip_nested_fdefs:skip_nested (fun n e ->
      match e.G.e with
      | G.Call ({ e = G.IdSpecial (G.Op _, _); _ }, _) -> n
      | G.Call _ | G.New _ -> n + 1
      | _ -> n) 0 fdef
  in
  { calls = resolved;
    callbacks = callbacks;
    unresolved_call_sites = max 0 (total - List.length resolved) }

let extract_decorator_calls ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(type_state : Type_state.t = Type_state.empty)
    ?(caller_parent_path = [])
    (attrs : G.attribute list) : (fn_id * Tok.t) list =
  List.fold_left (fun acc (attr : G.attribute) ->
    match attr with
    | G.NamedAttr (_, name, _args) ->
      let synth = G.{ e = G.N name; e_id = -1; e_range = None;
                       is_implicit_return = false; facts = [] } in
      let call_arity = match _args with
        | (_, args, _) -> List.length args
      in
      (match identify_callee ~lang ~all_funcs
               ~func_lookup ~type_state ~caller_parent_path
               ~call_arity synth with
       | Some fn_id ->
         let tok = match name with
           | G.Id ((_, t), _) -> t
           | G.IdQualified { name_last = ((_, t), _); _ } -> t
         in
         (fn_id, tok) :: acc
       | None -> acc)
    | _ -> acc
  ) [] attrs
  |> dedup_fn_ids

let extract_toplevel_calls ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(type_state : Type_state.t = Type_state.empty)
    (ast : G.program)
  : (fn_id * Tok.t) list =
  (* Clear [local_imports]: not inherited at top level. *)
  let func_lookup = Func_lookup.with_local_imports func_lookup None in
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for top-level statements");
  let memo_tbl : callee_memo = Hashtbl.create 64 in
  let identify_callee_cached ~(call_arity : int) (callee : G.expr) : fn_id option =
    memo_lookup_or_compute memo_tbl ~call_arity callee (fun () ->
        identify_callee ~lang ~all_funcs
          ~func_lookup ~type_state ~caller_parent_path:[] ~call_arity callee)
  in
  Walker.fold_exprs_in_program ~skip_nested_fdefs:true (fun acc e ->
    match e.G.e with
    | G.Call (callee, args) ->
      let call_arity =
        let _, args_list, _ = args in List.length args_list
      in
      (match identify_callee_cached ~call_arity callee with
       | Some fn_id ->
         let tok =
           match callee.G.e with
           | G.DotAccess (_, _, G.FN (G.Id ((_, method_tok), _))) ->
             method_tok
           | _ ->
             (match AST_generic_helpers.ii_of_any (G.E e) with
              | tok :: _ -> tok
              | [] -> Tok.unsafe_fake_tok "")
         in
         Log.debug (fun m ->
           m "CALL_EXTRACT: Found top-level call to %s" (show_fn_id fn_id));
         (fn_id, tok) :: acc
       | None -> acc)
    | _ -> acc) [] ast
  |> dedup_fn_ids

let extract_toplevel_hof_callbacks
    ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    (ast : G.program) : (fn_id * Tok.t) list =
  (* Filter operator pseudo-calls: PEP 604 unions would emit spurious callback edges. *)
  Walker.fold_exprs_in_program ~skip_nested_fdefs:true (fun acc e ->
    match e.G.e with
    | G.Call ({ e = G.IdSpecial (G.Op _, _); _ }, _) -> acc
    | G.Call (_, args) ->
      Tok.unbracket args
      |> List.concat_map
           (try_identify_callback_args ~lang ~all_funcs ~func_lookup
              ~caller_parent_path:[])
      |> List.fold_left (fun acc (cb_fn_id, tok, _tmp_opt) ->
        (cb_fn_id, tok) :: acc) acc
    | _ -> acc) [] ast
  |> dedup_fn_ids

let build_call_graph ~(lang : Lang.t) (ast : G.program)
    : Call_graph.G.t =
  let graph = Call_graph.G.create () in

  let top_level_node : node =
    Function_id.of_il_name (top_level_name_of_ast ast)
  in
  Call_graph.G.add_vertex graph top_level_node;

  let funcs =
    Visit_function_defs.fold_with_parent_path ~lang
      (fun funcs opt_ent parent_path fdef ->
        match fn_id_of_entity ~lang opt_ent parent_path fdef with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            (* Add vertex using the node (last element of fn_id) *)
            (match fn_id_to_node fn_id with
            | Some node -> Call_graph.G.add_vertex graph node
            | None -> ());
            func :: funcs
        | None -> funcs)
      [] ast
  in
  (* Visit all calls in the AST, tracking the current function context *)
  Visit_function_defs.visit_with_parent_path ~lang
    (fun opt_ent parent_path fdef ->
      match fn_id_of_entity ~lang opt_ent parent_path fdef with
      | Some fn_id ->
          let is_toplevel_lambda = match (opt_ent, parent_path) with
            | (None, [None]) | (None, []) -> true
            | _ -> false
          in

          let { calls = callee_calls; callbacks = callback_calls; _ } =
            extract_calls ~lang ~all_funcs:funcs ~caller_parent_path:fn_id fdef
          in

          (* Add labeled edges for each call - edge from callee to caller for bottom-up analysis *)
          List.iter
            (fun (callee_fn_id, call_tok) ->
              match fn_id_to_node callee_fn_id, fn_id_to_node fn_id with
              | Some callee_node, Some caller_node ->
                  Call_graph.add_edge graph ~src:callee_node ~dst:caller_node ~call_tok;
                  if is_toplevel_lambda then
                    Call_graph.add_edge graph ~src:callee_node ~dst:top_level_node ~call_tok
              | _ -> ())
            callee_calls;

          List.iter
            (fun (callback_fn_id, call_tok, tmp_opt) ->
              match fn_id_to_node callback_fn_id, fn_id_to_node fn_id with
              | Some callback_node, Some caller_node ->
                  let src_to_caller = match tmp_opt with
                    | Some tmp_name ->
                        let tmp_node = Function_id.of_il_name tmp_name in
                        Call_graph.add_edge graph ~src:callback_node ~dst:tmp_node ~call_tok;
                        tmp_node
                    | None -> callback_node
                  in
                  Call_graph.add_edge graph ~src:src_to_caller ~dst:caller_node ~call_tok;
                  if is_toplevel_lambda then
                    Call_graph.add_edge graph ~src:src_to_caller ~dst:top_level_node ~call_tok
              | _ -> ())
            callback_calls
      | None -> ())
    ast;

  (* Extract calls from top-level code (outside any function) and add edges to <top_level> *)
  let toplevel_calls = extract_toplevel_calls ~lang ~all_funcs:funcs ast in
  List.iter
    (fun (callee_fn_id, call_tok) ->
      match fn_id_to_node callee_fn_id with
      | Some callee_node ->
          Call_graph.add_edge graph ~src:callee_node ~dst:top_level_node ~call_tok
      | None -> ())
    toplevel_calls;
  Log.debug (fun m -> m "CALL_GRAPH: Added %d edges from top-level calls" (List.length toplevel_calls));

  (* Extract HOF callbacks from top-level code and add edges to <top_level> *)
  let toplevel_hof_callbacks =
    let method_hofs = Lang_config.hof_method_names lang in
    let function_hofs = Lang_config.hof_function_specs lang in
    Visit_function_defs.fold_toplevel_calls (fun acc _call_e callee args ->
      match callee.G.e with
      (* Operator pseudo-calls (PEP 604 unions) would emit spurious
         callback edges; same filter as [extract_toplevel_hof_callbacks]. *)
      | G.IdSpecial (G.Op _, _) -> acc
      | _ ->
        let found = extract_hof_callbacks_from_call
          ~lang ~method_hofs ~function_hofs ~all_funcs:funcs ~caller_parent_path:[]
          callee args
        in
        found @ acc
    ) [] ast
  in
  toplevel_hof_callbacks |> List.iter (fun (callback_fn_id, call_tok, tmp_opt) ->
    match fn_id_to_node callback_fn_id with
    | Some callback_node ->
        let src_to_caller = match tmp_opt with
          | Some tmp_name ->
              let tmp_node = Function_id.of_il_name tmp_name in
              Call_graph.add_edge graph ~src:callback_node ~dst:tmp_node ~call_tok;
              tmp_node
          | None -> callback_node
        in
        Call_graph.add_edge graph ~src:src_to_caller ~dst:top_level_node ~call_tok
    | None -> ());
  Log.debug (fun m -> m "CALL_GRAPH: Added %d edges from top-level HOF callbacks" (List.length toplevel_hof_callbacks));

  (* Add implicit edges from constructors to all methods in the same class.
     Constructors always execute before any method can be called on an object. *)
  List.iter
    (fun func ->
      let func_name_opt = get_fn_name func.fn_id in
      let func_name =
        Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) func_name_opt
      in
      let class_name_opt = Func_info.enclosing_class func.fn_id in
      let class_name_str =
        Option.map (fun n -> fst n.IL.ident) class_name_opt
      in
      if Object_initialization.is_constructor lang func_name class_name_str then
        (* Find all methods in the same class *)
        let same_class_methods =
          List.filter
            (fun other ->
              let other_name_opt = get_fn_name other.fn_id in
              let other_name =
                Option.fold ~none:""
                  ~some:(fun n -> fst n.IL.ident)
                  other_name_opt
              in
              let other_class_opt = Func_info.enclosing_class other.fn_id in
              let other_class_name_str =
                Option.map (fun n -> fst n.IL.ident) other_class_opt
              in
              (not
                 (Object_initialization.is_constructor lang other_name
                    other_class_name_str))
              && Option.equal
                   (fun n1 n2 ->
                     String.equal (fst n1.IL.ident) (fst n2.IL.ident))
                   class_name_opt other_class_opt)
            funcs
        in
        (* Add implicit edge from constructor to each method, only if no explicit edge exists *)
        List.iter
          (fun method_func ->
            match fn_id_to_node func.fn_id, fn_id_to_node method_func.fn_id with
            | Some constructor_node, Some method_node ->
                if not (Call_graph.G.mem_edge graph constructor_node method_node) then
                  Call_graph.add_edge graph ~src:constructor_node ~dst:method_node
                    ~call_tok:(Tok.unsafe_fake_tok "<implicit:constructor>")
            | _ -> ())
          same_class_methods)
    funcs;

  (* Add Class:* vertices for each class and implicit edges from class to methods.
     This handles classes without explicit constructors (e.g., Angular components using inject())
     and ensures class field initializers can propagate taint to methods.
     Edge direction: Class:* -> method (class init runs first, then methods can be called) *)
  let class_names = Object_initialization.collect_class_names ast in
  List.iter (fun class_g_name ->
    let class_il_name = AST_to_IL.var_of_name class_g_name in
    let class_str = fst class_il_name.IL.ident in
    (* Create Class:* node *)
    let class_init_node : node =
      Function_id.of_il_name (class_init_il_name class_str)
    in
    Call_graph.G.add_vertex graph class_init_node;

    (* Find all methods in this class *)
    let class_methods =
      List.filter
        (fun func ->
          let func_class_opt = Func_info.enclosing_class func.fn_id in
          match func_class_opt with
          | Some func_class_il_name ->
              String.equal (fst func_class_il_name.IL.ident) class_str
          | None -> false)
        funcs
    in
    (* Add implicit edge from Class:* to each method (class init happens first, then methods) *)
    List.iter
      (fun method_func ->
        match fn_id_to_node method_func.fn_id with
        | Some method_node ->
            Call_graph.add_edge graph ~src:class_init_node ~dst:method_node
              ~call_tok:(Tok.unsafe_fake_tok "<implicit:class-init>")
        | None -> ())
      class_methods)
    class_names;

  graph

(* Identify functions that contain byte ranges (from pattern matches) *)
let find_functions_containing_ranges ~(lang : Lang.t) (ast : G.program)
    (ranges : Range.t list) : Function_id.t list =
  (* Hash table to track ALL functions containing each range, along with function size *)
  (* Set keyed by [compare_fn_id] so same-leaf funcs at different positions stay distinct. *)
  let module FnIdSet =
    Set.Make (struct
      type t = fn_id
      let compare = compare_fn_id
    end)
  in
  let range_table : (Range.t, FnIdSet.t * (fn_id * int) list) Hashtbl.t =
    Hashtbl.create (List.length ranges)
  in
  List.iter (fun range ->
    Hashtbl.add range_table range (FnIdSet.empty, [])
  ) ranges;
  let add_to_range (range : Range.t) (fn_id : fn_id) (size : int) : unit =
    let seen, funcs = Hashtbl.find range_table range in
    if not (FnIdSet.mem fn_id seen) then
      Hashtbl.replace range_table range
        (FnIdSet.add fn_id seen, (fn_id, size) :: funcs)
  in

  let g_name_to_il_name (g_name : G.name) : IL.name option =
    match g_name with
    | G.Id ((str, tok), id_info) ->
      let id_info = { id_info with G.id_resolved = ref None } in
      Some IL.{ ident = (str, tok); sid = G.SId.unsafe_default; id_info }
    | _ -> None
  in
  let entity_to_il_name (ent : G.entity) : IL.name option =
    match ent.G.name with
    | G.EN name -> g_name_to_il_name name
    | _ -> None
  in
  let visitor = object
    inherit [_] G.iter_no_id_info as super

    method! visit_definition
        (env : G.name option * IL.name option list)
        ((ent, def_kind) as def) =
      let current_class, parent_path = env in
      match def_kind with
      | G.ClassDef cdef ->
          (* Non-[EN]-named class resets [current_class] to [None] (no inherit). *)
          let current_class' =
            match ent.name with
            | EN name -> Some name
            | _ -> None
          in
          let env' = (current_class', parent_path) in

          (* Get the class body range *)
          let (_, cbody_stmts, _) = cdef.cbody in
          let cbody_range_opt = AST_generic_helpers.range_of_any_opt (G.Flds cbody_stmts) in
          (match cbody_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let class_start = range.start in
              let class_end = range.end_ in
              let class_size = class_end - class_start in

              (* For each range, check if it's inside this class *)
              List.iter (fun (range : Range.t) ->
                if class_start <= range.Range.start && range.Range.end_ <= class_end then (
                  (* This class contains this range - add it to the list *)
                  match current_class' with
                  | Some class_g_name ->
                      let class_il_name = AST_to_IL.var_of_name class_g_name in
                      let class_str = fst class_il_name.IL.ident in
                      let class_fn_id =
                        [None; Some (class_init_il_name class_str)]
                      in
                      add_to_range range class_fn_id class_size
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env' def
          | None -> super#visit_definition env' def)
      | G.FuncDef fdef | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Get the entire function definition range (including parameters) *)
          let func_range_opt = AST_generic_helpers.range_of_any_opt (G.Def def) in
          (match func_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let func_start = range.start in
              let func_end = range.end_ in
              let func_size = func_end - func_start in

              (* For each range, check if it's inside this function *)
              List.iter (fun (range : Range.t) ->
                if func_start <= range.Range.start && range.Range.end_ <= func_end then (
                  let class_il = Option.bind current_class g_name_to_il_name in
                  let visitor_parent_path =
                    match parent_path with
                    | [] -> [class_il]
                    | _ -> parent_path
                  in
                  match fn_id_of_entity ~lang (Some ent) visitor_parent_path fdef with
                  | Some fn_id -> add_to_range range fn_id func_size
                  | None -> ()
                )
              ) ranges;

              let class_il = Option.bind current_class g_name_to_il_name in
              let func_il = entity_to_il_name ent in
              let current_fn_id =
                match parent_path with
                | [] -> [class_il; func_il]
                | _ -> parent_path @ [func_il]
              in
              let env' = (current_class, current_fn_id) in
              super#visit_definition env' def
          | None -> super#visit_definition env def)
      | _ -> super#visit_definition env def
  end in

  visitor#visit_program (None, []) ast;

  (* Now select the innermost (smallest) function for each range *)
  List.fold_left (fun matching_funcs range ->
    let _, funcs_list = Hashtbl.find range_table range in
    if List.is_empty funcs_list then
      let top_level_fn_id = [ None; Some (top_level_name_of_ast ast) ] in
      if List.exists (Func_info.equal_fn_id top_level_fn_id) matching_funcs
      then matching_funcs
      else top_level_fn_id :: matching_funcs
    else
      (* Prefer a named encloser over a lambda: a lambda isn't a call-graph vertex, so a source/sink on it would prune the enclosing function. *)
      let is_lambda_fn_id (fn_id : fn_id) =
        match List_.last_opt fn_id with
        | Some (Some name) -> String.equal (fst name.IL.ident) "_tmp_lambda"
        | _ -> false
      in
      let pick_smallest lst =
        List.fold_left (fun best ((_, sz) as cand) ->
          match best with
          | None -> Some cand
          | Some (_, best_sz) when sz < best_sz -> Some cand
          | _ -> best
        ) None lst
      in
      let innermost =
        match
          pick_smallest
            (List.filter (fun (fid, _) -> not (is_lambda_fn_id fid)) funcs_list)
        with
        | Some _ as r -> r
        | None -> pick_smallest funcs_list
      in
      match innermost with
      | None -> matching_funcs
      | Some (innermost_fn_id, _) ->
        if List.exists (Func_info.equal_fn_id innermost_fn_id) matching_funcs
        then matching_funcs
        else innermost_fn_id :: matching_funcs
  ) [] ranges
  |> List.filter_map fn_id_to_node
