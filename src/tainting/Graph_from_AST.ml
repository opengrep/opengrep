open Common
module G = AST_generic
module Log = Log_call_graph.Log
(*  *open Shape_and_sig *)
module Reachable = Graph_reachability
open Callee_resolution
open Callback_extraction

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

(* Extract all calls from a function body and resolve them to fn_ids *)
let extract_calls ~(lang : Lang.t) ?(object_mappings = []) ?(all_funcs = []) ?(caller_parent_path = [])
    (fdef : G.function_definition) : (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for function");
  let calls = ref [] in
  (* Check if an argument is an unresolved Id that could be a function call.
   * In Ruby, `foo(bar)` where `bar` is a method is actually `foo(bar())`.
   * If id_resolved is None and we can identify it as a function, add it as a call. *)
  let check_arg_for_unresolved_function_call arg =
    match arg with
    | G.Arg arg_exp ->
        (match arg_exp.G.e with
        | G.N (G.Id ((_, tok), id_info)) ->
            (* Check if this Id is unresolved *)
            (match !(id_info.G.id_resolved) with
            | None ->
                (* Unresolved - try to identify it as a function *)
                (match identify_callee ~lang ~object_mappings ~all_funcs ~caller_parent_path arg_exp with
                | Some fn_id ->
                    Log.debug (fun m -> m "CALL_EXTRACT: Found unresolved Id that is a function, adding as implicit call");
                    calls := (fn_id, tok) :: !calls
                | None -> ())
            | Some _ -> ())
        | _ -> ())
    | _ -> ()
  in
  let v =
    object (self)
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        | G.Call (callee, args) ->
            let (_, args_list, _) = args in
            let call_arity = List.length args_list in
            (match identify_callee ~lang ~object_mappings ~all_funcs ~caller_parent_path ~call_arity callee with
            | Some fn_id ->
                (* For DotAccess calls, use the method name token so it
                   matches the method_tok lookup in get_signature_for_object.
                   Exception: Ruby/Crystal ClassName.new() — use the class name
                   token (top of expression) since the constructor machinery
                   uses tok_of_eorig which points to the class name. *)
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
                calls := (fn_id, tok) :: !calls
            | None ->
                (* Invoke-method pattern: var.run() where var is a lambda.
                   If the method name is a configured invoke method, look for
                   a lambda with the receiver's name in the current scope. *)
                let invoke_methods = (Lang_config.get lang).invoke_methods in
                (match callee.G.e with
                | G.DotAccess ({ e = G.N (G.Id ((var_name, _), _)); _ }, _,
                               G.FN (G.Id ((method_name, method_tok), _)))
                  when List.mem method_name invoke_methods ->
                    let lambda_match =
                      find_func_in_scope all_funcs caller_parent_path var_name
                    in
                    (match lambda_match with
                    | Some f -> calls := (f.fn_id, method_tok) :: !calls
                    | None -> ())
                | _ -> ()));
            (* Check arguments for unresolved function calls (Ruby-style) *)
            List.iter check_arg_for_unresolved_function_call args_list;
            (* Visit callee expression for nested calls (e.g., Ruby's File.open(path_for(x)) do ... end
               where the callee is itself a Call containing path_for(x) in its args) *)
            self#visit_expr env callee;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | G.New (_tok, ty, _id_info, args) ->
            (* Constructor call: new ClassName(args).
               Use the class name token so it matches the eorig token
               in class_construction's constructor expression. *)
            (match resolve_constructor_from_type ~lang ~all_funcs ty with
            | Some fn_id ->
                let tok =
                  match AST_generic_helpers.ii_of_any (G.T ty) with
                  | tok :: _ -> tok
                  | [] -> Tok.unsafe_fake_tok ""
                in
                calls := (fn_id, tok) :: !calls
            | None -> ());
            let (_, args_list, _) = args in
            List.iter check_arg_for_unresolved_function_call args_list;
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id and tok *)
  !calls |> dedup_fn_ids

(* Extract calls from top-level statements (outside any function).
   This returns a list of (callee_fn_id, call_tok) pairs. *)
let extract_toplevel_calls ~(lang : Lang.t) ?(object_mappings = []) ?(all_funcs = []) (ast : G.program) : (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for top-level statements");
  let calls = ref [] in

  (* Build a set of byte ranges covered by function bodies *)
  let func_ranges = ref [] in
  List.iter (fun func ->
    let body_stmt = AST_generic_helpers.funcbody_to_stmt func.fdef.G.fbody in
    match AST_generic_helpers.range_of_any_opt (G.S body_stmt) with
    | Some (loc_start, loc_end) ->
        let range = Range.range_of_token_locations loc_start loc_end in
        func_ranges := (range.start, range.end_) :: !func_ranges
    | None -> ())
    all_funcs;

  (* Check if a position is inside any function body *)
  let is_inside_function pos =
    List.exists (fun (start, stop) -> pos >= start && pos <= stop) !func_ranges
  in

  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        | G.Call (callee, args) ->
            (* Check if this call is at top-level (not inside a function) *)
            let call_pos =
              match AST_generic_helpers.ii_of_any (G.E e) with
              | tok :: _ when not (Tok.is_fake tok) -> Tok.bytepos_of_tok tok
              | _ -> -1
            in
            if call_pos >= 0 && not (is_inside_function call_pos) then (
              (* Top-level call - no class context *)
              match identify_callee ~lang ~object_mappings ~all_funcs ~caller_parent_path:[] callee with
              | Some fn_id ->
                  let tok =
                    match AST_generic_helpers.ii_of_any (G.E e) with
                    | tok :: _ -> tok
                    | [] -> Tok.unsafe_fake_tok ""
                  in
                  Log.debug (fun m -> m "CALL_EXTRACT: Found top-level call to %s" (show_fn_id fn_id));
                  calls := (fn_id, tok) :: !calls
              | None -> ()
            );
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_program () ast;
  !calls |> dedup_fn_ids

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : Call_graph.G.t =
  let graph = Call_graph.G.create () in

  (* Create a special top_level node to represent code outside functions *)
  let top_level_node : node =
    let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
    let il_name = IL.{ ident = ("<top_level>", fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () } in
    Function_id.of_il_name il_name
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
          (* Check if this is a top-level lambda/block (no entity AND parent_path is [None] or []) *)
          (* Named functions (def foo) have opt_ent = Some _, lambdas have opt_ent = None *)
          let is_toplevel_lambda = match (opt_ent, parent_path) with
            | (None, [None]) | (None, []) -> true
            | _ -> false
          in

          (* Extract calls - class context is already in fn_id *)
          let callee_calls =
            extract_calls ~lang ~object_mappings ~all_funcs:funcs ~caller_parent_path:fn_id fdef
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

          (* Extract HOF callbacks and add edges: callback -> caller (or callback -> _tmp -> caller for ShortLambda) *)
          let callback_calls =
            extract_hof_callbacks ~_object_mappings:object_mappings ~all_funcs:funcs ~caller_parent_path:fn_id ~lang fdef
          in
          (* Add labeled edges for each callback - edge from callback to caller for bottom-up analysis.
             For ShortLambda, create intermediate _tmp node: callback -> _tmp -> caller *)
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
  let toplevel_calls = extract_toplevel_calls ~lang ~object_mappings ~all_funcs:funcs ast in
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
    let hof_configs = (Lang_config.get lang).hof_configs in
    let method_hofs =
      hof_configs |> List.concat_map (function
        | Lang_config.MethodHOF { methods; _ } -> methods
        | Lang_config.ReturningFunctionHOF { methods; _ } -> methods
        | _ -> [])
    in
    let function_hofs =
      hof_configs |> List.filter_map (function
        | Lang_config.FunctionHOF { functions; callback_index; _ } ->
            Some (functions, callback_index)
        | _ -> None)
    in
    Visit_function_defs.fold_toplevel_calls (fun acc _call_e callee args ->
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
      let class_name_opt = match func.fn_id with class_opt :: _ -> class_opt | [] -> None in
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
              let other_class_opt = match other.fn_id with class_opt :: _ -> class_opt | [] -> None in
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
      let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
      let il_name = IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = G.empty_id_info () } in
      Function_id.of_il_name il_name
    in
    Call_graph.G.add_vertex graph class_init_node;

    (* Find all methods in this class *)
    let class_methods =
      List.filter
        (fun func ->
          let func_class_opt = match func.fn_id with class_opt :: _ -> class_opt | [] -> None in
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

  (* DEBUG: Uncomment to dump call graph to DOT file
  let oc = open_out "/tmp/call_graph.dot" in
  Call_graph.Dot.output_graph oc graph;
  close_out oc;
  *)

  graph

(* Identify functions that contain byte ranges (from pattern matches) *)
let find_functions_containing_ranges ~(lang : Lang.t) (ast : G.program)
    (ranges : Range.t list) : Function_id.t list =
  (* Hash table to track ALL functions containing each range, along with function size *)
  let range_to_funcs : (Range.t, (fn_id * int) list) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun range -> Hashtbl.add range_to_funcs range []) ranges;

  let visitor = object (self)
    inherit [_] G.iter_no_id_info as super
    val current_class : G.name option ref = ref None
    val parent_path : IL.name option list ref = ref []
    (* True while visiting inside any function definition. Used to skip
       lambdas that are nested inside another function — for source/sink
       containment we want the enclosing non-lambda fn, since closure-like
       lambdas are handled by the IL-level inner extraction, not by treating
       them as the source/sink-bearing function. Top-level lambdas (e.g. JS
       [const f = () => …]) are not nested and are still registered as the
       containing function. *)
    val inside_function : bool ref = ref false

    (* Helper to convert G.name to IL.name *)
    method private g_name_to_il_name (g_name : G.name) : IL.name option =
      match g_name with
      | G.Id ((str, tok), id_info) ->
          let id_info = { id_info with G.id_resolved = ref None } in
          Some IL.{ ident = (str, tok); sid = G.SId.unsafe_default; id_info }
      | _ -> None

    (* Helper to get IL.name from entity *)
    method private entity_to_il_name (ent : G.entity) : IL.name option =
      match ent.G.name with
      | G.EN name -> self#g_name_to_il_name name
      | _ -> None

    method! visit_definition (env : unit) ((ent, def_kind) as def) =
      match def_kind with
      | G.ClassDef cdef ->
          let old_class = !current_class in
          (current_class :=
             match ent.name with
             | EN name -> Some name
             | _ -> None);

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
                  match !current_class with
                  | Some class_g_name ->
                      let class_il_name = AST_to_IL.var_of_name class_g_name in
                      let class_str = fst class_il_name.IL.ident in
                      let class_node_name =
                        let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
                        Some IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
                      in
                      let class_fn_id = [None; class_node_name] in
                      let existing = Hashtbl.find range_to_funcs range in
                      if not (List.exists (fun (fid, _) -> equal_fn_id fid class_fn_id) existing) then
                        Hashtbl.replace range_to_funcs range ((class_fn_id, class_size) :: existing)
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env def
          | None -> super#visit_definition env def);
          current_class := old_class
      | G.FuncDef fdef | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Get the entire function definition range (including parameters) *)
          let func_range_opt = AST_generic_helpers.range_of_any_opt (G.Def def) in
          (* A lambda nested inside another function is closure-like; we don't
             want it to win as the source/sink-containing fn for ranges inside
             its body. *)
          let is_nested_lambda =
            !inside_function
            &&
            (match fst fdef.fkind with
             | G.LambdaKind | G.Arrow -> true
             | _ -> false)
          in
          (match func_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let func_start = range.start in
              let func_end = range.end_ in
              let func_size = func_end - func_start in

              (* For each range, check if it's inside this function *)
              if not is_nested_lambda then
                List.iter (fun (range : Range.t) ->
                  if func_start <= range.Range.start && range.Range.end_ <= func_end then (
                    (* This function contains this range - add it to the list *)
                    (* Use proper parent_path tracking for nested functions *)
                    let class_il = Option.bind !current_class self#g_name_to_il_name in
                    let visitor_parent_path =
                      match !parent_path with
                      | [] -> [class_il]
                      | _ -> !parent_path
                    in
                    match fn_id_of_entity ~lang (Some ent) visitor_parent_path fdef with
                    | Some fn_id ->
                        let existing = Hashtbl.find range_to_funcs range in
                        if not (List.exists (fun (fid, _) -> equal_fn_id fid fn_id) existing) then
                          Hashtbl.replace range_to_funcs range ((fn_id, func_size) :: existing)
                    | None -> ()
                  )
                ) ranges;

              (* Push current function onto parent_path for nested functions,
                 and mark that we are now inside a function. *)
              let class_il = Option.bind !current_class self#g_name_to_il_name in
              let func_il = self#entity_to_il_name ent in
              let current_fn_id =
                match !parent_path with
                | [] -> [class_il; func_il]
                | _ -> !parent_path @ [func_il]
              in
              Common.save_excursion_unsafe parent_path current_fn_id (fun () ->
                Common.save_excursion_unsafe inside_function true (fun () ->
                  super#visit_definition env def))
          | None -> super#visit_definition env def)
      | _ -> super#visit_definition env def
  end in

  visitor#visit_program () ast;

  (* Now select the innermost (smallest) function for each range *)
  List.fold_left (fun matching_funcs range ->
    let funcs_list = Hashtbl.find range_to_funcs range in
    if List.is_empty funcs_list then
      (* No function contains this range - it's at top level *)
      let top_level_name =
        let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
        Some IL.{ ident = ("<top_level>", fake_tok);
                  sid = G.SId.unsafe_default;
                  id_info = AST_generic.empty_id_info () }
      in
      let top_level_fn_id = [None; top_level_name] in
      if List.mem top_level_fn_id matching_funcs then
        matching_funcs
      else
        top_level_fn_id :: matching_funcs
    else
      (* Sort by size and pick the smallest (innermost) *)
      let sorted =
        List.sort (fun (_, size1) (_, size2) -> compare size1 size2) funcs_list
      in
      let (innermost_fn_id, _) = List.hd sorted in
      if List.mem innermost_fn_id matching_funcs then
        matching_funcs
      else
        innermost_fn_id :: matching_funcs
  ) [] ranges
  |> List.filter_map fn_id_to_node
