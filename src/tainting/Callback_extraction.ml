open Common
module G = AST_generic
module Log = Log_call_graph.Log
open Callee_resolution

(* Helper to extract all callback names referenced by an argument expression.
   Handles:
     - foo, &foo, Module.foo, module.func (DotAccess)
     - Elixir &func/n (ShortLambda wrapping Call)
     - Record { cb: handler, ... } and Dict { "cb": handler, ... } — recurse
       into each entry's value
     - List/Tuple/Array/Set [handler, ...] — recurse into each element
     - Variable aliasing a record/container via [id_svalue]
       (set by [Dataflow_svalue] during parsing) — recurse into the svalue
   Returns a list of (callback_name, tok, shortlambda_tmp_opt).
   - shortlambda_tmp_opt is Some IL.name for the _tmp wrapper node when this
     is an Elixir ShortLambda.
   Over-approximates on purpose: any function-ref nested anywhere in the
   argument is treated as a potential callback. Precision at per-offset
   granularity is handled later by Sig_inst's offset-walk. *)
let rec extract_callbacks_from_arg ~(lang : Lang.t) (arg_expr : G.expr) :
    (IL.name * Tok.t * IL.name option) list =
  match arg_expr.G.e with
  (* Plain identifier: foo — may be a function name directly, OR a variable
     whose id_svalue wraps a record/container we should walk through. We
     always emit the direct interpretation (so [handler] still resolves
     even when it has no svalue), plus any svalue-walk recursion. *)
  | G.N (G.Id (id, id_info)) ->
      let direct =
        [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
      in
      let via_svalue =
        match !(id_info.id_svalue) with
        | Some (G.Sym inner) -> extract_callbacks_from_arg ~lang inner
        | _ -> []
      in
      direct @ via_svalue
  (* Address-of operator: &foo (C/C++ function pointers) *)
  | G.Ref (_, { e = G.N (G.Id (id, id_info)); _ }) ->
      [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
  (* Qualified identifier: Module.foo *)
  | G.N (G.IdQualified { name_last = id, _; name_info; _ }) ->
      [ (AST_to_IL.var_of_id_info id name_info, snd id, None) ]
  (* DotAccess: module.func or obj.method - common in Python/JS *)
  | G.DotAccess (_, _, G.FN (G.Id (id, id_info))) ->
      [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
  (* Elixir: &func/n or &Mod.func/n - ShortLambda wrapping a call to the
     named (local or remote) function. Structure:
     OtherExpr("ShortLambda", [Params[&1,...]; S(ExprStmt(Call(func, args)))])
     where func is either a plain Id or a DotAccess(..., FN(Id)).
     Create a _tmp node to match what AST_to_IL creates for the anonymous wrapper. *)
  | G.OtherExpr
      ( ("ShortLambda", shortlambda_tok),
        [ G.Params _; G.S { G.s = G.ExprStmt (inner_e, _); _ } ] ) -> (
      match inner_e.G.e with
      | G.Call
          ( {
              e =
                ( G.N (G.Id (id, id_info))
                | G.DotAccess (_, _, G.FN (G.Id (id, id_info))) );
              _;
            },
            _ ) ->
          let callback_name = AST_to_IL.var_of_id_info id id_info in
          let tmp_name =
            Visit_function_defs.synth_lambda_il_name_of_tok shortlambda_tok
          in
          [ (callback_name, snd id, Some tmp_name) ]
      | _ -> [])
  (* Record literal: recurse into each field's value *)
  | G.Record (_, fields, _) ->
      List.concat_map
        (fun f ->
          match f with
          | G.F
              {
                s =
                  G.DefStmt
                    (_, (G.VarDef { G.vinit = Some v; _ } | G.FieldDefColon { G.vinit = Some v; _ }));
                _;
              } ->
              extract_callbacks_from_arg ~lang v
          | _ -> [])
        fields
  (* Dict literal: entries are G.Container(G.Tuple, [key; val]); recurse val *)
  | G.Container (G.Dict, (_, kvs, _)) ->
      List.concat_map
        (fun kv ->
          match kv.G.e with
          | G.Container (G.Tuple, (_, [ _key; v ], _)) ->
              extract_callbacks_from_arg ~lang v
          | _ -> [])
        kvs
  (* List/Tuple/Array/Set literal: recurse into each element *)
  | G.Container ((G.List | G.Tuple | G.Array | G.Set), (_, xs, _)) ->
      List.concat_map (extract_callbacks_from_arg ~lang) xs
  (* Ruby [method(:name)]: a callable reference to the named function.
     Sym-prop carries the [Call(method, [Atom :name])] expression on
     the callback variable's [id_svalue], so the recursion above
     reaches us for an aliased binding [cb = method(:name); apply_cb(cb, ...)]. *)
  | G.Call
      ( { e = G.N (G.Id (("method", _), _)); _ },
        (_, [ G.Arg { e = G.L (G.Atom (_, atom_ident)); _ } ], _) )
    when lang =*= Lang.Ruby ->
      let synthetic_name =
        IL.
          {
            ident = atom_ident;
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          }
      in
      [ (synthetic_name, snd atom_ident, None) ]
  | _ -> []


(* Helper to identify a callback fn_id, checking nested functions in same scope first *)
let identify_callback ?(all_funcs = []) ?(caller_parent_path = [])
    (callback_name : IL.name) : fn_id option =
  let callback_name_str = fst callback_name.IL.ident in
  (* Extract class from caller_parent_path if present *)
  let current_class = match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  (* First check if it's a nested function in the same scope - position-aware match *)
  let nested_match =
    find_func_in_scope all_funcs caller_parent_path callback_name_str
  in

  (match nested_match with
  | Some f ->
      Log.debug (fun m -> m "HOF_EXTRACT: Found nested callback %s in same scope" callback_name_str);
      Some f.fn_id
  | None ->
      (* Fall back to class methods or top-level functions - match by string name *)
      let class_method_match = match current_class with
        | Some cls ->
            let class_name_str = fst cls.IL.ident in
            List.find_opt (fun f ->
              match f.fn_id with
              | [Some c; Some _] ->
                  fst c.IL.ident = class_name_str
                  && func_info_name_matches f callback_name_str
              | _ -> false
            ) all_funcs
        | None -> None
      in

      (match class_method_match with
      | Some f ->
          Log.debug (fun m -> m "HOF_EXTRACT: Found class method callback %s" callback_name_str);
          Some f.fn_id
      | None ->
          (* Check for top-level function - match by string name *)
          let top_level_match = List.find_opt (fun f ->
            match f.fn_id with
            | [None; Some _] -> func_info_name_matches f callback_name_str
            | _ -> false
          ) all_funcs in

          (match top_level_match with
          | Some f ->
              Log.debug (fun m -> m "HOF_EXTRACT: Found top-level callback %s" callback_name_str);
              Some f.fn_id
          | None ->
              Log.debug (fun m -> m "HOF_EXTRACT: Callback %s not found in functions list" callback_name_str);
              None)))

(* Try to identify a callback from a G.argument, returning fn_id, token, and optional _tmp node.
   The _tmp node is present for Elixir ShortLambda to create the intermediate wrapper node. *)
(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry multiple callbacks when it's a record/list
   containing several function references, or a variable whose [id_svalue]
   wraps such a container. See [extract_callbacks_from_arg]. *)
let try_identify_callback_args ~lang ~all_funcs ~caller_parent_path (arg : G.argument) :
    (fn_id * Tok.t * IL.name option) list =
  let resolve_in_expr expr =
    (* Also handle this.foo pattern *)
    let direct_this =
      match expr.G.e with
      | G.DotAccess
          ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
            _,
            G.FN (G.Id (id, id_info)) ) ->
          [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
      | _ -> []
    in
    let candidates = direct_this @ extract_callbacks_from_arg ~lang expr in
    List.filter_map
      (fun (callback_name, tok, tmp_opt) ->
        (* Use real token from the callback argument *)
        identify_callback ~all_funcs ~caller_parent_path callback_name
        |> Option.map (fun fn_id -> (fn_id, tok, tmp_opt)))
      candidates
  in
  match arg with
  | G.Arg expr -> resolve_in_expr expr
  (* Keyword args: Ruby [my_hof(cb: h, data: x)] and Python [f(cb=h, data=x)]
     lower each key-value as an [ArgKwd]. The key is a tag name; recurse on
     the value expression to extract any nested callback references. *)
  | G.ArgKwd (_, expr) | G.ArgKwdOptional (_, expr) -> resolve_in_expr expr
  | G.ArgType _ | G.OtherArg _ -> []

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
let extract_hof_callbacks_from_call ~lang ~method_hofs ~function_hofs ~all_funcs
    ~caller_parent_path (callee : G.expr) (args : G.arguments) :
    (fn_id * Tok.t * IL.name option) list =
  let try_arg arg =
    try_identify_callback_args ~lang ~all_funcs ~caller_parent_path arg
  in
  let try_arg_at_index idx =
    match List.nth_opt (Tok.unbracket args) idx with
    | Some arg -> try_arg arg
    | None -> []
  in
  (* Check ALL arguments for function references - any function passed as
     arg, nested inside a record/dict/list literal, or reachable via
     [id_svalue] through such a container, is treated as a callback. *)
  let all_callback_args =
    Tok.unbracket args |> List.concat_map try_arg
  in
  (* Check for specific configured HOF patterns for additional context *)
  let configured_callbacks =
    match callee.G.e with
    (* Method HOF: arr.map(callback) - callback at index 0 *)
    | G.DotAccess (_, _, G.FN (G.Id ((method_name, _), _)))
      when List.mem method_name method_hofs ->
        try_arg_at_index 0
    (* Function HOF: map(callback, arr) *)
    | G.N (G.Id (id, _id_info)) -> (
        let func_name = fst id in
        match
          List.find_opt
            (fun (names, _) -> List.mem func_name names)
            function_hofs
        with
        | Some (_, callback_index) -> try_arg_at_index callback_index
        | None -> [])
    | _ -> []
  in
  all_callback_args @ configured_callbacks

(* Extract HOF callbacks, returning (fn_id, tok, tmp_opt) tuples.
   tmp_opt is Some IL.name for ShortLambda callbacks that need a _tmp intermediate node. *)
let extract_hof_callbacks ?(_object_mappings = []) ?(all_funcs = [])
    ?(caller_parent_path = [])
    ~(lang : Lang.t) (fdef : G.function_definition) : (fn_id * Tok.t * IL.name option) list =
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

  let callbacks = ref [] in
  let v =
    object
      inherit [_] G.iter as super
      method! visit_expr env e =
        (match e.G.e with
        (* Ruby/Crystal/Scala block pattern: f(args) { block } is Call(Call(callee, inner_args), [block]).
           Merge inner_args and block args so the HOF detection sees all arguments together. *)
        | G.Call ({ e = G.Call (callee, inner_args); _ },
                  (_, ([ G.Arg { G.e = G.Lambda _; _ } ] as outer_arg), _))
          when Lang.(lang =*= Ruby || lang =*= Crystal || lang =*= Scala) ->
            let merged_args = Tok.unsafe_fake_bracket
              (Tok.unbracket inner_args @ outer_arg) in
            let found = extract_hof_callbacks_from_call
              ~lang ~method_hofs ~function_hofs ~all_funcs ~caller_parent_path
              callee merged_args
            in
            callbacks := found @ !callbacks
        | G.Call (callee, args) ->
            let found = extract_hof_callbacks_from_call
              ~lang ~method_hofs ~function_hofs ~all_funcs ~caller_parent_path
              callee args
            in
            callbacks := found @ !callbacks
        | _ -> ());
        super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  !callbacks
