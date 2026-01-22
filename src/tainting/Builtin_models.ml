(* Built-in models for standard library functions *)

open Shape_and_sig

(** Helper to create a callback variable *)
let make_callback_var () =
  {
    IL.ident = ("callback", Tok.unsafe_fake_tok "callback");
    sid = AST_generic.SId.unsafe_default;
    id_info = AST_generic.empty_id_info ();
  }

(** Helper to create args_taints list with taint at specified index *)
let make_args_taints taint_set taint_arg_index =
  List.init (taint_arg_index + 1) (fun idx ->
      if idx = taint_arg_index then IL.Unnamed (taint_set, Shape.Bot)
      else IL.Unnamed (Taint.Taint_set.empty, Shape.Bot))

(** Helper function to add HOF signatures that return a function. This is for
    languages like Ruby where arr.map() returns a function that takes a
    callback.

    @param db The builtin signature database to add to
    @param method_names List of method names to add signatures for
    @param taint_arg_index
      Which callback argument receives the taint (default 0). *)
let add_hof_returning_function_signatures db method_names ?(taint_arg_index = 0)
    () =
  let callback_var = make_callback_var () in

  (* Create a taint from BThis to pass to the callback *)
  let this_taint =
    Taint.{ orig = Var { base = BThis; offset = [] }; tokens = [] }
  in
  let this_taint_set = Taint.Taint_set.singleton this_taint in
  let args_taints = make_args_taints this_taint_set taint_arg_index in

  (* The effect when the returned function is called with a callback *)
  let hof_effect =
    Effect.ToSinkInCall
      {
        callee =
          {
            IL.e = IL.Fetch { base = IL.Var callback_var; rev_offset = [] };
            eorig = NoOrig;
          };
        arg = { Taint.name = "callback"; index = 0 };
        args_taints;
      }
  in

  (* The signature of the function that will be returned *)
  let returned_fun_sig =
    {
      Signature.params = [ Signature.P "callback" ];
      effects = Effects.singleton hof_effect;
    }
  in

  (* The signature for the method itself (arity 0) returns a Fun shape with the array's taints *)
  let return_effect =
    Effect.ToReturn
      {
        data_taints = this_taint_set;
        data_shape = Shape.Fun returned_fun_sig;
        control_taints = Taint.Taint_set.empty;
        return_tok = Tok.unsafe_fake_tok "builtin_hof";
      }
  in
  let method_sig =
    { Signature.params = []; effects = Effects.singleton return_effect }
  in

  (* Add signatures for all methods using simple string keys *)
  List.fold_left
    (fun acc_db method_name ->
      add_builtin_signature acc_db method_name { sig_ = method_sig; arity = 0 })
    db method_names

(** Helper function to add HOF signatures for standalone functions (not
    methods). This creates a signature that models: "Call the callback parameter
    with another parameter's value"

    For example, map(callback, iterable) passes iterable elements to the
    callback.

    @param db The builtin signature database to add to
    @param function_names List of function names to add signatures for
    @param arity The number of parameters the function takes
    @param callback_index Which parameter is the callback (default 0)
    @param data_index
      Which parameter provides the data to pass to callback (default 1)
    @param params The parameter signature
    @param taint_arg_index
      Which callback argument receives the taint (default 0) *)
let add_function_hof_signatures db function_names arity ?(callback_index = 0)
    ?(data_index = 1) ?(params = [ Signature.P "callback"; Signature.Other ])
    ?(taint_arg_index = 0) () =
  let callback_arg = { Taint.name = "callback"; index = callback_index } in
  let callback_var = make_callback_var () in

  (* Create a taint from the data parameter to pass to the callback *)
  let data_arg = { Taint.name = "data"; index = data_index } in
  let data_param_taint =
    Taint.{ orig = Var { base = BArg data_arg; offset = [] }; tokens = [] }
  in
  let data_taint_set = Taint.Taint_set.singleton data_param_taint in
  let args_taints = make_args_taints data_taint_set taint_arg_index in

  let hof_effect =
    Effect.ToSinkInCall
      {
        callee =
          {
            IL.e = IL.Fetch { base = IL.Var callback_var; rev_offset = [] };
            eorig = NoOrig;
          };
        arg = callback_arg;
        args_taints;
      }
  in

  (* Also add a ToReturn effect to propagate data taint to the return value.
     This is essential for chained HOFs like map(f, filter(g, data)). *)
  let return_effect =
    Effect.ToReturn
      {
        data_taints = data_taint_set;
        data_shape = Shape.Bot;
        control_taints = Taint.Taint_set.empty;
        return_tok = Tok.unsafe_fake_tok "builtin_hof";
      }
  in

  let hof_sig = { Signature.params; effects = Effects.of_list [hof_effect; return_effect] } in

  (* Add signatures for all functions using simple string keys *)
  List.fold_left
    (fun acc_db function_name ->
      add_builtin_signature acc_db function_name { sig_ = hof_sig; arity })
    db function_names

(** Helper function to add HOF signatures for a list of methods. This creates a
    signature that models: "Call the callback parameter with the receiver (this)
    as the first argument"

    For example, arr.map(callback) passes array elements to the callback.

    @param db The builtin signature database to add to
    @param method_names List of method names to add signatures for
    @param arity The number of parameters the function takes
    @param callback_index
      Which parameter is the callback (default 0). Use -1 for implicit blocks
      (Ruby/Scala/Kotlin).
    @param params The parameter signature (default [P "callback"])
    @param method_name_transform
      Optional transformation for method names (e.g., prefix "Enum.")
    @param taint_arg_index
      Which callback argument receives the taint (default 0). For reduce, use 1.
*)
let add_hof_signatures db method_names arity ?(callback_index = 0)
    ?(params = [ Signature.P "callback" ]) ?(method_name_transform = fun x -> x)
    ?(taint_arg_index = 0) () =
  let callback_arg = { Taint.name = "callback"; index = callback_index } in
  let callback_var = make_callback_var () in

  (* Create a taint from BThis to pass to the callback *)
  let this_taint =
    Taint.{ orig = Var { base = BThis; offset = [] }; tokens = [] }
  in
  let this_taint_set = Taint.Taint_set.singleton this_taint in
  let args_taints = make_args_taints this_taint_set taint_arg_index in

  let hof_effect =
    Effect.ToSinkInCall
      {
        callee =
          {
            IL.e = IL.Fetch { base = IL.Var callback_var; rev_offset = [] };
            eorig = NoOrig;
          };
        arg = callback_arg;
        args_taints;
      }
  in

  (* Also add a ToReturn effect to propagate this taint to the return value.
     This is essential for chained HOFs like arr.map(...).filter(...) where
     the result of map needs to carry taint for filter to propagate. *)
  let return_effect =
    Effect.ToReturn
      {
        data_taints = this_taint_set;
        data_shape = Shape.Bot;
        control_taints = Taint.Taint_set.empty;
        return_tok = Tok.unsafe_fake_tok "builtin_hof";
      }
  in

  let hof_sig = { Signature.params; effects = Effects.of_list [hof_effect; return_effect] } in

  (* Add signatures for all methods using simple string keys *)
  List.fold_left
    (fun acc_db method_name ->
      let transformed_name = method_name_transform method_name in
      add_builtin_signature acc_db transformed_name { sig_ = hof_sig; arity })
    db method_names

(** Create params list from arity and callback_index *)
let make_params arity callback_index =
  List.init arity (fun i ->
    if i = callback_index then Signature.P "callback"
    else Signature.Other)

(** Create a builtin signature database with built-in models for standard library HOFs *)
let create_builtin_models (lang : Lang.t) : builtin_signature_database =
  let db = empty_builtin_signature_database () in
  let config = Lang_config.get lang in

  (* Convert configs to signatures *)
  List.fold_left
    (fun acc_db hof_config ->
      match hof_config with
      | Lang_config.MethodHOF { methods; arity; taint_arg_index } ->
          add_hof_signatures acc_db methods arity ~taint_arg_index ()
      | Lang_config.FunctionHOF { functions; arity; callback_index; data_index } ->
          let params = make_params arity callback_index in
          add_function_hof_signatures acc_db functions arity ~callback_index
            ~data_index ~params ()
      | Lang_config.ReturningFunctionHOF { methods } ->
          add_hof_returning_function_signatures acc_db methods ())
    db config.hof_configs

(* ========================================================================== *)
(* Primitive helpers for building taint sets and effects *)
(* ========================================================================== *)

let this_taint_set () =
  let taint = Taint.{ orig = Var { base = BThis; offset = [] }; tokens = [] } in
  Taint.Taint_set.singleton taint

let arg_taint_set index =
  let arg = { Taint.name = "value"; index } in
  let taint = Taint.{ orig = Var { base = BArg arg; offset = [] }; tokens = [] } in
  Taint.Taint_set.singleton taint

let return_effect taint_set =
  Effect.ToReturn
    {
      data_taints = taint_set;
      data_shape = Shape.Bot;
      control_taints = Taint.Taint_set.empty;
      return_tok = Tok.unsafe_fake_tok "builtin";
    }

let to_lval_this taint_set =
  Effect.ToLval (taint_set, { Taint.base = BThis; offset = [] })

let add_method_signatures db method_names arity effects =
  let params = List.init arity (fun _ -> Signature.Other) in
  let sig_ = { Signature.params; effects } in
  List.fold_left
    (fun acc_db name -> add_builtin_signature acc_db name { sig_; arity })
    db method_names

(* Collection model signature builders *)

(** Add signatures where an argument taints 'this' (e.g., put, add, append) *)
let add_arg_taints_this_signatures db method_names arity ~taint_arg_index
    ?(returns_this = false) () =
  let to_lval = to_lval_this (arg_taint_set taint_arg_index) in
  let effects =
    if returns_this then
      Effects.of_list [ to_lval; return_effect (this_taint_set ()) ]
    else Effects.singleton to_lval
  in
  add_method_signatures db method_names arity effects

(** Add signatures where 'this' taints the return value (e.g., get, toString) *)
let add_this_taints_return_signatures db method_names arity =
  let effects = Effects.singleton (return_effect (this_taint_set ())) in
  add_method_signatures db method_names arity effects

(** Add collection models to a builtin signature database *)
let add_collection_models db (lang : Lang.t) : builtin_signature_database =
  let config = Lang_config.get lang in
  List.fold_left
    (fun acc_db coll_config ->
      match coll_config with
      | Lang_config.ArgTaintsThis { methods; arity; taint_arg_index; returns_this } ->
          add_arg_taints_this_signatures acc_db methods arity ~taint_arg_index
            ~returns_this ()
      | Lang_config.ThisTaintsReturn { methods; arity } ->
          add_this_taints_return_signatures acc_db methods arity)
    db config.collection_configs

(** Create a builtin signature database with all built-in models (HOFs + collections) *)
let create_all_builtin_models (lang : Lang.t) : builtin_signature_database =
  let db = create_builtin_models lang in
  add_collection_models db lang

(** Initialize the signature database. Now that builtin signatures are separate,
    this function just returns the user DB as-is (or empty if None). *)
let init_signature_database (user_db : signature_database option) :
    signature_database =
  match user_db with
  | Some db -> db
  | None -> empty_signature_database ()
