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
      if idx = taint_arg_index then
        IL.Unnamed (taint_set, Shape.Bot)
      else IL.Unnamed (Taint.Taint_set.empty, Shape.Bot))

(** Helper to create a function identifier *)
let make_fn_id ?class_name name_str =
  {
    class_name;
    name =
      Some
        {
          IL.ident = (name_str, Tok.unsafe_fake_tok name_str);
          sid = AST_generic.SId.unsafe_default;
          id_info = AST_generic.empty_id_info ();
        };
  }

(** Helper function to add HOF signatures that return a function.
    This is for languages like Ruby where arr.map() returns a function that takes a callback.

    @param db The signature database to add to
    @param method_names List of method names to add signatures for
    @param taint_arg_index Which callback argument receives the taint (default 0).
*)
let add_hof_returning_function_signatures db method_names
    ?(taint_arg_index = 0) () =
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
    { Signature.params = [Signature.P "callback"]; effects = Effects.singleton hof_effect }
  in

  (* The signature for the method itself (arity 0) returns a Fun shape with the array's taints *)
  let return_effect = Effect.ToReturn {
    data_taints = this_taint_set;
    data_shape = Shape.Fun returned_fun_sig;
    control_taints = Taint.Taint_set.empty;
    return_tok = Tok.unsafe_fake_tok "builtin_hof";
  } in
  let method_sig =
    { Signature.params = []; effects = Effects.singleton return_effect }
  in

  (* Add signatures for all methods *)
  List.fold_left
    (fun acc_db method_name ->
      let fn_id = make_fn_id method_name in
      add_signature acc_db fn_id { sig_ = method_sig; arity = 0 })
    db method_names

(** Helper function to add HOF signatures for standalone functions (not methods).
    This creates a signature that models: "Call the callback parameter with another parameter's value"

    For example, map(callback, iterable) passes iterable elements to the callback.

    @param db The signature database to add to
    @param function_names List of function names to add signatures for
    @param arity The number of parameters the function takes
    @param callback_index Which parameter is the callback (default 0)
    @param data_index Which parameter provides the data to pass to callback (default 1)
    @param params The parameter signature
    @param taint_arg_index Which callback argument receives the taint (default 0)
*)
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

  let hof_sig = { Signature.params; effects = Effects.singleton hof_effect } in

  (* Add signatures for all functions *)
  List.fold_left
    (fun acc_db function_name ->
      let fn_id = make_fn_id function_name in
      add_signature acc_db fn_id { sig_ = hof_sig; arity })
    db function_names

(** Helper function to add HOF signatures for a list of methods.
    This creates a signature that models: "Call the callback parameter with the receiver (this) as the first argument"

    For example, arr.map(callback) passes array elements to the callback.

    @param db The signature database to add to
    @param method_names List of method names to add signatures for
    @param arity The number of parameters the function takes
    @param callback_index Which parameter is the callback (default 0). Use -1 for implicit blocks (Ruby/Scala/Kotlin).
    @param params The parameter signature (default [P "callback"])
    @param method_name_transform Optional transformation for method names (e.g., prefix "Enum.")
    @param taint_arg_index Which callback argument receives the taint (default 0). For reduce, use 1.
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

  let hof_sig = { Signature.params; effects = Effects.singleton hof_effect } in

  (* Add signatures for all methods *)
  List.fold_left
    (fun acc_db method_name ->
      let transformed_name = method_name_transform method_name in
      let fn_id = make_fn_id transformed_name in
      add_signature acc_db fn_id { sig_ = hof_sig; arity })
    db method_names

(** Create a signature database with built-in models for standard library HOFs *)
let create_builtin_models (lang : Lang.t) : signature_database =
  let db = empty_signature_database () in

  match lang with
  | Lang.Js
  | Lang.Ts ->
      (* JavaScript/TypeScript Array HOF methods *)
      let array_hof_methods =
        [
          "map";
          "flatMap";
          "filter";
          "forEach";
          "find";
          "findIndex";
          "some";
          "every";
        ]
      in
      (* reduce/reduceRight have different callback signature: (acc, element) => ...
       * The element is the SECOND parameter (index 1), not the first
       * They also take 2 arguments: (callback, initialValue) *)
      let db' = add_hof_signatures db array_hof_methods 1 () in
      add_hof_signatures db' [ "reduce"; "reduceRight" ] 2 ~taint_arg_index:1 ()

  | Lang.Python ->
      (* Python built-in HOFs: map(callback, iterable), filter(callback, iterable)
       * These are standalone functions, not methods, so taint comes from param 1 (iterable) *)
      add_function_hof_signatures db [ "map"; "filter" ] 2 ()

  | Lang.Ruby ->
      (* Ruby Array methods: map, each, select, filter, flat_map, etc.
       * Ruby blocks are implicit - arr.map() with arity 0 returns a function that takes a callback *)
      let ruby_hof_methods =
        [ "map"; "each"; "select"; "filter"; "flat_map"; "collect"; "find"; "detect" ]
      in
      add_hof_returning_function_signatures db ruby_hof_methods ()

  | Lang.Php ->
      (* PHP array functions - these are standalone functions, not methods *)
      (* array_map(callback, array) - callback at 0, data at 1 *)
      let db = add_function_hof_signatures db [ "array_map" ] 2 ~callback_index:0 ~data_index:1 () in
      (* array_filter(array, callback) and array_walk(array, callback) - data at 0, callback at 1 *)
      add_function_hof_signatures db [ "array_filter"; "array_walk" ] 2 ~callback_index:1 ~data_index:0 ()

  | Lang.Java ->
      (* Java Stream methods: map, filter, forEach, flatMap *)
      add_hof_signatures db [ "map"; "filter"; "forEach"; "flatMap" ] 1 ()

  | Lang.Kotlin ->
      (* Kotlin collection methods: map, filter, forEach, flatMap, etc.
       * Lambdas can be passed in different ways, try multiple arities *)
      let kotlin_hof_methods = [ "map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all" ] in
      let db' = add_hof_signatures db kotlin_hof_methods 0 () in
      add_hof_signatures db' kotlin_hof_methods 1 ()

  | Lang.Swift ->
      (* Swift array methods: map, filter, forEach, flatMap, etc. *)
      let swift_hof_methods =
        [ "map"; "filter"; "forEach"; "flatMap"; "compactMap"; "first"; "contains" ]
      in
      add_hof_signatures db swift_hof_methods 1 ()

  | Lang.Scala ->
      (* Scala collection methods: map, filter, foreach, flatMap, etc. *)
      add_hof_signatures db [ "map"; "filter"; "foreach"; "flatMap"; "find"; "exists"; "forall" ] 1 ()

  | Lang.Csharp ->
      (* C# LINQ methods: Select, Where, ForEach, SelectMany, etc. *)
      add_hof_signatures db [ "Select"; "Where"; "ForEach"; "SelectMany"; "First"; "Any"; "All" ] 1 ()

  | Lang.Rust ->
      (* Rust iterator methods: map, for_each, filter, flat_map, etc. *)
      add_hof_signatures db [ "map"; "for_each"; "filter"; "flat_map"; "find"; "any"; "all" ] 1 ()

  | Lang.Julia ->
      (* Julia built-in HOFs: map(callback, iterable), filter(callback, iterable)
       * These are standalone functions, not methods, so taint comes from param 1 (iterable) *)
      add_function_hof_signatures db [ "map"; "foreach"; "filter" ] 2 ()

  | Lang.Cpp ->
      (* C++ algorithm functions: for_each(begin, end, callback)
       * Standalone function where callback is parameter 2, data comes from iterator at param 0 *)
      let db' = add_function_hof_signatures db [ "for_each" ] 3 ~callback_index:2 ~data_index:0
        ~params:[ Signature.Other; Signature.Other; Signature.P "callback" ] () in
      (* C++ transform(begin, end, out, callback) has 4 params, callback at index 3 *)
      add_function_hof_signatures db' [ "transform" ] 4 ~callback_index:3 ~data_index:0
        ~params:[ Signature.Other; Signature.Other; Signature.Other; Signature.P "callback" ] ()

  | Lang.Elixir ->
      (* Elixir Enum module HOFs: Enum.map(enumerable, callback)
       * These are module functions where enumerable is param 0, callback is param 1 *)
      let elixir_hof_methods = [ "map"; "each"; "filter"; "flat_map"; "find" ] in
      add_function_hof_signatures db
        (List.map (fun m -> "Enum." ^ m) elixir_hof_methods)
        2 ~callback_index:1 ~data_index:0
        ~params:[ Signature.Other; Signature.P "callback" ] ()

  | _ -> db
  (** Other languages (C, Apex, Lua, Go) don't have widespread built-in HOFs or
      we haven't modeled them yet *)

let init_signature_database (lang : Lang.t)
    (user_db : signature_database option) : signature_database =
  let builtin_db = create_builtin_models lang in
  match user_db with
  | Some db ->
      (* Merge: user DB signatures + builtin signatures *)
      (* User signatures don't override builtins, they're additive *)
      let merged_signatures =
        FunctionMap.union
          (fun _fn_id user_sigs builtin_sigs ->
            (* Merge both signature sets *)
            Some (SignatureSet.union user_sigs builtin_sigs))
          db.signatures builtin_db.signatures
      in
      { db with signatures = merged_signatures }
  | None -> builtin_db
