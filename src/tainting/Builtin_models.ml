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

(** Helper function to add HOF signatures that return a function. This is for
    languages like Ruby where arr.map() returns a function that takes a
    callback.

    @param db The signature database to add to
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

  (* Add signatures for all methods *)
  List.fold_left
    (fun acc_db method_name ->
      let fn_id = make_fn_id method_name in
      add_signature acc_db fn_id { sig_ = method_sig; arity = 0 })
    db method_names

(** Helper function to add HOF signatures for standalone functions (not
    methods). This creates a signature that models: "Call the callback parameter
    with another parameter's value"

    For example, map(callback, iterable) passes iterable elements to the
    callback.

    @param db The signature database to add to
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

  let hof_sig = { Signature.params; effects = Effects.singleton hof_effect } in

  (* Add signatures for all functions *)
  List.fold_left
    (fun acc_db function_name ->
      let fn_id = make_fn_id function_name in
      add_signature acc_db fn_id { sig_ = hof_sig; arity })
    db function_names

(** Helper function to add HOF signatures for a list of methods. This creates a
    signature that models: "Call the callback parameter with the receiver (this)
    as the first argument"

    For example, arr.map(callback) passes array elements to the callback.

    @param db The signature database to add to
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

  let hof_sig = { Signature.params; effects = Effects.singleton hof_effect } in

  (* Add signatures for all methods *)
  List.fold_left
    (fun acc_db method_name ->
      let transformed_name = method_name_transform method_name in
      let fn_id = make_fn_id transformed_name in
      add_signature acc_db fn_id { sig_ = hof_sig; arity })
    db method_names

(** HOF configuration types for reusable abstraction *)
type hof_kind =
  | MethodHOF of { methods : string list; arity : int; taint_arg_index : int }
  | FunctionHOF of {
      functions : string list;
      arity : int;
      callback_index : int;
      data_index : int;
      params : Signature.param list option;
    }
  | ReturningFunctionHOF of { methods : string list }

(** Get HOF configurations for a language without creating signatures. This
    abstraction is used by both signature creation and call graph building. *)
let get_hof_configs (lang : Lang.t) : hof_kind list =
  match lang with
  | Lang.Js
  | Lang.Ts ->
      [
        MethodHOF
          {
            methods =
              [
                "map";
                "flatMap";
                "filter";
                "forEach";
                "find";
                "findIndex";
                "some";
                "every";
              ];
            arity = 1;
            taint_arg_index = 0;
          };
        MethodHOF
          {
            methods = [ "reduce"; "reduceRight" ];
            arity = 2;
            taint_arg_index = 1;
          };
      ]
  | Lang.Python ->
      [
        FunctionHOF
          {
            functions = [ "map"; "filter" ];
            arity = 2;
            callback_index = 0;
            data_index = 1;
            params = None;
          };
      ]
  | Lang.Ruby ->
      [
        ReturningFunctionHOF
          {
            methods =
              [
                "map";
                "each";
                "select";
                "filter";
                "flat_map";
                "collect";
                "find";
                "detect";
              ];
          };
      ]
  | Lang.Php ->
      [
        FunctionHOF
          {
            functions = [ "array_map" ];
            arity = 2;
            callback_index = 0;
            data_index = 1;
            params = None;
          };
        FunctionHOF
          {
            functions = [ "array_filter"; "array_walk" ];
            arity = 2;
            callback_index = 1;
            data_index = 0;
            params = None;
          };
      ]
  | Lang.Java ->
      [
        MethodHOF
          {
            methods = [ "map"; "filter"; "forEach"; "flatMap" ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Kotlin ->
      [
        MethodHOF
          {
            methods =
              [ "map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all" ];
            arity = 0;
            taint_arg_index = 0;
          };
        MethodHOF
          {
            methods =
              [ "map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all" ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Swift ->
      [
        MethodHOF
          {
            methods =
              [
                "map";
                "filter";
                "forEach";
                "flatMap";
                "compactMap";
                "first";
                "contains";
              ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Scala ->
      [
        MethodHOF
          {
            methods =
              [
                "map";
                "filter";
                "foreach";
                "flatMap";
                "find";
                "exists";
                "forall";
              ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Csharp ->
      [
        MethodHOF
          {
            methods =
              [
                "Select";
                "Where";
                "ForEach";
                "SelectMany";
                "First";
                "Any";
                "All";
              ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Rust ->
      [
        MethodHOF
          {
            methods =
              [ "map"; "for_each"; "filter"; "flat_map"; "find"; "any"; "all" ];
            arity = 1;
            taint_arg_index = 0;
          };
      ]
  | Lang.Julia ->
      [
        FunctionHOF
          {
            functions = [ "map"; "foreach"; "filter" ];
            arity = 2;
            callback_index = 0;
            data_index = 1;
            params = None;
          };
      ]
  | Lang.Cpp ->
      [
        FunctionHOF
          {
            functions = [ "for_each" ];
            arity = 3;
            callback_index = 2;
            data_index = 0;
            params =
              Some [ Signature.Other; Signature.Other; Signature.P "callback" ];
          };
        FunctionHOF
          {
            functions = [ "transform" ];
            arity = 4;
            callback_index = 3;
            data_index = 0;
            params =
              Some
                [
                  Signature.Other;
                  Signature.Other;
                  Signature.Other;
                  Signature.P "callback";
                ];
          };
      ]
  | Lang.Elixir ->
      [
        FunctionHOF
          {
            functions =
              List.map
                (fun m -> "Enum." ^ m)
                [ "map"; "each"; "filter"; "flat_map"; "find" ];
            arity = 2;
            callback_index = 1;
            data_index = 0;
            params = Some [ Signature.Other; Signature.P "callback" ];
          };
      ]
  | _ -> []

(** Create a signature database with built-in models for standard library HOFs
*)
let create_builtin_models (lang : Lang.t) : signature_database =
  let db = empty_signature_database () in
  let configs = get_hof_configs lang in

  (* Convert configs to signatures *)
  List.fold_left
    (fun acc_db config ->
      match config with
      | MethodHOF { methods; arity; taint_arg_index } ->
          add_hof_signatures acc_db methods arity ~taint_arg_index ()
      | FunctionHOF { functions; arity; callback_index; data_index; params }
        -> (
          match params with
          | Some p ->
              add_function_hof_signatures acc_db functions arity ~callback_index
                ~data_index ~params:p ()
          | None ->
              add_function_hof_signatures acc_db functions arity ~callback_index
                ~data_index ())
      | ReturningFunctionHOF { methods } ->
          add_hof_returning_function_signatures acc_db methods ())
    db configs

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
