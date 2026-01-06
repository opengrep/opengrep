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
        (* For .each(&callback) pattern - callback receives tainted elements *)
        MethodHOF
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
            arity = 1;
            taint_arg_index = 0;
          };
        (* For chained enumerator pattern: arr.each.with_index { |x, i| ... } *)
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
  (* Apex: No native HOF methods on collections, and no lambda support.
     Third-party libraries use interface callbacks which need devirtualization. *)
  | _ -> []

(** Create a builtin signature database with built-in models for standard library HOFs *)
let create_builtin_models (lang : Lang.t) : builtin_signature_database =
  let db = empty_builtin_signature_database () in
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

(* ========================================================================== *)
(* Collection models for taint propagation through put/get, add/get patterns  *)
(* ========================================================================== *)

(** Collection model configuration types *)
type collection_model_kind =
  | ArgTaintsThis of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      returns_this : bool;
    }
  | ThisTaintsReturn of { methods : string list; arity : int }

(** Get collection model configurations for a language *)
let get_collection_configs (lang : Lang.t) : collection_model_kind list =
  match lang with
  | Lang.Java ->
      [
        (* HashMap/Map: put(key, value) - value (arg 1) taints this, returns previous value (not this) *)
        ArgTaintsThis
          {
            methods = [ "put"; "putIfAbsent" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* List: add(item) - item (arg 0) taints this, returns boolean *)
        ArgTaintsThis
          {
            methods = [ "add"; "addFirst"; "addLast"; "push"; "offer" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* List: add(index, item) - item (arg 1) taints this, returns void *)
        ArgTaintsThis
          {
            methods = [ "add"; "set" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* StringBuilder: append(str) - str (arg 0) taints this, RETURNS THIS for fluent chaining *)
        ArgTaintsThis
          {
            methods = [ "append"; "insert" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* Collection accessors: get(key/index) - this taints return *)
        ThisTaintsReturn
          {
            methods =
              [ "get"; "getFirst"; "getLast"; "peek"; "poll"; "pop"; "remove" ];
            arity = 1;
          };
        (* No-arg accessors *)
        ThisTaintsReturn
          {
            methods =
              [ "toString"; "getFirst"; "getLast"; "peek"; "poll"; "pop" ];
            arity = 0;
          };
        (* Iterator: next() - this taints return *)
        ThisTaintsReturn { methods = [ "next" ]; arity = 0 };
      ]
  | Lang.Js
  | Lang.Ts ->
      [
        (* Map.set(key, value) - value taints this, returns this (fluent) *)
        ArgTaintsThis
          {
            methods = [ "set" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = true;
          };
        (* Array.push(item) - item taints this, returns new length *)
        ArgTaintsThis
          {
            methods = [ "push"; "unshift" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* Set.add(item) - item taints this, returns this (fluent) *)
        ArgTaintsThis
          {
            methods = [ "add" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* Map.get(key), Array.pop(), Array.shift() - this taints return *)
        ThisTaintsReturn { methods = [ "get" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "pop"; "shift"; "at" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "at" ]; arity = 1 };
        (* String methods that return modified strings *)
        ThisTaintsReturn
          {
            methods = [ "toString"; "valueOf"; "join" ];
            arity = 0;
          };
      ]
  | Lang.Python ->
      [
        (* list.append(item), set.add(item) - item taints this *)
        ArgTaintsThis
          {
            methods = [ "append"; "add"; "insert" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* list.extend(iterable) - iterable taints this *)
        ArgTaintsThis
          {
            methods = [ "extend"; "update" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* dict.update with single arg *)
        ArgTaintsThis
          {
            methods = [ "update" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* list.pop(), dict.get(key), dict.pop(key) - this taints return *)
        ThisTaintsReturn { methods = [ "pop" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "get"; "pop"; "setdefault" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "get"; "pop"; "setdefault" ]; arity = 2 };
        (* Iteration helpers *)
        ThisTaintsReturn { methods = [ "copy"; "keys"; "values"; "items" ]; arity = 0 };
      ]
  | Lang.Ruby ->
      [
        (* Array.push, Array.<<, Array.append - item taints this *)
        ArgTaintsThis
          {
            methods = [ "push"; "append"; "unshift"; "prepend" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* Hash[]= is handled differently, but merge taints this *)
        ArgTaintsThis
          {
            methods = [ "merge!"; "update" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* Array.pop, Array.shift, Hash.fetch - this taints return *)
        ThisTaintsReturn { methods = [ "pop"; "shift"; "first"; "last" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "fetch"; "dig"; "slice" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "fetch"; "dig" ]; arity = 2 };
        ThisTaintsReturn { methods = [ "to_s"; "join"; "flatten" ]; arity = 0 };
      ]
  | Lang.Csharp ->
      [
        (* List.Add, HashSet.Add - item taints this *)
        ArgTaintsThis
          {
            methods = [ "Add"; "Push"; "Enqueue"; "Insert" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* Dictionary.Add(key, value) - value taints this *)
        ArgTaintsThis
          {
            methods = [ "Add"; "TryAdd" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* List[i], Dictionary[key], Queue.Dequeue, Stack.Pop - this taints return *)
        ThisTaintsReturn { methods = [ "Pop"; "Dequeue"; "Peek" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "ElementAt"; "GetValueOrDefault" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "ToString" ]; arity = 0 };
      ]
  | Lang.Kotlin ->
      [
        (* MutableList.add, MutableSet.add - item taints this *)
        ArgTaintsThis
          {
            methods = [ "add"; "addFirst"; "addLast" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* MutableMap.put(key, value) - value taints this *)
        ArgTaintsThis
          {
            methods = [ "put"; "putIfAbsent" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* StringBuilder.append - returns this *)
        ArgTaintsThis
          {
            methods = [ "append" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* get, removeAt, removeLast - this taints return *)
        ThisTaintsReturn { methods = [ "get"; "getOrNull"; "getOrDefault" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "first"; "last"; "removeFirst"; "removeLast" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "toString" ]; arity = 0 };
      ]
  | Lang.Swift ->
      [
        (* Array.append - item taints this *)
        ArgTaintsThis
          {
            methods = [ "append"; "insert" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* Dictionary updateValue(value, forKey:) - value taints this *)
        ArgTaintsThis
          {
            methods = [ "updateValue" ];
            arity = 2;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* Array subscript, popLast, removeFirst - this taints return *)
        ThisTaintsReturn { methods = [ "popLast"; "removeFirst"; "removeLast"; "first"; "last" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "remove" ]; arity = 1 };
      ]
  | Lang.Rust ->
      [
        (* Vec.push - item taints this *)
        ArgTaintsThis
          {
            methods = [ "push"; "push_front"; "push_back" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = false;
          };
        (* HashMap.insert(key, value) - value taints this *)
        ArgTaintsThis
          {
            methods = [ "insert" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* Vec.pop, HashMap.get, HashMap.remove - this taints return *)
        ThisTaintsReturn { methods = [ "pop"; "pop_front"; "pop_back" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "get"; "get_mut"; "remove" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "into_iter"; "iter"; "iter_mut" ]; arity = 0 };
      ]
  | Lang.Scala ->
      [
        (* mutable collections: += or add *)
        ArgTaintsThis
          {
            methods = [ "append"; "prepend"; "addOne"; "add" ];
            arity = 1;
            taint_arg_index = 0;
            returns_this = true;
          };
        (* mutable Map: put(key, value) or update(key, value) *)
        ArgTaintsThis
          {
            methods = [ "put"; "update"; "addOne" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        (* accessors *)
        ThisTaintsReturn { methods = [ "head"; "last"; "apply"; "get" ]; arity = 0 };
        ThisTaintsReturn { methods = [ "apply"; "get"; "getOrElse" ]; arity = 1 };
        ThisTaintsReturn { methods = [ "mkString"; "toString" ]; arity = 0 };
      ]
  | Lang.Go ->
      [
        (* Go uses map indexing m[k]=v, not methods, but append is a builtin *)
        (* append(slice, items...) - handled differently as builtin function *)
        (* For method-based APIs like sync.Map *)
        ArgTaintsThis
          {
            methods = [ "Store" ];
            arity = 2;
            taint_arg_index = 1;
            returns_this = false;
          };
        ThisTaintsReturn { methods = [ "Load" ]; arity = 1 };
      ]
  | _ -> []

(* Primitive helpers for building taint sets and effects *)

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
  let configs = get_collection_configs lang in
  List.fold_left
    (fun acc_db config ->
      match config with
      | ArgTaintsThis { methods; arity; taint_arg_index; returns_this } ->
          add_arg_taints_this_signatures acc_db methods arity ~taint_arg_index
            ~returns_this ()
      | ThisTaintsReturn { methods; arity } ->
          add_this_taints_return_signatures acc_db methods arity)
    db configs

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
