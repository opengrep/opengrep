(* Built-in models for standard library functions *)

open Shape_and_sig

(** Helper to create a callback variable *)
let make_callback_var () =
  {
    IL.ident = ("callback", Tok.unsafe_fake_tok "callback");
    sid = AST_generic.SId.unsafe_default;
    id_info = AST_generic.empty_id_info ();
  }

(** Synthetic [params_il] for builtin signatures. Builtins have no
    real IL body, but [Signature.t] requires [length params_il =
    length params]. Each slot is [ParamFixme], whose [pname_of_param]
    is [None], so guard substitution never finds an anchor here. *)
let synthetic_params_il (params : Signature_params.params) : IL.param list =
  List_.map (fun _ -> IL.ParamFixme) params

(** Helper to create args_taints list with taint at specified index *)
let make_args_taints ((taints, shape) : Taint.taints * Shape.shape)
    taint_arg_index =
  List.init (taint_arg_index + 1) (fun idx ->
      if idx = taint_arg_index then IL.Unnamed (taints, shape)
      else IL.Unnamed (Taint.Taint_set.empty, Shape.Bot))

let whole_value_taint_set (lval : Taint.lval) : Taint.taints =
  Taint.(
    Taint_set.union ~merge:Keep_best
      (Taint_set.singleton (taint_of_orig (Var lval)))
      (Taint_set.singleton (taint_of_orig (Shape_var lval))))

(* The value [formal] supplies at [offset]: its taints and its shape. *)
let value_at (formal : Taint.formal) (offset : Taint.offset list) :
    Taint.taints * Shape.shape =
  let lval = { Taint.base = Taint.base_of_formal formal; offset } in
  ( Taint.Taint_set.singleton (Taint.taint_of_orig (Var lval)),
    Shape.Arg (formal, [ offset ]) )

(* Any element of the collection [formal] supplies at [offset]. *)
let element_at (formal : Taint.formal) (offset : Taint.offset list) :
    Taint.taints * Shape.shape =
  value_at formal (offset @ [ Taint.Oany ])

let callback_callee () : IL.exp =
  {
    IL.e = IL.Fetch { base = IL.Var (make_callback_var ()); rev_offset = [] };
    eorig = NoOrig;
  }

let hof_return_effects (result : Lang_config.hof_result)
    ~(input : Taint.taints) ~(callee : IL.exp) ~(arg : Taint.formal)
    ~(arg_offset : Taint.offset list) ~(guards : Effect_guard.t) :
    Effect.t list =
  let returns ((data_taints, data_shape) : Taint.taints * Shape.shape) =
    [
      Effect.ToReturn
        {
          data_taints;
          data_shape;
          several_results = false;
          control_taints = Taint.Taint_set.empty;
          return_tok = Tok.unsafe_fake_tok "builtin_hof";
          guards;
        };
    ]
  in
  match result with
  | Lang_config.Input_elements -> returns (input, Shape.Bot)
  | Lang_config.Callback_results ->
      returns
        (value_at
           (Taint.Result
              {
                Taint.callee = arg;
                callee_offset = arg_offset;
                loc = Taint.call_loc_of_exp callee;
              })
           [])
  | Lang_config.Nothing -> []

(** Helper function to add HOF signatures that return a function. This is for
    languages like Ruby where arr.map() returns a function that takes a
    callback.

    @param db The builtin signature database to add to
    @param method_names List of method names to add signatures for
    @param taint_arg_index
      Which callback argument receives the taint (default 0). *)
let add_hof_returning_function_signatures db method_names
    ~(result : Lang_config.hof_result) ?(taint_arg_index = 0) () =
  let callee = callback_callee () in
  let callback = Taint.Param { Taint.name = "callback"; index = 0 } in

  (* Create a taint from BThis to pass to the callback *)
  let this_taint_set =
    whole_value_taint_set { Taint.base = BThis; offset = [] }
  in
  let args_taints =
    make_args_taints (element_at Taint.Receiver []) taint_arg_index
  in

  (* The effect when the returned function is called with a callback *)
  let hof_effect =
    Effect.ToSinkInCall
      {
        callee;
        arg = callback;
        arg_offset = [];
        args_taints;
        guards = Effect_guard.top;
      }
  in

  (* The signature of the function that will be returned *)
  let params = [ Signature_params.P "callback" ] in
  let returned_fun_sig =
    {
      Signature.params;
      params_il = synthetic_params_il params;
      captured = [];
      effects =
        Effects.of_list ~merge:Taint.Keep_best
          (hof_effect
          :: hof_return_effects result ~input:this_taint_set ~callee
               ~arg:callback ~arg_offset:[] ~guards:Effect_guard.top);
    }
  in

  (* The signature for the method itself (arity 0) returns a Fun shape with the array's taints *)
  let return_effect =
    Effect.ToReturn
      {
        data_taints = this_taint_set;
        data_shape =
          closure_of_definition
            ( Function_id.of_string_and_tok
                (Printf.sprintf "builtin_hof/%d" taint_arg_index)
                (Tok.unsafe_fake_tok "builtin_hof"),
              returned_fun_sig )
            [];
        several_results = false;
        control_taints = Taint.Taint_set.empty;
        return_tok = Tok.unsafe_fake_tok "builtin_hof";
        guards = Effect_guard.top;
      }
  in
  let method_sig =
    {
      Signature.params = [];
      params_il = synthetic_params_il [];
      captured = [];
      effects = Effects.singleton return_effect;
    }
  in

  (* Add signatures for all methods using simple string keys *)
  List.fold_left
    (fun acc_db method_name ->
      add_builtin_signature acc_db method_name { sig_ = method_sig; arity = Arity_exact 0 })
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
    ?(data_index = 1) ?(params = [ Signature_params.P "callback"; Signature_params.Other ])
    ?(taint_arg_index = 0) ~(result : Lang_config.hof_result) () =
  let callback = Taint.Param { Taint.name = "callback"; index = callback_index } in
  let callee = callback_callee () in

  (* Create a taint from the data parameter to pass to the callback *)
  let data_arg = { Taint.name = "data"; index = data_index } in
  let data_taint_set =
    whole_value_taint_set { Taint.base = BArg data_arg; offset = [] }
  in
  let args_taints =
    make_args_taints (element_at (Taint.Param data_arg) []) taint_arg_index
  in

  let hof_effect =
    Effect.ToSinkInCall
      {
        callee;
        arg = callback;
        arg_offset = [];
        args_taints;
        guards = Effect_guard.top;
      }
  in

  (* Also add a ToReturn effect for what the result holds: the data
     argument's elements or the callback's results. This is essential for
     chained HOFs like map(f, filter(g, data)). *)
  let return_effects =
    hof_return_effects result ~input:data_taint_set ~callee ~arg:callback
      ~arg_offset:[] ~guards:Effect_guard.top
  in

  let hof_sig =
    {
      Signature.params;
      params_il = synthetic_params_il params;
      captured = [];
      effects = Effects.of_list ~merge:Taint.Keep_best (hof_effect :: return_effects);
    }
  in

  (* Add signatures for all functions using simple string keys *)
  List.fold_left
    (fun acc_db function_name ->
      add_builtin_signature acc_db function_name { sig_ = hof_sig; arity = Arity_exact arity })
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
    ?(params = [ Signature_params.P "callback" ]) ?(method_name_transform = fun x -> x)
    ?(taint_arg_index = 0) ~(result : Lang_config.hof_result) () =
  let callback = Taint.Param { Taint.name = "callback"; index = callback_index } in
  let callee = callback_callee () in

  (* Create a taint from BThis to pass to the callback *)
  let this_taint_set =
    whole_value_taint_set { Taint.base = BThis; offset = [] }
  in
  let args_taints =
    make_args_taints (element_at Taint.Receiver []) taint_arg_index
  in

  let hof_effect =
    Effect.ToSinkInCall
      {
        callee;
        arg = callback;
        arg_offset = [];
        args_taints;
        guards = Effect_guard.top;
      }
  in

  (* Also add a ToReturn effect for what the result holds: the receiver's
     elements or the callback's results. This is essential for chained HOFs
     like arr.map(...).filter(...) where the result of map needs to carry
     taint for filter to propagate. *)
  let return_effects =
    hof_return_effects result ~input:this_taint_set ~callee ~arg:callback
      ~arg_offset:[] ~guards:Effect_guard.top
  in

  let hof_sig =
    {
      Signature.params;
      params_il = synthetic_params_il params;
      captured = [];
      effects = Effects.of_list ~merge:Taint.Keep_best (hof_effect :: return_effects);
    }
  in

  (* Add signatures for all methods using simple string keys *)
  List.fold_left
    (fun acc_db method_name ->
      let transformed_name = method_name_transform method_name in
      add_builtin_signature acc_db transformed_name { sig_ = hof_sig; arity = Arity_exact arity })
    db method_names

(** Create params list from arity and callback_index *)
let make_params arity callback_index =
  List.init arity (fun i ->
    if i = callback_index then Signature_params.P "callback"
    else Signature_params.Other)

(** Build the effects contributed by a single FunctionHOF overload in the
    packed-CList form Clojure uses. Each effect is guarded by
    [length(impl) == arity] so that multiple overloads for the same
    function name (e.g. [reduce/2] and [reduce/3]) can share one
    IL-arity-1 signature and get disambiguated by the caller's CList
    length at instantiation time.

    The callback invocation is itself packed in Clojure — [(cb x y)] lowers
    to [cb(CList[x, y])] — so we emit a single-element [args_taints] whose
    sole shape is an [Obj] with the per-position taints indexed. *)
let clojure_hof_effects ~(lang : Lang.t) ~(atoms : Effect_guard.atoms) ~arity ~callback_index
    ~data_index ~taint_arg_index ~(result : Lang_config.hof_result) =
  let impl_arg = { Taint.name = "impl"; index = 0 } in
  let callee = callback_callee () in
  let data_taint_set =
    whole_value_taint_set
      { Taint.base = BArg impl_arg; offset = [ Oint data_index ] }
  in
  (* Packed callback args: a single Obj-shaped CList whose [taint_arg_index]
   * slot carries the data taint. *)
  let callback_obj =
    let element_taints, element_shape =
      element_at (Taint.Param impl_arg) [ Oint data_index ]
    in
    let tainted_cell = Shape.Cell (`Tainted element_taints, element_shape) in
    Shape.Obj
      {
        sites = Shape_and_sig.Sites.empty;
        summary = false;
        fields = Fields.singleton (Taint.Oint taint_arg_index) tainted_cell;
      }
  in
  let args_taints =
    [ IL.Unnamed (Taint.Taint_set.empty, callback_obj) ]
  in
  (* Build the guard [length(impl) == arity] as an IL.exp. [impl_il_name]
   * is the synthetic [IL.name] used in the cond's [Fetch] base and in
   * [param_refs]; the evaluator matches them back via [IL.equal_name]. *)
  let guards =
    let fake_tok = Tok.unsafe_fake_tok "builtin_hof" in
    let impl_il_name : IL.name =
      {
        ident = ("impl", fake_tok);
        sid = AST_generic.SId.unsafe_default;
        id_info = AST_generic.empty_id_info ();
      }
    in
    let impl_fetch : IL.exp =
      {
        IL.e = IL.Fetch { base = IL.Var impl_il_name; rev_offset = [] };
        eorig = IL.NoOrig;
      }
    in
    let length_exp : IL.exp =
      {
        IL.e =
          IL.Operator
            ((AST_generic.Length, fake_tok), [ IL.Unnamed impl_fetch ]);
        eorig = IL.NoOrig;
      }
    in
    let arity_exp : IL.exp =
      {
        IL.e = IL.Literal (AST_generic.Int (Parsed_int.of_int arity));
        eorig = IL.NoOrig;
      }
    in
    let cond : IL.exp =
      {
        IL.e =
          IL.Operator
            ( (AST_generic.Eq, fake_tok),
              [ IL.Unnamed length_exp; IL.Unnamed arity_exp ] );
        eorig = IL.NoOrig;
      }
    in
    { Effect_guard.cond = Effect_guard.of_exp ~lang atoms cond;
      param_refs = [ (impl_il_name, 0) ] }
  in
  let hof_effect =
    Effect.ToSinkInCall
      {
        callee;
        arg = Taint.Param impl_arg;
        arg_offset = [ Oint callback_index ];
        args_taints;
        guards;
      }
  in
  hof_effect
  :: hof_return_effects result ~input:data_taint_set ~callee
       ~arg:(Taint.Param impl_arg) ~arg_offset:[ Oint callback_index ] ~guards

(** Group [FunctionHOF] configs by function name. A single Clojure
    function name (e.g. [reduce]) can appear in several [FunctionHOF]
    configs, one per arity; we collect them so
    [add_function_hof_signatures_clojure] can emit one IL-arity-1
    signature per name whose effects carry per-overload guards. *)
let group_function_hofs_by_name (hof_configs : Lang_config.hof_kind list) :
    (string * Lang_config.hof_kind list) list =
  let rec add_overload fn hof = function
    | [] -> [ (fn, [ hof ]) ]
    | (n, xs) :: rest when String.equal n fn -> (n, hof :: xs) :: rest
    | entry :: rest -> entry :: add_overload fn hof rest
  in
  List.fold_left
    (fun acc -> function
      | Lang_config.FunctionHOF { functions; _ } as hof ->
          List.fold_left (fun acc fn -> add_overload fn hof acc) acc functions
      | _ -> acc)
    [] hof_configs

(** Register Clojure FunctionHOF overloads grouped by name, one packed-form
    signature per name. Each overload contributes effects guarded by its
    language-level arity. *)
let add_function_hof_signatures_clojure ~(lang : Lang.t)
    ~(atoms : Effect_guard.atoms) db (grouped : (string * Lang_config.hof_kind list) list) =
  List.fold_left
    (fun acc_db (function_name, overloads) ->
      let effects =
        overloads
        |> List.concat_map (function
             | Lang_config.FunctionHOF
                 { arity; callback_index; data_index; taint_arg_index; result; _ } ->
                 clojure_hof_effects ~lang ~atoms ~arity ~callback_index ~data_index
                   ~taint_arg_index ~result
             | _ -> [])
      in
      let params = [ Signature_params.P "impl" ] in
      let hof_sig =
        {
          Signature.params;
          params_il = synthetic_params_il params;
      captured = [];
          effects = Effects.of_list ~merge:Taint.Keep_best effects;
        }
      in
      add_builtin_signature acc_db function_name
        { sig_ = hof_sig; arity = Arity_exact 1 })
    db grouped

(** Create a builtin signature database with built-in models for standard library HOFs *)
let create_builtin_models ~(atoms : Effect_guard.atoms) (lang : Lang.t) :
    builtin_signature_database =
  let db = empty_builtin_signature_database () in
  let config = Lang_config.get lang in
  let is_clojure = Lang.equal lang Lang.Clojure in
  (* Clojure: every FunctionHOF overload shares the same IL-arity (1, from
   * packed-CList lowering), so we group overloads by name and emit one
   * signature per name with per-overload guards to disambiguate. *)
  let db =
    if is_clojure then
      add_function_hof_signatures_clojure ~lang ~atoms db
        (group_function_hofs_by_name config.hof_configs)
    else db
  in
  (* Convert configs to signatures *)
  List.fold_left
    (fun acc_db hof_config ->
      match hof_config with
      | Lang_config.MethodHOF { methods; arity; taint_arg_index; result } ->
          add_hof_signatures acc_db methods arity ~taint_arg_index ~result ()
      | Lang_config.FunctionHOF
          { functions; arity; callback_index; data_index; taint_arg_index; result } ->
          if is_clojure then acc_db
          else
            let params = make_params arity callback_index in
            add_function_hof_signatures acc_db functions arity ~callback_index
              ~data_index ~params ~taint_arg_index ~result ()
      | Lang_config.ReturningFunctionHOF { methods; result } ->
          add_hof_returning_function_signatures acc_db methods ~result ())
    db config.hof_configs

(* ========================================================================== *)
(* Primitive helpers for building taint sets and effects *)
(* ========================================================================== *)

let this_taint_set () = whole_value_taint_set { Taint.base = BThis; offset = [] }

let value_arg index = { Taint.name = "value"; index }

let return_effect ((taints, shape) : Taint.taints * Shape.shape) =
  Effect.ToReturn
    {
      data_taints = taints;
      data_shape = shape;
      several_results = false;
      control_taints = Taint.Taint_set.empty;
      return_tok = Tok.unsafe_fake_tok "builtin";
      guards = Effect_guard.top;
    }

let to_lval_this_element ((taints, shape) : Taint.taints * Shape.shape) =
  Effect.ToLval
    {
      taints;
      shape;
      lval = { Taint.base = BThis; offset = [ Oany ] };
      guards = Effect_guard.top;
    }

let add_method_signatures db method_names arity effects =
  let params = List.init arity (fun _ -> Signature_params.Other) in
  let sig_ =
    { Signature.params; params_il = synthetic_params_il params;
      captured = []; effects }
  in
  List.fold_left
    (fun acc_db name -> add_builtin_signature acc_db name { sig_; arity = Arity_exact arity })
    db method_names

(* Collection model signature builders *)

(** Add signatures where an argument taints 'this' (e.g., put, add, append) *)
let add_arg_taints_this_signatures db method_names arity
    ~(stored : Taint.taints * Shape.shape) ?(returns_this = false) () =
  let to_lval = to_lval_this_element stored in
  let effects =
    if returns_this then
      (* The return value is 'this' after being tainted by the arg. We include
       * both 'this' (for any pre-existing taint) and the arg (for the new taint)
       * because both effects are instantiated from the pre-call env, before
       * ToLval has a chance to update 'this'. *)
      let stored_taints, stored_shape = stored in
      let stored_element =
        Shape.Obj
          {
            sites = Shape_and_sig.Sites.empty;
            summary = false;
            fields =
              Fields.singleton Taint.Oany
                (Shape.Cell (`Tainted stored_taints, stored_shape));
          }
      in
      Effects.of_list ~merge:Taint.Keep_best
        [
          to_lval;
          return_effect (value_at Taint.Receiver []);
          return_effect (Taint.Taint_set.empty, stored_element);
        ]
    else Effects.singleton to_lval
  in
  add_method_signatures db method_names arity effects

(** Add signatures where 'this' taints the return value (e.g., get, toString) *)
let add_this_taints_return_signatures db method_names arity
    (returned : Taint.taints * Shape.shape) =
  let effects = Effects.singleton (return_effect returned) in
  add_method_signatures db method_names arity effects

(** Add collection models to a builtin signature database *)
let add_collection_models db (lang : Lang.t) : builtin_signature_database =
  let config = Lang_config.get lang in
  List.fold_left
    (fun acc_db coll_config ->
      match coll_config with
      | Lang_config.ArgIsElement { methods; arity; taint_arg_index; returns_this } ->
          add_arg_taints_this_signatures acc_db methods arity
            ~stored:(value_at (Taint.Param (value_arg taint_arg_index)) [])
            ~returns_this ()
      | Lang_config.ArgElementsAreElements
          { methods; arity; taint_arg_index; returns_this } ->
          add_arg_taints_this_signatures acc_db methods arity
            ~stored:(element_at (Taint.Param (value_arg taint_arg_index)) [])
            ~returns_this ()
      | Lang_config.ReturnsElement { methods; arity } ->
          add_this_taints_return_signatures acc_db methods arity
            (element_at Taint.Receiver [])
      | Lang_config.ReturnsWholeValue { methods; arity } ->
          add_this_taints_return_signatures acc_db methods arity
            (this_taint_set (), Shape.Bot)
      | Lang_config.ReturnsSameElements { methods; arity } ->
          add_this_taints_return_signatures acc_db methods arity
            (value_at Taint.Receiver [])
      | Lang_config.ElementProperty _ -> acc_db)
    db config.collection_configs

(** Create a builtin signature database with all built-in models (HOFs + collections) *)
let create_all_builtin_models ~(atoms : Effect_guard.atoms) (lang : Lang.t) :
    builtin_signature_database =
  let db = create_builtin_models ~atoms lang in
  add_collection_models db lang

(** Initialize the signature database. Now that builtin signatures are separate,
    this function just returns the user DB as-is (or empty if None). *)
let init_signature_database (user_db : signature_database option) :
    signature_database =
  match user_db with
  | Some db -> db
  | None -> empty_signature_database ()
