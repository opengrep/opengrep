module G = AST_generic
open Callee_resolution

(* A function referred to by an argument: a name or a member access whose
   binding or receiver decides the function, or a callable written as a
   string (PHP), which the language resolves by name when the program runs. *)
type reference =
  | Bound of G.expr
  | Written of G.expr

(* [tmp] is the IL name of the wrapper an Elixir [&f/n] lowers to. *)
type candidate = {
  reference : reference;
  tok : Tok.t;
  tmp : IL.name option;
  callable : G.expr list;
}

let expr_of_receiver_any (any : G.any) : G.expr option =
  match any with
  | G.E (receiver : G.expr) -> Some receiver
  | G.T (ty : G.type_) -> (
    match ty.G.t with
    | G.TyN (name : G.name) -> Some (G.N name |> G.e)
    | G.TyExpr (receiver : G.expr) -> Some receiver
    | _ -> None)
  | _ -> None

let dotted_reference ~(tok : Tok.t) (receiver : G.expr) (id : G.ident)
    (id_info : G.id_info) : G.expr =
  G.DotAccess (receiver, tok, G.FN (G.Id (id, id_info))) |> G.e

let dotted_reference_of_method_reference (e : G.expr) : G.expr option =
  match e.G.e with
  | G.OtherExpr
      ( ("MethodRef", _),
        G.E ({ G.e = G.DotAccess (_, _, G.FN (G.Id _)); _ } as reference) :: _ )
    ->
      Some reference
  | G.OtherExpr (("MethodRef", tok), receiver_any :: (_ :: _ as rest)) -> (
    match (expr_of_receiver_any receiver_any, List_.last_opt rest) with
    | Some (receiver : G.expr), Some (G.I (id : G.ident)) ->
      Some (dotted_reference ~tok receiver id (G.empty_id_info ()))
    | _ -> None)
  | G.OtherExpr (("::", tok), [ G.E field; G.E receiver ]) -> (
    match field.G.e with
    | G.N (G.Id ((id : G.ident), (id_info : G.id_info))) ->
      Some (dotted_reference ~tok receiver id id_info)
    | _ -> None)
  | _ -> None

let name_of_qualified_segments (segments : G.ident list) : G.name option =
  match List.rev segments with
  | [] -> None
  | (last : G.ident) :: (rev_middle : G.ident list) ->
    Some
      (G.IdQualified
         { G.name_last = (last, None);
           name_middle =
             (match List.rev rev_middle with
              | [] -> None
              | (middle : G.ident list) ->
                Some (G.QDots (List.map (fun (id : G.ident) -> (id, None))
                                 middle)));
           name_top = Some (snd last);
           name_info = G.empty_id_info () })

let segments_of_written_name ~(tok : Tok.t) (written : string) : G.ident list =
  String.split_on_char '\\' written
  |> List.filter (fun (segment : string) -> String.length segment > 0)
  |> List.map (fun (segment : string) -> (segment, tok))

let scope_operator_at (written : string) : int option =
  let last = String.length written - 1 in
  let rec search (from : int) : int option =
    if from >= last then None
    else
      match String.index_from_opt written from ':' with
      | Some (at : int) when at < last && Char.equal written.[at + 1] ':' ->
        Some at
      | Some (at : int) -> search (at + 1)
      | None -> None
  in
  search 0

let reference_of_written_callable ~(tok : Tok.t) (written : string)
    : G.expr option =
  match scope_operator_at written with
  | Some (at : int) ->
    let class_written = String.sub written 0 at in
    let method_written =
      String.sub written (at + 2) (String.length written - at - 2)
    in
    if Int.equal (String.length method_written) 0 then None
    else
      Option.map
        (fun (class_name : G.name) ->
          dotted_reference ~tok (G.N class_name |> G.e) (method_written, tok)
            (G.empty_id_info ()))
        (name_of_qualified_segments
           (segments_of_written_name ~tok class_written))
  | None ->
    Option.map
      (fun (qualified : G.name) -> G.N qualified |> G.e)
      (name_of_qualified_segments (segments_of_written_name ~tok written))

let is_closure_from_callable (callee : G.expr) : bool =
  match callee.G.e with
  | G.N (G.IdQualified
           { G.name_last = (("fromCallable", _), None);
             name_middle = Some (G.QDots [ (("Closure", _), None) ]); _ }) ->
    true
  | _ -> false

let denoted_reference (e : G.expr) (build : unit -> G.expr option) :
    G.expr option =
  match G.callable_reference_of e with
  | Some (reference : G.expr) -> Some reference
  | None -> build ()

let reference_of_callable_literal ~(lang : Lang.t) (e : G.expr)
    : G.expr option =
  if not (Lang_config.get lang).Lang_config.reflection.Lang_config.callable_literals
  then
    None
  else
    denoted_reference e @@ fun () ->
    match e.G.e with
    | G.L (G.String (_, ((written : string), (tok : Tok.t)), _)) ->
      reference_of_written_callable ~tok written
    | G.Container
        ( (G.List | G.Array),
          (_, [ (receiver : G.expr);
                { G.e = G.L (G.String (_, ((method_written : string),
                                           (tok : Tok.t)), _)); _ } ], _) ) ->
      Some
        (dotted_reference ~tok receiver (method_written, tok)
           (G.empty_id_info ()))
    | _ -> None

let id_of_reference (e : G.expr) : (G.ident * G.id_info) option =
  match e.G.e with
  | G.N name
  | G.Ref (_, { G.e = G.N name; _ })
  | G.DotAccess (_, _, G.FN name) ->
      Some (AST_generic_helpers.id_of_name name)
  | _ -> None

let expr_of_reference (reference : reference) : G.expr =
  match reference with
  | Bound e
  | Written e ->
      e

let candidate ?(tmp : IL.name option) ?(callable : G.expr list = [])
    (reference : reference) : candidate list =
  match id_of_reference (expr_of_reference reference) with
  | Some ((_, tok), _) -> [ { reference; tok; tmp; callable } ]
  | None -> []

let method_object_member ~(lang : Lang.t) (e : G.expr) :
    (G.expr * G.ident) option =
  match e.G.e with
  | G.Call
      ( ({ e =
             ( G.N (G.Id ((name, _), _))
             | G.DotAccess (_, _, G.FN (G.Id ((name, _), _))) );
           _ } as callee),
        (_, [ G.Arg { e = G.L (G.Atom (atom_tok, member)); _ } ], _) )
    when Option.equal String.equal
           (Lang_config.get lang).Lang_config.reflection.Lang_config.method_object
           (Some name) -> (
      match callee.G.e with
      | G.DotAccess (receiver, _, _) -> Some (receiver, member)
      | _ -> Some (G.IdSpecial (G.Self, atom_tok) |> G.e, member))
  | _ -> None

let is_reference (e : G.expr) : bool =
  match e.G.e with
  | G.N _
  | G.DotAccess (_, _, G.FN _) ->
      true
  | _ -> false

(* The functions an argument refers to:
     - foo, &foo, Module.foo, obj.method, this.method, method references
     - Elixir &func/n (ShortLambda wrapping Call)
     - Ruby method(:name) and recv.method(:name), a method of self or of
       recv
     - PHP callables written as strings
     - Record { cb: handler, ... } and Dict { "cb": handler, ... }: each
       entry's value
     - List/Tuple/Array/Set [handler, ...]: each element
     - a variable holding a record or a container ([id_svalue], set by
       [Dataflow_svalue] during parsing); a variable holding a reference to a
       function reaches it through its binding, which the graph's resolver
       follows
   Over-approximates on purpose: any function reference nested anywhere in
   the argument is treated as a potential callback. Precision at per-offset
   granularity is handled later by Sig_inst's offset-walk. *)
let rec extract_callbacks_from_arg ~(lang : Lang.t) (arg_expr : G.expr) :
    candidate list =
  match arg_expr.G.e with
  | G.N (G.Id (_, id_info)) ->
      let via_svalue =
        match !(id_info.id_svalue) with
        | Some (G.Sym inner) when not (is_reference inner) ->
            extract_callbacks_from_arg ~lang inner
        | _ -> []
      in
      candidate (Bound arg_expr) @ via_svalue
  (* Address-of operator: &foo (C/C++ function pointers) *)
  | G.Ref (_, ({ e = G.N (G.Id _); _ } as inner)) -> candidate (Bound inner)
  (* Ruby [&:name] calls [name] on each value the block receives, a receiver
     this file does not know *)
  | G.Ref (_, { e = G.L (G.Atom _); _ })
    when (Lang_config.get lang).Lang_config.block_pass_operator ->
      []
  | G.Ref (_, (operand : G.expr))
    when (Lang_config.get lang).Lang_config.block_pass_operator ->
      extract_callbacks_from_arg ~lang operand
  | G.N (G.IdQualified _)
  | G.DotAccess (_, _, G.FN _) ->
      candidate (Bound arg_expr)
  | G.OtherExpr (("MethodRef", _), _)
  | G.OtherExpr (("::", _), _) -> (
      match dotted_reference_of_method_reference arg_expr with
      | Some reference -> extract_callbacks_from_arg ~lang reference
      | None -> [])
  | G.L (G.String _) -> (
      match reference_of_callable_literal ~lang arg_expr with
      | Some reference -> candidate ~callable:[ arg_expr ] (Written reference)
      | None -> [])
  | G.Call (callee, (_, [ G.Arg (inner : G.expr) ], _))
    when (Lang_config.get lang).Lang_config.reflection.Lang_config.callable_literals
         && is_closure_from_callable callee ->
      List.map
        (fun (found : candidate) ->
          { found with callable = arg_expr :: found.callable })
        (extract_callbacks_from_arg ~lang inner)
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
          ( ({ e = G.N (G.Id _) | G.DotAccess (_, _, G.FN (G.Id _)); _ } as
             callee),
            _ ) ->
          let tmp_name =
            Visit_function_defs.synth_lambda_il_name_of_tok shortlambda_tok
          in
          candidate ~tmp:tmp_name (Bound callee)
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
  | G.Container
      ((G.List | G.Array), (_, [ _; { G.e = G.L (G.String _); _ } ], _)) -> (
      match reference_of_callable_literal ~lang arg_expr with
      | Some reference -> candidate ~callable:[ arg_expr ] (Written reference)
      | None ->
          List.concat_map (extract_callbacks_from_arg ~lang)
            (match arg_expr.G.e with
             | G.Container (_, (_, xs, _)) -> xs
             | _ -> []))
  (* List/Tuple/Array/Set literal: recurse into each element *)
  | G.Container ((G.List | G.Tuple | G.Array | G.Set), (_, xs, _)) ->
      List.concat_map (extract_callbacks_from_arg ~lang) xs
  (* Ruby [method(:name)] and [recv.method(:name)]: the method [name] of self
     or of [recv]. Sym-prop carries the
     [Call(method, [Atom :name])] expression on the callback variable's
     [id_svalue], so the recursion above reaches us for an aliased binding
     [cb = method(:name); apply_cb(cb, ...)]. *)
  | G.Call _ -> (
      match
        denoted_reference arg_expr (fun () ->
            Option.map
              (fun ((receiver : G.expr), ((_, member_tok) as member : G.ident)) ->
                dotted_reference ~tok:member_tok receiver member
                  (G.empty_id_info ()))
              (method_object_member ~lang arg_expr))
      with
      | Some reference -> candidate ~callable:[ arg_expr ] (Bound reference)
      | None -> [])
  | _ -> []

type callback_resolver =
  caller:Function_id.t option -> reference -> Symbol_table.resolution

(* [?allow_located_fake]: synthetic lambda names are located fakes — they
   carry the lambda's def position and key [Function_id] like a real token.
   Direct-call write-back and callback-argument stamping both stamp them.
   The sid name is the ident string, matching [Function_id.compute_key], so
   [Function_id.of_sid] rebuilds the exact vertex/DB key. *)
let resolved_name_of_fn_id ?(allow_located_fake = false) (fn_id : fn_id)
    : G.resolved_name option =
  match List.rev fn_id with
  | Some (n : IL.name) :: _ ->
    let tok = snd n.IL.ident in
    if Tok.is_fake tok && not allow_located_fake then None
    else (
      try
        let file = Fpath.to_string (Tok.file_of_tok tok) in
        let sid =
          (* The definition's own sid, when naming bound it, so that a
             stamp equals the definition's identity. An alias-synthetic
             bare name (cf. fn_id_to_node) carries the
             TARGET's sid under a different name; propagating it points
             at where the def and its signature live. A definition naming
             did not bind is identified by its site. *)
          let made_here (rsid : G.SId.t) : bool =
            let _, rfile, rline, rcol = G.SId.to_loc rsid in
            match Tok.loc_of_tok tok with
            | Ok (loc : Tok.location) ->
                String.equal rfile file
                && Int.equal rline loc.Tok.pos.Pos.line
                && Int.equal rcol loc.Tok.pos.Pos.column
            | Error _ -> false
          in
          (* A binding made elsewhere (a constructor's name binds its
             class) does not identify this definition. *)
          match !(n.IL.id_info.G.id_resolved) with
          | Some (_, rsid)
            when (not (G.SId.is_unsafe_default rsid)) && made_here rsid ->
              rsid
          | _ -> G.SId.of_site ~name:(fst n.IL.ident) ~file tok
        in
        Some (G.Global, sid)
      with Tok.NoTokenLocation _ -> None)
  | _ -> None

(* Sets [ii.id_callee_definition] to the sids of the given definitions, and
   leaves it unchanged when there are none; mutating the ref mutates the
   shared AST. *)
let set_callee_definition ?allow_located_fake (ii : G.id_info)
    (fn_ids : fn_id list) : unit =
  match
    List.filter_map
      (fun (fn_id : fn_id) ->
        Option.map snd (resolved_name_of_fn_id ?allow_located_fake fn_id))
      fn_ids
  with
  | [] -> ()
  | sids -> ii.G.id_callee_definition := sids

(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry multiple callbacks when it's a record/list
   containing several function references, or a variable whose [id_svalue]
   wraps such a container. See [extract_callbacks_from_arg]. *)
let try_identify_callback_args ~(lang : Lang.t)
    ~(resolve_callback : callback_resolver)
    ~(caller_parent_path : IL.name option list) (arg : G.argument) :
    (fn_id * Tok.t * IL.name option) list =
  let caller = fn_id_to_node caller_parent_path in
  let resolve_in_expr (expr : G.expr) =
    extract_callbacks_from_arg ~lang expr
    |> List.concat_map (fun (candidate : candidate) ->
           let fn_ids =
             match resolve_callback ~caller candidate.reference with
             | Symbol_table.Defined (funcs : func_info list) ->
                 List_.map (fun (func : func_info) -> func.fn_id) funcs
             | Symbol_table.External -> []
           in
           let reference = expr_of_reference candidate.reference in
           Option.iter
             (fun ((_, ii) : G.ident * G.id_info) ->
               set_callee_definition ~allow_located_fake:true ii fn_ids)
             (id_of_reference reference);
           if not (List_.null fn_ids) then
             List.iter
               (fun (callable : G.expr) ->
                 if Option.is_none (G.callable_reference_of callable) then
                   callable.G.facts <-
                     G.Callable_reference reference :: callable.G.facts)
               candidate.callable;
           List_.map
             (fun (fn_id : fn_id) -> (fn_id, candidate.tok, candidate.tmp))
             fn_ids)
  in
  match arg with
  | G.Arg expr -> resolve_in_expr expr
  (* Keyword args: Ruby [my_hof(cb: h, data: x)] and Python [f(cb=h, data=x)]
     lower each key-value as an [ArgKwd]. The key is a tag name; recurse on
     the value expression to extract any nested callback references. *)
  | G.ArgKwd (_, expr) | G.ArgKwdOptional (_, expr) -> resolve_in_expr expr
  | G.ArgType _ | G.OtherArg _ -> []

let extract_hof_callbacks_from_call ~(lang : Lang.t)
    ~(method_hofs : string list) ~(function_hofs : (string list * int) list)
    ~(resolve_callback : callback_resolver)
    ~(caller_parent_path : IL.name option list) (callee : G.expr)
    (args : G.arguments)
    : (fn_id * Tok.t * IL.name option) list =
  let try_arg arg =
    try_identify_callback_args ~lang ~resolve_callback ~caller_parent_path arg
  in
  let try_arg_at_index idx =
    match List.nth_opt (Tok.unbracket args) idx with
    | Some arg -> try_arg arg
    | None -> []
  in
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

