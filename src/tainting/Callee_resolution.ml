open Common
module G = AST_generic
module Log = Log_call_graph.Log

(* Function identifier as a path from outermost to innermost scope.
 * For example:
 * - [Some class_name; Some method_name; Some nested_fn] for nested function
 * - [Some class_name; Some method_name] for a method
 * - [Some fn_name] for a top-level function
 * - [] for top-level/anonymous
 *)
type fn_id = IL.name option list
[@@deriving show, eq, ord]

let show_fn_id (fn_id : fn_id) : string =
  match fn_id with
  | [] -> "<anonymous>"
  | path ->
      path
      |> List.map (fun name_opt ->
          Option.value ~default:"<anon>" (Option.map (fun name -> fst name.IL.ident) name_opt))
      |> String.concat "::"

(** Extract the function name (last element) from the fn_id path *)
let get_fn_name (fn_id : fn_id) : IL.name option =
  List_.last_opt fn_id |> Option.join

(* Type for function information including AST node *)
type func_info = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

(* Position-aware equality for fn_id paths. Compares function identifiers
   using both name AND source position (file, line, column) via Function_id.equal. *)
let equal_with_pos f1 f2 =
  let equal_il_name n1 n2 =
    Function_id.equal
      (Function_id.of_il_name n1)
      (Function_id.of_il_name n2)
  in
  List.equal (Option.equal equal_il_name) f1 f2

(* Match a [func_info] against [name_str] by either the fn_id's last ident
   (regular functions/methods) or the entity's name (named lambdas, whose
   fn_id is the synthetic [_tmp_lambda] but whose entity carries the binding). *)
let func_info_name_matches (f : func_info) (name_str : string) : bool =
  (match List_.init_and_last_opt f.fn_id with
   | Some (_, Some name) when String.equal (fst name.IL.ident) name_str -> true
   | _ -> false)
  ||
  (match f.entity with
   | Some ent ->
       (match AST_to_IL.name_of_entity ent with
        | Some n -> String.equal (fst n.IL.ident) name_str
        | None -> false)
   | None -> false)

(* Find a [func_info] in [all_funcs] whose name matches [name_str] (per
   [func_info_name_matches]) and whose parent path equals [caller_parent_path]
   (position-aware, distinguishing same-named functions in different scopes). *)
let find_func_in_scope (all_funcs : func_info list)
    (caller_parent_path : IL.name option list) (name_str : string)
    : func_info option =
  List.find_opt (fun f ->
    if func_info_name_matches f name_str then
      match List_.init_and_last_opt f.fn_id with
      | Some (f_parent, _) -> equal_with_pos f_parent caller_parent_path
      | _ -> false
    else false
  ) all_funcs

(* Get arity of a function from its definition *)
let get_func_arity (fdef : G.function_definition) : int =
  let params = fdef.fparams in
  List.length (Tok.unbracket params)

(* Disambiguate among candidate functions matching a call site by name.
   [matches] are the candidates; [call_arity] is the number of arguments
   at the call site ([None] when arity info is not available). Returns
   [Some] only when the result is unambiguous:
   - exactly one candidate by name: use it regardless of arity;
   - multiple candidates with arity info: filter by exact arity, accept
     iff exactly one survives;
   - multiple candidates without arity info: cannot disambiguate, give
     up rather than guess.
   Unlike [Shape_and_sig.find_by_arity], which disambiguates among
   stored signatures of a single function and understands variadic tags
   ([Arity_at_least]), this helper only sees raw parameter lists and
   uses strict exact-arity matching. *)
let pick_by_arity (call_arity : int option) (matches : func_info list)
    : fn_id option =
  match matches with
  | [single_match] -> Some single_match.fn_id
  | [] ->
      Log.debug (fun m -> m "PICK_BY_ARITY: no candidates");
      None
  | _ ->
      (match call_arity with
      | Some arity ->
          let arity_matches = List.filter (fun f ->
            Int.equal (get_func_arity f.fdef) arity
          ) matches in
          (match arity_matches with
          | [single_match] -> Some single_match.fn_id
          | [] ->
              Log.debug (fun m ->
                m "PICK_BY_ARITY: %d candidates, none with arity %d; giving up"
                  (List.length matches) arity);
              None
          | _ ->
              (* Should not fire: requires two functions sharing both name
                 and arity, with the same class/module scope; defensive. *)
              Log.debug (fun m ->
                m "PICK_BY_ARITY: %d candidates, %d still match arity %d; giving up"
                  (List.length matches) (List.length arity_matches) arity);
              None)
      | None ->
          Log.debug (fun m ->
            m "PICK_BY_ARITY: %d candidates, no arity info; giving up"
              (List.length matches));
          None)

(* Graph node type - reuse from Call_graph for consistency *)
type node = Call_graph.node

(* Extract graph node from fn_id - takes the last element *)
let fn_id_to_node (fn_id : fn_id) : node option =
  match List.rev fn_id with
  | Some name :: _ -> Some (Function_id.of_il_name name)
  | _ -> None

(* Equality for fn_id using compare_fn_id *)
let equal_fn_id f1 f2 = Int.equal (compare_fn_id f1 f2) 0

(* Helper function to identify the callee fn_id from a call expression's callee *)
(* Resolve a type to its constructor fn_id using lang config.
   e.g. Foo → Foo#<init> (Java), Foo → Foo#__init__ (Python), Foo → Foo#initialize (Ruby) *)
let resolve_constructor_from_type ~(lang : Lang.t) ~all_funcs (ty : G.type_) : fn_id option =
  let class_name = match ty.G.t with
    | G.TyN (G.Id ((name, _), _)) -> Some name
    | G.TyExpr { G.e = G.N (G.Id ((name, _), _)); _ } -> Some name
    | _ -> None
  in
  match class_name with
  | None -> None
  | Some cls ->
      List.find_opt (fun f ->
        match f.fn_id with
        | [Some c; Some m] ->
            fst c.IL.ident = cls
            && Object_initialization.is_constructor lang (fst m.IL.ident) (Some cls)
        | _ -> false
      ) all_funcs |> Option.map (fun f -> f.fn_id)

let identify_callee ~(lang : Lang.t) ?(object_mappings = []) ?(all_funcs = [])
    ?(caller_parent_path = []) ?(call_arity : int option) (callee : G.expr) : fn_id option =
  (* Extract class from caller_parent_path if present *)
  let current_class = match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  match callee.G.e with
    (* Simple function call: foo() *)
    | G.N (G.Id ((id, _), _id_info)) ->
        let callee_name_str = id in
        (* First check if it's a nested function in the same scope.
           Use position-aware match to distinguish same-named parent functions. *)
        let nested_match =
          find_func_in_scope all_funcs caller_parent_path callee_name_str
        in
        begin
          match nested_match with
          | Some f ->
              Log.debug (fun m -> m "CALL_EXTRACT: Found nested function %s in same scope" callee_name_str);
              Some f.fn_id
          | None ->
              (* For class-based languages, foo() might be an implicit this.foo() call.
                 Check if a method with this name exists in the current class. *)
              match current_class with
              | Some class_name ->
                  let class_name_str = fst class_name.IL.ident in
                  (* Check if this method exists in the class - use string matching *)
                  let method_match = List.find_opt (fun f ->
                      match f.fn_id with
                      | [Some c; Some _] ->
                          fst c.IL.ident = class_name_str
                          && func_info_name_matches f callee_name_str
                      | _ -> false
                  ) all_funcs in
                  Log.debug (fun m ->
                      (* Debug: show all function names *)
                      let all_names =
                        all_funcs
                        |> List.map (fun f -> show_fn_id f.fn_id)
                        |> String.concat ", "
                      in
                      m "CALL_EXTRACT: In class %s, call to %s, checking %d funcs, method_exists=%b, ALL: [%s]"
                        class_name_str callee_name_str (List.length all_funcs) (Option.is_some method_match) all_names);
                  (match method_match with
                  | Some f -> Some f.fn_id
                  | None ->
                      (* It's a free function call, not a method - use string matching *)
                      let free_fn_match = List.find_opt (fun f ->
                          match f.fn_id with
                          | [None; Some _] -> func_info_name_matches f callee_name_str
                          | _ -> false
                      ) all_funcs in
                      Option.map (fun f -> f.fn_id) free_fn_match)
              | None ->
                  (* Top-level free function - use string matching *)
                  let free_fn_match =
                    List.find_opt (fun f ->
                      match f.fn_id with
                      | [None; Some _] -> func_info_name_matches f callee_name_str
                      | _ -> false
                    ) all_funcs in
                  (match Option.map (fun f -> f.fn_id) free_fn_match with
                  | Some _ as r -> r
                  | None ->
                      (* Try as constructor: ClassName() → ClassName#__init__ etc. *)
                      let ty = G.{ t = TyN (G.Id ((callee_name_str, G.fake callee_name_str), G.empty_id_info ())); t_attrs = [] } in
                      resolve_constructor_from_type ~lang ~all_funcs ty)
        end
        (* Qualified call: Module.foo() *)
        | G.N (G.IdQualified { name_last = (id, _), _; _ }) ->
            let callee_name_str = id in
            (* Use string matching to find the qualified function *)
            let qualified_match = List.find_opt (fun f ->
              match f.fn_id with
              | [None; Some _] -> func_info_name_matches f callee_name_str
              | _ -> false
            ) all_funcs in
            Option.map (fun f -> f.fn_id) qualified_match
        (* Method call: this.method() or self.method() *)
        | G.DotAccess
            ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            (* Use string matching to find the method in current class *)
            (match current_class with
            | Some class_name ->
                let class_name_str = fst class_name.IL.ident in
                (* Find all methods matching class and name *)
                let method_matches = List.filter (fun f ->
                  match f.fn_id with
                  | [Some c; Some _] ->
                      fst c.IL.ident = class_name_str
                      && func_info_name_matches f method_name_str
                  | _ -> false
                ) all_funcs in
                pick_by_arity call_arity method_matches
            | None -> None)
        (* Method call: obj.method() - look up obj's class *)
        | G.DotAccess
            ( { e = G.N (G.Id ((obj_name, _), obj_id_info)); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            let obj_resolved = !(obj_id_info.G.id_resolved) in
            let obj_class_opt =
              object_mappings
              |> List.find_opt (fun (var_name, _class_name) ->
                     match var_name with
                     | G.Id ((var_str, _), var_id_info) ->
                         var_str = obj_name &&
                         (match (obj_resolved, !(var_id_info.G.id_resolved)) with
                          | Some (_, sid1), Some (_, sid2) -> G.SId.equal sid1 sid2
                          | _ -> true (* fallback to name-only if unresolved *))
                     | _ -> false)
              |> Option.map (fun (_var_name, class_name) -> class_name)
            in
            (* Fallback: use the type annotation (e.g. `def f(x: ClassName)`) *)
            let obj_class_opt = match obj_class_opt with
              | Some _ -> obj_class_opt
              | None -> (match !(obj_id_info.G.id_type) with
                  | Some { G.t = G.TyN (G.Id _ as n); _ }
                  | Some { G.t = G.TyExpr { G.e = G.N (G.Id _ as n); _ }; _ } -> Some n
                  | _ -> None)
            in
            (match obj_class_opt with
            | Some class_name ->
                let class_name_str = match class_name with
                  | G.Id ((str, _), _) -> str
                  | _ -> ""
                in
                (* Find all methods matching class and name *)
                let method_matches = List.filter (fun f ->
                  match f.fn_id with
                  | [Some c; Some _] ->
                      fst c.IL.ident = class_name_str
                      && func_info_name_matches f method_name_str
                  | _ -> false
                ) all_funcs in
                pick_by_arity call_arity method_matches
            | None ->
                (* obj not in object_mappings — try as ClassName.new() constructor *)
                let ty = G.{ t = TyN (G.Id ((obj_name, G.fake obj_name), G.empty_id_info ())); t_attrs = [] } in
                resolve_constructor_from_type ~lang ~all_funcs ty)
        (* Chained call: Constructor(...).method() — receiver is a constructor.
           Python/Kotlin/Scala: ClassName(args).method()
           Java/JS/TS/C#:       new ClassName(args).method()
           Ruby/Crystal:        ClassName.new(args).method() *)
        | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _))) ->
            let class_name_opt = match receiver.G.e with
              (* Python/Kotlin/Scala: ClassName(args) *)
              | G.Call ({ e = G.N (G.Id ((cn, _), _)); _ }, _)
                when Lang.(lang =*= Python || lang =*= Kotlin || lang =*= Scala) -> Some cn
              (* Java/JS/TS/C#: new ClassName(args)
                 Dart: ClassName(args) — the parser produces G.New even
                 without the (optional) `new` keyword *)
              | G.New (_, ty, _, _)
                when Lang.(lang =*= Java || lang =*= Js || lang =*= Ts || lang =*= Csharp
                           || lang =*= Dart) ->
                  (match ty.G.t with
                  | G.TyN (G.Id ((cn, _), _)) -> Some cn
                  | G.TyExpr { G.e = G.N (G.Id ((cn, _), _)); _ } -> Some cn
                  | _ -> None)
              (* Ruby/Crystal: ClassName.new(args) *)
              | G.Call ({ e = G.DotAccess (
                    { e = G.N (G.Id ((cn, _), _)); _ }, _,
                    G.FN (G.Id (("new", _), _))); _ }, _)
                when Lang.(lang =*= Ruby || lang =*= Crystal) -> Some cn
              | _ -> None
            in
            (match class_name_opt with
            | Some class_name ->
                let method_matches = List.filter (fun f ->
                  match f.fn_id with
                  | [Some c; Some _] ->
                      fst c.IL.ident = class_name
                      && func_info_name_matches f method_name
                  | _ -> false
                ) all_funcs in
                pick_by_arity call_arity method_matches
            | None -> None)
        | _ ->
            Log.debug (fun m ->
                m "CALL_EXTRACT: Unmatched call pattern: %s"
                  (G.show_expr callee));
            None
