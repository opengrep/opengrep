open Common
module G = AST_generic
module Log = Log_call_graph.Log

let unique_call_threshold = 32

let pathological_candidate_cap = unique_call_threshold * 4

let infer_class_max_depth = 8

let leaf_of_callee (e : G.expr) : string option =
  match e.G.e with
  | G.N (G.Id ((s, _), _))
  | G.N (G.IdQualified { name_last = ((s, _), _); _ })
  | G.DotAccess (_, _, G.FN (G.Id ((s, _), _)))
  | G.DotAccess (_, _, G.FN (G.IdQualified { name_last = ((s, _), _); _ }))
    -> Some s
  | _ -> None


(* Function identifier as a path from outermost to innermost scope.
 * For example:
 * - [Some class_name; Some method_name; Some nested_fn] for nested function
 * - [Some class_name; Some method_name] for a method
 * - [Some fn_name] for a top-level function
 * - [] for top-level/anonymous
 *)
type fn_id = Func_info.fn_id
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

(* Re-expose Func_info.t's record so [.fn_id] etc. resolve unqualified here. *)
type func_info = Func_info.t = {
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

(* Free-function check that also matches named-lambda var bindings (which [as_free] alone reports as [_tmp_lambda]). *)
let is_free_named (f : func_info) (name_str : string) : bool =
  Option.is_some (Func_info.as_free f.fn_id)
  && func_info_name_matches f name_str

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
let prefer_concrete (matches : func_info list) : func_info list =
  let is_abstract (f : func_info) : bool =
    match f.fdef.G.fbody with
    | G.FBDecl _ | G.FBNothing -> true
    | _ -> false
  in
  let concrete = List.filter (fun f -> not (is_abstract f)) matches in
  match concrete with
  | [] -> matches
  | _ -> concrete

let pick_by_arity (call_arity : int option) (matches : func_info list)
    : fn_id option =
  let matches = prefer_concrete matches in
  (* Reject a body-less synth candidate (Ruby [attr_reader]) against a positional-arg call. *)
  let single_synth_with_args (f : func_info) : bool =
    match call_arity, f.fdef.G.fbody with
    | Some n, (G.FBNothing | G.FBDecl _) when n > 0 -> true
    | _ -> false
  in
  match matches with
  | [single_match] when single_synth_with_args single_match ->
      None
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
  | Some name :: _ ->
    let ident_node = Function_id.of_il_name name in
    (* Alias-synthetic leaf (cf. Ts_class_aliases): the exposed ident
       sits at the TARGET's position while the resolved sid carries the
       target's own name — same position, different name is that
       deliberate signature, and the node must be the target's identity
       (that's where the def and its signature live). Real defs resolve
       to their own name, so this is a no-op for them. *)
    (match !(name.IL.id_info.G.id_resolved) with
     | Some (_, sid) when not (G.SId.is_unsafe_default sid) ->
       let sid_node = Function_id.of_sid sid in
       let sid_name, _, _, _ = G.SId.to_loc sid in
       let same_position =
         let f1, l1, c1 = Function_id.to_file_line_col ident_node in
         let f2, l2, c2 = Function_id.to_file_line_col sid_node in
         String.equal f1 f2 && Int.equal l1 l2 && Int.equal c1 c2
       in
       if same_position
          && not (String.equal sid_name (fst name.IL.ident))
       then Some sid_node
       else Some ident_node
     | _ -> Some ident_node)
  | _ -> None


(* Helper function to identify the callee fn_id from a call expression's callee *)
let uses_new_keyword (lang : Lang.t) : bool =
  (Lang_config.get lang).Lang_config.uses_new_keyword

(* Resolve a class name to its constructor fn_id using lang config.
   e.g. Foo → Foo#<init> (Java), Foo → Foo#__init__ (Python), Foo → Foo#initialize (Ruby) *)
let resolve_constructor ~(lang : Lang.t) ~all_funcs (class_name : string)
    : fn_id option =
  List.find_opt (fun f ->
    match Func_info.as_method f.fn_id with
    | Some (c, m) ->
        fst c.IL.ident = class_name
        && Object_initialization.is_constructor lang (fst m.IL.ident)
             (Some class_name)
    | None -> false
  ) all_funcs |> Option.map (fun f -> f.fn_id)

let resolve_constructor_from_type ~(lang : Lang.t) ~all_funcs (ty : G.type_) : fn_id option =
  match ty.G.t with
  | G.TyN (G.Id ((name, _), _))
  | G.TyExpr { G.e = G.N (G.Id ((name, _), _)); _ } ->
    resolve_constructor ~lang ~all_funcs name
  | _ -> None

let funcs_with_leaf ~(func_lookup : Func_lookup.t)
    ~(all_funcs : func_info list) (leaf : string) : func_info list =
  Func_lookup.funcs_with_leaf func_lookup ~all_funcs leaf

(* Leaf-name narrowing of [all_funcs] is required for tractability. *)
let rec identify_callee ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(type_state : Type_state.t = Type_state.empty)
    ?(caller_parent_path = []) ?(call_arity : int option) (callee : G.expr) : fn_id option =
  let is_locally_imported (name : string) : bool =
    Func_lookup.is_locally_imported func_lookup name
  in
  let rec collect_dotted_chain (e : G.expr) : (string * string list) option =
    match e.G.e with
    | G.N (G.Id ((s, _), _)) -> Some (s, [])
    | G.DotAccess (sub, _, G.FN (G.Id ((s, _), _))) ->
      (match collect_dotted_chain sub with
       | Some (base, parts) -> Some (base, parts @ [s])
       | None -> None)
    | _ -> None
  in
  let current_file_of_caller : string option =
    let rec first_real_tok = function
      | [] -> None
      | None :: rest -> first_real_tok rest
      | Some (n : IL.name) :: rest ->
        let tok = snd n.IL.ident in
        if Tok.is_fake tok then first_real_tok rest
        else
          (try Some (Fpath.to_string (Tok.file_of_tok tok))
           with Tok.NoTokenLocation _ -> first_real_tok rest)
    in
    first_real_tok caller_parent_path
  in
  let func_def_file (f : func_info) : string option =
    try Some (Fpath.to_string (Tok.file_of_tok (snd f.fdef.G.fkind)))
    with Tok.NoTokenLocation _ -> None
  in
  let same_file_filter (matches : func_info list) : func_info list =
    match current_file_of_caller with
    | None -> matches
    | Some cf ->
      let same = List.filter (fun f ->
        match func_def_file f with
        | Some df -> String.equal df cf
        | None -> false
      ) matches in
      (match same with [] -> matches | _ -> same)
  in
  (* Prefer the caller's own directory (Go packages are directory-scoped). *)
  let same_dir_filter (matches : func_info list) : func_info list =
    if not Lang.(lang =*= Go) then matches
    else
    match current_file_of_caller with
    | None -> matches
    | Some cf ->
      let cdir = Filename.dirname cf in
      let same = List.filter (fun f ->
        match func_def_file f with
        | Some df -> String.equal (Filename.dirname df) cdir
        | None -> false
      ) matches in
      (match same with [] -> matches | _ -> same)
  in
  let narrow_by_package_qualifier (qual : string option)
      (matches : func_info list) : func_info list =
    match qual with
    | None -> matches
    | Some q ->
      let want = Func_lookup.resolve_alias func_lookup q in
      let in_package (f : func_info) =
        match func_def_file f with
        | None -> false
        | Some df ->
          (match want, Func_lookup.module_qn_of_file func_lookup df with
           | Some w, Some g ->
             String.equal (Names.Module_qn.to_string w)
               (Names.Module_qn.to_string g)
           | _ ->
             String.equal (Filename.basename (Filename.dirname df)) q)
      in
      (match List.filter in_package matches with
       | [] -> matches
       | qm -> qm)
  in
  (* Prefer the caller's own file, then dir (Go packages are directory-scoped:
     same-leaf collisions). *)
  let narrow_file_then_dir matches =
    if List.length matches > 1 then
      let by_file = same_file_filter matches in
      if List.length by_file > 1 then same_dir_filter by_file
      else by_file
    else matches
  in
  let resolve_class_method ?qualifier ~class_name ~method_name matches
      : fn_id option =
    let matches = narrow_by_package_qualifier qualifier matches in
    let matches = narrow_file_then_dir matches in
    (* Route to an [FBDecl] match; [Interfile_dispatch] fills its sig (else
       [prefer_concrete] strips it). *)
    let interface_match =
      List.find_opt (fun (f : func_info) ->
        Func_info.is_method_of ~class_name ~method_name f.fn_id
        && (match f.fdef.G.fbody with G.FBDecl _ -> true | _ -> false)
      ) matches
    in
    match interface_match with
    | Some f -> Some f.fn_id
    | None -> pick_by_arity call_arity matches
  in
  let exceeds (lst : 'a list) (n : int) : bool =
    let rec go lst k =
      if k < 0 then true
      else match lst with
      | [] -> false
      | _ :: rest -> go rest (k - 1)
    in
    go lst n
  in
  let try_unique_by_distinct_key
      ~(candidate_filter : func_info -> bool)
      ~(distinct_key : func_info -> string option)
      (name : string) : fn_id option =
    let cands =
      funcs_with_leaf ~func_lookup ~all_funcs name
      |> List.filter candidate_filter
    in
    match cands with
    | [] -> None
    | _ when exceeds cands pathological_candidate_cap -> None
    | _ ->
      let distinct xs =
        List.sort_uniq String.compare (List.filter_map distinct_key xs)
      in
      let dk = distinct cands in
      if Int.equal (List.length dk) 1 then pick_by_arity call_arity cands
      else if List.length dk > unique_call_threshold then None
      else
        let same = same_file_filter cands in
        if Int.equal (List.length (distinct same)) 1
           && List.length same < List.length cands then
          pick_by_arity call_arity same
        else None
  in
  let try_unique_callee ~(callee_name : string) : fn_id option =
    if Lang.equal lang Lang.Ruby then None
    else
      try_unique_by_distinct_key
        ~candidate_filter:(fun _ -> true)
        ~distinct_key:(fun f ->
          match List_.init_and_last_opt f.fn_id with
          | Some (parents, _) ->
            Some (String.concat "::" (List.map (fun p ->
              match p with
              | Some n -> fst n.IL.ident
              | None -> "_") parents))
          | _ -> None)
        callee_name
  in
  let try_nested_callee ~(callee_name : string) : fn_id option =
    match Func_lookup.nested_in_same_file func_lookup callee_name with
    | [] -> None
    | [single] -> Some single.fn_id
    | _ -> None
  in
  let try_imported_callee ~(callee_name : string) : fn_id option =
    match Func_lookup.resolve_alias func_lookup callee_name with
    | None -> None
    | Some imported_qn ->
      (match Names.Module_qn.split_last imported_qn with
       | None -> None
       | Some (module_qn, _) when Names.Module_qn.is_empty module_qn -> None
       | Some (module_qn, leaf_name) ->
         let candidates =
           Func_lookup.funcs_in_module func_lookup module_qn
           |> List.filter (fun f -> is_free_named f leaf_name)
         in
         pick_by_arity call_arity candidates)
  in
  let try_unique_method_call ~(method_name : string) : fn_id option =
    try_unique_by_distinct_key
      ~candidate_filter:(fun f ->
        Option.is_some (Func_info.as_method f.fn_id))
      ~distinct_key:(fun f ->
        Option.map (fun (c, _) -> fst c.IL.ident)
          (Func_info.as_method f.fn_id))
      method_name
  in
  let try_module_qn_call ~(base : string) ~(parts : string list)
      ~(method_name : string) : fn_id option =
    if not (Func_lookup.imports_indexed func_lookup) then None
    else
    match Func_lookup.resolve_alias func_lookup base with
    | None -> None
    | Some base_qn ->
      let target_qn = match parts with
        | [] -> base_qn
        | _ ->
          Names.Module_qn.of_string
            (Names.Module_qn.to_string base_qn ^ "."
             ^ String.concat "." parts)
      in
      let candidates =
        Func_lookup.funcs_in_module func_lookup target_qn
        |> List.filter (fun f -> is_free_named f method_name)
      in
      (match pick_by_arity call_arity candidates with
          | Some _ as r -> r
          | None ->
            let cls_simple =
              match List_.last_opt parts with
              | None -> Names.Module_qn.leaf base_qn
              | Some p -> p
            in
            if not (Type_state.has_class type_state cls_simple) then None
            else
              let method_matches =
                Type_state.find_methods type_state ~fallback:[]
                  ~class_name:cls_simple ~method_name
              in
              pick_by_arity call_arity method_matches)
  in
  (* Kept un-narrowed for the bare-generic [foo<T>()] reroute. *)
  let unnarrowed_all_funcs = all_funcs in
  (* leaf-narrowed [all_funcs] is keyed on class name, so re-narrow by constructor names. *)
  let ctor_candidate_funcs () =
    let names = (Lang_config.get lang).Lang_config.constructor_names in
    match
      List.concat_map
        (fun cn ->
          match Func_lookup.narrow_candidates_by_leaf func_lookup cn with
          | Some fs -> fs
          | None -> [])
        names
    with
    | [] -> unnarrowed_all_funcs
    | fs -> fs
  in
  let all_funcs =
    match leaf_of_callee callee with
    | Some leaf ->
      (match Func_lookup.narrow_candidates_by_leaf func_lookup leaf with
       | Some narrowed -> narrowed
       | None -> all_funcs)
    | None -> all_funcs
  in
  let current_class = Func_info.enclosing_class caller_parent_path in
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
                  (* [methods_by_class] first: catches MRO-inherited methods. *)
                  let method_matches =
                    Type_state.find_methods type_state ~fallback:all_funcs
                      ~class_name:class_name_str ~method_name:callee_name_str
                  in
                  let method_match = match method_matches with
                    | [] -> None
                    | x :: _ -> Some x
                  in
                  (* Build [all_names] inside the lambda: O(N) only when debug is on. *)
                  Log.debug (fun m ->
                      let all_names =
                          all_funcs
                          |> List.map (fun f -> show_fn_id f.fn_id)
                          |> String.concat ", "
                      in
                      m "CALL_EXTRACT: In class %s, call to %s, checking %d funcs, method_exists=%b, ALL: [%s]"
                          class_name_str callee_name_str (List.length all_funcs) (Option.is_some method_match) all_names);
                  (match method_match with
                  | Some f -> Some f.fn_id
                  | None when is_locally_imported callee_name_str ->
                      try_imported_callee ~callee_name:callee_name_str
                  | None ->
                      let free_fn_match =
                        List.find_opt (fun f -> is_free_named f callee_name_str)
                          all_funcs
                      in
                      (match Option.map (fun f -> f.fn_id) free_fn_match with
                       | Some _ as r -> r
                       | None -> try_nested_callee ~callee_name:callee_name_str))
              | None when is_locally_imported callee_name_str ->
                  try_imported_callee ~callee_name:callee_name_str
              | None ->
                  (* Top-level free function - use string matching *)
                  let free_fn_match =
                    List.find_opt (fun f -> is_free_named f callee_name_str)
                      all_funcs
                  in
                  (match Option.map (fun f -> f.fn_id) free_fn_match with
                  | Some _ as r -> r
                  | None ->
                    match try_nested_callee ~callee_name:callee_name_str with
                    | Some _ as r -> r
                    | None ->
                      (* Try as constructor: ClassName() → ClassName#__init__ etc. *)
                      (match resolve_constructor ~lang
                               ~all_funcs:(ctor_candidate_funcs ())
                               callee_name_str with
                       | Some _ as r -> r
                       | None ->
                         if not (Func_lookup.imports_indexed func_lookup) then
                           try_unique_callee ~callee_name:callee_name_str
                         else
                           (match Func_lookup.resolve_alias func_lookup callee_name_str with
                            | None -> None
                            | Some imported_qn ->
                              (match Names.Module_qn.split_last imported_qn with
                               | None -> None
                               | Some (module_qn, _)
                                 when Names.Module_qn.is_empty module_qn ->
                                 None
                               | Some (module_qn, leaf_name) ->
                                 let candidates =
                                   Func_lookup.funcs_in_module func_lookup module_qn
                                   |> List.filter (fun f -> is_free_named f leaf_name)
                                 in
                                 (match pick_by_arity call_arity candidates with
                                  | Some _ as r -> r
                                  | None ->
                                    try_unique_callee ~callee_name:callee_name_str)))))
        end
        (* Bare generic [foo<T>()]: reroute through [N (Id)] for the [try_unique_callee] fallback. *)
        | G.N (G.IdQualified
                 { name_last = (id, _typeargs); name_middle = None;
                   name_top = None; name_info; _ }) ->
            let synth = { callee with G.e = G.N (G.Id (id, name_info)) } in
            identify_callee ~lang
              ~all_funcs:unnarrowed_all_funcs ~func_lookup ~type_state
              ~caller_parent_path ?call_arity synth
        | G.N (G.IdQualified { name_last = (id, _), _; name_middle; _ }) ->
            let callee_name_str = id in
            let qualified_match =
              List.find_opt (fun f -> is_free_named f callee_name_str) all_funcs
            in
            (match qualified_match with
            | Some f -> Some f.fn_id
            | None ->
                let single_qualifier_opt =
                  match name_middle with
                  | Some (G.QDots [(seg, _), _]) -> Some seg
                  | _ -> None
                in
                let alias_match =
                  match single_qualifier_opt with
                  | None -> None
                  | Some q ->
                    if not (Func_lookup.imports_indexed func_lookup) then None
                    else
                      match Func_lookup.resolve_alias func_lookup q with
                      | None -> None
                      | Some module_qn ->
                        let candidates =
                          Func_lookup.funcs_in_module func_lookup module_qn
                          |> List.filter (fun f ->
                               is_free_named f callee_name_str)
                        in
                        pick_by_arity call_arity candidates
                in
                (match alias_match with
                | Some _ as r -> r
                | None ->
                  let class_name_opt =
                    match name_middle with
                    | Some (G.QDots dots) ->
                      Option.map (fun (cls_id, _) -> fst cls_id)
                        (List_.last_opt dots)
                    | _ -> None
                  in
                  (match class_name_opt with
                  | None -> None
                  | Some class_name_str ->
                    let method_matches =
                      Type_state.find_methods type_state
                        ~fallback:all_funcs
                        ~class_name:class_name_str
                        ~method_name:callee_name_str
                    in
                    pick_by_arity call_arity method_matches)))
        (* Method call: this.method() or self.method() *)
        | G.DotAccess
            ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            (match current_class with
            | Some class_name ->
                let class_name_str = fst class_name.IL.ident in
                (* [find_methods] unions the class's own methods with the
                   MRO-inherited ones, so [self.m()] resolves to a method
                   defined on an ancestor (incl. inherited staticmethods). *)
                let method_matches =
                  Type_state.find_methods type_state ~fallback:all_funcs
                    ~class_name:class_name_str ~method_name:method_name_str
                in
                pick_by_arity call_arity method_matches
            | None -> None)
        (* No ctor/fuzzy fallback here (FP-prone on namespaced libs). *)
        | G.DotAccess
            ( { e = G.N (G.IdQualified { name_last = ((obj_name, _), _); _ }); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            let class_member_matches =
              Type_state.find_methods type_state ~fallback:all_funcs
                ~class_name:obj_name ~method_name:method_name_str
            in
            pick_by_arity call_arity class_member_matches
        (* Method call: obj.method() - look up obj's class *)
        | G.DotAccess
            ( { e = G.N (G.Id ((obj_name, _), obj_id_info)); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            (* Receiver type published on [id_info] by projidx augment / intrafile broadcast. *)
            let obj_class_opt =
              Option.bind !(obj_id_info.G.id_type)
                Ty_leaf.qualified_class_name_of_ty
            in
            (match obj_class_opt with
            | Some class_name ->
                let class_name_str =
                  Option.value (Ty_leaf.leaf_of_name class_name) ~default:""
                in
                let method_matches =
                  Type_state.find_methods type_state ~fallback:all_funcs
                    ~class_name:class_name_str ~method_name:method_name_str
                in
                resolve_class_method
                  ?qualifier:(Ty_leaf.qualifier_of_name class_name)
                  ~class_name:class_name_str
                  ~method_name:method_name_str method_matches
            | None ->
                let class_member_matches =
                  let from_class =
                    Type_state.find_methods type_state ~fallback:[]
                      ~class_name:obj_name ~method_name:method_name_str
                  in
                  let from_all =
                    List.filter (fun (f : Func_info.t) ->
                      Func_info.is_method_of ~class_name:obj_name
                        ~method_name:method_name_str f.fn_id
                    ) all_funcs
                  in
                  from_class @ from_all
                in
                (match pick_by_arity call_arity class_member_matches with
                | Some _ as r -> r
                | None ->
                    let module_match =
                      try_module_qn_call ~base:obj_name ~parts:[]
                        ~method_name:method_name_str
                    in
                    (match module_match with
                    | Some _ as r -> r
                    | None ->
                      let pkg_match =
                        let candidates =
                          Func_lookup.funcs_in_package func_lookup obj_name
                          |> List.filter (fun f -> is_free_named f method_name_str)
                        in
                        pick_by_arity call_arity candidates
                      in
                      (match pkg_match with
                       | Some _ as r -> r
                       | None ->
                         let ctor_via_new =
                           if String.equal method_name_str "new"
                              && Lang.(lang =*= Ruby || lang =*= Crystal) then
                             resolve_constructor ~lang
                               ~all_funcs:(ctor_candidate_funcs ()) obj_name
                           else None
                         in
                         (match ctor_via_new with
                          | Some _ as r -> r
                          | None ->
                         (match resolve_constructor ~lang ~all_funcs obj_name with
                          | Some _ as r -> r
                          | None ->
                            try_unique_method_call ~method_name:method_name_str))))))
        (* Chained call: Constructor(...).method() — receiver is a constructor.
           Python/Kotlin/Scala: ClassName(args).method()
           Java/JS/TS/C#:       new ClassName(args).method()
           Ruby/Crystal:        ClassName.new(args).method() *)
        | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _))) ->
            let module_match =
              match collect_dotted_chain receiver with
              | None -> None
              | Some (base, parts) ->
                try_module_qn_call ~base ~parts ~method_name
            in
            (match module_match with
            | Some _ as r -> r
            | None ->
            let ctx : Type_infer.ctx = {
              (* No project-wide free-fn return index. *)
              Type_infer.function_return = (fun _ -> None);
              method_return = (fun ~class_name ~method_name ->
                Type_state.method_return type_state ~class_name ~method_name);
              field_type = (fun ~class_name ~field_name ->
                Type_state.field_type_for_caller type_state
                  ~class_name ~field_name
                  ~caller_dir:(Option.map Filename.dirname
                                 current_file_of_caller));
              parent_of = (fun cls -> Type_state.parent type_state cls);
              has_class = (fun cls -> Type_state.has_class type_state cls);
              current_class =
                Option.map (fun (n : IL.name) ->
                  G.Id (n.IL.ident, G.empty_id_info ())) current_class;
              uses_new_keyword = uses_new_keyword lang;
            } in
            let inferred_receiver_type =
              Type_infer.type_of_expr ~max_depth:infer_class_max_depth ~ctx
                receiver
            in
            let class_name_opt = match receiver.G.e with
              (* Ruby/Crystal [ClassName.new(args)]: [new] is a constructor,
                 not a method-return lookup. *)
              | G.Call ({ e = G.DotAccess (
                    { e = G.N (G.Id ((cn, _), _)
                             | G.IdQualified
                                 { name_last = ((cn, _), _); _ }); _ }, _,
                    G.FN (G.Id (("new", _), _))); _ }, _)
                when Lang.(lang =*= Ruby || lang =*= Crystal) -> Some cn
              | _ ->
                Option.bind inferred_receiver_type Ty_leaf.leaf_of_name
            in
            let qualifier_hint : string option =
              Option.bind inferred_receiver_type Ty_leaf.qualifier_of_name
            in
            (match class_name_opt with
            | Some class_name ->
                let method_matches =
                  Type_state.find_methods type_state ~fallback:all_funcs
                    ~class_name ~method_name
                in
                (match resolve_class_method ?qualifier:qualifier_hint
                         ~class_name ~method_name method_matches with
                 | Some _ as r -> r
                 | None -> try_unique_method_call ~method_name)
            | None ->
              try_unique_method_call ~method_name))
        | _ ->
            Log.debug (fun m ->
                m "CALL_EXTRACT: Unmatched call pattern: %s"
                  (G.show_expr callee));
            None

