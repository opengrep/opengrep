open Common
module G = AST_generic
module Log = Log_call_graph.Log

let unique_call_threshold = 32

let pathological_candidate_cap = unique_call_threshold * 4

let infer_class_max_depth = 8

let bare_name_of_callee (e : G.expr) : string option =
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

(* Arity of a function definition as seen from a call site: the parameters
   a call fills, so arity comparison against call sites matches for
   methods. *)
let get_func_arity ~(lang : Lang.t) (f : func_info) : int =
  Receiver.arity lang ~is_method:(Receiver.is_method f.fdef)
    ~is_static:(Receiver.is_static f.entity)
    (Tok.unbracket f.fdef.G.fparams)

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

(* The callee for a tie between same-arity overloads of one scope: the
   earliest by position, whose signature interfile dispatch widens to the
   union over the group (see [Structural_dispatch.emit_overload_edges]).
   None when the tie spans scopes, when the language has no overloads by
   type, or when no union is built, as in a single-file graph: giving up
   is then the conservative answer. *)
let overload_representative ~(lang : Lang.t) ~(overload_groups : bool)
    (matches : func_info list) : fn_id option =
  let scope (f : func_info) : (string option * string option) =
    ( (match Func_info.enclosing_class f.fn_id with
       | Some (cls : IL.name) -> Some (fst cls.IL.ident)
       | None -> Func_info.entity_qualifier f),
      Option.map Fpath.to_string (Func_info.def_file_opt f) )
  in
  match matches with
  | [] -> None
  | _ when not (overload_groups && Lang_config.overloads_by_type lang) -> None
  | first :: rest ->
      let same_scope =
        List.for_all
          (fun (f : func_info) ->
            let cls1, file1 = scope first and cls2, file2 = scope f in
            Option.equal String.equal cls1 cls2
            && Option.equal String.equal file1 file2)
          rest
      in
      if not same_scope then None
      else
        matches
        |> List.filter_map (fun (f : func_info) ->
               Option.map
                 (fun (bare_name : IL.name) ->
                    (Function_id.of_il_name bare_name, f))
                 (Func_info.bare_name f.fn_id))
        |> List.sort (fun ((a : Function_id.t), _) ((b : Function_id.t), _) ->
               Function_id.compare a b)
        |> List_.hd_opt
        |> Option.map (fun (_, (f : func_info)) -> f.fn_id)

let pick_by_arity ?(overload_groups = false) ~(lang : Lang.t)
    (call_arity : int option) (matches : func_info list) : fn_id option =
  let matches = prefer_concrete matches in
  (* Reject a body-less synth candidate (Ruby [attr_reader]: [FBNothing]
     with an empty param list) against a positional-arg call.  A body-less
     decl WITH params is an interface/abstract declaration and must stay
     resolvable — dispatch merges the concrete impls' signatures into the
     decl's vertex, so dropping its edge severs impl dispatch. *)
  let single_synth_with_args (f : func_info) : bool =
    match call_arity, f.fdef.G.fbody with
    | Some n, (G.FBNothing | G.FBDecl _)
      when n > 0 && List_.null (Tok.unbracket f.fdef.G.fparams) -> true
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
            Int.equal (get_func_arity ~lang f) arity
          ) matches in
          (match arity_matches with
          | [single_match] -> Some single_match.fn_id
          | [] ->
              Log.debug (fun m ->
                m "PICK_BY_ARITY: %d candidates, none with arity %d; giving up"
                  (List.length matches) arity);
              None
          | _ -> (
              (* Overloads by parameter type, or homonyms across scopes. *)
              match
                overload_representative ~lang ~overload_groups arity_matches
              with
              | Some _ as representative -> representative
              | None ->
                  Log.debug (fun m ->
                    m "PICK_BY_ARITY: %d candidates, %d still match arity %d \
                       across scopes; giving up"
                      (List.length matches) (List.length arity_matches) arity);
                  None))
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
    (* An alias-synthetic bare name is recorded at
       the target's position, while the resolved sid carries the target's
       own name. That pair, one position with two names, identifies an
       alias, and the node must be the target's identity, because the
       target holds the definition and its signature. A real definition
       resolves to its own name, so this branch changes nothing for it. *)
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
  Lang_config.uses_new_keyword lang

(* Resolve a class name to its constructor fn_id using lang config.
   e.g. Foo → Foo#<init> (Java), Foo → Foo#__init__ (Python), Foo → Foo#initialize (Ruby) *)
let resolve_constructor ~(lang : Lang.t) ~all_funcs (class_name : string)
    : fn_id option =
  List.find_opt (fun f ->
    match Func_info.as_method f.fn_id with
    | Some (c, m) ->
        String.equal (fst c.IL.ident) class_name
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

let funcs_with_bare_name ~(func_lookup : Func_lookup.t)
    ~(all_funcs : func_info list) (bare_name : string) : func_info list =
  Func_lookup.funcs_with_bare_name func_lookup ~all_funcs bare_name

type construction_resolver =
  call_arity:int -> G.type_ -> fn_id option

type invocation_resolver =
  caller_parent_path:IL.name option list -> string -> fn_id option

type call_site_resolver =
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?call_arity:int ->
  ?allow_constructor:bool ->
  G.expr ->
  fn_id option

(* Bare-name narrowing of [all_funcs] is required for tractability. *)
let rec identify_callee ~(lang : Lang.t)
    ?(all_funcs = [])
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(type_state : Type_state.t = Type_state.empty)
    ?(caller_parent_path = []) ?(call_arity : int option)
    ?(allow_constructor = true) (callee : G.expr) : fn_id option =
  let is_locally_imported (name : string) : bool =
    Func_lookup.is_locally_imported func_lookup name
  in
  (* every arity tie-break below knows whether overload groups exist *)
  let pick_by_arity ~(lang : Lang.t) (call_arity : int option)
      (matches : func_info list) : fn_id option =
    pick_by_arity
      ~overload_groups:(Func_lookup.overload_groups func_lookup)
      ~lang call_arity matches
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
      Func_info.prefer matches ~keep:(fun f ->
        match func_def_file f with
        | Some df -> String.equal df cf
        | None -> false)
  in
  (* Prefer the caller's own directory (Go packages are directory-scoped). *)
  let same_dir_filter (matches : func_info list) : func_info list =
    if not Lang.(lang =*= Go) then matches
    else
    match current_file_of_caller with
    | None -> matches
    | Some cf ->
      let cdir = Filename.dirname cf in
      Func_info.prefer matches ~keep:(fun f ->
        match func_def_file f with
        | Some df -> String.equal (Filename.dirname df) cdir
        | None -> false)
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
      Func_info.prefer ~keep:in_package matches
  in
  (* The candidates defined in the caller's own file are preferred, then
     those defined in the caller's directory. A Go package is one directory,
     so two classes of one bare name collide across directories. *)
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
    | None -> pick_by_arity ~lang call_arity matches
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
      funcs_with_bare_name ~func_lookup ~all_funcs name
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
      if Int.equal (List.length dk) 1 then pick_by_arity ~lang call_arity cands
      else if List.length dk > unique_call_threshold then None
      else
        let same = same_file_filter cands in
        if Int.equal (List.length (distinct same)) 1
           && List.length same < List.length cands then
          pick_by_arity ~lang call_arity same
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
    (* Methods are excluded: this resolves a BARE call, which supplies no
       receiver, so a same-named method of some class in the file is not
       a candidate (it would wire spurious cross-class edges). *)
    match
      Func_lookup.nested_in_same_file func_lookup callee_name
      |> List.filter (fun (f : func_info) ->
             Option.is_none (Func_info.enclosing_class f.fn_id))
    with
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
       | Some (module_qn, bare_name) ->
         let candidates =
           Func_lookup.funcs_in_module func_lookup module_qn
           |> List.filter (fun f -> is_free_named f bare_name)
         in
         pick_by_arity ~lang call_arity candidates)
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
      (match pick_by_arity ~lang call_arity candidates with
          | Some _ as r -> r
          | None ->
            let cls_simple =
              match List_.last_opt parts with
              | None -> Names.Module_qn.bare_name base_qn
              | Some p -> p
            in
            if not (Type_state.has_class type_state cls_simple) then None
            else
              let method_matches =
                Type_state.find_methods type_state ~fallback:[]
                  ~class_name:cls_simple ~method_name
              in
              pick_by_arity ~lang call_arity method_matches)
  in
  (* Kept un-narrowed for the bare-generic [foo<T>()] reroute. *)
  let unnarrowed_all_funcs = all_funcs in
  (* The result is the first constructor the lookup's constructor index
     holds for the class, and [None] when the index holds no constructor
     for the class. *)
  let ctor_of_class (class_name : string) : fn_id option =
    match Func_lookup.constructors_of_class func_lookup class_name with
    | (func : func_info) :: _ -> Some func.fn_id
    | [] -> None
  in
  let all_funcs =
    match bare_name_of_callee callee with
    | Some bare_name ->
      (match Func_lookup.narrow_candidates_by_bare_name func_lookup bare_name with
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
                       | None ->
                         (match try_nested_callee ~callee_name:callee_name_str with
                          | Some _ as r -> r
                          | None ->
                            (* [Cls(...)] inside a method is a constructor too
                               (no [new] keyword in Python/Ruby, so it parses
                               as a plain call, not [G.New]). The module-level
                               arm below already does this; without it here a
                               constructor call written in a method resolved to
                               nothing. Gated by [allow_constructor] so an
                               argument being probed as a possible call
                               ([unresolved_arg_call]) is not mistaken for a
                               construction — passing a class is not
                               constructing it. *)
                            if allow_constructor then
                              ctor_of_class callee_name_str
                            else None)))
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
                      (* Try as constructor: ClassName() → ClassName#__init__ etc.
                         [allow_constructor] false when probing a bare-identifier
                         ARGUMENT as a possible call (see method arm above). *)
                      (match (if allow_constructor then
                                ctor_of_class callee_name_str
                              else None) with
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
                               | Some (module_qn, bare_name) ->
                                 let candidates =
                                   Func_lookup.funcs_in_module func_lookup module_qn
                                   |> List.filter (fun f -> is_free_named f bare_name)
                                 in
                                 (match pick_by_arity ~lang call_arity candidates with
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
                        pick_by_arity ~lang call_arity candidates
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
                    pick_by_arity ~lang call_arity method_matches)))
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
                pick_by_arity ~lang call_arity method_matches
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
            pick_by_arity ~lang call_arity class_member_matches
        (* Method call: obj.method() - look up obj's class *)
        | G.DotAccess
            ( { e = G.N (G.Id ((obj_name, _), obj_id_info)); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) ->
            let method_name_str = id in
            (* Receiver's instance class, published on [id_info] by projidx
               augment / intrafile broadcast, else its declared type. *)
            let obj_class_opt =
              Option.bind (Ty_bare_name.instance_or_declared_type obj_id_info)
                Ty_bare_name.qualified_class_name_of_ty
            in
            (match obj_class_opt with
            | Some class_name ->
                let class_name_str =
                  Option.value (Ty_bare_name.bare_name_of_name class_name) ~default:""
                in
                let method_matches =
                  Type_state.find_methods type_state ~fallback:all_funcs
                    ~class_name:class_name_str ~method_name:method_name_str
                in
                resolve_class_method
                  ?qualifier:(Ty_bare_name.qualifier_of_name class_name)
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
                (match pick_by_arity ~lang call_arity class_member_matches with
                | Some _ as r -> r
                | None ->
                    let module_match =
                      try_module_qn_call ~base:obj_name ~parts:[]
                        ~method_name:method_name_str
                    in
                    (match module_match with
                    | Some _ as r -> r
                    | None ->
                         let ctor_via_new =
                           if String.equal method_name_str "new"
                              && Lang.(lang =*= Ruby || lang =*= Crystal) then
                             ctor_of_class obj_name
                           else None
                         in
                         (match ctor_via_new with
                          | Some _ as r -> r
                          | None ->
                         (match resolve_constructor ~lang ~all_funcs obj_name with
                          | Some _ as r -> r
                          | None ->
                            try_unique_method_call ~method_name:method_name_str)))))
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
                Option.bind inferred_receiver_type Ty_bare_name.bare_name_of_name
            in
            let qualifier_hint : string option =
              Option.bind inferred_receiver_type Ty_bare_name.qualifier_of_name
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

let expr_of_type_name (ty : G.type_) : G.expr option =
  match ty.G.t with
  | G.TyN (name : G.name) -> Some (G.N name |> G.e)
  | G.TyExpr (e : G.expr) -> Some e
  | _ -> None

type binding_target =
  | Bound_module of Names.Module_qn.t
  | Bound_class of Names.Class_qn.t
  | Bound_functions of func_info list
  | Bound_object of func_info list Common.SMap.t

type name_position =
  | Term_position
  | Type_position

type dotted_chain = {
  dc_rooted : bool;
  dc_segments : string list;
}

let rec dotted_chain_of_expr (e : G.expr) : dotted_chain option =
  match e.G.e with
  | G.N name -> dotted_chain_of_name name
  | G.DotAccess (receiver, _, G.FN (G.Id ((segment, _), _))) ->
    Option.map
      (fun (chain : dotted_chain) ->
        { chain with dc_segments = chain.dc_segments @ [ segment ] })
      (dotted_chain_of_expr receiver)
  | _ -> None

and dotted_chain_of_name (name : G.name) : dotted_chain option =
  match name with
  | G.Id ((segment, _), _) ->
    Some { dc_rooted = false; dc_segments = [ segment ] }
  | G.IdQualified { name_last = ((last, _), _); name_middle = None;
                    name_top; _ } ->
    Some { dc_rooted = Option.is_some name_top; dc_segments = [ last ] }
  | G.IdQualified { name_last = ((last, _), _);
                    name_middle = Some (G.QDots middle);
                    name_top; _ } ->
    Some
      { dc_rooted = Option.is_some name_top;
        dc_segments =
          List.map (fun (((segment, _), _) : G.ident * _) -> segment) middle
          @ [ last ] }
  | G.IdQualified _ -> None

let entries_in_scope ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (name : string)
    : Func_lookup.scope_entry list =
  Func_lookup.resolve_in_scope func_lookup ~caller_parent_path name

let class_in_scope ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (name : string)
    : Names.Class_qn.t option =
  Func_lookup.class_of_entries
    (entries_in_scope ~func_lookup ~caller_parent_path name)

let head_binding ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(position : name_position)
    (segment : string) : binding_target option =
  let entries = entries_in_scope ~func_lookup ~caller_parent_path segment in
  match
    (position, Func_lookup.companion_of_entries entries)
  with
  | Term_position, Some (companion_qn : Names.Class_qn.t) ->
    Some (Bound_class companion_qn)
  | Term_position, None
  | Type_position, _ -> (
  match Func_lookup.class_of_entries entries with
  | Some (class_qn : Names.Class_qn.t) -> Some (Bound_class class_qn)
  | None -> (
    match Func_lookup.object_of_entries entries with
    | Some (members : func_info list Common.SMap.t) ->
      Some (Bound_object members)
    | None ->
      Option.map
        (fun (qn : Names.Module_qn.t) -> Bound_module qn)
        (Func_lookup.resolve_alias func_lookup segment)))

let attribute_of ~(func_lookup : Func_lookup.t) (target : binding_target)
    (segment : string) : binding_target option =
  match target with
  | Bound_module (module_qn : Names.Module_qn.t) -> (
    match Func_lookup.module_attribute func_lookup module_qn segment with
    | Some (Func_lookup.Attr_functions funcs) -> Some (Bound_functions funcs)
    | Some (Func_lookup.Attr_class class_qn) -> Some (Bound_class class_qn)
    | Some (Func_lookup.Attr_class_with_companion (_, companion_qn)) ->
      Some (Bound_class companion_qn)
    | Some (Func_lookup.Attr_module submodule_qn) ->
      Some (Bound_module submodule_qn)
    | None -> None)
  | Bound_class (class_qn : Names.Class_qn.t) -> (
    match
      Func_lookup.find_along_order func_lookup ~receiver:Func_lookup.On_class
        (Func_lookup.resolution_order func_lookup class_qn)
        (fun _ -> [ segment ])
    with
    | [] ->
      let nested = Names.Class_qn.concat class_qn segment in
      if Func_lookup.is_known_class func_lookup nested then
        Some (Bound_class nested)
      else None
    | (_ :: _) as funcs -> Some (Bound_functions funcs))
  | Bound_object (members : func_info list Common.SMap.t) ->
    Option.map
      (fun (funcs : func_info list) -> Bound_functions funcs)
      (Common.SMap.find_opt segment members)
  | Bound_functions _ -> None

let longest_accepted_prefix ~(accept : string list -> bool)
    (chain : string list) : (string list * string list) option =
  let rec search (taken : string list) (rest : string list)
      (best : (string list * string list) option)
      : (string list * string list) option =
    match rest with
    | []
    | [ _ ] -> best
    | segment :: after ->
      let taken = taken @ [ segment ] in
      let best = if accept taken then Some (taken, after) else best in
      search taken after best
  in
  search [] chain None

let global_attribute_binding ~(func_lookup : Func_lookup.t)
    (chain : string list) : (binding_target * string list) option =
  match chain with
  | [ (segment : string) ] -> (
    match
      Func_lookup.module_attribute func_lookup Names.Module_qn.empty segment
    with
    | Some (Func_lookup.Attr_functions (funcs : func_info list)) ->
      Some (Bound_functions funcs, [])
    | Some (Func_lookup.Attr_class (class_qn : Names.Class_qn.t)) ->
      Some (Bound_class class_qn, [])
    | Some (Func_lookup.Attr_class_with_companion (_, companion_qn)) ->
      Some (Bound_class companion_qn, [])
    | Some (Func_lookup.Attr_module (module_qn : Names.Module_qn.t)) ->
      Some (Bound_module module_qn, [])
    | None -> None)
  | []
  | _ :: _ :: _ -> None

let qualified_prefix_binding ~(func_lookup : Func_lookup.t)
    (chain : string list) : (binding_target * string list) option =
  match
    longest_accepted_prefix
      ~accept:(fun (prefix : string list) ->
        Func_lookup.is_known_class func_lookup (Names.Class_qn.of_parts prefix))
      chain
  with
  | Some ((prefix : string list), (segments : string list)) ->
    Some (Bound_class (Names.Class_qn.of_parts prefix), segments)
  | None ->
    Option.map
      (fun ((prefix : string list), (segments : string list)) ->
        (Bound_module (Names.Module_qn.of_parts prefix), segments))
      (longest_accepted_prefix
         ~accept:(fun (prefix : string list) ->
           Func_lookup.is_known_module func_lookup
             (Names.Module_qn.of_parts prefix))
         chain)

let in_own_modules ~(func_lookup : Func_lookup.t) (chain : string list)
    : (binding_target * string list) option =
  List.find_map
    (fun (region : Names.Module_qn.t) ->
      if Names.Module_qn.is_empty region then None
      else
        qualified_prefix_binding ~func_lookup
          (Names.Module_qn.parts region @ chain))
    (Func_lookup.own_modules func_lookup)

let follow_chain ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(position : name_position)
    (chain : dotted_chain) : binding_target option =
  let segments_of_chain = chain.dc_segments in
  let in_position ((target : binding_target), (segments : string list))
      : binding_target * string list =
    match (position, target) with
    | Term_position, Bound_class (class_qn : Names.Class_qn.t) -> (
      match Func_lookup.companion_of_class func_lookup class_qn with
      | Some (companion_qn : Names.Class_qn.t) ->
        (Bound_class companion_qn, segments)
      | None -> (target, segments))
    | Term_position, (Bound_module _ | Bound_functions _ | Bound_object _)
    | Type_position, _ -> (target, segments)
  in
  let completed ((target : binding_target), (segments : string list))
      : binding_target option =
    List.fold_left
      (fun (target : binding_target option) (segment : string) ->
        Option.bind target (fun target ->
          attribute_of ~func_lookup target segment))
      (Some target) segments
  in
  match segments_of_chain with
  | [] -> None
  | head :: segments -> (
    if chain.dc_rooted then
      match
        match qualified_prefix_binding ~func_lookup segments_of_chain with
        | Some _ as bound -> bound
        | None -> global_attribute_binding ~func_lookup segments_of_chain
      with
      | None -> None
      | Some (start : binding_target * string list) ->
        completed (in_position start)
    else
      match head_binding ~func_lookup ~caller_parent_path ~position head with
      | Some (target : binding_target) -> completed (target, segments)
      | None ->
        List.find_map
          (fun (start : unit -> (binding_target * string list) option) ->
            Option.bind (start ()) (fun start -> completed (in_position start)))
          [ (fun () -> in_own_modules ~func_lookup segments_of_chain);
            (fun () ->
              qualified_prefix_binding ~func_lookup segments_of_chain) ])

let constructor_of_class ~(lang : Lang.t) ~(func_lookup : Func_lookup.t)
    (class_qn : Names.Class_qn.t) : func_info list =
  Func_lookup.find_along_order func_lookup ~receiver:Func_lookup.On_any
    (Func_lookup.resolution_order func_lookup class_qn)
    (fun (ancestor : Names.Class_qn.t) ->
      Object_initialization.constructor_names_of_class ~lang
        ~class_name:(Names.Class_qn.bare_name ancestor))

let enclosing_class_qn ~(func_lookup : Func_lookup.t)
    (caller_parent_path : IL.name option list) : Names.Class_qn.t option =
  Option.bind (Func_info.enclosing_class caller_parent_path)
    (Func_lookup.class_qn_of_definition func_lookup)

let id_info_of_name (name : G.name) : G.id_info =
  match name with
  | G.Id (_, id_info) -> id_info
  | G.IdQualified { name_info; _ } -> name_info

let constant_in_nesting ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (chain : dotted_chain)
    : Names.Class_qn.t option =
  let known (parts : string list) : Names.Class_qn.t option =
    let candidate = Names.Class_qn.of_parts parts in
    if Func_lookup.is_known_class func_lookup candidate then Some candidate
    else None
  in
  let at_top_level () : Names.Class_qn.t option = known chain.dc_segments in
  if chain.dc_rooted then at_top_level ()
  else
    let enclosing = enclosing_class_qn ~func_lookup caller_parent_path in
    let scopes : string list list =
      match enclosing with
      | None -> []
      | Some (class_qn : Names.Class_qn.t) ->
        List.map Names.Class_qn.parts
          (Names.Class_qn.prefixes class_qn
           @ Func_lookup.resolution_order func_lookup class_qn)
    in
    match
      List.find_map
        (fun (scope : string list) -> known (scope @ chain.dc_segments))
        scopes
    with
    | Some _ as resolved -> resolved
    | None -> at_top_level ()

let is_constant_name (name : G.name) : bool =
  IdFlags.is_constant !((id_info_of_name name).G.id_flags)

let reads_own_class ~(lang : Lang.t) (e : G.expr) : bool =
  let accessor_of (receiver : G.expr) (name : string) : bool =
    (match receiver.G.e with
     | G.IdSpecial ((G.This | G.Self), _) -> true
     | _ -> false)
    && List.exists (String.equal name)
         (Lang_config.get lang).Lang_config.class_accessor_methods
  in
  match e.G.e with
  | G.DotAccess (receiver, _, G.FN (G.Id ((name, _), _))) ->
    accessor_of receiver name
  | G.Call ({ G.e = G.DotAccess (receiver, _, G.FN (G.Id ((name, _), _))); _ },
            _) ->
    accessor_of receiver name
  | _ -> false

let class_named_by ~(lang : Lang.t) ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (e : G.expr)
    : Names.Class_qn.t option =
  if reads_own_class ~lang e then
    enclosing_class_qn ~func_lookup caller_parent_path
  else
    match e.G.e with
    | G.N (name : G.name) when is_constant_name name ->
      Option.bind (dotted_chain_of_name name)
        (constant_in_nesting ~func_lookup ~caller_parent_path)
    | _ -> None

let class_constructed_by ~(lang : Lang.t) ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (callee : G.expr)
    : Names.Class_qn.t option =
  match callee.G.e with
  | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _)))
    when Option.equal String.equal (Lang_config.construction_method lang)
           (Some method_name) ->
    class_named_by ~lang ~func_lookup ~caller_parent_path receiver
  | _ -> None

let class_qn_of_resolution ~(func_lookup : Func_lookup.t) (name : G.name)
    : Names.Class_qn.t option =
  match !((id_info_of_name name).G.id_resolved) with
  | Some ((G.ImportedEntity parts | G.GlobalName (parts, _)), _) ->
    let class_qn = Names.Class_qn.of_parts parts in
    if Func_lookup.is_known_class func_lookup class_qn then Some class_qn
    else None
  | Some _
  | None -> None

let class_qn_in_module_of ~(func_lookup : Func_lookup.t)
    ~(owner : Names.Class_qn.t) (bare_name : string)
    : Names.Class_qn.t option =
  match Names.Class_qn.split_last owner with
  | None -> None
  | Some ((parent : Names.Class_qn.t), _) -> (
    match
      Func_lookup.module_attribute func_lookup
        (Names.Module_qn.of_string (Names.Class_qn.to_string parent)) bare_name
    with
    | Some (Func_lookup.Attr_class (class_qn : Names.Class_qn.t))
    | Some (Func_lookup.Attr_class_with_companion (class_qn, _)) -> Some class_qn
    | Some (Func_lookup.Attr_functions _)
    | Some (Func_lookup.Attr_module _)
    | None -> None)

let class_qn_of_type_name ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list)
    ~(owner : Names.Class_qn.t option) (name : G.name)
    : Names.Class_qn.t option =
  match class_qn_of_resolution ~func_lookup name with
  | Some _ as resolved -> resolved
  | None -> (
    match name with
    | G.IdQualified { G.name_middle = Some (G.QDots (_ :: _)); _ } ->
      Option.bind (dotted_chain_of_name name)
        (fun (chain : dotted_chain) ->
          match
            follow_chain ~func_lookup ~caller_parent_path
              ~position:Type_position chain
          with
          | Some (Bound_class (class_qn : Names.Class_qn.t)) -> Some class_qn
          | Some (Bound_module _)
          | Some (Bound_object _)
          | Some (Bound_functions _)
          | None -> None)
    | G.Id _
    | G.IdQualified _ ->
      Option.bind (Ty_bare_name.bare_name_of_name name)
        (fun (bare_name : string) ->
          match
            Option.bind owner (fun (owner : Names.Class_qn.t) ->
              class_qn_in_module_of ~func_lookup ~owner bare_name)
          with
          | Some _ as resolved -> resolved
          | None -> (
            match
              Option.bind (dotted_chain_of_name name)
                (constant_in_nesting ~func_lookup ~caller_parent_path)
            with
            | Some _ as resolved -> resolved
            | None -> class_in_scope ~func_lookup ~caller_parent_path bare_name)))

let declared_class_name_of_ty (ty : G.type_) : G.name option =
  match Ty_bare_name.dotted_class_name_of_ty ty with
  | Some _ as dotted -> dotted
  | None -> Ty_bare_name.qualified_class_name_of_ty ty

let return_type_class_qn ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (funcs : func_info list)
    : Names.Class_qn.t option =
  List.find_map
    (fun (f : func_info) ->
      Option.bind f.fdef.G.frettype (fun (ty : G.type_) ->
        Option.bind (declared_class_name_of_ty ty)
          (class_qn_of_type_name ~func_lookup ~caller_parent_path ~owner:None)))
    funcs

let rec receiver_class_qn ~(lang : Lang.t) ~(func_lookup : Func_lookup.t)
    ~(type_state : Type_state.t)
    ~(caller_parent_path : IL.name option list) (receiver : G.expr)
    : Names.Class_qn.t option =
  let of_receiver = receiver_class_qn ~lang ~func_lookup ~type_state
      ~caller_parent_path in
  match receiver.G.e with
  | G.IdSpecial ((G.This | G.Self | G.LateStatic), _) ->
    enclosing_class_qn ~func_lookup caller_parent_path
  | G.DeRef (_, (inner : G.expr)) -> of_receiver inner
  | G.N (G.Id ((name, _), id_info)) ->
    if Receiver.is_self_name lang name then
      enclosing_class_qn ~func_lookup caller_parent_path
    else
      Option.bind
        (Option.bind (Ty_bare_name.instance_or_declared_type id_info)
           declared_class_name_of_ty)
        (class_qn_of_type_name ~func_lookup ~caller_parent_path ~owner:None)
  | G.Call (callee, _)
    when Option.is_some
           (class_constructed_by ~lang ~func_lookup ~caller_parent_path callee)
    ->
    class_constructed_by ~lang ~func_lookup ~caller_parent_path callee
  | G.Call (callee, _) -> (
    match
      Option.bind (dotted_chain_of_expr callee)
        (follow_chain ~func_lookup ~caller_parent_path ~position:Term_position)
    with
    | Some (Bound_class (class_qn : Names.Class_qn.t)) -> Some class_qn
    | Some (Bound_functions (funcs : func_info list))
      when Option.is_some
             (return_type_class_qn ~func_lookup ~caller_parent_path funcs) ->
      return_type_class_qn ~func_lookup ~caller_parent_path funcs
    | Some (Bound_functions _)
    | Some (Bound_object _)
    | Some (Bound_module _)
    | None -> (
      match callee.G.e with
      | G.DotAccess (inner, _, G.FN (G.Id ((method_name, _), _))) ->
        Option.bind (of_receiver inner) (fun (owner : Names.Class_qn.t) ->
          Option.bind
            (Type_state.method_return type_state
               ~class_name:(Names.Class_qn.bare_name owner)
               ~method_name)
            (class_qn_of_type_name ~func_lookup ~caller_parent_path
               ~owner:(Some owner)))
      | _ -> None))
  | G.New (_, (ty : G.type_), _, _) ->
    Option.bind (declared_class_name_of_ty ty)
      (class_qn_of_type_name ~func_lookup ~caller_parent_path ~owner:None)
  | G.DotAccess (inner, _, G.FN (G.Id ((field_name, _), _))) ->
    Option.bind (of_receiver inner) (fun (owner : Names.Class_qn.t) ->
      Option.bind
        (Type_state.field_type_for_caller type_state
           ~class_name:(Names.Class_qn.bare_name owner) ~field_name
           ~caller_dir:None)
        (class_qn_of_type_name ~func_lookup ~caller_parent_path
               ~owner:(Some owner)))
  | _ -> None

let receiver_parameter_type (f : func_info) : G.type_ option =
  match Tok.unbracket f.fdef.G.fparams with
  | G.ParamReceiver { G.ptype = Some (ty : G.type_); _ } :: _ -> Some ty
  | _ -> None

let extension_functions ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list)
    ~(receiver_qn : Names.Class_qn.t) (method_name : string) : func_info list =
  (let nearest = entries_in_scope ~func_lookup ~caller_parent_path method_name in
   Func_lookup.extensions_of_entries nearest
   @ Func_lookup.functions_of_entries nearest)
  |> List.filter (fun (f : func_info) ->
       match
         Option.bind (receiver_parameter_type f)
           Ty_bare_name.qualified_class_name_of_ty
       with
       | None -> false
       | Some (type_name : G.name) -> (
         match
           class_qn_of_type_name ~func_lookup ~caller_parent_path ~owner:None
             type_name
         with
         | Some (declared : Names.Class_qn.t) ->
           Names.Class_qn.equal declared receiver_qn
         | None -> false))

let is_super_call (e : G.expr) : bool =
  match e.G.e with
  | G.Call ({ G.e = G.N (G.Id (("super", _), _)); _ }, _) -> true
  | G.IdSpecial ((G.Super | G.Parent), _) -> true
  | _ -> false

let identify_callee_interfile ~(lang : Lang.t)
    ~(type_state : Type_state.t)
    ?(func_lookup : Func_lookup.t = Func_lookup.empty)
    ?(caller_parent_path : IL.name option list = [])
    ?(call_arity : int option)
    ?(allow_constructor = Lang_config.constructs_by_bare_call lang)
    (callee : G.expr) : fn_id option =
  let pick (matches : func_info list) : fn_id option =
    pick_by_arity
      ~overload_groups:(Func_lookup.overload_groups func_lookup)
      ~lang call_arity matches
  in
  let along_order ~(receiver : Func_lookup.method_receiver)
      (class_qn : Names.Class_qn.t) (method_name : string) : fn_id option =
    pick
      (Func_lookup.find_along_order func_lookup ~receiver
         (Func_lookup.resolution_order func_lookup class_qn)
         (fun _ -> [ method_name ]))
  in
  let of_target (target : binding_target option) : fn_id option =
    match target with
    | Some (Bound_functions funcs) -> pick funcs
    | Some (Bound_class class_qn) ->
      if allow_constructor then
        pick (constructor_of_class ~lang ~func_lookup class_qn)
      else None
    | Some (Bound_object _)
    | Some (Bound_module _)
    | None -> None
  in
  let of_bare_name (id : string) : fn_id option =
    let nearest = entries_in_scope ~func_lookup ~caller_parent_path id in
    match Func_lookup.class_of_entries nearest with
    | Some (class_qn : Names.Class_qn.t) ->
      of_target (Some (Bound_class class_qn))
    | None -> pick (Func_lookup.functions_of_entries nearest)
  in
  match callee.G.e with
  | G.IdSpecial (G.Super, _) -> (
    match
      (enclosing_class_qn ~func_lookup caller_parent_path,
       List.rev caller_parent_path)
    with
    | Some (class_qn : Names.Class_qn.t), Some (method_il : IL.name) :: _ -> (
      match Func_lookup.resolution_order func_lookup class_qn with
      | []
      | [ _ ] -> None
      | _ :: after_self ->
        pick
          (Func_lookup.find_along_order func_lookup
             ~receiver:Func_lookup.On_any after_self
             (fun _ -> [ fst method_il.IL.ident ])))
    | _ -> None)
  | G.N (G.Id ((id, _), _id_info)) -> of_bare_name id
  | G.N (G.IdQualified
           { name_last = ((id, _), _typeargs); name_middle = None;
             name_top = None; _ }) -> of_bare_name id
  | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _)))
    when is_super_call receiver -> (
    match enclosing_class_qn ~func_lookup caller_parent_path with
    | None -> None
    | Some (class_qn : Names.Class_qn.t) -> (
      match Func_lookup.resolution_order func_lookup class_qn with
      | [] | [ _ ] -> None
      | _ :: after_self ->
        pick (Func_lookup.find_along_order func_lookup
                ~receiver:Func_lookup.On_any after_self
                (fun _ -> [ method_name ]))))
  | G.DotAccess
      (({ G.e = G.IdSpecial ((G.This | G.Self | G.LateStatic), _); _ }
       | { G.e =
             G.DeRef (_,
               { G.e = G.IdSpecial ((G.This | G.Self | G.LateStatic), _); _ });
           _ }),
       _, G.FN (G.Id ((method_name, _), _))) -> (
    match enclosing_class_qn ~func_lookup caller_parent_path with
    | None -> None
    | Some (class_qn : Names.Class_qn.t) ->
      along_order ~receiver:Func_lookup.On_any class_qn method_name)
  | G.DotAccess ({ G.e = G.N (G.Id ((receiver_name, _), _)); _ },
                 _, G.FN (G.Id ((method_name, _), _)))
    when Receiver.is_self_name lang receiver_name -> (
    match enclosing_class_qn ~func_lookup caller_parent_path with
    | None -> None
    | Some (class_qn : Names.Class_qn.t) ->
      along_order ~receiver:Func_lookup.On_any class_qn method_name)
  | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _))) -> (
    match
      class_constructed_by ~lang ~func_lookup ~caller_parent_path callee
    with
    | Some (class_qn : Names.Class_qn.t) ->
      pick (constructor_of_class ~lang ~func_lookup class_qn)
    | None -> (
    match
      of_target
        (Option.bind (dotted_chain_of_expr callee)
        (follow_chain ~func_lookup ~caller_parent_path
           ~position:Term_position))
    with
    | Some _ as resolved -> resolved
    | None -> (
      match
        receiver_class_qn ~lang ~func_lookup ~type_state ~caller_parent_path
          receiver
      with
      | None -> None
      | Some (class_qn : Names.Class_qn.t) -> (
        match along_order ~receiver:Func_lookup.On_instance class_qn method_name with
        | Some _ as resolved -> resolved
        | None ->
          pick
            (extension_functions ~func_lookup ~caller_parent_path
               ~receiver_qn:class_qn method_name)))))
  | G.N (G.IdQualified _) ->
    of_target
      (Option.bind (dotted_chain_of_expr callee)
         (follow_chain ~func_lookup ~caller_parent_path
            ~position:Term_position))
  | _ -> None
