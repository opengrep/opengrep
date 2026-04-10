open Common
module G = AST_generic
module Log = Log_call_graph.Log
(*  *open Shape_and_sig *)
module Reachable = Graph_reachability
module CanonicalMap = Map.Make (struct
  type t = string list

  let compare = Stdlib.compare
end)
module StringMap = Map.Make (String)

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

type project_file = {
  path : Target.path;
  ast : G.program;
  object_mappings : (G.name * G.name) list;
  funcs : func_info list;
}

type import_binding_state =
  | Imported
  | Shadowed

type import_lookup_scope =
  | FinalModuleState
  | TopLevelAtCallSite of Tok.t

(* Position-aware equality for fn_id paths. Compares function identifiers
   using both name AND source position (file, line, column) via Function_id.equal. *)
let equal_with_pos f1 f2 =
  let equal_il_name n1 n2 =
    Function_id.equal
      (Function_id.of_il_name n1)
      (Function_id.of_il_name n2)
  in
  List.equal (Option.equal equal_il_name) f1 f2

(* Get arity of a function from its definition *)
let get_func_arity (fdef : G.function_definition) : int =
  let params = fdef.fparams in
  List.length (Tok.unbracket params)

(* Graph node type - reuse from Call_graph for consistency *)
type node = Call_graph.node

(* Extract graph node from fn_id - takes the last element *)
let fn_id_to_node (fn_id : fn_id) : node option =
  match List.rev fn_id with
  | Some name :: _ -> Some (Function_id.of_il_name name)
  | _ -> None

(* Equality for fn_id using compare_fn_id *)
let equal_fn_id f1 f2 = Int.equal (compare_fn_id f1 f2) 0

let normalize_file = Fun.compose Fpath.to_string Fpath.normalize

let matches_current_file current_file (func : func_info) =
  match fn_id_to_node func.fn_id with
  | Some node ->
      let file, _, _ = Function_id.to_file_line_col node in
      String.equal file (normalize_file current_file)
  | None -> false

let rec suffixes = function
  | [] -> []
  | (_ :: xs as segments) -> segments :: suffixes xs

let full_module_path_of_path (path : Fpath.t) =
  match List.rev (Fpath.segs (Fpath.normalize path)) with
  | [] -> []
  | base :: rev_dirs ->
      let dirs = List.rev rev_dirs in
      let _, base_name, _ = Filename_.dbe_of_filename_noext_ok base in
      match base_name with
      | "__init__"
      | "index" -> dirs
      | _ -> dirs @ [ base_name ]

let module_candidates_of_path (path : Fpath.t) =
  full_module_path_of_path path |> suffixes
  |> List.filter (fun segments -> segments <> [])
  |> List.sort_uniq Stdlib.compare

let is_relative_segment segment =
  (not (String.equal segment ""))
  && String.for_all (fun c -> Char.equal c '.') segment

let split_relative_canonical canonical =
  let rec aux depth = function
    | segment :: rest
      when String.equal segment "" || is_relative_segment segment ->
        let depth_incr = Int.max 1 (String.length segment) in
        aux (depth + depth_incr) rest
    | rest -> (depth, rest)
  in
  aux 0 canonical

let take n xs =
  let rec aux n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> aux (n - 1) (x :: acc) rest
  in
  aux n [] xs

let drop_last_n n xs =
  take (Int.max 0 (List.length xs - n)) xs

let dedup_canonicals canonicals =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | canonical :: rest ->
        if List.mem canonical seen then aux seen acc rest
        else aux (canonical :: seen) (canonical :: acc) rest
  in
  aux [] [] canonicals

let canonical_lookup_candidates ?(current_file : Fpath.t option) canonical =
  let relative_depth, target_segments = split_relative_canonical canonical in
  if Int.equal relative_depth 0 then
    match current_file with
    | None -> [ canonical ]
    | Some file ->
        let current_modules = module_candidates_of_path file in
        current_modules
        |> List_.map (fun current_module -> current_module @ canonical)
        |> fun qualified -> dedup_canonicals (canonical :: qualified)
  else
    let fallback = [ target_segments ] in
    match current_file with
    | None -> fallback
    | Some file ->
        let levels_up = Int.max 0 (relative_depth - 1) in
        let current_modules =
          module_candidates_of_path file
          |> List.sort (fun xs ys ->
                 match Int.compare (List.length ys) (List.length xs) with
                 | 0 -> Stdlib.compare xs ys
                 | order -> order)
        in
        current_modules
        |> List_.filter_map (fun current_module ->
               match List.rev current_module with
               | [] -> None
               | _current_name :: rev_package ->
                   let package = List.rev rev_package in
                   Some (drop_last_n levels_up package @ target_segments))
        |> fun resolved -> dedup_canonicals (resolved @ fallback)

let lookup_imported_entity ?current_file imported_entity_index canonical =
  canonical_lookup_candidates ?current_file canonical
  |> List.find_map (fun candidate ->
         match CanonicalMap.find_opt candidate imported_entity_index with
         | Some [ func ] -> Some func.fn_id
         | Some _
         | None ->
             None)

let lookup_imported_func ?current_file imported_entity_index canonical =
  canonical_lookup_candidates ?current_file canonical
  |> List.find_map (fun candidate ->
         match CanonicalMap.find_opt candidate imported_entity_index with
         | Some [ func ] -> Some func
         | Some _
         | None ->
             None)

let add_imported_entity imported_entity_index canonical func =
  CanonicalMap.update canonical
    (function
      | Some funcs -> Some (func :: funcs)
      | None -> Some [ func ])
    imported_entity_index

let class_name_of_constructor_fn_id ~(lang : Lang.t) (fn_id : fn_id) :
    G.name option =
  match fn_id with
  | [ Some cls; Some meth ] ->
      let class_name = fst cls.IL.ident in
      let method_name = fst meth.IL.ident in
      if Object_initialization.is_constructor lang method_name (Some class_name)
      then Some (G.Id ((class_name, snd cls.IL.ident), G.empty_id_info ()))
      else None
  | _ -> None

let imported_class_name ~(lang : Lang.t) ~imported_entity_index
    ~(canonical : string list) ~(default_tok : Tok.t) ?current_file :
    unit -> G.name option =
 fun () ->
  let rec has_prefix prefix segments =
    match (prefix, segments) with
    | [], _ -> true
    | _, [] -> false
    | p :: prefix_rest, s :: segments_rest ->
        String.equal p s && has_prefix prefix_rest segments_rest
  in
  match
    Option.bind
      (lookup_imported_entity ?current_file imported_entity_index canonical)
      (class_name_of_constructor_fn_id ~lang)
  with
  | Some _ as result -> result
  | None ->
      let candidate_matches =
        canonical_lookup_candidates ?current_file canonical
        |> List.exists (fun candidate ->
               CanonicalMap.exists
                 (fun indexed_canonical _funcs ->
                   has_prefix candidate indexed_canonical
                   && List.length indexed_canonical > List.length candidate)
                 imported_entity_index)
      in
      if candidate_matches then
        match List_.last_opt canonical with
        | Some class_name ->
            Some (G.Id ((class_name, default_tok), G.empty_id_info ()))
        | None -> None
      else None

let imported_constructor_class_name ~(lang : Lang.t) ~imported_entity_index
    (expr : G.expr) ?current_file : unit -> G.name option =
 fun () ->
  let default_tok =
    match AST_generic_helpers.ii_of_any (G.E expr) with
    | tok :: _ -> tok
    | [] -> Tok.unsafe_fake_tok "<imported-class>"
  in
  match expr.G.e with
  | G.Call (callee, _) -> (
      match callee.G.e with
      | G.N (G.Id (_, id_info)) -> (
          match !(id_info.G.id_resolved) with
          | Some (G.ImportedEntity canonical, _sid) ->
              imported_class_name ~lang ~imported_entity_index ~canonical
                ~default_tok ?current_file ()
          | _ -> None)
      | G.N (G.IdQualified ({ name_last = _; name_info; _ } as qualified_info)) -> (
          match !(name_info.G.id_resolved) with
          | Some (G.ImportedEntity canonical, _sid) ->
              imported_class_name ~lang ~imported_entity_index ~canonical
                ~default_tok ?current_file ()
          | _ ->
              let canonical =
                AST_generic_helpers.dotted_ident_of_name
                  (G.IdQualified qualified_info)
                |> List_.map fst
              in
              imported_class_name ~lang ~imported_entity_index ~canonical
                ~default_tok ?current_file ())
      | G.DotAccess ({ e = G.N (G.Id ((obj_name, _), obj_info)); _ }, _, G.FN (G.Id ((id, _), _))) -> (
          match !(obj_info.G.id_resolved) with
          | Some (G.ImportedModule canonical_module, _sid) ->
              imported_class_name ~lang ~imported_entity_index
                ~canonical:(canonical_module @ [ id ])
                ~default_tok ?current_file ()
          | Some (G.ImportedEntity canonical_entity, _sid) ->
              imported_class_name ~lang ~imported_entity_index
                ~canonical:(canonical_entity @ [ id ])
                ~default_tok ?current_file ()
          | _ ->
              imported_class_name ~lang ~imported_entity_index
                ~canonical:[ obj_name; id ] ~default_tok ?current_file ())
      | _ -> None)
  | _ -> None

let detect_imported_object_initialization ~(lang : Lang.t) ~imported_entity_index
    (ast : G.program) ?current_file : unit -> (G.name * G.name) list =
 fun () ->
  let object_mappings = ref [] in
  let add_mapping var_name init_expr =
    match
      imported_constructor_class_name ~lang ~imported_entity_index init_expr
        ?current_file ()
    with
    | Some cls -> object_mappings := (var_name, cls) :: !object_mappings
    | None -> ()
  in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_stmt () stmt =
        (match stmt.G.s with
        | G.ExprStmt (expr, _) -> (
            match expr.G.e with
            | G.Assign (lval_expr, _, rval_expr)
            | G.AssignOp (lval_expr, _, rval_expr) -> (
                match lval_expr.G.e with
                | G.N var_name -> add_mapping var_name rval_expr
                | _ -> ())
            | _ -> ())
        | _ -> ());
        super#visit_stmt () stmt

      method! visit_definition () def =
        (match def with
        | entity, G.VarDef var_def -> (
            match (entity.G.name, var_def.G.vinit) with
            | G.EN var_name, Some init_expr -> add_mapping var_name init_expr
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in
  visitor#visit_program () ast;
  !object_mappings

let starts_with_segments ~prefix segments =
  let rec aux prefix segments =
    match (prefix, segments) with
    | [], _ -> true
    | _, [] -> false
    | p :: prefix_rest, s :: segments_rest ->
        String.equal p s && aux prefix_rest segments_rest
  in
  aux prefix segments

let stmt_bytepos (stmt : G.stmt) =
  AST_generic_helpers.ii_of_any (G.S stmt)
  |> List.find_opt (fun tok -> not (Tok.is_fake tok))
  |> Option.map Tok.bytepos_of_tok

let simple_entity_name (ent : G.entity) =
  match ent.G.name with
  | G.EN (G.Id ((name, _), _)) -> Some name
  | _ -> None

let imported_local_names (directive : G.directive) =
  match directive.G.d with
  | G.ImportFrom (_, _, imported_names) ->
      imported_names
      |> List_.map (function
           | _id, Some (alias, _id_info) -> fst alias
           | id, None -> fst id)
  | G.ImportAs (_, G.DottedName (head :: _), alias_opt) -> (
      match alias_opt with
      | Some (alias, _id_info) -> [ fst alias ]
      | None -> [ fst head ])
  | _ -> []

let add_import_binding_event acc name pos state =
  StringMap.update name
    (function
      | Some events -> Some ((pos, state) :: events)
      | None -> Some [ (pos, state) ])
    acc

let add_import_binding_events acc (stmt : G.stmt) =
  match stmt_bytepos stmt with
  | None -> acc
  | Some pos -> (
      match stmt.G.s with
      | G.DirectiveStmt directive ->
          imported_local_names directive
          |> List.fold_left
               (fun acc name -> add_import_binding_event acc name pos Imported)
               acc
      | G.DefStmt (ent, (G.FuncDef _ | G.ClassDef _ | G.VarDef _)) -> (
          match simple_entity_name ent with
          | Some name -> add_import_binding_event acc name pos Shadowed
          | None -> acc)
      | G.ExprStmt
          ({ e = G.Assign ({ e = G.N (G.Id ((name, _), _)); _ }, _, _); _ }, _) ->
          add_import_binding_event acc name pos Shadowed
      | _ -> acc)

let finalize_import_binding_timeline timeline =
  timeline |> StringMap.map List.rev

let build_import_binding_timeline (ast : G.program) =
  ast |> List.fold_left add_import_binding_events StringMap.empty
  |> finalize_import_binding_timeline

let build_local_import_binding_timeline (fdef : G.function_definition) =
  let timeline = ref StringMap.empty in
  let body_stmt = AST_generic_helpers.funcbody_to_stmt fdef.G.fbody in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_stmt env stmt =
        timeline := add_import_binding_events !timeline stmt;
        match stmt.G.s with
        | G.DefStmt (_, (G.FuncDef _ | G.ClassDef _)) -> ()
        | _ -> super#visit_stmt env stmt
    end
  in
  visitor#visit_stmt () body_stmt;
  finalize_import_binding_timeline !timeline

let latest_import_binding_state_before events pos =
  let rec aux latest = function
    | [] -> latest
    | (event_pos, state) :: rest ->
        if event_pos > pos then latest else aux (Some state) rest
  in
  aux None events

let is_active_import_binding import_binding_timeline ~name ~scope =
  match StringMap.find_opt name import_binding_timeline with
  | None -> true
  | Some [] -> true
  | Some events -> (
      match scope with
      | FinalModuleState -> (
          match List_.last_opt events with
          | Some (_, Imported) -> true
          | Some (_, Shadowed) -> false
          | None -> true)
      | TopLevelAtCallSite tok ->
          if Tok.is_fake tok then true
          else
            match latest_import_binding_state_before events (Tok.bytepos_of_tok tok) with
            | Some Imported -> true
            | Some Shadowed -> false
            | None -> true)

let local_import_binding_state_before local_import_binding_timeline ~name tok =
  match StringMap.find_opt name local_import_binding_timeline with
  | None
  | Some [] ->
      None
  | Some events ->
      if Tok.is_fake tok then None
      else
        latest_import_binding_state_before events (Tok.bytepos_of_tok tok)

let is_active_import_binding_for_call import_binding_timeline
    local_import_binding_timeline ~name ~scope ~call_tok =
  match
    local_import_binding_state_before local_import_binding_timeline ~name
      call_tok
  with
  | Some Imported -> true
  | Some Shadowed -> false
  | None ->
      is_active_import_binding import_binding_timeline ~name ~scope

let dotted_name_segments_of_expr (expr : G.expr) =
  AST_generic_helpers.name_of_dot_access expr
  |> Option.map AST_generic_helpers.dotted_ident_of_name
  |> Option.map (List_.map fst)

let rec callsite_tok_of_callee_expr (expr : G.expr) : Tok.t option =
  match expr.G.e with
  | G.N (G.Id ((_name, tok), _id_info)) when not (Tok.is_fake tok) -> Some tok
  | G.N (G.IdQualified { name_last = (_, _typeargsTODO); name_info; _ }) -> (
      match AST_generic_helpers.ii_of_any (G.E expr) with
      | tok :: _ when not (Tok.is_fake tok) -> Some tok
      | _ -> (
          match !(name_info.G.id_resolved) with
          | _ -> None))
  | G.DotAccess (_obj, _, G.FN (G.Id ((_name, tok), _id_info)))
    when not (Tok.is_fake tok) ->
      Some tok
  | G.Call (callee, _) -> callsite_tok_of_callee_expr callee
  | _ -> (
      match AST_generic_helpers.ii_of_any (G.E expr) with
      | tok :: _ when not (Tok.is_fake tok) -> Some tok
      | _ -> None)

(* Extract Go receiver type from method *)
let extract_go_receiver_type (fdef : G.function_definition) : string option =
  let params = Tok.unbracket fdef.fparams in
  match params with
  (* Non-pointer receiver: func (r Type) ... *)
  | G.ParamReceiver { ptype = Some { t = G.TyN (G.Id ((name, _), _)); _ }; _ }
    :: _ ->
      Some name
  (* Pointer receiver: func (r *Type) ... *)
  | G.ParamReceiver
      {
        ptype =
          Some
            { t = G.TyPointer (_, { t = G.TyN (G.Id ((name, _), _)); _ }); _ };
        _;
      }
    :: _ ->
      Some name
  | _ -> None

(* Build fn_id from entity, or generate _tmp name for anonymous functions *)
let fn_id_of_entity ~(lang : Lang.t) (opt_ent : G.entity option)
    (parent_path : IL.name option list) (fdef : G.function_definition) : fn_id option =
  (* Ensure parent_path starts with [None] for top-level functions *)
  let normalized_parent_path =
    match parent_path with
    | [] -> [None]  (* Top-level: empty path becomes [None] *)
    | path -> path
  in
  match opt_ent with
  | Some ent -> (
      match AST_to_IL.name_of_entity ent with
      | Some name ->
          (* For Go methods, extract receiver type as class name *)
          let go_receiver_il =
            match lang with
            | Lang.Go -> (
                match extract_go_receiver_type fdef with
                | Some recv_name ->
                    let fake_tok = Tok.unsafe_fake_tok recv_name in
                    Some
                      IL.
                        {
                          ident = (recv_name, fake_tok);
                          sid = AST_generic.SId.unsafe_default;
                          id_info = AST_generic.empty_id_info ();
                        }
                | None -> None)
            | _ -> None
          in
          (* If we have a Go receiver and parent_path is [None], replace with receiver *)
          let adjusted_parent_path =
            match (go_receiver_il, normalized_parent_path) with
            | Some recv, [None] -> [Some recv]
            | Some recv, None :: rest -> Some recv :: rest
            | _, path -> path
          in
          Some (adjusted_parent_path @ [Some name])
      | None -> None)
  | None ->
      (* Anonymous function - use _tmp with fake token to match AST_to_IL behavior.
         AST_to_IL.fresh_var creates fake tokens for _tmp variables. *)
      let tok = match fdef.fkind with (_, tok) -> tok in
      let fake_tok = Tok.fake_tok tok "_tmp" in
      let tmp_name = IL.{
        ident = ("_tmp", fake_tok);
        sid = G.SId.unsafe_default;
        id_info = G.empty_id_info ();
      } in
      Some (normalized_parent_path @ [Some tmp_name])

let anonymous_tmp_name_of_fdef (fdef : G.function_definition) : IL.name =
  let tok = match fdef.fkind with (_, tok) -> tok in
  let fake_tok = Tok.fake_tok tok "_tmp" in
  IL.
    {
      ident = ("_tmp", fake_tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    }

let anonymous_fn_id_of_fdef (parent_path : IL.name option list)
    (fdef : G.function_definition) : fn_id =
  let normalized_parent_path =
    match parent_path with
    | [] -> [None]
    | path -> path
  in
  normalized_parent_path @ [Some (anonymous_tmp_name_of_fdef fdef)]

let dedup_fn_ids (ids : (fn_id * Tok.t) list) : (fn_id * Tok.t) list =
  ids |>
  List.sort_uniq (fun (f1, t1) (f2, t2) ->
    let cmp = compare_fn_id f1 f2 in
    if cmp <> 0 then cmp else Tok.compare t1 t2)

(* Helper function to identify the callee fn_id from a call expression's callee *)
let identify_callee ?(object_mappings = []) ?(all_funcs = [])
    ?(import_binding_timeline = StringMap.empty)
    ?(local_import_binding_timeline = StringMap.empty)
    ?(imported_entity_index = CanonicalMap.empty) ?(current_file : Fpath.t option)
    ?(caller_parent_path = []) ?(call_arity : int option)
    ?call_tok ?(import_lookup_scope = FinalModuleState) (callee : G.expr) :
    fn_id option =
  (* Extract class from caller_parent_path if present *)
  let current_class = match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  let is_import_binding_active name =
    match call_tok with
    | Some tok ->
        is_active_import_binding_for_call import_binding_timeline
          local_import_binding_timeline ~name ~scope:import_lookup_scope
          ~call_tok:tok
    | None ->
        is_active_import_binding import_binding_timeline ~name
          ~scope:import_lookup_scope
  in
  let is_local_function func =
    match current_file with
    | Some file -> matches_current_file file func
    | None -> true
  in
  let resolve_local_function_call callee_name_str =
    let nested_match =
      List.find_opt
        (fun f ->
          match List_.init_and_last_opt f.fn_id with
          | Some (f_parent, Some name)
            when String.equal (fst name.IL.ident) callee_name_str ->
              equal_with_pos f_parent caller_parent_path
          | _ -> false)
        all_funcs
    in
    match nested_match with
    | Some f ->
        Log.debug (fun m ->
            m "CALL_EXTRACT: Found nested function %s in same scope"
              callee_name_str);
        Some f.fn_id
    | None -> (
        match current_class with
        | Some class_name ->
            let class_name_str = fst class_name.IL.ident in
            let method_match =
              List.find_opt
                (fun f ->
                  is_local_function f
                  &&
                  match f.fn_id with
                  | [ Some c; Some m ] ->
                      fst c.IL.ident = class_name_str
                      && fst m.IL.ident = callee_name_str
                  | _ -> false)
                all_funcs
            in
            let all_names =
              all_funcs |> List.map (fun f -> show_fn_id f.fn_id)
              |> String.concat ", "
            in
            Log.debug (fun m ->
                m
                  "CALL_EXTRACT: In class %s, call to %s, checking %d funcs, \
                   method_exists=%b, ALL: [%s]"
                  class_name_str callee_name_str (List.length all_funcs)
                  (Option.is_some method_match) all_names);
            (match method_match with
            | Some f -> Some f.fn_id
            | None ->
                let free_fn_match =
                  List.find_opt
                    (fun f ->
                      is_local_function f
                      &&
                      match f.fn_id with
                      | [ None; Some name ] ->
                          fst name.IL.ident = callee_name_str
                      | _ -> false)
                    all_funcs
                in
                Option.map (fun f -> f.fn_id) free_fn_match)
        | None ->
            let free_fn_match =
              List.find_opt
                (fun f ->
                  is_local_function f
                  &&
                  match f.fn_id with
                  | [ None; Some name ] ->
                      fst name.IL.ident = callee_name_str
                  | _ -> false)
                all_funcs
            in
            Option.map (fun f -> f.fn_id) free_fn_match)
  in
  let resolve_method_call_in_class class_name_str method_name_str =
    let method_matches =
      List.filter
        (fun f ->
          is_local_function f
          &&
          match f.fn_id with
          | [ Some c; Some m ] ->
              String.equal (fst c.IL.ident) class_name_str
              && String.equal (fst m.IL.ident) method_name_str
          | _ -> false)
        all_funcs
    in
    match method_matches with
    | [ single_match ] -> Some single_match.fn_id
    | [] -> None
    | _ -> (
        match call_arity with
        | Some arity ->
            let arity_matches =
              List.filter
                (fun f -> Int.equal (get_func_arity f.fdef) arity)
                method_matches
            in
            (match arity_matches with
            | [ single_match ] -> Some single_match.fn_id
            | _ -> None)
        | None -> None)
  in
  match callee.G.e with
    (* Simple function call: foo() *)
    | G.N (G.Id ((id, _), id_info)) -> (
        match resolve_local_function_call id with
        | Some _ as result -> result
        | None -> (
            match !(id_info.G.id_resolved) with
            | Some (G.ImportedEntity canonical, _sid) when is_import_binding_active id ->
                lookup_imported_entity ?current_file imported_entity_index
                  canonical
            | Some (G.ImportedModule _, _sid) -> None
            | None when is_import_binding_active id ->
                lookup_imported_entity ?current_file imported_entity_index
                  [ id ]
            | Some _
            | None ->
                None))
        (* Qualified call: Module.foo() *)
        | G.N
            (G.IdQualified
              ({ name_last = (id, _typeargsTODO); name_info; _ } as qualified_info))
          -> (
            match !(name_info.G.id_resolved) with
            | Some (G.ImportedEntity canonical, _sid) ->
                lookup_imported_entity ?current_file imported_entity_index canonical
            | Some _
            | None ->
                let qualified_name =
                  AST_generic_helpers.dotted_ident_of_name
                    (G.IdQualified qualified_info)
                in
                let canonical = qualified_name |> List_.map fst in
                match lookup_imported_entity ?current_file imported_entity_index canonical with
                | Some _ as result -> result
                | None ->
                    let callee_name_str = fst id in
                    (* Use string matching to find the qualified function *)
                    let qualified_match =
                      List.find_opt
                        (fun f ->
                          is_local_function f
                          &&
                          match f.fn_id with
                          | [ None; Some name ] ->
                              fst name.IL.ident = callee_name_str
                          | _ -> false)
                        all_funcs
                    in
                    Option.map (fun f -> f.fn_id) qualified_match)
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
                  is_local_function f &&
                  match f.fn_id with
                  | [Some c; Some m] when fst c.IL.ident = class_name_str && fst m.IL.ident = method_name_str -> true
                  | _ -> false
                ) all_funcs in
                (match method_matches with
                | [single_match] -> Some single_match.fn_id  (* Exactly one match by name *)
                | [] -> None
                | _ ->
                    (* Multiple matches - filter by arity if available *)
                    (match call_arity with
                    | Some arity ->
                        let arity_matches = List.filter (fun f ->
                          Int.equal (get_func_arity f.fdef) arity
                        ) method_matches in
                        (match arity_matches with
                        | [single_match] -> Some single_match.fn_id
                        | _ -> None)  (* Still 0 or multiple matches *)
                    | None -> None))  (* No arity info, can't disambiguate *)
            | None -> None)
        | G.DotAccess
            ({ e = G.DotAccess _; _ } as obj, _, G.FN (G.Id ((id, _), _id_info)))
          -> (
            match dotted_name_segments_of_expr obj with
            | Some canonical_module ->
                lookup_imported_entity ?current_file imported_entity_index
                  (canonical_module @ [ id ])
            | None -> None)
        | G.DotAccess
            ( { e = G.N (G.Id ((_obj_name, _), obj_info)); _ },
              _,
              G.FN (G.Id ((id, _), _id_info)) ) -> (
            match !(obj_info.G.id_resolved) with
            | Some (G.ImportedModule canonical_module, _sid)
              when is_import_binding_active _obj_name ->
                lookup_imported_entity ?current_file imported_entity_index
                  (canonical_module @ [ id ])
            | Some (G.ImportedEntity canonical_entity, _sid)
              when is_import_binding_active _obj_name ->
                lookup_imported_entity ?current_file imported_entity_index
                  (canonical_entity @ [ id ])
            | Some _
            | None ->
                let method_name_str = id in
                (* First try: treat obj as a module name and look up in
                   imported entities. This handles bare `import runner` in
                   Python where the naming pass does not set ImportedModule. *)
                let module_lookup =
                  if is_import_binding_active _obj_name then
                    lookup_imported_entity ?current_file imported_entity_index
                      [ _obj_name; method_name_str ]
                  else None
                in
                (match module_lookup with
                | Some _ as result -> result
                | None ->
                (* Fall back: look up obj's class in object_mappings *)
                let obj_class_opt =
                  object_mappings
                  |> List.find_opt (fun (var_name, _class_name) ->
                         match var_name with
                         | G.Id ((var_str, _), _) -> var_str = _obj_name
                         | _ -> false)
                  |> Option.map (fun (_var_name, class_name) -> class_name)
                in
                (match obj_class_opt with
                | Some class_name ->
                    let class_name_str = match class_name with
                      | G.Id ((str, _), _) -> str
                      | _ -> ""
                    in
                    let imported_method =
                      lookup_imported_entity ?current_file imported_entity_index
                        [ class_name_str; method_name_str ]
                    in
                    (* Find all methods matching class and name *)
                    let method_matches = List.filter (fun f ->
                      is_local_function f &&
                      match f.fn_id with
                      | [Some c; Some m] when fst c.IL.ident = class_name_str && fst m.IL.ident = method_name_str -> true
                      | _ -> false
                    ) all_funcs in
                    (match imported_method with
                    | Some _ as result -> result
                    | None ->
                    match method_matches with
                    | [single_match] -> Some single_match.fn_id  (* Exactly one match by name *)
                    | [] -> None
                    | _ ->
                        (* Multiple matches - filter by arity if available *)
                        (match call_arity with
                        | Some arity ->
                            let arity_matches = List.filter (fun f ->
                              Int.equal (get_func_arity f.fdef) arity
                            ) method_matches in
                            (match arity_matches with
                            | [single_match] -> Some single_match.fn_id
                            | _ -> None)  (* Still 0 or multiple matches *)
                        | None -> None))  (* No arity info, can't disambiguate *)
                | None -> None)))
        | G.DotAccess
            ({ e = G.Call (constructor_callee, _); _ }, _,
             G.FN (G.Id ((id, _), _id_info))) -> (
            let method_name_str = id in
            let imported_method =
              let lookup canonical =
                lookup_imported_entity ?current_file imported_entity_index
                  (canonical @ [ method_name_str ])
              in
              match constructor_callee.G.e with
              | G.N (G.Id ((_class_name, _), class_info)) -> (
                  match !(class_info.G.id_resolved) with
                  | Some (G.ImportedEntity canonical_entity, _sid)
                  | Some (G.ImportedModule canonical_entity, _sid) ->
                      lookup canonical_entity
                  | Some _
                  | None ->
                      None)
              | G.N
                  (G.IdQualified
                    ({ name_last = _; name_info; _ } as qualified_info)) -> (
                  match !(name_info.G.id_resolved) with
                  | Some (G.ImportedEntity canonical_entity, _sid)
                  | Some (G.ImportedModule canonical_entity, _sid) ->
                      lookup canonical_entity
                  | Some _
                  | None ->
                      let canonical =
                        AST_generic_helpers.dotted_ident_of_name
                          (G.IdQualified qualified_info)
                        |> List_.map fst
                      in
                      lookup canonical)
              | G.DotAccess _ -> (
                  match dotted_name_segments_of_expr constructor_callee with
                  | Some canonical -> lookup canonical
                  | None -> None)
              | _ -> None
            in
            match imported_method with
            | Some _ as result -> result
            | None ->
                let class_name_str_opt =
                  match constructor_callee.G.e with
                  | G.N (G.Id ((class_name, _), _)) -> Some class_name
                  | _ ->
                      (match dotted_name_segments_of_expr constructor_callee with
                      | Some canonical -> List_.last_opt canonical
                      | None -> None)
                in
                Option.bind class_name_str_opt (fun class_name_str ->
                    resolve_method_call_in_class class_name_str method_name_str))
        (* Method call: obj.method() - look up obj's class *)
        | _ ->
            Log.debug (fun m ->
                m "CALL_EXTRACT: Unmatched call pattern: %s"
                  (G.show_expr callee));
            None

(* Extract all calls from a function body and resolve them to fn_ids *)
let extract_calls ?(object_mappings = []) ?(all_funcs = []) ?(caller_parent_path = [])
    ?(import_binding_timeline = StringMap.empty)
    ?(imported_entity_index = CanonicalMap.empty) ?(current_file : Fpath.t option)
    (fdef : G.function_definition) : (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for function");
  let calls = ref [] in
  let local_import_binding_timeline =
    build_local_import_binding_timeline fdef
  in
  (* Check if an argument is an unresolved Id that could be a function call.
   * In Ruby, `foo(bar)` where `bar` is a method is actually `foo(bar())`.
   * If id_resolved is None and we can identify it as a function, add it as a call. *)
  let check_arg_for_unresolved_function_call arg =
    match arg with
    | G.Arg arg_exp ->
        (match arg_exp.G.e with
        | G.N (G.Id ((_, tok), id_info)) ->
            (* Check if this Id is unresolved *)
            (match !(id_info.G.id_resolved) with
            | None ->
                (* Unresolved - try to identify it as a function *)
                (match
                   identify_callee ~object_mappings ~all_funcs
                     ~import_binding_timeline
                     ~local_import_binding_timeline
                     ~imported_entity_index ?current_file ~caller_parent_path
                     ~call_tok:tok
                     arg_exp
                 with
                | Some fn_id ->
                    Log.debug (fun m -> m "CALL_EXTRACT: Found unresolved Id that is a function, adding as implicit call");
                    calls := (fn_id, tok) :: !calls
                | None -> ())
            | Some _ -> ())
        | _ -> ())
    | _ -> ()
  in
  let v =
    object (self)
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        | G.Call (callee, args) ->
            let (_, args_list, _) = args in
            let call_arity = List.length args_list in
            let tok =
              match callsite_tok_of_callee_expr callee with
              | Some tok -> tok
              | None -> (
                  match AST_generic_helpers.ii_of_any (G.E e) with
                  | tok :: _ -> tok
                  | [] -> Tok.unsafe_fake_tok "")
            in
            (match
               identify_callee ~object_mappings ~all_funcs
                 ~import_binding_timeline
                 ~local_import_binding_timeline
                 ~imported_entity_index ?current_file ~caller_parent_path
                 ~call_arity ~call_tok:tok callee
             with
            | Some fn_id ->
                calls := (fn_id, tok) :: !calls
            | None -> ());
            (* Check arguments for unresolved function calls (Ruby-style) *)
            List.iter check_arg_for_unresolved_function_call args_list;
            (* Visit callee expression for nested calls (e.g., Ruby's File.open(path_for(x)) do ... end
               where the callee is itself a Call containing path_for(x) in its args) *)
            self#visit_expr env callee;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id and tok *)
  !calls |> dedup_fn_ids

(* Extract calls from top-level statements (outside any function).
   This returns a list of (callee_fn_id, call_tok) pairs. *)
let extract_toplevel_calls ?(object_mappings = []) ?(all_funcs = [])
    ?(import_binding_timeline = StringMap.empty)
    ?(imported_entity_index = CanonicalMap.empty) ?(current_file : Fpath.t option)
    (ast : G.program) : (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for top-level statements");
  let calls = ref [] in

  (* Build a set of byte ranges covered by function bodies *)
  let func_ranges = ref [] in
  let funcs_in_current_file =
    match current_file with
    | Some file ->
        all_funcs |> List.filter (matches_current_file file)
    | None -> all_funcs
  in
  List.iter
    (fun func ->
      let body_stmt = AST_generic_helpers.funcbody_to_stmt func.fdef.G.fbody in
      match AST_generic_helpers.range_of_any_opt (G.S body_stmt) with
      | Some (loc_start, loc_end) ->
          let range = Range.range_of_token_locations loc_start loc_end in
          func_ranges := (range.start, range.end_) :: !func_ranges
      | None -> ())
    funcs_in_current_file;

  (* Check if a position is inside any function body *)
  let is_inside_function pos =
    List.exists (fun (start, stop) -> pos >= start && pos <= stop) !func_ranges
  in

  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        | G.Call (callee, args) ->
            (* Check if this call is at top-level (not inside a function) *)
            let call_pos =
              match AST_generic_helpers.ii_of_any (G.E e) with
              | tok :: _ when not (Tok.is_fake tok) -> Tok.bytepos_of_tok tok
              | _ -> -1
            in
            if call_pos >= 0 && not (is_inside_function call_pos) then (
              (* Top-level call - no class context *)
              let tok =
                match callsite_tok_of_callee_expr callee with
                | Some tok -> tok
                | None -> (
                    match AST_generic_helpers.ii_of_any (G.E e) with
                    | tok :: _ -> tok
                    | [] -> Tok.unsafe_fake_tok "")
              in
              match
                identify_callee ~object_mappings ~all_funcs
                  ~import_binding_timeline
                  ~imported_entity_index ?current_file ~caller_parent_path:[]
                  ~call_tok:tok ~import_lookup_scope:(TopLevelAtCallSite tok)
                  callee
              with
              | Some fn_id ->
                  Log.debug (fun m -> m "CALL_EXTRACT: Found top-level call to %s" (show_fn_id fn_id));
                  calls := (fn_id, tok) :: !calls
              | None -> ()
            );
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_program () ast;
  !calls |> dedup_fn_ids

(* Helper to extract callback name from an argument expression.
   Handles: foo, &foo, Module.foo, module.func (DotAccess), Elixir &func/n
   Returns: (callback_name, tok, shortlambda_tmp_opt)
   - shortlambda_tmp_opt is Some IL.name for the _tmp wrapper node when this is an Elixir ShortLambda *)
let extract_callback_from_arg (arg_expr : G.expr) : (IL.name * Tok.t * IL.name option) option =
  match arg_expr.G.e with
  (* Plain identifier: foo *)
  | G.N (G.Id (id, id_info)) ->
      let callback_name = AST_to_IL.var_of_id_info id id_info in
      Some (callback_name, snd id, None)
  (* Address-of operator: &foo (C/C++ function pointers) *)
  | G.Ref (_, { e = G.N (G.Id (id, id_info)); _ }) ->
      let callback_name = AST_to_IL.var_of_id_info id id_info in
      Some (callback_name, snd id, None)
  (* Qualified identifier: Module.foo *)
  | G.N (G.IdQualified { name_last = id, _; name_info; _ }) ->
      let callback_name = AST_to_IL.var_of_id_info id name_info in
      Some (callback_name, snd id, None)
  (* DotAccess: module.func or obj.method - common in Python/JS *)
  | G.DotAccess (_, _, G.FN (G.Id (id, id_info))) ->
      let callback_name = AST_to_IL.var_of_id_info id id_info in
      Some (callback_name, snd id, None)
  (* Elixir: &func/n - ShortLambda wrapping a call to the named function.
     Structure: OtherExpr("ShortLambda", [Params[&1,...]; S(ExprStmt(Call(func, args)))])
     Create a _tmp node to match what AST_to_IL creates for the anonymous wrapper. *)
  | G.OtherExpr (("ShortLambda", shortlambda_tok),
                 [G.Params _; G.S { G.s = G.ExprStmt (inner_e, _); _ }]) ->
      (match inner_e.G.e with
      | G.Call ({ e = G.N (G.Id (id, id_info)); _ }, _) ->
          let callback_name = AST_to_IL.var_of_id_info id id_info in
          (* Create _tmp IL.name using Tok.fake_tok like AST_to_IL.fresh_var does *)
          let tmp_tok = Tok.fake_tok shortlambda_tok "_tmp" in
          let tmp_name = IL.{
            ident = ("_tmp", tmp_tok);
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          } in
          Some (callback_name, snd id, Some tmp_name)
      | _ -> None)
  | _ -> None

(* Helper to identify a callback fn_id, checking nested functions in same scope first *)
let identify_callback ?(all_funcs = []) ?(caller_parent_path = [])
    ?(imported_entity_index = CanonicalMap.empty) ?(current_file : Fpath.t option)
    (callback_name : IL.name) : fn_id option =
  let callback_name_str = fst callback_name.IL.ident in
  (* Extract class from caller_parent_path if present *)
  let current_class = match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  let is_local_function func =
    match current_file with
    | Some file -> matches_current_file file func
    | None -> true
  in
  match !(callback_name.IL.id_info.G.id_resolved) with
  | Some (G.ImportedEntity canonical, _sid) ->
      lookup_imported_entity ?current_file imported_entity_index canonical
  | Some _
  | None ->

  (* First check if it's a nested function in the same scope - position-aware match *)
  let nested_match = List.find_opt (fun f ->
    match List_.init_and_last_opt f.fn_id with
    | Some (f_parent, Some name) when String.equal (fst name.IL.ident) callback_name_str ->
        (* Check if it's in the caller's scope *)
        equal_with_pos f_parent caller_parent_path
    | _ -> false
  ) all_funcs in

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
              is_local_function f &&
              match f.fn_id with
              | [Some c; Some m] when fst c.IL.ident = class_name_str && fst m.IL.ident = callback_name_str -> true
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
            is_local_function f &&
            match f.fn_id with
            | [None; Some name] when fst name.IL.ident = callback_name_str -> true
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
let try_identify_callback_arg ~all_funcs ~caller_parent_path
    ?(imported_entity_index = CanonicalMap.empty)
    ?(current_file : Fpath.t option) (arg : G.argument) :
    (fn_id * Tok.t * IL.name option) option =
  match arg with
  | G.Arg { G.e = G.Lambda fdef; _ } ->
      let tok = match fdef.fkind with (_, tok) -> tok in
      Some (anonymous_fn_id_of_fdef caller_parent_path fdef, tok, None)
  | G.Arg expr ->
      (* Also handle this.foo pattern *)
      let callback_opt = match expr.G.e with
        | G.DotAccess ({ e = G.IdSpecial ((G.This | G.Self), _); _ }, _, G.FN (G.Id (id, id_info))) ->
            Some (AST_to_IL.var_of_id_info id id_info, snd id, None)
        | _ -> extract_callback_from_arg expr
      in
      (match callback_opt with
      | Some (callback_name, tok, tmp_opt) ->
          (* Use real token from the callback argument *)
          identify_callback ~all_funcs ~imported_entity_index ?current_file
            ~caller_parent_path callback_name
          |> Option.map (fun fn_id -> (fn_id, tok, tmp_opt))
      | None -> None)
  | _ -> None

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
let extract_hof_callbacks_from_call ~method_hofs ~function_hofs ~all_funcs
    ~caller_parent_path ?(imported_entity_index = CanonicalMap.empty)
    ?(current_file : Fpath.t option) (callee : G.expr) (args : G.arguments) :
    (fn_id * Tok.t * IL.name option) list =
  let try_arg arg =
    try_identify_callback_arg ~all_funcs ~imported_entity_index ?current_file
      ~caller_parent_path arg
  in
  let try_arg_at_index idx =
    match List.nth_opt (Tok.unbracket args) idx with
    | Some arg -> try_arg arg
    | None -> None
  in
  (* Check ALL arguments for function references - any function passed as arg is a callback *)
  let all_callback_args =
    Tok.unbracket args
    |> List.filter_map try_arg
  in
  (* Check for specific configured HOF patterns for additional context *)
  let configured_callbacks = match callee.G.e with
  (* Method HOF: arr.map(callback) - callback at index 0 *)
  | G.DotAccess (_, _, G.FN (G.Id ((method_name, _), _)))
    -> (
      match
        List.find_opt (fun (methods, _) -> List.mem method_name methods)
          method_hofs
      with
      | Some (_, callback_index) ->
          try_arg_at_index callback_index |> Option.to_list
      | None -> [])
  (* Function HOF: map(callback, arr) *)
  | G.N (G.Id (id, _id_info)) ->
      let func_name = fst id in
      (match List.find_opt (fun (names, _) -> List.mem func_name names) function_hofs with
      | Some (_, callback_index) ->
          try_arg_at_index callback_index |> Option.to_list
      | None -> [])
  | _ -> []
  in
  all_callback_args @ configured_callbacks

(* Extract HOF callbacks, returning (fn_id, tok, tmp_opt) tuples.
   tmp_opt is Some IL.name for ShortLambda callbacks that need a _tmp intermediate node. *)
let extract_hof_callbacks ?(_object_mappings = []) ?(all_funcs = [])
    ?(imported_entity_index = CanonicalMap.empty)
    ?(current_file : Fpath.t option) ?(caller_parent_path = [])
    ~(lang : Lang.t) (fdef : G.function_definition) : (fn_id * Tok.t * IL.name option) list =
  let hof_configs = (Lang_config.get lang).hof_configs in
  let method_hofs =
    hof_configs |> List.concat_map (function
      | Lang_config.MethodHOF { methods; callback_index; _ } ->
          [ (methods, callback_index) ]
      | Lang_config.ReturningFunctionHOF { methods; _ } -> [ (methods, 0) ]
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
        | G.Call (callee, args) ->
            let found = extract_hof_callbacks_from_call
              ~method_hofs ~function_hofs ~all_funcs ~imported_entity_index
              ?current_file ~caller_parent_path
              callee args
            in
            callbacks := found @ !callbacks
        | _ -> ());
        super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  !callbacks

let collect_functions ~(lang : Lang.t) (ast : G.program) : func_info list =
  Visit_function_defs.fold_with_parent_path
    (fun funcs opt_ent parent_path fdef ->
      match fn_id_of_entity ~lang opt_ent parent_path fdef with
      | Some fn_id ->
          let func = { fn_id; entity = opt_ent; fdef } in
          func :: funcs
      | None -> funcs)
    [] ast

let first_program_tok ?current_file (ast : G.program) : Tok.t =
  match current_file with
  | Some file -> Tok.first_tok_of_file file
  | None -> (
      match AST_generic_helpers.ii_of_any (G.Pr ast) |> List.find_opt Tok.is_origintok with
      | Some tok -> tok
      | None -> Tok.unsafe_fake_tok "<top_level>")

let top_level_il_name ?current_file (ast : G.program) : IL.name =
  let tok = first_program_tok ?current_file ast in
  IL.
    {
      ident = ("<top_level>", tok);
      sid = G.SId.unsafe_default;
      id_info = AST_generic.empty_id_info ();
    }

let class_init_il_name (class_g_name : G.name) : IL.name =
  let class_il_name = AST_to_IL.var_of_name class_g_name in
  let class_str, class_tok = class_il_name.IL.ident in
  IL.
    {
      ident = ("Class:" ^ class_str, class_tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    }

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph_with_context ~(lang : Lang.t) ?(object_mappings = [])
    ?(all_funcs : func_info list option)
    ?(imported_entity_index = CanonicalMap.empty)
    ?(current_file : Fpath.t option) (ast : G.program) : Call_graph.G.t =
  let graph = Call_graph.G.create () in
  let import_binding_timeline = build_import_binding_timeline ast in
  let is_local_function func =
    match current_file with
    | Some file -> matches_current_file file func
    | None -> true
  in

  (* Create a special top_level node to represent code outside functions *)
  let top_level_node : node =
    Function_id.of_il_name (top_level_il_name ?current_file ast)
  in
  Call_graph.G.add_vertex graph top_level_node;

  let local_funcs = collect_functions ~lang ast in
  let funcs = Option.value ~default:local_funcs all_funcs in
  funcs
  |> List.iter (fun func ->
         match fn_id_to_node func.fn_id with
         | Some node -> Call_graph.G.add_vertex graph node
         | None -> ());
  (* Visit all calls in the AST, tracking the current function context *)
  Visit_function_defs.visit_with_parent_path
    (fun opt_ent parent_path fdef ->
      match fn_id_of_entity ~lang opt_ent parent_path fdef with
      | Some fn_id ->
          (* Check if this is a top-level lambda/block (no entity AND parent_path is [None] or []) *)
          (* Named functions (def foo) have opt_ent = Some _, lambdas have opt_ent = None *)
          let is_toplevel_lambda = match (opt_ent, parent_path) with
            | (None, [None]) | (None, []) -> true
            | _ -> false
          in

          (* Extract calls - class context is already in fn_id *)
          let callee_calls =
            extract_calls ~object_mappings ~all_funcs:funcs
              ~import_binding_timeline
              ~imported_entity_index ?current_file ~caller_parent_path:fn_id
              fdef
          in

          (* Add labeled edges for each call - edge from callee to caller for bottom-up analysis *)
          List.iter
            (fun (callee_fn_id, call_tok) ->
              match fn_id_to_node callee_fn_id, fn_id_to_node fn_id with
              | Some callee_node, Some caller_node ->
                  Call_graph.add_edge graph ~src:callee_node ~dst:caller_node ~call_tok;
                  if is_toplevel_lambda then
                    Call_graph.add_edge graph ~src:callee_node ~dst:top_level_node ~call_tok
              | _ -> ())
            callee_calls;

          (* Extract HOF callbacks and add edges: callback -> caller (or callback -> _tmp -> caller for ShortLambda) *)
          let callback_calls =
            extract_hof_callbacks ~_object_mappings:object_mappings
              ~all_funcs:funcs ~imported_entity_index ?current_file
              ~caller_parent_path:fn_id ~lang fdef
          in
          (* Add labeled edges for each callback - edge from callback to caller for bottom-up analysis.
             For ShortLambda, create intermediate _tmp node: callback -> _tmp -> caller *)
          List.iter
            (fun (callback_fn_id, call_tok, tmp_opt) ->
              match fn_id_to_node callback_fn_id, fn_id_to_node fn_id with
              | Some callback_node, Some caller_node ->
                  let src_to_caller = match tmp_opt with
                    | Some tmp_name ->
                        let tmp_node = Function_id.of_il_name tmp_name in
                        Call_graph.add_edge graph ~src:callback_node ~dst:tmp_node ~call_tok;
                        tmp_node
                    | None -> callback_node
                  in
                  Call_graph.add_edge graph ~src:src_to_caller ~dst:caller_node ~call_tok;
                  if is_toplevel_lambda then
                    Call_graph.add_edge graph ~src:src_to_caller ~dst:top_level_node ~call_tok
              | _ -> ())
            callback_calls
      | None -> ())
    ast;

  (* Extract calls from top-level code (outside any function) and add edges to <top_level> *)
  let toplevel_calls =
    extract_toplevel_calls ~object_mappings ~all_funcs:funcs
      ~import_binding_timeline
      ~imported_entity_index ?current_file ast
  in
  List.iter
    (fun (callee_fn_id, call_tok) ->
      match fn_id_to_node callee_fn_id with
      | Some callee_node ->
          Call_graph.add_edge graph ~src:callee_node ~dst:top_level_node ~call_tok
      | None -> ())
    toplevel_calls;
  Log.debug (fun m -> m "CALL_GRAPH: Added %d edges from top-level calls" (List.length toplevel_calls));

  (* Extract HOF callbacks from top-level code and add edges to <top_level> *)
  let toplevel_hof_callbacks =
    let hof_configs = (Lang_config.get lang).hof_configs in
    let method_hofs =
      hof_configs |> List.concat_map (function
        | Lang_config.MethodHOF { methods; callback_index; _ } ->
            [ (methods, callback_index) ]
        | Lang_config.ReturningFunctionHOF { methods; _ } -> [ (methods, 0) ]
        | _ -> [])
    in
    let function_hofs =
      hof_configs |> List.filter_map (function
        | Lang_config.FunctionHOF { functions; callback_index; _ } ->
            Some (functions, callback_index)
        | _ -> None)
    in
    Visit_function_defs.fold_toplevel_calls (fun acc _call_e callee args ->
      let found = extract_hof_callbacks_from_call
        ~method_hofs ~function_hofs ~all_funcs:funcs
        ~imported_entity_index ?current_file ~caller_parent_path:[]
        callee args
      in
      found @ acc
    ) [] ast
  in
  toplevel_hof_callbacks |> List.iter (fun (callback_fn_id, call_tok, tmp_opt) ->
    match fn_id_to_node callback_fn_id with
    | Some callback_node ->
        let src_to_caller = match tmp_opt with
          | Some tmp_name ->
              let tmp_node = Function_id.of_il_name tmp_name in
              Call_graph.add_edge graph ~src:callback_node ~dst:tmp_node ~call_tok;
              tmp_node
          | None -> callback_node
        in
        Call_graph.add_edge graph ~src:src_to_caller ~dst:top_level_node ~call_tok
    | None -> ());
  Log.debug (fun m -> m "CALL_GRAPH: Added %d edges from top-level HOF callbacks" (List.length toplevel_hof_callbacks));

  (* Add implicit edges from constructors to all methods in the same class.
     Constructors always execute before any method can be called on an object. *)
  List.iter
    (fun func ->
      let func_name_opt = get_fn_name func.fn_id in
      let func_name =
        Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) func_name_opt
      in
      let class_name_opt = match func.fn_id with class_opt :: _ -> class_opt | [] -> None in
      let class_name_str =
        Option.map (fun n -> fst n.IL.ident) class_name_opt
      in
      if Object_initialization.is_constructor lang func_name class_name_str then
        (* Find all methods in the same class *)
        let same_class_methods =
          List.filter
            (fun other ->
              let other_name_opt = get_fn_name other.fn_id in
              let other_name =
                Option.fold ~none:""
                  ~some:(fun n -> fst n.IL.ident)
                  other_name_opt
              in
              let other_class_opt = match other.fn_id with class_opt :: _ -> class_opt | [] -> None in
              let other_class_name_str =
                Option.map (fun n -> fst n.IL.ident) other_class_opt
              in
              is_local_function other
              &&
              (not
                 (Object_initialization.is_constructor lang other_name
                    other_class_name_str))
              && Option.equal
                   (fun n1 n2 ->
                     String.equal (fst n1.IL.ident) (fst n2.IL.ident))
                   class_name_opt other_class_opt)
            funcs
        in
        (* Add implicit edge from constructor to each method, only if no explicit edge exists *)
        List.iter
          (fun method_func ->
            match fn_id_to_node func.fn_id, fn_id_to_node method_func.fn_id with
            | Some constructor_node, Some method_node ->
                if not (Call_graph.G.mem_edge graph constructor_node method_node) then
                  Call_graph.add_edge graph ~src:constructor_node ~dst:method_node
                    ~call_tok:(Tok.unsafe_fake_tok "<implicit:constructor>")
            | _ -> ())
          same_class_methods)
    funcs;

  (* Add Class:* vertices for each class and implicit edges from class to methods.
     This handles classes without explicit constructors (e.g., Angular components using inject())
     and ensures class field initializers can propagate taint to methods.
     Edge direction: Class:* -> method (class init runs first, then methods can be called) *)
  let class_names = Object_initialization.collect_class_names ast in
  List.iter (fun class_g_name ->
    let class_il_name = AST_to_IL.var_of_name class_g_name in
    let class_str = fst class_il_name.IL.ident in
    (* Create Class:* node *)
    let class_init_node : node =
      Function_id.of_il_name (class_init_il_name class_g_name)
    in
    Call_graph.G.add_vertex graph class_init_node;

    (* Find all methods in this class *)
    let class_methods =
      List.filter
        (fun func ->
          is_local_function func &&
          let func_class_opt = match func.fn_id with class_opt :: _ -> class_opt | [] -> None in
          match func_class_opt with
          | Some func_class_il_name ->
              String.equal (fst func_class_il_name.IL.ident) class_str
          | None -> false)
        funcs
    in
    (* Add implicit edge from Class:* to each method (class init happens first, then methods) *)
    List.iter
      (fun method_func ->
        match fn_id_to_node method_func.fn_id with
        | Some method_node ->
            Call_graph.add_edge graph ~src:class_init_node ~dst:method_node
              ~call_tok:(Tok.unsafe_fake_tok "<implicit:class-init>")
        | None -> ())
      class_methods)
    class_names;

  (* DEBUG: Uncomment to dump call graph to DOT file
  let oc = open_out "/tmp/call_graph.dot" in
  Call_graph.Dot.output_graph oc graph;
  close_out oc;
  *)

  graph

let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : Call_graph.G.t =
  build_call_graph_with_context ~lang ~object_mappings ast

let importable_segments_of_fn_id (fn_id : fn_id) =
  match fn_id with
  | [ None; Some name ] -> Some [ fst name.IL.ident ]
  | [ Some cls; Some meth ] ->
      Some [ fst cls.IL.ident; fst meth.IL.ident ]
  | _ -> None

let constructor_import_alias_segments ~(lang : Lang.t) (fn_id : fn_id) =
  match fn_id with
  | [ Some cls; Some meth ] ->
      let class_name = fst cls.IL.ident in
      let method_name = fst meth.IL.ident in
      if Object_initialization.is_constructor lang method_name (Some class_name)
      then Some [ class_name ]
      else None
  | _ -> None

let collect_imported_aliases (ast : G.program) :
    (string list * string) list =
  let aliases = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_directive env directive =
        (match directive.G.d with
        | G.ImportFrom (_, G.DottedName xs, imported_names) ->
            imported_names
            |> List.iter (function
                 | id, Some (alias, _id_info) ->
                     aliases :=
                       (G.dotted_to_canonical (xs @ [ id ]), fst alias)
                       :: !aliases
                 | id, None ->
                     aliases :=
                       (G.dotted_to_canonical (xs @ [ id ]), fst id) :: !aliases)
        | G.ImportAs (_, G.DottedName xs, Some (alias, _id_info)) ->
            aliases := (G.dotted_to_canonical xs, fst alias) :: !aliases
        | _ -> ());
        super#visit_directive env directive
    end
  in
  visitor#visit_program () ast;
  !aliases

let collect_imported_wildcards (ast : G.program) : string list list =
  let wildcards = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_directive env directive =
        (match directive.G.d with
        | G.ImportAll (_, G.DottedName xs, _) ->
            wildcards := G.dotted_to_canonical xs :: !wildcards
        | _ -> ());
        super#visit_directive env directive
    end
  in
  visitor#visit_program () ast;
  !wildcards

let build_imported_entity_index ~(lang : Lang.t) (files : project_file list) =
  let module_candidates_for_file file =
    (* Keep every suffix of the full filesystem path. Trimming the shared
     * prefix drops package segments when all files live under the same
     * package tree, which breaks package import resolution. *)
    module_candidates_of_path file.path.internal_path_to_content
    |> List.sort_uniq Stdlib.compare
  in
  let base_index =
    List.fold_left
      (fun acc file ->
        let module_candidates = module_candidates_for_file file in
        file.funcs
        |> List.fold_left
             (fun acc func ->
               let entity_segments =
                 importable_segments_of_fn_id func.fn_id |> Option.to_list
               in
               let entity_segments =
                 match constructor_import_alias_segments ~lang func.fn_id with
                 | Some alias_segments when not (List.mem alias_segments entity_segments)
                   ->
                     alias_segments :: entity_segments
                 | Some _
                 | None ->
                     entity_segments
               in
               entity_segments
               |> List.fold_left
                    (fun acc entity_segments ->
                      module_candidates
                      |> List.fold_left
                           (fun acc module_name ->
                             let canonical = module_name @ entity_segments in
                             add_imported_entity acc canonical func)
                           acc)
                    acc)
             acc)
      CanonicalMap.empty files
  in
  List.fold_left
    (fun acc file ->
      let module_candidates = module_candidates_for_file file in
      let acc =
        collect_imported_aliases file.ast
        |> List.fold_left
             (fun acc (canonical_target, local_name) ->
               let acc =
                 match
                   lookup_imported_func
                     ~current_file:file.path.internal_path_to_content base_index
                     canonical_target
                 with
                 | None -> acc
                 | Some func ->
                     module_candidates
                     |> List.fold_left
                          (fun acc module_name ->
                            add_imported_entity acc (module_name @ [ local_name ])
                              func)
                          acc
               in
               CanonicalMap.fold
                 (fun canonical funcs acc ->
                   if
                     List.length funcs <> 1
                     || List.length canonical <= List.length canonical_target
                     || not
                          (starts_with_segments ~prefix:canonical_target
                             canonical)
                   then acc
                   else
                     let tail =
                       List_.drop (List.length canonical_target) canonical
                     in
                     let func = List.hd funcs in
                     module_candidates
                     |> List.fold_left
                          (fun acc module_name ->
                            add_imported_entity acc
                              (module_name @ (local_name :: tail))
                              func)
                          acc)
                 base_index acc)
             acc
      in
      collect_imported_wildcards file.ast
      |> List.fold_left
           (fun acc canonical_target ->
             let resolved_targets =
               canonical_lookup_candidates
                 ~current_file:file.path.internal_path_to_content canonical_target
             in
             (* Expand wildcard imports from the alias-augmented index built so
              * far, not just the raw base index. Package __init__.py re-exports
              * are added in the alias pass above, and wildcard imports like
              * `from pkg import *` need to see those bindings too. *)
             CanonicalMap.fold
               (fun canonical funcs acc ->
                 if List.length funcs <> 1 then acc
                 else
                   match List.find_opt (fun target -> starts_with_segments ~prefix:target canonical) resolved_targets with
                   | None -> acc
                   | Some target -> (
                       match List_.drop (List.length target) canonical with
                       | [ local_name ] ->
                           let func = List.hd funcs in
                           module_candidates
                           |> List.fold_left
                                (fun acc module_name ->
                                  add_imported_entity acc
                                    (module_name @ [ local_name ])
                                    func)
                                acc
                       | _ -> acc))
               acc acc)
           acc)
    base_index files

let build_project_call_graph ~(lang : Lang.t)
    (files : (Target.path * G.program * (G.name * G.name) list) list) :
    Call_graph.G.t =
  let files =
    files
    |> List_.map (fun (path, ast, object_mappings) ->
           { path; ast; object_mappings; funcs = collect_functions ~lang ast })
  in
  let graph = Call_graph.G.create () in
  let imported_entity_index = build_imported_entity_index ~lang files in
  let all_funcs = files |> List.concat_map (fun file -> file.funcs) in
  files
  |> List.iter (fun file ->
         let imported_object_mappings =
           detect_imported_object_initialization ~lang ~imported_entity_index
             file.ast ~current_file:file.path.internal_path_to_content ()
         in
         let object_mappings =
           file.object_mappings @ imported_object_mappings
         in
         let local_graph =
           build_call_graph_with_context ~lang
             ~object_mappings
             ~all_funcs ~imported_entity_index
             ~current_file:file.path.internal_path_to_content file.ast
         in
         Call_graph.G.iter_vertex (Call_graph.G.add_vertex graph) local_graph;
         Call_graph.G.iter_edges_e (Call_graph.G.add_edge_e graph) local_graph);
  graph

(* Identify functions that contain byte ranges (from pattern matches) *)
let find_functions_containing_ranges ~(lang : Lang.t)
    ?(current_file : Fpath.t option) (ast : G.program) (ranges : Range.t list) :
    Function_id.t list =
  (* Hash table to track ALL functions containing each range, along with function size *)
  let range_to_funcs : (Range.t, (fn_id * int) list) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun range -> Hashtbl.add range_to_funcs range []) ranges;

  let visitor = object (self)
    inherit [_] G.iter_no_id_info as super
    val current_class : G.name option ref = ref None
    val parent_path : IL.name option list ref = ref []

    (* Helper to convert G.name to IL.name *)
    method private g_name_to_il_name (g_name : G.name) : IL.name option =
      match g_name with
      | G.Id ((str, tok), id_info) ->
          let id_info = { id_info with G.id_resolved = ref None } in
          Some IL.{ ident = (str, tok); sid = G.SId.unsafe_default; id_info }
      | _ -> None

    (* Helper to get IL.name from entity *)
    method private entity_to_il_name (ent : G.entity) : IL.name option =
      match ent.G.name with
      | G.EN name -> self#g_name_to_il_name name
      | _ -> None

    method private record_function_range (fn_id : fn_id) (range : Range.t) =
      let func_start = range.start in
      let func_end = range.end_ in
      let func_size = func_end - func_start in
      List.iter (fun (range : Range.t) ->
        if func_start <= range.Range.start && range.Range.end_ <= func_end then (
          let existing = Hashtbl.find range_to_funcs range in
          if not (List.exists (fun (fid, _) -> equal_fn_id fid fn_id) existing) then
            Hashtbl.replace range_to_funcs range ((fn_id, func_size) :: existing)
        )
      ) ranges

    method! visit_definition (env : unit) ((ent, def_kind) as def) =
      match def_kind with
      | G.ClassDef cdef ->
          let old_class = !current_class in
          (current_class :=
             match ent.name with
             | EN name -> Some name
             | _ -> None);

          (* Get the class body range *)
          let (_, cbody_stmts, _) = cdef.cbody in
          let cbody_range_opt = AST_generic_helpers.range_of_any_opt (G.Flds cbody_stmts) in
          (match cbody_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let class_start = range.start in
              let class_end = range.end_ in
              let class_size = class_end - class_start in

              (* For each range, check if it's inside this class *)
              List.iter (fun (range : Range.t) ->
                if class_start <= range.Range.start && range.Range.end_ <= class_end then (
                  (* This class contains this range - add it to the list *)
                  match !current_class with
                  | Some class_g_name ->
                      let class_node_name =
                        Some (class_init_il_name class_g_name)
                      in
                      let class_fn_id = [None; class_node_name] in
                      let existing = Hashtbl.find range_to_funcs range in
                      if not (List.exists (fun (fid, _) -> equal_fn_id fid class_fn_id) existing) then
                        Hashtbl.replace range_to_funcs range ((class_fn_id, class_size) :: existing)
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env def
          | None -> super#visit_definition env def);
          current_class := old_class
      | G.FuncDef fdef | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Get the entire function definition range (including parameters) *)
          let func_range_opt = AST_generic_helpers.range_of_any_opt (G.Def def) in
          (match func_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let class_il = Option.bind !current_class self#g_name_to_il_name in
              let visitor_parent_path =
                match !parent_path with
                | [] -> [class_il]
                | _ -> !parent_path
              in
              (match fn_id_of_entity ~lang (Some ent) visitor_parent_path fdef with
              | Some fn_id -> self#record_function_range fn_id range
              | None -> ());

              (* Push current function onto parent_path for nested functions *)
              let old_path = !parent_path in
              let class_il = Option.bind !current_class self#g_name_to_il_name in
              let func_il = self#entity_to_il_name ent in
              let current_fn_id =
                match !parent_path with
                | [] -> [class_il; func_il]
                | _ -> !parent_path @ [func_il]
              in
              parent_path := current_fn_id;

              (* Visit nested functions with updated parent_path without re-visiting
                 the outer lambda/function expression itself. *)
              let body = AST_generic_helpers.funcbody_to_stmt fdef.G.fbody in
              self#visit_stmt env body;

              (* Restore parent_path *)
              parent_path := old_path
          | None -> super#visit_definition env def)
      | _ -> super#visit_definition env def

  end in

  visitor#visit_program () ast;

  (* Now select the innermost (smallest) function for each range *)
  List.fold_left (fun matching_funcs range ->
    let funcs_list = Hashtbl.find range_to_funcs range in
    if List.is_empty funcs_list then
      (* No function contains this range - it's at top level *)
      let top_level_name = Some (top_level_il_name ?current_file ast) in
      let top_level_fn_id = [None; top_level_name] in
      if List.mem top_level_fn_id matching_funcs then
        matching_funcs
      else
        top_level_fn_id :: matching_funcs
    else
      (* Sort by size and pick the smallest (innermost) *)
      let sorted =
        List.sort (fun (_, size1) (_, size2) -> compare size1 size2) funcs_list
      in
      let (innermost_fn_id, _) = List.hd sorted in
      if List.mem innermost_fn_id matching_funcs then
        matching_funcs
      else
        innermost_fn_id :: matching_funcs
  ) [] ranges
  |> List.filter_map fn_id_to_node
