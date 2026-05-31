open Common
module G = AST_generic
module Log = Log_call_graph.Log
(*  *open Shape_and_sig *)
module Reachable = Graph_reachability

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
  let is_lambda =
    match fst fdef.fkind with
    | G.LambdaKind | G.Arrow -> true
    | _ -> false
  in
  match opt_ent with
  | _ when is_lambda ->
      (* All lambdas (named [cb = lambda x: ...] or anonymous) share one
         identity scheme: the lambda's own definition position. The binding
         variable is treated as an alias, not a distinct callable. *)
      Some (normalized_parent_path @ [Some (Visit_function_defs.synth_lambda_il_name fdef)])
  | None ->
      Log.warn (fun m ->
          m "fn_id_of_entity: anonymous non-lambda function definition \
             at %s; falling back to lambda-style identity"
            (Tok.stringpos_of_tok (snd fdef.fkind)));
      Some (normalized_parent_path @ [Some (Visit_function_defs.synth_lambda_il_name fdef)])
  | Some ent ->
      (match AST_to_IL.name_of_entity ent with
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

let dedup_fn_ids (ids : (fn_id * Tok.t) list) : (fn_id * Tok.t) list =
  ids |>
  List.sort_uniq (fun (f1, t1) (f2, t2) ->
    let cmp = compare_fn_id f1 f2 in
    if cmp <> 0 then cmp else Tok.compare t1 t2)

(* Helper function to identify the callee fn_id from a call expression's callee.
 * (Constructor resolution lives in the cross-file helpers block below, where the
 * class hierarchy is available; see [resolve_constructor_from_type].) *)

(*****************************************************************************)
(* Cross-file (interfile) import resolution                                  *)
(*****************************************************************************)
(* A definition's identity carries its source file via the token on its
 * [IL.name]. An import reference carries a module-path string (from its
 * [id_resolved] canonical name). We resolve an imported call to a definition
 * by suffix-matching the import's path parts against the definition's file
 * path parts. This is what lets cross-file calls resolve over the merged AST
 * for languages that bind imports by module specifier (JS/TS/Python/...). *)

let path_segments_of_string (s : string) : string list =
  s |> String.split_on_char '/'
  |> List.map (String.split_on_char '\\')
  |> List.flatten
  |> List.filter (fun part -> part <> "" && part <> "." && part <> "..")

let path_segments_preserving_relative (s : string) : string list =
  s |> String.split_on_char '/'
  |> List.map (String.split_on_char '\\')
  |> List.flatten
  |> List.filter (fun part -> part <> "")

let normalize_path_parts (parts : string list) : string list =
  let rec aux acc = function
    | [] -> List.rev acc
    | "." :: rest -> aux acc rest
    | ".." :: rest ->
        let acc = match acc with [] -> [] | _ :: parent -> parent in
        aux acc rest
    | part :: rest -> aux (part :: acc) rest
  in
  aux [] parts

let is_known_source_extension (ext : string) : bool =
  match String.lowercase_ascii ext with
  | "c" | "cc" | "cjs" | "clj" | "cljs" | "cljc" | "cpp" | "cs" | "css" | "cxx"
  | "dart" | "erl" | "ex" | "exs" | "go" | "h" | "hh" | "hpp" | "hrl" | "hxx"
  | "java" | "js" | "jsx" | "kt" | "kts" | "lua" | "mjs" | "ml" | "mli" | "php"
  | "py" | "r" | "rb" | "rs" | "scala" | "sh" | "swift" | "ts" | "tsx" | "vue" ->
      true
  | _ -> false

let remove_known_source_extension (segment : string) : string =
  match String.rindex_opt segment '.' with
  | Some idx when idx > 0 ->
      let ext = String.sub segment (idx + 1) (String.length segment - idx - 1) in
      if is_known_source_extension ext then String.sub segment 0 idx else segment
  | _ -> segment

let map_last f xs =
  match List.rev xs with
  | [] -> []
  | last :: rev_prefix -> List.rev (f last :: rev_prefix)

let import_path_parts_of_part (part : string) : string list =
  part |> path_segments_of_string |> map_last remove_known_source_extension

let import_path_parts_of_part_preserving_relative (part : string) : string list =
  part |> path_segments_preserving_relative
  |> map_last remove_known_source_extension

let import_path_parts_of_canonical (canonical : G.canonical_name) : string list =
  canonical |> List.map import_path_parts_of_part |> List.flatten

let import_path_parts_of_canonical_preserving_relative
    (canonical : G.canonical_name) : string list =
  canonical
  |> List.map import_path_parts_of_part_preserving_relative
  |> List.flatten

let drop_last xs =
  match List.rev xs with [] -> [] | _ :: rest -> List.rev rest

let file_path_parts_of_tok (tok : Tok.t) : string list option =
  if Tok.is_fake tok then None
  else
    Some
      (Tok.file_of_tok tok |> Fpath.rem_ext |> Fpath.to_string
     |> path_segments_of_string)

let file_path_parts_of_il_name (name : IL.name) : string list option =
  file_path_parts_of_tok (snd name.IL.ident)

let list_ends_with ~suffix xs =
  let xs_len = List.length xs in
  let suffix_len = List.length suffix in
  let rec drop n xs =
    if n <= 0 then xs
    else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest
  in
  suffix_len <= xs_len
  && List.equal String.equal (drop (xs_len - suffix_len) xs) suffix

let il_name_file_matches_module_path (name : IL.name) module_path_parts =
  match file_path_parts_of_il_name name with
  | Some file_path_parts -> (
      match module_path_parts with
      | [] -> false
      | _ -> list_ends_with ~suffix:module_path_parts file_path_parts)
  | _ -> false

let caller_relative_module_path_parts call_tok module_canonical =
  match file_path_parts_of_tok call_tok with
  | None -> None
  | Some caller_file_parts -> (
      let module_path_parts =
        import_path_parts_of_canonical_preserving_relative module_canonical
      in
      match module_path_parts with
      | [] -> None
      | _ ->
          Some
            (normalize_path_parts
               (drop_last caller_file_parts @ module_path_parts)))

let fn_id_matches_imported_entity (canonical : G.canonical_name) (fn_id : fn_id) :
    bool =
  match (List.rev canonical, get_fn_name fn_id) with
  | imported_name :: rev_module_path, Some fn_name ->
      String.equal imported_name (fst fn_name.IL.ident)
      && il_name_file_matches_module_path fn_name
           (import_path_parts_of_canonical (List.rev rev_module_path))
  | _ -> false

let fn_id_matches_imported_module_export (canonical : G.canonical_name)
    (fn_id : fn_id) : bool =
  match get_fn_name fn_id with
  | Some fn_name ->
      String.equal (fst fn_name.IL.ident) "module.exports"
      && il_name_file_matches_module_path fn_name
           (import_path_parts_of_canonical canonical)
  | _ -> false

(* Resolve a call whose callee [id_info] is an imported entity/module to the
 * matching top-level definition in another file. Disambiguates by the caller's
 * own directory (relative imports) then by arity; never guesses. *)
let pick_imported_match (id_info : G.id_info) call_tok (call_arity : int option)
    (all_funcs : func_info list) : fn_id option =
  let pick_with_caller_relative_module module_canonical matches =
    match caller_relative_module_path_parts call_tok module_canonical with
    | Some caller_relative_path_parts -> (
        let caller_relative_matches =
          matches
          |> List.filter (fun f ->
                 match get_fn_name f.fn_id with
                 | Some fn_name ->
                     il_name_file_matches_module_path fn_name
                       caller_relative_path_parts
                 | None -> false)
        in
        match caller_relative_matches with
        | _ :: _ -> pick_by_arity call_arity caller_relative_matches
        | [] -> pick_by_arity call_arity matches)
    | None -> pick_by_arity call_arity matches
  in
  match !(id_info.G.id_resolved) with
  | Some (G.ImportedEntity canonical, _) ->
      let module_canonical =
        match List.rev canonical with
        | _imported_name :: rev_module_path -> List.rev rev_module_path
        | [] -> []
      in
      let matches =
        all_funcs
        |> List.filter (fun f -> fn_id_matches_imported_entity canonical f.fn_id)
      in
      pick_with_caller_relative_module module_canonical matches
  | Some (G.ImportedModule canonical, _) ->
      let matches =
        all_funcs
        |> List.filter (fun f ->
               fn_id_matches_imported_module_export canonical f.fn_id)
      in
      pick_with_caller_relative_module canonical matches
  | _ -> None

(*****************************************************************************)
(* Cross-file object / class / module-member resolution helpers              *)
(*****************************************************************************)

let module_path_parts_of_canonical (canonical : G.canonical_name) :
    string list option =
  match import_path_parts_of_canonical canonical with
  | [] -> None
  | module_path_parts -> Some module_path_parts

let fn_id_file_matches_tok (fn_id : fn_id) (tok : Tok.t) : bool =
  match (get_fn_name fn_id, file_path_parts_of_tok tok) with
  | Some name, Some call_path_parts -> (
      match file_path_parts_of_il_name name with
      | Some fn_path_parts ->
          List.equal String.equal fn_path_parts call_path_parts
      | None -> false)
  | _ -> false

let pick_by_file_then_arity (call_tok : Tok.t) (call_arity : int option)
    (matches : func_info list) : fn_id option =
  let same_file_matches =
    matches |> List.filter (fun f -> fn_id_file_matches_tok f.fn_id call_tok)
  in
  match same_file_matches with
  | _ :: _ -> pick_by_arity call_arity same_file_matches
  | [] -> pick_by_arity call_arity matches

let string_of_name (name : G.name) : string option =
  match name with
  | G.Id ((name, _), _) -> Some name
  | G.IdQualified { name_last = (name, _), _; _ } -> Some name

let rec string_of_type (ty : G.type_) : string option =
  match ty.G.t with
  | G.TyN name -> string_of_name name
  | G.TyApply (inner_ty, _) -> string_of_type inner_ty
  | G.TyExpr { G.e = G.N name; _ } -> string_of_name name
  | _ -> None

type class_hierarchy = (string * string list) list

let collect_class_hierarchy (ast : G.program) : class_hierarchy =
  let hierarchy = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.ClassDef cdef -> (
            match entity.G.name with
            | G.EN name -> (
                match string_of_name name with
                | Some class_name ->
                    let parent_names =
                      cdef.G.cextends
                      |> List.filter_map (fun (parent_ty, _) ->
                             string_of_type parent_ty)
                    in
                    hierarchy := (class_name, parent_names) :: !hierarchy
                | None -> ())
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in
  List.iter
    (fun item ->
      match item.G.s with
      | G.DefStmt def -> visitor#visit_definition () def
      | _ -> ())
    ast;
  !hierarchy

let dedup_strings_preserve_order (strings : string list) : string list =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | x :: xs ->
        if List.mem x seen then aux seen acc xs
        else aux (x :: seen) (x :: acc) xs
  in
  aux [] [] strings

let class_lineage (hierarchy : class_hierarchy) (class_name : string) :
    string list =
  let rec aux seen name =
    if List.mem name seen then []
    else
      let parents = Option.value ~default:[] (List.assoc_opt name hierarchy) in
      name :: List.concat_map (aux (name :: seen)) parents
  in
  aux [] class_name |> dedup_strings_preserve_order

let method_matches_in_class (all_funcs : func_info list) class_name method_name :
    func_info list =
  all_funcs
  |> List.filter (fun f ->
         match f.fn_id with
         | [ Some c; Some m ] ->
             fst c.IL.ident = class_name && fst m.IL.ident = method_name
         | _ -> false)

let pick_method_in_lineage (hierarchy : class_hierarchy)
    (all_funcs : func_info list) (call_arity : int option) class_name
    method_name : fn_id option =
  class_lineage hierarchy class_name
  |> List.find_map (fun candidate_class ->
         method_matches_in_class all_funcs candidate_class method_name
         |> pick_by_arity call_arity)

let class_init_fn_id class_name : fn_id =
  let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_name) in
  let il_name =
    IL.
      {
        ident = ("Class:" ^ class_name, fake_tok);
        sid = G.SId.unsafe_default;
        id_info = G.empty_id_info ();
      }
  in
  [ None; Some il_name ]

type module_import = {
  import_file : Fpath.t;
  local_name : string;
  module_path_parts : string list;
}

let module_import_of_directive (dir : G.directive) : module_import option =
  match dir.d with
  | G.ImportAs (_, G.DottedName xs, alias_opt) -> (
      match (xs, alias_opt) with
      | [], _ -> None
      | (first_name, first_tok) :: _, None ->
          Some
            {
              import_file = Tok.file_of_tok first_tok;
              local_name = first_name;
              module_path_parts =
                xs |> List.map fst |> import_path_parts_of_canonical;
            }
      | _, Some ((alias_name, alias_tok), _) ->
          Some
            {
              import_file = Tok.file_of_tok alias_tok;
              local_name = alias_name;
              module_path_parts =
                xs |> List.map fst |> import_path_parts_of_canonical;
            })
  | _ -> None

let module_imports_of_ast (ast : G.program) : module_import list =
  ast
  |> List.filter_map (function
       | { G.s = G.DirectiveStmt dir; _ } -> module_import_of_directive dir
       | _ -> None)

let module_paths_for_receiver ~(module_imports : module_import list) obj_name
    obj_tok (obj_id_info : G.id_info) : string list list =
  match !(obj_id_info.G.id_resolved) with
  | Some (G.ImportedModule canonical, _) ->
      module_path_parts_of_canonical canonical |> Option.to_list
  | _ ->
      module_imports
      |> List.filter_map (fun import ->
             if
               String.equal import.local_name obj_name
               && Fpath.equal import.import_file (Tok.file_of_tok obj_tok)
             then Some import.module_path_parts
             else None)

let fn_id_matches_module_member module_paths member_name (fn_id : fn_id) : bool =
  match fn_id with
  | [ None; Some fn_name ] ->
      String.equal member_name (fst fn_name.IL.ident)
      && List.exists
           (fun module_path_parts ->
             il_name_file_matches_module_path fn_name module_path_parts)
           module_paths
  | _ -> false

let object_mapping_class object_mappings obj_name obj_resolved =
  object_mappings
  |> List.find_opt (fun (var_name, _class_name) ->
         match var_name with
         | G.Id ((var_str, _), var_id_info) -> (
             var_str = obj_name
             &&
             match (obj_resolved, !(var_id_info.G.id_resolved)) with
             | Some (_, sid1), Some (_, sid2) -> G.SId.equal sid1 sid2
             | _ -> true)
         | _ -> false)
  |> Option.map (fun (_var_name, class_name) -> class_name)

let type_class_name id_info =
  match !(id_info.G.id_type) with
  | Some { G.t = G.TyN (G.Id _ as n); _ }
  | Some { G.t = G.TyExpr { G.e = G.N (G.Id _ as n); _ }; _ } ->
      Some n
  | _ -> None

(* Hierarchy-aware constructor resolution (shadows the simpler earlier one for
 * [identify_callee] below). *)
let resolve_constructor_from_type ~(lang : Lang.t) ?(class_hierarchy = [])
    ~all_funcs (ty : G.type_) : fn_id option =
  match string_of_type ty with
  | None -> None
  | Some cls -> (
      let lineage = class_lineage class_hierarchy cls in
      let explicit_constructor =
        lineage
        |> List.find_map (fun candidate_class ->
               List.find_opt
                 (fun f ->
                   match f.fn_id with
                   | [ Some c; Some m ] ->
                       fst c.IL.ident = candidate_class
                       && Object_initialization.is_constructor lang
                            (fst m.IL.ident) (Some candidate_class)
                   | _ -> false)
                 all_funcs
               |> Option.map (fun f -> f.fn_id))
      in
      match explicit_constructor with
      | Some _ as constructor -> constructor
      | None ->
          lineage
          |> List.find_map (fun candidate_class ->
                 if List.mem_assoc candidate_class class_hierarchy then
                   Some (class_init_fn_id candidate_class)
                 else None))

let identify_callee ~(lang : Lang.t) ?(object_mappings = [])
    ?(module_imports = []) ?(class_hierarchy = []) ?(all_funcs = [])
    ?(caller_parent_path = []) ?(call_arity : int option) (callee : G.expr) :
    fn_id option =
  (* Extract class from caller_parent_path if present *)
  let current_class =
    match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  match callee.G.e with
  (* Simple function call: foo() *)
  | G.N (G.Id ((id, call_tok), id_info)) -> (
      let callee_name_str = id in
      let fallback () =
        (* First check if it's a nested function in the same scope.
             Use position-aware match to distinguish same-named parent functions. *)
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
            (* For class-based languages, foo() might be an implicit this.foo() call.
                 Check if a method with this name exists in the current class. *)
            match current_class with
            | Some class_name -> (
                let class_name_str = fst class_name.IL.ident in
                let method_match =
                  pick_method_in_lineage class_hierarchy all_funcs call_arity
                    class_name_str callee_name_str
                in
                (* Debug: show all function names *)
                let all_names =
                  all_funcs
                  |> List.map (fun f -> show_fn_id f.fn_id)
                  |> String.concat ", "
                in
                Log.debug (fun m ->
                    m
                      "CALL_EXTRACT: In class %s, call to %s, checking %d \
                       funcs, method_exists=%b, ALL: [%s]"
                      class_name_str callee_name_str (List.length all_funcs)
                      (Option.is_some method_match)
                      all_names);
                match method_match with
                | Some fn_id -> Some fn_id
                | None ->
                    (* It's a free function call, not a method - use string matching *)
                    let free_fn_matches =
                      List.filter
                        (fun f ->
                          match f.fn_id with
                          | [ None; Some name ]
                            when fst name.IL.ident = callee_name_str ->
                              true
                          | _ -> false)
                        all_funcs
                    in
                    pick_by_file_then_arity call_tok call_arity free_fn_matches)
            | None -> (
                (* Top-level free function - use string matching *)
                let free_fn_matches =
                  List.filter
                    (fun f ->
                      match f.fn_id with
                      | [ None; Some name ]
                        when fst name.IL.ident = callee_name_str ->
                          true
                      | _ -> false)
                    all_funcs
                in
                match
                  pick_by_file_then_arity call_tok call_arity free_fn_matches
                with
                | Some _ as r -> r
                | None ->
                    (* Try as constructor: ClassName() -> ClassName#__init__ etc. *)
                    let ty =
                      G.
                        {
                          t =
                            TyN
                              (G.Id
                                 ( (callee_name_str, G.fake callee_name_str),
                                   G.empty_id_info () ));
                          t_attrs = [];
                        }
                    in
                    resolve_constructor_from_type ~lang ~class_hierarchy
                      ~all_funcs ty))
      in
      match pick_imported_match id_info call_tok call_arity all_funcs with
      | Some _ as imported_match -> imported_match
      | None -> fallback ())
  (* Qualified call: Module.foo() *)
  | G.N (G.IdQualified { name_last = (id, _), _; _ }) ->
      let callee_name_str = id in
      (* Use string matching to find the qualified function *)
      let qualified_match =
        List.find_opt
          (fun f ->
            match f.fn_id with
            | [ None; Some name ] when fst name.IL.ident = callee_name_str ->
                true
            | _ -> false)
          all_funcs
      in
      Option.map (fun f -> f.fn_id) qualified_match
  (* Method call: this.method() or self.method() *)
  | G.DotAccess
      ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
        _,
        G.FN (G.Id ((id, _), _id_info)) ) -> (
      let method_name_str = id in
      (* Use string matching to find the method in current class *)
      match current_class with
      | Some class_name ->
          let class_name_str = fst class_name.IL.ident in
          pick_method_in_lineage class_hierarchy all_funcs call_arity
            class_name_str method_name_str
      | None -> None)
  (* Method call: obj.method() - look up obj's class *)
  | G.DotAccess
      ( { e = G.N (G.Id ((obj_name, obj_tok), obj_id_info)); _ },
        _,
        G.FN (G.Id ((id, _), _id_info)) ) -> (
      let method_name_str = id in
      let obj_resolved = !(obj_id_info.G.id_resolved) in
      let obj_class_opt =
        object_mapping_class object_mappings obj_name obj_resolved
      in
      (* Fallback: use the type annotation (e.g. `def f(x: ClassName)`) *)
      let obj_class_opt =
        match obj_class_opt with
        | Some _ -> obj_class_opt
        | None -> type_class_name obj_id_info
      in
      match obj_class_opt with
      | Some class_name ->
          let class_name_str =
            match class_name with
            | G.Id ((str, _), _) -> str
            | G.IdQualified { name_last = (str, _), _; _ } -> str
          in
          pick_method_in_lineage class_hierarchy all_funcs call_arity
            class_name_str method_name_str
      | None -> (
          (* For static/class calls like Java's ClassName.method(), the
                   receiver is the class name, not an object mapping. *)
          match
            pick_method_in_lineage class_hierarchy all_funcs call_arity obj_name
              method_name_str
          with
          | Some _ as result -> result
          | None -> (
              let module_paths =
                module_paths_for_receiver ~module_imports obj_name obj_tok
                  obj_id_info
              in
              let module_function_matches =
                all_funcs
                |> List.filter (fun f ->
                       fn_id_matches_module_member module_paths method_name_str
                         f.fn_id)
              in
              match pick_by_arity call_arity module_function_matches with
              | Some _ as result -> result
              | None ->
                  (* obj not in object_mappings — try as ClassName.new() constructor *)
                  let ty =
                    G.
                      {
                        t =
                          TyN
                            (G.Id
                               ((obj_name, G.fake obj_name), G.empty_id_info ()));
                        t_attrs = [];
                      }
                  in
                  resolve_constructor_from_type ~lang ~class_hierarchy
                    ~all_funcs ty)))
  (* Method call through an instance field: this.field.method().
           JavaScript and TypeScript require class fields to be read through
           this, so the receiver appears as a nested DotAccess instead of a
           plain variable. *)
  | G.DotAccess
      ( {
          e =
            G.DotAccess
              ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
                _,
                G.FN (G.Id ((field_name, _), field_id_info)) );
          _;
        },
        _,
        G.FN (G.Id ((method_name, _), _)) ) -> (
      let field_resolved = !(field_id_info.G.id_resolved) in
      let field_class_opt =
        match
          object_mapping_class object_mappings field_name field_resolved
        with
        | Some _ as class_name -> class_name
        | None -> type_class_name field_id_info
      in
      match field_class_opt with
      | Some class_name ->
          let class_name_str =
            match class_name with
            | G.Id ((str, _), _) -> str
            | G.IdQualified { name_last = (str, _), _; _ } -> str
          in
          pick_method_in_lineage class_hierarchy all_funcs call_arity
            class_name_str method_name
      | None -> None)
  (* Method call through a collection entry: items[0].method().
           DI multi-provider APIs commonly inject collections under one token.
           Object initialization maps the collection variable to the provider
           class, and this case lets indexed element calls use that mapping. *)
  | G.DotAccess
      ( {
          e =
            G.ArrayAccess ({ e = G.N (G.Id ((obj_name, _), obj_id_info)); _ }, _);
          _;
        },
        _,
        G.FN (G.Id ((method_name, _), _)) ) -> (
      let obj_resolved = !(obj_id_info.G.id_resolved) in
      let obj_class_opt =
        match object_mapping_class object_mappings obj_name obj_resolved with
        | Some _ as class_name -> class_name
        | None -> type_class_name obj_id_info
      in
      match obj_class_opt with
      | Some class_name ->
          let class_name_str =
            match class_name with
            | G.Id ((str, _), _) -> str
            | G.IdQualified { name_last = (str, _), _; _ } -> str
          in
          pick_method_in_lineage class_hierarchy all_funcs call_arity
            class_name_str method_name
      | None -> None)
  (* Chained call: Constructor(...).method() — receiver is a constructor.
           Python/Kotlin/Scala: ClassName(args).method()
           Java/JS/TS/C#:       new ClassName(args).method()
           Ruby:                ClassName.new(args).method() *)
  | G.DotAccess (receiver, _, G.FN (G.Id ((method_name, _), _))) -> (
      let class_name_opt =
        match receiver.G.e with
        (* Python/Kotlin/Scala: ClassName(args) *)
        | G.Call ({ e = G.N (G.Id ((cn, _), _)); _ }, _)
          when Lang.(lang =*= Python || lang =*= Kotlin || lang =*= Scala) ->
            Some cn
        (* Java/JS/TS/C#: new ClassName(args) *)
        | G.New (_, ty, _, _)
          when Lang.(
                 lang =*= Java || lang =*= Js || lang =*= Ts || lang =*= Csharp)
          -> (
            match ty.G.t with
            | G.TyN (G.Id ((cn, _), _)) -> Some cn
            | G.TyExpr { G.e = G.N (G.Id ((cn, _), _)); _ } -> Some cn
            | _ -> None)
        (* Ruby: ClassName.new(args) *)
        | G.Call
            ( {
                e =
                  G.DotAccess
                    ( { e = G.N (G.Id ((cn, _), _)); _ },
                      _,
                      G.FN (G.Id (("new", _), _)) );
                _;
              },
              _ )
          when Lang.(lang =*= Ruby) ->
            Some cn
        | _ -> None
      in
      match class_name_opt with
      | Some class_name ->
          pick_method_in_lineage class_hierarchy all_funcs call_arity class_name
            method_name
      | None -> None)
  | _ ->
      Log.debug (fun m ->
          m "CALL_EXTRACT: Unmatched call pattern: %s" (G.show_expr callee));
      None

(* Extract all calls from a function body and resolve them to fn_ids *)
let extract_calls ~(lang : Lang.t) ?(object_mappings = [])
    ?(module_imports = []) ?(class_hierarchy = []) ?(all_funcs = [])
    ?(caller_parent_path = []) (fdef : G.function_definition) :
    (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for function");
  let calls = ref [] in
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
                (match identify_callee ~lang ~object_mappings ~module_imports ~class_hierarchy
                  ~all_funcs ~caller_parent_path arg_exp with
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
            (match identify_callee ~lang ~object_mappings ~module_imports ~class_hierarchy
                  ~all_funcs ~caller_parent_path ~call_arity callee with
            | Some fn_id ->
                (* For DotAccess calls, use the method name token so it
                   matches the method_tok lookup in get_signature_for_object.
                   Exception: Ruby's ClassName.new() — use the class name
                   token (top of expression) since the constructor machinery
                   uses tok_of_eorig which points to the class name. *)
                let tok =
                  match callee.G.e with
                  | G.DotAccess (_, _, G.FN (G.Id (("new", _), _)))
                    when Lang.(lang =*= Ruby) ->
                      (match AST_generic_helpers.ii_of_any (G.E e) with
                      | tok :: _ -> tok
                      | [] -> Tok.unsafe_fake_tok "")
                  | G.DotAccess (_, _, G.FN (G.Id ((_, method_tok), _))) ->
                      method_tok
                  | _ ->
                      (match AST_generic_helpers.ii_of_any (G.E e) with
                      | tok :: _ -> tok
                      | [] -> Tok.unsafe_fake_tok "")
                in
                calls := (fn_id, tok) :: !calls
            | None ->
                (* Invoke-method pattern: var.run() where var is a lambda.
                   If the method name is a configured invoke method, look for
                   a lambda with the receiver's name in the current scope. *)
                let invoke_methods = (Lang_config.get lang).invoke_methods in
                (match callee.G.e with
                | G.DotAccess ({ e = G.N (G.Id ((var_name, _), _)); _ }, _,
                               G.FN (G.Id ((method_name, method_tok), _)))
                  when List.mem method_name invoke_methods ->
                    let lambda_match =
                      find_func_in_scope all_funcs caller_parent_path var_name
                    in
                    (match lambda_match with
                    | Some f -> calls := (f.fn_id, method_tok) :: !calls
                    | None -> ())
                | _ -> ()));
            (* Check arguments for unresolved function calls (Ruby-style) *)
            List.iter check_arg_for_unresolved_function_call args_list;
            (* Visit callee expression for nested calls (e.g., Ruby's File.open(path_for(x)) do ... end
               where the callee is itself a Call containing path_for(x) in its args) *)
            self#visit_expr env callee;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | G.New (_tok, ty, _id_info, args) ->
            (* Constructor call: new ClassName(args).
               Use the class name token so it matches the eorig token
               in class_construction's constructor expression. *)
            (match resolve_constructor_from_type ~lang ~all_funcs ty with
            | Some fn_id ->
                let tok =
                  match AST_generic_helpers.ii_of_any (G.T ty) with
                  | tok :: _ -> tok
                  | [] -> Tok.unsafe_fake_tok ""
                in
                calls := (fn_id, tok) :: !calls
            | None -> ());
            let (_, args_list, _) = args in
            List.iter check_arg_for_unresolved_function_call args_list;
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id and tok *)
  !calls |> dedup_fn_ids

(* Extract calls from top-level statements (outside any function).
   This returns a list of (callee_fn_id, call_tok) pairs. *)
let extract_toplevel_calls ~(lang : Lang.t) ?(object_mappings = [])
    ?(module_imports = []) ?(class_hierarchy = []) ?(all_funcs = [])
    (ast : G.program) : (fn_id * Tok.t) list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for top-level statements");
  let calls = ref [] in

  (* Build a set of byte ranges covered by function bodies *)
  let func_ranges = ref [] in
  List.iter (fun func ->
    let body_stmt = AST_generic_helpers.funcbody_to_stmt func.fdef.G.fbody in
    match AST_generic_helpers.range_of_any_opt (G.S body_stmt) with
    | Some (loc_start, loc_end) ->
        let range = Range.range_of_token_locations loc_start loc_end in
        func_ranges := (range.start, range.end_) :: !func_ranges
    | None -> ())
    all_funcs;

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
              match identify_callee ~lang ~object_mappings ~module_imports ~class_hierarchy
                  ~all_funcs ~caller_parent_path:[] callee with
              | Some fn_id ->
                  (* For DotAccess calls, use the method name token so it
                     matches the method_tok lookup in get_signature_for_object
                     (same logic as extract_calls). Without this, top-level
                     method calls like `obj.method()` keyed the edge at the
                     receiver token and never resolved. *)
                  let tok =
                    match callee.G.e with
                    | G.DotAccess (_, _, G.FN (G.Id (("new", _), _)))
                      when Lang.(lang =*= Ruby) ->
                        (match AST_generic_helpers.ii_of_any (G.E e) with
                        | tok :: _ -> tok
                        | [] -> Tok.unsafe_fake_tok "")
                    | G.DotAccess (_, _, G.FN (G.Id ((_, method_tok), _))) ->
                        method_tok
                    | _ ->
                        (match AST_generic_helpers.ii_of_any (G.E e) with
                        | tok :: _ -> tok
                        | [] -> Tok.unsafe_fake_tok "")
                  in
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

(* Helper to extract all callback names referenced by an argument expression.
   Handles:
     - foo, &foo, Module.foo, module.func (DotAccess)
     - Elixir &func/n (ShortLambda wrapping Call)
     - Record { cb: handler, ... } and Dict { "cb": handler, ... } — recurse
       into each entry's value
     - List/Tuple/Array/Set [handler, ...] — recurse into each element
     - Variable aliasing a record/container via [id_svalue]
       (set by [Dataflow_svalue] during parsing) — recurse into the svalue
   Returns a list of (callback_name, tok, shortlambda_tmp_opt).
   - shortlambda_tmp_opt is Some IL.name for the _tmp wrapper node when this
     is an Elixir ShortLambda.
   Over-approximates on purpose: any function-ref nested anywhere in the
   argument is treated as a potential callback. Precision at per-offset
   granularity is handled later by Sig_inst's offset-walk. *)
let rec extract_callbacks_from_arg ~(lang : Lang.t) (arg_expr : G.expr) :
    (IL.name * Tok.t * IL.name option) list =
  match arg_expr.G.e with
  (* Plain identifier: foo — may be a function name directly, OR a variable
     whose id_svalue wraps a record/container we should walk through. We
     always emit the direct interpretation (so [handler] still resolves
     even when it has no svalue), plus any svalue-walk recursion. *)
  | G.N (G.Id (id, id_info)) ->
      let direct =
        [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
      in
      let via_svalue =
        match !(id_info.id_svalue) with
        | Some (G.Sym inner) -> extract_callbacks_from_arg ~lang inner
        | _ -> []
      in
      direct @ via_svalue
  (* Address-of operator: &foo (C/C++ function pointers) *)
  | G.Ref (_, { e = G.N (G.Id (id, id_info)); _ }) ->
      [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
  (* Qualified identifier: Module.foo *)
  | G.N (G.IdQualified { name_last = id, _; name_info; _ }) ->
      [ (AST_to_IL.var_of_id_info id name_info, snd id, None) ]
  (* DotAccess: module.func or obj.method - common in Python/JS *)
  | G.DotAccess (_, _, G.FN (G.Id (id, id_info))) ->
      [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
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
          ( {
              e =
                ( G.N (G.Id (id, id_info))
                | G.DotAccess (_, _, G.FN (G.Id (id, id_info))) );
              _;
            },
            _ ) ->
          let callback_name = AST_to_IL.var_of_id_info id id_info in
          let tmp_name =
            Visit_function_defs.synth_lambda_il_name_of_tok shortlambda_tok
          in
          [ (callback_name, snd id, Some tmp_name) ]
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
  (* List/Tuple/Array/Set literal: recurse into each element *)
  | G.Container ((G.List | G.Tuple | G.Array | G.Set), (_, xs, _)) ->
      List.concat_map (extract_callbacks_from_arg ~lang) xs
  (* Ruby [method(:name)]: a callable reference to the named function.
     Sym-prop carries the [Call(method, [Atom :name])] expression on
     the callback variable's [id_svalue], so the recursion above
     reaches us for an aliased binding [cb = method(:name); apply_cb(cb, ...)]. *)
  | G.Call
      ( { e = G.N (G.Id (("method", _), _)); _ },
        (_, [ G.Arg { e = G.L (G.Atom (_, atom_ident)); _ } ], _) )
    when lang =*= Lang.Ruby ->
      let synthetic_name =
        IL.
          {
            ident = atom_ident;
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          }
      in
      [ (synthetic_name, snd atom_ident, None) ]
  | _ -> []


(* Helper to identify a callback fn_id, checking nested functions in same scope first *)
let identify_callback ?(all_funcs = []) ?(caller_parent_path = [])
    (callback_name : IL.name) : fn_id option =
  let callback_name_str = fst callback_name.IL.ident in
  (* Extract class from caller_parent_path if present *)
  let current_class = match caller_parent_path with
    | Some cls :: _ -> Some cls
    | _ -> None
  in
  (* First check if it's a nested function in the same scope - position-aware match *)
  let nested_match =
    find_func_in_scope all_funcs caller_parent_path callback_name_str
  in

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
              match f.fn_id with
              | [Some c; Some _] ->
                  fst c.IL.ident = class_name_str
                  && func_info_name_matches f callback_name_str
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
            match f.fn_id with
            | [None; Some _] -> func_info_name_matches f callback_name_str
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
(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry multiple callbacks when it's a record/list
   containing several function references, or a variable whose [id_svalue]
   wraps such a container. See [extract_callbacks_from_arg]. *)
let try_identify_callback_args ~lang ~all_funcs ~caller_parent_path (arg : G.argument) :
    (fn_id * Tok.t * IL.name option) list =
  let resolve_in_expr expr =
    (* Also handle this.foo pattern *)
    let direct_this =
      match expr.G.e with
      | G.DotAccess
          ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
            _,
            G.FN (G.Id (id, id_info)) ) ->
          [ (AST_to_IL.var_of_id_info id id_info, snd id, None) ]
      | _ -> []
    in
    let candidates = direct_this @ extract_callbacks_from_arg ~lang expr in
    List.filter_map
      (fun (callback_name, tok, tmp_opt) ->
        (* Use real token from the callback argument *)
        identify_callback ~all_funcs ~caller_parent_path callback_name
        |> Option.map (fun fn_id -> (fn_id, tok, tmp_opt)))
      candidates
  in
  match arg with
  | G.Arg expr -> resolve_in_expr expr
  (* Keyword args: Ruby [my_hof(cb: h, data: x)] and Python [f(cb=h, data=x)]
     lower each key-value as an [ArgKwd]. The key is a tag name; recurse on
     the value expression to extract any nested callback references. *)
  | G.ArgKwd (_, expr) | G.ArgKwdOptional (_, expr) -> resolve_in_expr expr
  | G.ArgType _ | G.OtherArg _ -> []

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
let extract_hof_callbacks_from_call ~lang ~method_hofs ~function_hofs ~all_funcs
    ~caller_parent_path (callee : G.expr) (args : G.arguments) :
    (fn_id * Tok.t * IL.name option) list =
  let try_arg arg =
    try_identify_callback_args ~lang ~all_funcs ~caller_parent_path arg
  in
  let try_arg_at_index idx =
    match List.nth_opt (Tok.unbracket args) idx with
    | Some arg -> try_arg arg
    | None -> []
  in
  (* Check ALL arguments for function references - any function passed as
     arg, nested inside a record/dict/list literal, or reachable via
     [id_svalue] through such a container, is treated as a callback. *)
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

(* Extract HOF callbacks, returning (fn_id, tok, tmp_opt) tuples.
   tmp_opt is Some IL.name for ShortLambda callbacks that need a _tmp intermediate node. *)
let extract_hof_callbacks ?(_object_mappings = []) ?(all_funcs = [])
    ?(caller_parent_path = [])
    ~(lang : Lang.t) (fdef : G.function_definition) : (fn_id * Tok.t * IL.name option) list =
  let hof_configs = (Lang_config.get lang).hof_configs in
  let method_hofs =
    hof_configs |> List.concat_map (function
      | Lang_config.MethodHOF { methods; _ } -> methods
      | Lang_config.ReturningFunctionHOF { methods; _ } -> methods
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
        (* Ruby/Scala block pattern: f(args) { block } is Call(Call(callee, inner_args), [block]).
           Merge inner_args and block args so the HOF detection sees all arguments together. *)
        | G.Call ({ e = G.Call (callee, inner_args); _ },
                  (_, ([ G.Arg { G.e = G.Lambda _; _ } ] as outer_arg), _))
          when Lang.(lang =*= Ruby || lang =*= Scala) ->
            let merged_args = Tok.unsafe_fake_bracket
              (Tok.unbracket inner_args @ outer_arg) in
            let found = extract_hof_callbacks_from_call
              ~lang ~method_hofs ~function_hofs ~all_funcs ~caller_parent_path
              callee merged_args
            in
            callbacks := found @ !callbacks
        | G.Call (callee, args) ->
            let found = extract_hof_callbacks_from_call
              ~lang ~method_hofs ~function_hofs ~all_funcs ~caller_parent_path
              callee args
            in
            callbacks := found @ !callbacks
        | _ -> ());
        super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  !callbacks

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : Call_graph.G.t =
  let graph = Call_graph.G.create () in
  (* Cross-file resolution context: module imports (for `import * as m; m.f()`)
   * and the class hierarchy (for method resolution through inheritance). *)
  let module_imports = module_imports_of_ast ast in
  let class_hierarchy = collect_class_hierarchy ast in

  (* Create a special top_level node to represent code outside functions *)
  let top_level_node : node =
    let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
    let il_name = IL.{ ident = ("<top_level>", fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () } in
    Function_id.of_il_name il_name
  in
  Call_graph.G.add_vertex graph top_level_node;

  let funcs =
    Visit_function_defs.fold_with_parent_path ~lang
      (fun funcs opt_ent parent_path fdef ->
        match fn_id_of_entity ~lang opt_ent parent_path fdef with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            (* Add vertex using the node (last element of fn_id) *)
            (match fn_id_to_node fn_id with
            | Some node -> Call_graph.G.add_vertex graph node
            | None -> ());
            func :: funcs
        | None -> funcs)
      [] ast
  in
  (* Visit all calls in the AST, tracking the current function context *)
  Visit_function_defs.visit_with_parent_path ~lang
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
            extract_calls ~lang ~object_mappings ~module_imports
              ~class_hierarchy ~all_funcs:funcs ~caller_parent_path:fn_id fdef
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
            extract_hof_callbacks ~_object_mappings:object_mappings ~all_funcs:funcs ~caller_parent_path:fn_id ~lang fdef
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
    extract_toplevel_calls ~lang ~object_mappings ~module_imports
      ~class_hierarchy ~all_funcs:funcs ast
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
        | Lang_config.MethodHOF { methods; _ } -> methods
        | Lang_config.ReturningFunctionHOF { methods; _ } -> methods
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
        ~lang ~method_hofs ~function_hofs ~all_funcs:funcs ~caller_parent_path:[]
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
      let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
      let il_name = IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = G.empty_id_info () } in
      Function_id.of_il_name il_name
    in
    Call_graph.G.add_vertex graph class_init_node;

    (* Find all methods in this class *)
    let class_methods =
      List.filter
        (fun func ->
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

(* Identify functions that contain byte ranges (from pattern matches) *)
let find_functions_containing_ranges ~(lang : Lang.t) (ast : G.program)
    (ranges : (Range.t * Fpath.t) list) : Function_id.t list =
  (* Hash table to track ALL functions containing each range, along with function size.
   * Each range is paired with the file it lives in; inter-file ASTs merge files that
   * each keep their own 0-based offsets, so a range from one file can byte-fall inside
   * a function in another. The containment check requires the range's file to match
   * the enclosing function's file (no-op for single-file scans). *)
  let range_to_funcs : (Range.t * Fpath.t, (fn_id * int) list) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun rf -> Hashtbl.add range_to_funcs rf []) ranges;

  let visitor = object (self)
    inherit [_] G.iter_no_id_info as super
    val current_class : G.name option ref = ref None
    val parent_path : IL.name option list ref = ref []
    (* True while visiting inside any function definition. Used to skip
       lambdas that are nested inside another function — for source/sink
       containment we want the enclosing non-lambda fn, since closure-like
       lambdas are handled by the IL-level inner extraction, not by treating
       them as the source/sink-bearing function. Top-level lambdas (e.g. JS
       [const f = () => …]) are not nested and are still registered as the
       containing function. *)
    val inside_function : bool ref = ref false

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
              let class_file = loc_start.Tok.pos.file in

              (* For each range, check if it's inside this class *)
              List.iter (fun ((range : Range.t), rfile) ->
                if Fpath.equal rfile class_file
                   && class_start <= range.Range.start && range.Range.end_ <= class_end then (
                  (* This class contains this range - add it to the list *)
                  match !current_class with
                  | Some class_g_name ->
                      let class_il_name = AST_to_IL.var_of_name class_g_name in
                      let class_str = fst class_il_name.IL.ident in
                      let class_node_name =
                        let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
                        Some IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
                      in
                      let class_fn_id = [None; class_node_name] in
                      let existing = Hashtbl.find range_to_funcs (range, rfile) in
                      if not (List.exists (fun (fid, _) -> equal_fn_id fid class_fn_id) existing) then
                        Hashtbl.replace range_to_funcs (range, rfile) ((class_fn_id, class_size) :: existing)
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env def
          | None -> super#visit_definition env def);
          current_class := old_class
      | G.FuncDef fdef | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Get the entire function definition range (including parameters) *)
          let func_range_opt = AST_generic_helpers.range_of_any_opt (G.Def def) in
          (* A lambda nested inside another function is closure-like; we don't
             want it to win as the source/sink-containing fn for ranges inside
             its body. *)
          let is_nested_lambda =
            !inside_function
            &&
            (match fst fdef.fkind with
             | G.LambdaKind | G.Arrow -> true
             | _ -> false)
          in
          (match func_range_opt with
          | Some (loc_start, loc_end) ->
              let range = Range.range_of_token_locations loc_start loc_end in
              let func_start = range.start in
              let func_end = range.end_ in
              let func_size = func_end - func_start in
              let func_file = loc_start.Tok.pos.file in

              (* For each range, check if it's inside this function *)
              if not is_nested_lambda then
                List.iter (fun ((range : Range.t), rfile) ->
                  if Fpath.equal rfile func_file
                     && func_start <= range.Range.start && range.Range.end_ <= func_end then (
                    (* This function contains this range - add it to the list *)
                    (* Use proper parent_path tracking for nested functions *)
                    let class_il = Option.bind !current_class self#g_name_to_il_name in
                    let visitor_parent_path =
                      match !parent_path with
                      | [] -> [class_il]
                      | _ -> !parent_path
                    in
                    match fn_id_of_entity ~lang (Some ent) visitor_parent_path fdef with
                    | Some fn_id ->
                        let existing = Hashtbl.find range_to_funcs (range, rfile) in
                        if not (List.exists (fun (fid, _) -> equal_fn_id fid fn_id) existing) then
                          Hashtbl.replace range_to_funcs (range, rfile) ((fn_id, func_size) :: existing)
                    | None -> ()
                  )
                ) ranges;

              (* Push current function onto parent_path for nested functions,
                 and mark that we are now inside a function. *)
              let class_il = Option.bind !current_class self#g_name_to_il_name in
              let func_il = self#entity_to_il_name ent in
              let current_fn_id =
                match !parent_path with
                | [] -> [class_il; func_il]
                | _ -> !parent_path @ [func_il]
              in
              Common.save_excursion_unsafe parent_path current_fn_id (fun () ->
                Common.save_excursion_unsafe inside_function true (fun () ->
                  super#visit_definition env def))
          | None -> super#visit_definition env def)
      | _ -> super#visit_definition env def
  end in

  visitor#visit_program () ast;

  (* Now select the innermost (smallest) function for each range *)
  List.fold_left (fun matching_funcs (range, rfile) ->
    let funcs_list = Hashtbl.find range_to_funcs (range, rfile) in
    if List.is_empty funcs_list then
      (* No function contains this range - it's at top level *)
      let top_level_name =
        let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
        Some IL.{ ident = ("<top_level>", fake_tok);
                  sid = G.SId.unsafe_default;
                  id_info = AST_generic.empty_id_info () }
      in
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
