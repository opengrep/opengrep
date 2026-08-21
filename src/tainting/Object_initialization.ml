(* Object Initialization Detection for All Languages
 *
 * This module provides comprehensive object initialization detection
 * for all languages supported by Semgrep, enabling sophisticated
 * taint analysis across object constructors and method calls.
 *)

module G = AST_generic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Object mapping: variable -> class *)
type object_mapping = G.name * G.name

(* Matcher type: extracts class name from constructor expression *)
type matcher = G.expr -> G.name list -> G.name option

(*****************************************************************************)
(* Common Matchers *)
(*****************************************************************************)

(* Matched on leaf name, so a qualified ref matches a class by its simple name. *)
let is_known_class (name : G.name) (class_names : G.name list) : bool =
  match Ty_leaf.leaf_of_name name with
  | None -> false
  | Some s1 ->
    List.exists
      (fun class_name ->
        match Ty_leaf.leaf_of_name class_name with
        | Some s2 -> String.equal s1 s2
        | None -> false)
      class_names

(* Check if string starts with uppercase *)
let is_uppercase_start str =
  String.length str > 0 && Char.uppercase_ascii str.[0] = str.[0]

(* Matcher: new ClassName(args) - basic form *)
let match_new_basic rval_expr class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | _ -> None)
  | _ -> None

(* Matcher: new ClassName(args) - with TyExpr fallback *)
let match_new_with_tyexpr rval_expr class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | G.TyExpr expr -> (
          match expr.G.e with
          | G.N (G.Id ((_, _), _) as name) when is_known_class name class_names ->
              Some name
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Matcher: ClassName(args) - call with uppercase class name *)
let match_call_uppercase rval_expr class_names =
  match rval_expr.G.e with
  | G.Call (class_expr, _) -> (
      match class_expr.G.e with
      | G.N (G.Id ((str, _), _) as name)
        when is_uppercase_start str && is_known_class name class_names ->
          Some name
      | _ -> None)
  | _ -> None

(* Matcher: ClassName.new(args) - Ruby/Rust style *)
let match_dot_new rval_expr class_names =
  match rval_expr.G.e with
  | G.Call (dot_access, _) -> (
      match dot_access.G.e with
      | G.DotAccess (class_expr, _, G.FN (G.Id (("new", _), _))) -> (
          match class_expr.G.e with
          | G.N name when is_known_class name class_names -> Some name
          | _ -> None)
      | _ -> None)
  | _ -> None

let rec match_rust_struct rval_expr class_names =
  match rval_expr.G.e with
  | G.Constructor (name, _) when is_known_class name class_names -> Some name
  | G.Ref (_, inner) -> match_rust_struct inner class_names
  | _ -> None

(* Matcher: Go &Struct{} or Struct{} *)
let match_go_struct rval_expr class_names =
  match rval_expr.G.e with
  | G.Ref (_, struct_expr) -> (
      match struct_expr.G.e with
      | G.New (_, struct_type, _, _) -> (
          match struct_type.G.t with
          | G.TyN name when is_known_class name class_names -> Some name
          | _ -> None)
      | _ -> None)
  | G.New (_, struct_type, _, _) -> (
      match struct_type.G.t with
      | G.TyN name when is_known_class name class_names -> Some name
      | _ -> None)
  | _ -> None

(* Combine multiple matchers - try each until one succeeds *)
let combine matchers rval_expr class_names =
  List.find_map (fun m -> m rval_expr class_names) matchers

(* Apex: doesn't validate against class_names *)
let match_apex rval_expr _class_names =
  match rval_expr.G.e with
  | G.New (_, class_type, _, _) -> (
      match class_type.G.t with
      | G.TyN name -> Some name
      | _ -> None)
  | _ -> None

(*****************************************************************************)
(* Language Matcher Lookup *)
(*****************************************************************************)

let get_matcher (lang : Lang.t) : matcher option =
  match lang with
  | Lang.Java | Lang.Csharp | Lang.Vb -> Some match_new_basic
  | Lang.Php -> Some match_new_with_tyexpr
  | Lang.Python | Lang.Python2 | Lang.Python3 | Lang.Swift -> Some match_call_uppercase
  | Lang.Crystal | Lang.Ruby -> Some match_dot_new
  | Lang.Rust -> Some (combine [match_dot_new; match_rust_struct])
  | Lang.Go -> Some match_go_struct
  | Lang.Apex -> Some match_apex
  | Lang.Kotlin | Lang.Cpp -> Some (combine [match_new_basic; match_call_uppercase])
  | Lang.Scala | Lang.Js | Lang.Ts | Lang.Dart -> Some (combine [match_new_with_tyexpr; match_call_uppercase])
  | _ -> None

(*****************************************************************************)
(* Class Detection *)
(*****************************************************************************)

(* Collect all class names from the AST *)
let collect_class_names (ast : G.program) : G.name list =
  let class_names = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_definition () def =
        (match def with
        | entity, G.ClassDef _ -> (
            match entity.G.name with
            | G.EN name -> class_names := name :: !class_names
            | _ -> ())
        (* Handle Go struct definitions - TypeDef with TyRecordAnon *)
        | entity, G.TypeDef type_def -> (
            match (entity.G.name, type_def.G.tbody) with
            | G.EN name, G.NewType { G.t = G.TyRecordAnon ((G.Class, _), _); _ }
              ->
                class_names := name :: !class_names
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
  !class_names

(*****************************************************************************)
(* Object Initialization Detection *)
(*****************************************************************************)

(* Extract class name from constructor call expression *)
let extract_class_name_from_constructor (rval_expr : G.expr) (lang : Lang.t)
    (class_names : G.name list) : G.name option =
  match get_matcher lang with
  | Some matcher -> matcher rval_expr class_names
  | None -> None

(* [extra_class_names] supplies project-wide/interfile classes, deduped. *)
let detect_object_initialization
    ?(extra_class_names : G.name list = [])
    (ast : G.program) (lang : Lang.t) :
    object_mapping list =
  let class_names =
    let module StringSet = Set.Make (String) in
    let _, acc =
      List.fold_left (fun (seen, acc) n ->
        match n with
        | G.Id ((s, _), _) when not (StringSet.mem s seen) ->
          (StringSet.add s seen, n :: acc)
        | _ -> (seen, acc)
      ) (StringSet.empty, []) (collect_class_names ast @ extra_class_names)
    in
    acc
  in
  let object_mappings = ref [] in

  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_stmt () stmt =
        (match stmt.G.s with
        | G.DefStmt (entity, def_kind) -> (
            match def_kind with
            | G.VarDef var_def -> (
                (* Rust [let s = ...] gives [EPattern], not [EN]; get the leaf id. *)
                let normalized_name =
                  match entity.G.name with
                  | G.EN _ -> entity.G.name
                  | G.EPattern p ->
                      let rec leaf = function
                        | G.PatId (id, info) -> Some (G.Id (id, info))
                        | G.PatTyped (inner, _) -> leaf inner
                        | _ -> None
                      in
                      (match leaf p with
                       | Some n -> G.EN n
                       | None -> entity.G.name)
                  | _ -> entity.G.name
                in
                match (normalized_name, var_def.G.vinit) with
                | G.EN var_name, Some init_expr -> (
                    let class_name =
                      extract_class_name_from_constructor init_expr lang
                        class_names
                    in
                    let class_name =
                      match (class_name, lang) with
                      | Some cls, _ -> Some cls
                      | None, Lang.Cpp -> (
                          match var_def.G.vtype with
                          | Some var_type -> (
                              match var_type.G.t with
                              | G.TyN name when is_known_class name class_names
                                ->
                                  Some name
                              | _ -> None)
                          | None -> None)
                      | None, _ -> None
                    in
                    match class_name with
                    | Some cls ->
                        object_mappings := (var_name, cls) :: !object_mappings
                    | _ -> ())
                | G.EN var_name, None
                  when Lang.equal lang Lang.Cpp
                       || Lang.equal lang Lang.Rust -> (
                    match var_def.G.vtype with
                    | Some var_type -> (
                        match Ty_leaf.inner_class_name_of_ty
                                ~through_funty:true var_type with
                        | Some name when is_known_class name class_names ->
                          object_mappings := (var_name, name) :: !object_mappings
                        | _ -> ())
                    | None -> ())
                | _ -> ())
            | _ -> ())
        | G.ExprStmt (expr, _) -> (
            match expr.G.e with
            | G.Assign (lval_expr, _, rval_expr) -> (
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  | G.DotAccess (obj_expr, _, G.FN _)
                    when Lang.equal lang Lang.Go -> (
                      match obj_expr.G.e with
                      | G.N obj_name -> (
                          let existing_mapping =
                            List.find_opt
                              (fun (var, _) ->
                                match (var, obj_name) with
                                | G.Id ((str1, _), _), G.Id ((str2, _), _) ->
                                    str1 = str2
                                | _ -> false)
                              !object_mappings
                          in
                          match existing_mapping with
                          | Some (_, _) -> None
                          | None -> Some obj_name)
                      | _ -> None)
                  | _ -> None
                in
                let class_name =
                  extract_class_name_from_constructor rval_expr lang class_names
                in
                match (var_name, class_name) with
                | Some var, Some cls ->
                    object_mappings := (var, cls) :: !object_mappings
                | _ -> ())
            | G.AssignOp (lval_expr, _, rval_expr) -> (
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  | _ -> None
                in
                let class_name =
                  extract_class_name_from_constructor rval_expr lang class_names
                in
                match (var_name, class_name) with
                | Some var, Some cls ->
                    object_mappings := (var, cls) :: !object_mappings
                | _ -> ())
            | _ -> ())
        | _ -> ());
        super#visit_stmt () stmt

      method! visit_definition () def =
        (match def with
        | entity, G.VarDef var_def -> (
            match (entity.G.name, var_def.G.vinit) with
            | G.EN var_name, Some init_expr -> (
                let class_name =
                  extract_class_name_from_constructor init_expr lang class_names
                in
                match class_name with
                | Some cls ->
                    object_mappings := (var_name, cls) :: !object_mappings
                | _ -> ())
            | _ -> ())
        | _ -> ());
        super#visit_definition () def
    end
  in

  visitor#visit_program () ast;
  !object_mappings

(* Stamp each mapping's class onto every occurrence's [id_type] so consumers
   read it off the AST (fill-on-None; a [TyFun] [id_type] is overwritten —
   C++'s most vexing parse). First mapping per leaf wins. *)
let stamp_id_types (mappings : object_mapping list) (ast : G.program) : unit =
  let sid_of_name (n : G.name) : G.SId.t option =
    match n with
    | G.Id (_, info) -> (
        match !(info.G.id_resolved) with Some (_, sid) -> Some sid | None -> None)
    | _ -> None
  in
  (* sid equality required only when both sides have resolved sids; else
     name-only — which can stamp a same-named var from an unrelated scope
     when sids are unresolved (Go/C++ often lack one). *)
  let by_leaf : (string, (G.SId.t option * G.name) list) Hashtbl.t =
    Hashtbl.create (max 16 (2 * List.length mappings))
  in
  List.iter
    (fun (lhs, ty) ->
      match Ty_leaf.leaf_of_name lhs with
      | Some s ->
        let prev = Option.value (Hashtbl.find_opt by_leaf s) ~default:[] in
        Hashtbl.replace by_leaf s ((sid_of_name lhs, ty) :: prev)
      | None -> ())
    mappings;
  Hashtbl.filter_map_inplace (fun _ l -> Some (List.rev l)) by_leaf;
  let class_of_occurrence (n : G.name) : G.name option =
    let sid_o = sid_of_name n in
    match Ty_leaf.leaf_of_name n with
    | None -> None
    | Some s -> (
        match Hashtbl.find_opt by_leaf s with
        | None -> None
        | Some cands ->
          List.find_map
            (fun (sid_m, ty) ->
              match (sid_o, sid_m) with
              | Some a, Some b -> if G.SId.equal a b then Some ty else None
              | _ -> Some ty)
            cands)
  in
  (* A stamped [id_type] carries the class name — strings and location
     tokens — but NOT live [id_info]s: a name stamped with its AST
     [id_info] embeds that node's own (possibly later-stamped) refs,
     growing payloads into cross-file webs (cyclic ones on
     self-referential classes) that every structural traversal then
     crawls or overflows on. *)
  let detacher =
    object
      inherit [_] G.map
      method! visit_id_info _env _ii = G.empty_id_info ()
    end
  in
  let stamper =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_expr () e =
        (match e.G.e with
        | G.N (G.Id (_, info) as n)
          when (match !(info.G.id_type) with
               | None | Some { G.t = G.TyFun _; _ } -> true
               | Some _ -> false) -> (
            match class_of_occurrence n with
            | Some ty ->
                info.G.id_type :=
                  Some { G.t = G.TyN (detacher#visit_name () ty);
                         G.t_attrs = [] }
            | None -> ())
        | _ -> ());
        super#visit_expr () e
    end
  in
  stamper#visit_program () ast

(*****************************************************************************)
(* Constructor Detection Utilities *)
(*****************************************************************************)

(* Check if a function is a constructor for the given language *)
let is_constructor (lang : Lang.t) (func_name : string)
    (class_name_opt : string option) : bool =
  let config = Lang_config.get lang in
  List.exists (String.equal func_name) config.constructor_names
  ||
  Option.fold ~none:false ~some:(String.equal func_name) class_name_opt

(* Get all constructor method names for a language *)
let get_constructor_names (lang : Lang.t) : string list =
  (Lang_config.get lang).constructor_names

(* Check if language uses 'new' keyword *)
let uses_new_keyword (lang : Lang.t) : bool =
  (Lang_config.get lang).uses_new_keyword

(*****************************************************************************)
(* Unified Constructor Execution *)
(*****************************************************************************)

let execute_unified_constructor constructor_exp args args_taints
    check_function_call_fn env =
  check_function_call_fn env constructor_exp args args_taints

let execute_constructor_call lang constructor_name class_name args =
  if is_constructor lang constructor_name class_name then
    Some (constructor_name, class_name, args)
  else None

(*****************************************************************************)
(* C++ Constructor Statement Detection *)
(*****************************************************************************)

let detect_cpp_constructor_defstmt stmt class_names =
  match stmt.G.s with
  | G.DefStmt (ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) -> (
      match (ent.name, ty.G.t) with
      | G.EN (G.Id ((var_name, _), _)), G.TyFun (params, return_type) -> (
          match return_type.G.t with
          | G.TyN (G.Id ((class_name, _), _) as name)
            when is_known_class name class_names ->
              Some (var_name, class_name, params)
          | _ -> None)
      | _ -> None)
  | _ -> None

(*****************************************************************************)
(* Debugging and Display *)
(*****************************************************************************)

let show_object_mapping (var_name, class_name) =
  let var_str =
    match var_name with
    | G.Id ((str, _), _) -> str
    | _ -> "???"
  in
  let class_str =
    match class_name with
    | G.Id ((str, _), _) -> str
    | _ -> "???"
  in
  Printf.sprintf "%s -> %s" var_str class_str

let show_object_mappings (mappings : object_mapping list) =
  mappings
  |> List.map show_object_mapping
  |> String.concat "; " |> Printf.sprintf "[%s]"
