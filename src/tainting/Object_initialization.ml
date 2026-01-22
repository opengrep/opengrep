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

(* Check if a name is in the known classes list *)
let is_known_class (name : G.name) (class_names : G.name list) : bool =
  List.exists
    (fun class_name ->
      match (name, class_name) with
      | G.Id ((str1, _), _), G.Id ((str2, _), _) -> str1 = str2
      | _ -> false)
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
  | Lang.Ruby | Lang.Rust -> Some match_dot_new
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

(* Object initialization detection for different languages *)
let detect_object_initialization (ast : G.program) (lang : Lang.t) :
    object_mapping list =
  let class_names = collect_class_names ast in
  let object_mappings = ref [] in

  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_stmt () stmt =
        (match stmt.G.s with
        | G.DefStmt (entity, def_kind) -> (
            match def_kind with
            | G.VarDef var_def -> (
                match (entity.G.name, var_def.G.vinit) with
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
                | G.EN var_name, None when lang = Lang.Cpp -> (
                    match var_def.G.vtype with
                    | Some var_type -> (
                        match var_type.G.t with
                        | G.TyN name when is_known_class name class_names ->
                            object_mappings :=
                              (var_name, name) :: !object_mappings
                        | G.TyFun (_, return_type) -> (
                            match return_type.G.t with
                            | G.TyN name when is_known_class name class_names ->
                                object_mappings :=
                                  (var_name, name) :: !object_mappings
                            | _ -> ())
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
                  | G.DotAccess (obj_expr, _, G.FN _) when lang = Lang.Go -> (
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

(*****************************************************************************)
(* Constructor Detection Utilities *)
(*****************************************************************************)

(* Check if a function is a constructor for the given language *)
let is_constructor (lang : Lang.t) (func_name : string)
    (class_name_opt : string option) : bool =
  let config = Lang_config.get lang in
  List.mem func_name config.constructor_names
  ||
  match class_name_opt with
  | Some class_name ->
      func_name = class_name
  | None -> false

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
  match check_function_call_fn env constructor_exp args args_taints with
  | Some (call_taints, shape, updated_lval_env) ->
      Some (call_taints, shape, updated_lval_env)
  | None -> None

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
