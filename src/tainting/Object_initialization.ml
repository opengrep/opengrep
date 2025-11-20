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

(* Constructor pattern for a language *)
type constructor_pattern = {
  (* Language-specific pattern matching function *)
  match_pattern : G.expr -> G.name list -> G.name option;
  (* Constructor method names for this language *)
  constructor_names : string list;
  (* Whether the language uses explicit 'new' keyword *)
  uses_new_keyword : bool;
}

(*****************************************************************************)
(* Language-specific Constructor Patterns *)
(*****************************************************************************)

(* Check if a name is in the known classes list *)
let is_known_class (name : G.name) (class_names : G.name list) : bool =
  List.exists
    (fun class_name ->
      match (name, class_name) with
      | G.Id ((str1, _), _), G.Id ((str2, _), _) -> str1 = str2
      | _ -> false)
    class_names

(* Java: new ClassName(args) *)
let java_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | _ -> None);
    constructor_names = [ "<init>" ];
    uses_new_keyword = true;
  }

(* C#: new ClassName(args) - similar to Java *)
let csharp_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | _ -> None);
    constructor_names = [ ".ctor" ];
    uses_new_keyword = true;
  }

(* VB.NET: New ClassName(args) - similar to C# *)
let vb_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | _ -> None);
    constructor_names = [ "New" ];
    uses_new_keyword = true;
  }

(* Kotlin: ClassName(args) or new ClassName(args) *)
let kotlin_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "<init>"; "init"; "constructor" ];
    uses_new_keyword = false;
    (* Optional in Kotlin *)
  }

(* Scala: new ClassName(args) or ClassName(args) *)
let scala_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | G.TyExpr expr -> (
                match expr.G.e with
                | G.N (G.Id ((_, _), _) as name)
                  when is_known_class name class_names ->
                    Some name
                | _ -> None)
            | _ -> None)
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "<init>" ];
    uses_new_keyword = false;
    (* Optional in Scala *)
  }

(* Ruby: ClassName.new(args) *)
let ruby_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.Call (dot_access, _) -> (
            match dot_access.G.e with
            | G.DotAccess (class_expr, _, G.FN (G.Id (("new", _), _))) -> (
                match class_expr.G.e with
                | G.N name when is_known_class name class_names -> Some name
                | _ -> None)
            | _ -> None)
        | _ -> None);
    constructor_names = [ "initialize" ];
    uses_new_keyword = false;
  }

(* Python: ClassName(args) *)
let python_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "__init__" ];
    uses_new_keyword = false;
  }

(* JavaScript/TypeScript: new ClassName(args) or ClassName() *)
let javascript_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | G.TyExpr expr -> (
                match expr.G.e with
                | G.N (G.Id ((_, _), _) as name)
                  when is_known_class name class_names ->
                    Some name
                | _ -> None)
            | _ -> None)
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "constructor" ];
    uses_new_keyword = true;
    (* Preferred *)
  }

(* Swift: ClassName(args) *)
let swift_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "init" ];
    uses_new_keyword = false;
  }

(* Go: Go struct initialization patterns *)
let go_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        (* Handle &StructName{field: value} - Go reference to struct literal *)
        | G.Ref (_, struct_expr) -> (
            match struct_expr.G.e with
            | G.New (_, struct_type, _, (_, _args, _)) -> (
                match struct_type.G.t with
                | G.TyN name when is_known_class name class_names -> Some name
                | _ -> None)
            | _ -> None)
        (* Handle direct StructName{} - Go struct literal *)
        | G.New (_, struct_type, _, _) -> (
            match struct_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | _ -> None);
    constructor_names = [];
    (* Go uses struct literals, no named constructors *)
    uses_new_keyword = false;
  }

(* Rust: Rust object initialization patterns - simplified for now *)
let rust_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.Call (dot_access, _) -> (
            match dot_access.G.e with
            | G.DotAccess (class_expr, _, G.FN (G.Id (("new", _), _))) -> (
                match class_expr.G.e with
                | G.N name when is_known_class name class_names -> Some name
                | _ -> None)
            | _ -> None)
        | _ -> None);
    constructor_names = [ "new" ];
    uses_new_keyword = false;
  }

(* Dart: new ClassName(args) or ClassName(args) *)
let dart_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | G.TyExpr expr -> (
                match expr.G.e with
                | G.N (G.Id ((_, _), _) as name)
                  when is_known_class name class_names ->
                    Some name
                | _ -> None)
            | _ -> None)
        | G.Call (class_expr, _) -> (
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | _ -> None);
    constructor_names = [ "constructor" ];
    uses_new_keyword = false;
    (* Optional in modern Dart *)
  }

(* C++: User user(args) - direct constructor calls *)
let cpp_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            (* C++ new ClassName(args) *)
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | _ -> None)
        | G.Call (class_expr, _) -> (
            (* C++ ClassName(args) constructor call *)
            match class_expr.G.e with
            | G.N (G.Id ((str, _), _) as name) ->
                if
                  String.length str > 0
                  && Char.uppercase_ascii str.[0] = str.[0]
                  && is_known_class name class_names
                then Some name
                else None
            | _ -> None)
        | G.Container (G.Array, (_, _, _)) ->
            (* C++ User user(args) parsed as Container(Array, ...) - check variable type *)
            (* This pattern doesn't directly give us the class name, we need type info *)
            None
            (* Handle this in a different way - see vinit approach *)
        | _ -> None);
    constructor_names = [];
    (* C++ constructors have same name as class *)
    uses_new_keyword = false;
    (* C++ often uses direct constructor calls *)
  }

(* PHP: new ClassName(args) *)
let php_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | G.TyExpr expr -> (
                match expr.G.e with
                | G.N (G.Id ((_, _), _) as name)
                  when is_known_class name class_names ->
                    Some name
                | _ -> None)
            | _ -> None)
        | _ -> None);
    constructor_names = [ "__construct" ];
    uses_new_keyword = true;
  }

(* Apex: new ClassName(args) - similar to Java *)
let apex_constructor_pattern : constructor_pattern =
  {
    match_pattern =
      (fun rval_expr class_names ->
        match rval_expr.G.e with
        | G.New (_, class_type, _, _) -> (
            match class_type.G.t with
            | G.TyN name when is_known_class name class_names -> Some name
            | G.TyN name ->
                (* For anonymous classes, accept the interface name even if not in class_names *)
                Some name
            | _ -> None)
        | _ -> None);
    constructor_names = [ "<init>" ];
    uses_new_keyword = true;
  }

(*****************************************************************************)
(* Language Pattern Mapping *)
(*****************************************************************************)

let get_constructor_pattern (lang : Lang.t) : constructor_pattern option =
  match lang with
  | Lang.Java -> Some java_constructor_pattern
  | Lang.Apex -> Some apex_constructor_pattern
  | Lang.Csharp -> Some csharp_constructor_pattern
  | Lang.Vb -> Some vb_constructor_pattern
  | Lang.Kotlin -> Some kotlin_constructor_pattern
  | Lang.Scala -> Some scala_constructor_pattern
  | Lang.Ruby -> Some ruby_constructor_pattern
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      Some python_constructor_pattern
  | Lang.Js
  | Lang.Ts ->
      Some javascript_constructor_pattern
  | Lang.Swift -> Some swift_constructor_pattern
  | Lang.Go -> Some go_constructor_pattern
  | Lang.Rust -> Some rust_constructor_pattern
  | Lang.Dart -> Some dart_constructor_pattern
  | Lang.Cpp -> Some cpp_constructor_pattern
  | Lang.Php -> Some php_constructor_pattern
  | _ -> None (* Unsupported language *)

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
  match get_constructor_pattern lang with
  | Some pattern -> pattern.match_pattern rval_expr class_names
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
            (* Handle C++ style declarations like: User user(args); *)
            match def_kind with
            | G.VarDef var_def -> (
                match (entity.G.name, var_def.G.vinit) with
                | G.EN var_name, Some init_expr -> (
                    (* For C++, try to get class name from variable type if constructor pattern fails *)
                    let class_name =
                      extract_class_name_from_constructor init_expr lang
                        class_names
                    in
                    let class_name =
                      match (class_name, lang) with
                      | Some cls, _ -> Some cls
                      | None, Lang.Cpp -> (
                          (* C++ fallback: extract class name from variable type *)
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
                    (* C++ constructor call might be embedded in the type or variable declaration *)
                    (* Check if this is actually a constructor call like User user(args) *)
                    match var_def.G.vtype with
                    | Some var_type -> (
                        match var_type.G.t with
                        | G.TyN name when is_known_class name class_names ->
                            object_mappings :=
                              (var_name, name) :: !object_mappings
                        | G.TyFun (_, return_type) -> (
                            (* C++ constructor calls like User user(args) are parsed as TyFun *)
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
            (* Look for assignment patterns *)
            match expr.G.e with
            | G.Assign (lval_expr, _, rval_expr) -> (
                (* Extract variable name from left side *)
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  (* Handle Go struct field assignments: user.name = ... *)
                  | G.DotAccess (obj_expr, _, G.FN _) when lang = Lang.Go -> (
                      match obj_expr.G.e with
                      | G.N obj_name -> (
                          (* Check if this object is already mapped to a struct *)
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
                          | Some (_, _) ->
                              (* We already know this object maps to a struct, no need to re-add *)
                              None
                          | None -> Some obj_name
                          (* New object, extract name *))
                      | _ -> None)
                  | _ -> None
                in
                (* Extract class name from right side *)
                let class_name =
                  extract_class_name_from_constructor rval_expr lang class_names
                in
                (* Add mapping if both var and class found *)
                match (var_name, class_name) with
                | Some var, Some cls ->
                    object_mappings := (var, cls) :: !object_mappings
                | _ -> ())
            | G.AssignOp (lval_expr, _, rval_expr) -> (
                (* Extract variable name from left side *)
                let var_name =
                  match lval_expr.G.e with
                  | G.N name -> Some name
                  | _ -> None
                in
                (* Extract class name from right side *)
                let class_name =
                  extract_class_name_from_constructor rval_expr lang class_names
                in
                (* Add mapping if both var and class found *)
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
            (* Handle variable declarations with initialization *)
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

  (* Visit all statements in the program *)
  visitor#visit_program () ast;
  !object_mappings

(*****************************************************************************)
(* Constructor Detection Utilities *)
(*****************************************************************************)

(* Check if a function is a constructor for the given language *)
let is_constructor (lang : Lang.t) (func_name : string)
    (class_name_opt : string option) : bool =
  match get_constructor_pattern lang with
  | Some pattern -> (
      List.mem func_name pattern.constructor_names
      ||
      match class_name_opt with
      | Some class_name ->
          func_name
          = class_name (* Java/C#/etc. constructor has same name as class *)
      | None -> false)
  | None -> false

(* Get all constructor method names for a language *)
let get_constructor_names (lang : Lang.t) : string list =
  match get_constructor_pattern lang with
  | Some pattern -> pattern.constructor_names
  | None -> []

(* Check if language uses 'new' keyword *)
let uses_new_keyword (lang : Lang.t) : bool =
  match get_constructor_pattern lang with
  | Some pattern -> pattern.uses_new_keyword
  | None -> false

(*****************************************************************************)
(* Unified Constructor Execution *)
(*****************************************************************************)

(* Unified constructor execution mechanism for all languages *)
let execute_unified_constructor constructor_exp args args_taints
    check_function_call_fn env =
  (* Steps 2-4: Use existing check_function_call mechanism *)
  (* This automatically does: signature lookup, instantiation, effect propagation *)
  match check_function_call_fn env constructor_exp args args_taints with
  | Some (call_taints, shape, updated_lval_env) ->
      Some (call_taints, shape, updated_lval_env)
  | None -> None

(* Language-dependent constructor discovery - calls unified execution *)
let execute_constructor_call lang constructor_name class_name args =
  if is_constructor lang constructor_name class_name then
    Some (constructor_name, class_name, args)
  else None

(*****************************************************************************)
(* C++ Constructor Statement Detection *)
(*****************************************************************************)

(* Detect C++ constructor patterns in DefStmt(VarDef) with TyFun types *)
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
