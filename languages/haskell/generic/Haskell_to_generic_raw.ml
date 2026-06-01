(* Haskell tree-sitter RAW node to AST_generic converter *)
(* This version works directly with Tree_sitter_bindings.Tree_sitter_output_t.node
   instead of going through CST.ml, allowing us to use the original
   tree-sitter-haskell parser without grammar compatibility issues *)

module H = Parse_tree_sitter_helpers
module G = AST_generic

(* Forward declaration for env *)
(* Guard branch: represents one branch in a guarded function clause *)
type guard_branch = {
  guard: G.expr option;  (* None = no guard (direct body), Some expr = | guard -> body *)
  body: G.expr;
}

type clause_info = {
  name: string;
  name_tok: Tok.t;
  params: G.parameter list;
  branches: guard_branch list;  (* List of guard branches: [| guard1 -> body1; | guard2 -> body2] *)
  frettype: G.type_ option;
}

type env = {
  file : Fpath.t;
  conv : (int * int) -> int;
  src : Tree_sitter_run.Src_file.t;
  (* Thread-safe per-file state *)
  type_signatures : (string, string) Hashtbl.t;
  function_clauses : (string, clause_info list ref) Hashtbl.t;
  (* Pattern mode: true when parsing Semgrep patterns, false for regular code *)
  is_pattern_mode : bool;
  metavar_map : (string, string) Hashtbl.t;
  (* Track if we're inside a do-block for monadic binding taint tracking *)
  in_do_block : bool;
}
type node = Tree_sitter_bindings.Tree_sitter_output_t.node

let token = H.token [@@warning "-32"]
let str = H.str [@@warning "-32"]
let fb = Tok.unsafe_fake_bracket
let fake_tok s = Tok.unsafe_fake_tok s

(******************************************************************************)
(* Helper functions for env state (thread-safe) *)
(******************************************************************************)

let add_type_signature env name type_text =
  Hashtbl.replace env.type_signatures name type_text

let find_type_signature env name =
  Hashtbl.find_opt env.type_signatures name

let add_function_clause env clause =
  let existing = match Hashtbl.find_opt env.function_clauses clause.name with
    | Some clauses_ref -> clauses_ref
    | None ->
        let new_ref = ref [] in
        Hashtbl.add env.function_clauses clause.name new_ref;
        new_ref
  in
  existing := clause :: !existing

let get_all_function_names env =
  Hashtbl.fold (fun name _ acc -> name :: acc) env.function_clauses []

let get_clauses_for_function env name =
  match Hashtbl.find_opt env.function_clauses name with
  | Some clauses_ref -> !clauses_ref
  | None -> []

(******************************************************************************)
(* Helpers *)
(******************************************************************************)

(* Normalize node type names for safe use in identifiers *)
(* Replaces special characters with underscores to avoid invalid identifiers *)
let normalize_node_type (node_type : string) : string =
  String.map (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> c
    | _ -> '_'  (* Replace all special chars with underscore *)
  ) node_type

let get_children (node : node) : node list =
  match node.Tree_sitter_bindings.Tree_sitter_output_t.children with
  | Some children -> children
  | None -> []

let get_text (env : env) (node : node) : string =
  let open Tree_sitter_bindings.Tree_sitter_output_t in
  Tree_sitter_run.Src_file.get_region env.src node.start_pos node.end_pos

let make_tok ?override_text (env : env) (node : node) : Tok.t =
  let open Tree_sitter_bindings.Tree_sitter_output_t in
  let text =
    match override_text with
    | Some s -> s
    | None -> get_text env node
  in
  let start_loc = node.start_pos in
  let line = start_loc.row + 1 in
  let column = start_loc.column in
  match env.conv (line, column) with
  | bytepos ->
      let pos = Pos.make ~line ~column env.file bytepos in
      Tok.tok_of_loc { Tok.str = text; pos }
  | exception Not_found ->
      fake_tok text

(* Pattern guard detection removed - cannot be implemented without proper tree-sitter AST support *)

(******************************************************************************)
(* Metavariable Detection *)
(******************************************************************************)

(* Check if a string is a Semgrep metavariable *)
(* Pattern: $[A-Z_][A-Z_0-9]* *)
let is_metavar_name (s : string) : bool =
  String.length s >= 2 &&
  s.[0] = '$' &&
  let first_char = s.[1] in
  (first_char >= 'A' && first_char <= 'Z') || first_char = '_'

(* Check if a string is an ellipsis metavariable *)
(* Pattern: $...[A-Z_][A-Z_0-9]* *)
let is_metavar_ellipsis (s : string) : bool =
  String.length s >= 5 &&
  String.sub s 0 4 = "$..." &&
  let first_char = s.[4] in
  (first_char >= 'A' && first_char <= 'Z') || first_char = '_'

(* Extract metavariable name without $ prefix *)
let metavar_name (s : string) : string =
  if is_metavar_ellipsis s then
    String.sub s 4 (String.length s - 4)  (* Remove $... prefix *)
  else if is_metavar_name s then
    String.sub s 1 (String.length s - 1)  (* Remove $ prefix *)
  else
    s
[@@warning "-32"]

(* Check if we're in pattern context and string is a metavar *)
let resolve_metavar_text (env : env) (s : string) : string =
  match Hashtbl.find_opt env.metavar_map s with
  | Some original -> original
  | None -> s

(* Check if we're in pattern context and string is a metavar *)
let is_metavar_in_context (env : env) (s : string) : bool =
  let s = resolve_metavar_text env s in
  env.is_pattern_mode && (is_metavar_name s || is_metavar_ellipsis s)

(******************************************************************************)
(* Protection analysis helpers *)
(******************************************************************************)

(* Helper: Extract variable name from protection check *)
(* Detects patterns like: var > 0, var /= 0, var >= 1, 0 < var, etc. *)
let extract_protected_variable (expr : G.expr) : string option =
  match expr.G.e with
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       | G.N (G.Id ((op_name, _), _)) when
           op_name = ">" || op_name = ">=" || op_name = "/=" ||
           op_name = "!=" || op_name = "<" || op_name = "<=" ->
           (* Binary comparison operator - check both sides *)
           (match args with
            | [G.Arg left; G.Arg right] ->
                (* Check if left is variable and right is 0 (var > 0) *)
                (match left.G.e, right.G.e with
                 | G.N (G.Id ((var_name, _), _)), G.L (G.Int (Some 0L, _))
                   when op_name = ">" || op_name = "/=" || op_name = "!=" || op_name = ">=" ->
                     Some var_name
                 | G.N (G.Id ((var_name, _), _)), G.L (G.Int (Some 1L, _))
                   when op_name = ">=" ->
                     Some var_name
                 (* Check if left is 0 and right is variable (0 < var) *)
                 | G.L (G.Int (Some 0L, _)), G.N (G.Id ((var_name, _), _))
                   when op_name = "<" || op_name = "/=" || op_name = "!=" ->
                     Some var_name
                 | G.L (G.Int (Some 1L, _)), G.N (G.Id ((var_name, _), _))
                   when op_name = "<=" ->
                     Some var_name
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Helper: Check if a guard expression protects against index bounds *)
(* Detects patterns like: i < 0, i >= length xs - these protect the OTHERWISE branch *)
let rec guard_protects_bounds (expr : G.expr) : string option =
  match expr.G.e with
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       (* Check for: i < 0 or i >= length xs patterns *)
       | G.N (G.Id ((op_name, _), _)) when
           op_name = "<" || op_name = ">=" || op_name = ">" || op_name = "<=" ->
           (match args with
            | [G.Arg left; G.Arg right] ->
                (match left.G.e, right.G.e with
                 (* i < 0 *)
                 | G.N (G.Id ((var_name, _), _)), G.L (G.Int (Some 0L, _))
                   when op_name = "<" -> Some var_name
                 (* i >= length xs or i > length xs *)
                 | G.N (G.Id ((var_name, _), _)), G.Call (length_func, _)
                   when op_name = ">=" || op_name = ">" ->
                     (match length_func.G.e with
                      | G.N (G.Id ((length_name, _), _)) when length_name = "length" -> Some var_name
                      | _ -> None)
                 (* 0 > i *)
                 | G.L (G.Int (Some 0L, _)), G.N (G.Id ((var_name, _), _))
                   when op_name = ">" -> Some var_name
                 (* length xs <= i or length xs < i *)
                 | G.Call (length_func, _), G.N (G.Id ((var_name, _), _))
                   when op_name = "<=" || op_name = "<" ->
                     (match length_func.G.e with
                      | G.N (G.Id ((length_name, _), _)) when length_name = "length" -> Some var_name
                      | _ -> None)
                 | _ -> None)
            | _ -> None)
       (* Check for: i < 0 || i >= length xs (combined check with ||) *)
       | G.N (G.Id ((op_name, _), _)) when op_name = "||" ->
           (* If any side of OR protects bounds, return that *)
           (match args with
            | [G.Arg left; G.Arg right] ->
                (match guard_protects_bounds left with
                 | Some var -> Some var
                 | None -> guard_protects_bounds right)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Helper: Check if a guard expression protects lists from being empty *)
(* Detects patterns like: null xs, xs == [], length xs == 0 - these protect the OTHERWISE branch *)
let guard_protects_empty (expr : G.expr) : string option =
  match expr.G.e with
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       (* Check for: null xs *)
       | G.N (G.Id ((func_name, _), _)) when func_name = "null" ->
           (match args with
            | [G.Arg var_expr] ->
                (match var_expr.G.e with
                 | G.N (G.Id ((var_name, _), _)) -> Some var_name
                 | _ -> None)
            | _ -> None)
       (* Check for: xs == [] or [] == xs *)
       | G.N (G.Id ((op_name, _), _)) when op_name = "==" ->
           (match args with
            | [G.Arg left; G.Arg right] ->
                (match left.G.e, right.G.e with
                 | G.N (G.Id ((var_name, _), _)), G.Container (G.List, (_, [], _)) -> Some var_name
                 | G.Container (G.List, (_, [], _)), G.N (G.Id ((var_name, _), _)) -> Some var_name
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Helper: Check if a guard expression protects against zero *)
(* Detects patterns like: n <= 0, n < 1, n == 0 - these protect the OTHERWISE branch *)
let guard_protects_zero (expr : G.expr) : string option =
  match expr.G.e with
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       | G.N (G.Id ((op_name, _), _)) when
           op_name = "<=" || op_name = "<" || op_name = "==" ->
           (* Check patterns: var <= 0, var < 1, var == 0 *)
           (match args with
            | [G.Arg left; G.Arg right] ->
                (match left.G.e, right.G.e with
                 | G.N (G.Id ((var_name, _), _)), G.L (G.Int (Some 0L, _))
                   when op_name = "<=" || op_name = "==" ->
                     Some var_name  (* n <= 0 or n == 0 protects otherwise *)
                 | G.N (G.Id ((var_name, _), _)), G.L (G.Int (Some 1L, _))
                   when op_name = "<" ->
                     Some var_name  (* n < 1 protects otherwise *)
                 (* Check reversed: 0 >= var, 0 == var, 1 > var *)
                 | G.L (G.Int (Some 0L, _)), G.N (G.Id ((var_name, _), _))
                   when op_name = ">=" || op_name = "==" ->
                     Some var_name
                 | G.L (G.Int (Some 1L, _)), G.N (G.Id ((var_name, _), _))
                   when op_name = ">" ->
                     Some var_name
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Helper: Check if an expression is guaranteed to be non-zero *)
(* Handles:
   - Literal non-zero integers (42, 365, etc.)
   - denominator function from Data.Ratio (always > 0)
   - Multiplication of non-zero values (a≠0 ∧ b≠0 ⇒ a×b≠0)
*)
let rec is_expr_guaranteed_nonzero (expr : G.expr) : bool =
  match expr.G.e with
  (* Case 1: Literal non-zero integer *)
  | G.L (G.Int (Some n, _)) when n <> 0L -> true

  (* Case 2: Function calls *)
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       (* denominator function from Data.Ratio always returns > 0 *)
       | G.N (G.Id (("denominator", _), _)) -> true

       (* Multiplication: non-zero if BOTH operands are non-zero *)
       | G.N (G.Id (("*", _), _)) ->
           (match args with
            | [G.Arg left; G.Arg right] ->
                is_expr_guaranteed_nonzero left && is_expr_guaranteed_nonzero right
            | _ -> false)

       (* Other functions: unknown, assume can be zero *)
       | _ -> false)

  (* All other expressions: unknown, assume can be zero *)
  | _ -> false

(* Helper: Check if a pattern is a literal 0 *)
let is_pattern_zero (pat : G.pattern) : bool =
  match pat with
  | G.PatLiteral (G.Int (Some 0L, _)) -> true
  | _ -> false

(* Helper: Check if a pattern is an empty list [] *)
let is_pattern_empty_list (pat : G.pattern) : bool =
  match pat with
  | G.PatConstructor (G.Id (("[]", _), _), []) -> true
  | _ -> false

(* Helper: Check if a pattern is an empty string "" *)
let is_pattern_empty_string (pat : G.pattern) : bool =
  match pat with
  | G.PatLiteral (G.String (_, ("", _), _)) -> true
  | G.PatLiteral (G.String (_, ("\"\"", _), _)) -> true
  | _ -> false

(* Helper: Check if a case contains an empty pattern (0, [], "") *)
let case_has_empty_pattern (case : G.case) : bool =
  match case with
  | G.Case (_, pat) ->
      is_pattern_zero pat ||
      is_pattern_empty_list pat ||
      is_pattern_empty_string pat
  | _ -> false

(* Helper: Annotate dangerous operations in an expression as protected *)
(* Strategy: Replace div/mod/quot/head/tail/last/!! with safe__ prefix so semgrep won't match *)
let rec annotate_protected_expr (expr : G.expr) : G.expr =
  match expr.G.e with
  | G.Call (func, (t1, args, t2)) ->
      (* Check if this is a dangerous operation call *)
      (match func.G.e with
       | G.N (G.Id ((name, tok), id_info)) when
           name = "div" || name = "mod" || name = "quot" ||
           name = "head" || name = "tail" || name = "last" || name = "!!" ->

           (* SPECIAL CASE: For division operations, check if divisor is safe *)
           let is_safe_division =
             if name = "div" || name = "mod" || name = "quot" then
               match args with
               | [_; G.Arg divisor] ->
                   (* Check if divisor is guaranteed non-zero *)
                   is_expr_guaranteed_nonzero divisor
               | _ -> false
             else
               false  (* Not a division operation *)
           in

           (* If division is safe, mark it as protected *)
           if is_safe_division then
             let safe_name = "safe__" ^ name in
             let new_func = G.N (G.Id ((safe_name, tok), id_info)) |> G.e in
             { expr with G.e = G.Call (new_func, (t1, args, t2)) }
           else
             (* Not safe or not division: recursively annotate arguments *)
             let annotated_args = List.map (fun arg ->
               match arg with
               | G.Arg e -> G.Arg (annotate_protected_expr e)
               | other -> other
             ) args in
             { expr with G.e = G.Call (annotate_protected_expr func, (t1, annotated_args, t2)) }

       | _ ->
           (* Not a dangerous operation: recursively annotate arguments *)
           let annotated_args = List.map (fun arg ->
             match arg with
             | G.Arg e -> G.Arg (annotate_protected_expr e)
             | other -> other
           ) args in
           { expr with G.e = G.Call (annotate_protected_expr func, (t1, annotated_args, t2)) })
  | G.Seq exprs ->
      (* When a Seq contains LetPattern nodes, ALL expressions in the sequence are protected
         if this function was called in a protected context (like otherwise branch).
         We recursively annotate all expressions in the sequence. *)
      { expr with G.e = G.Seq (List.map annotate_protected_expr exprs) }
  | G.LetPattern (pat, e) ->
      (* Annotate the RHS of the binding - the pattern itself doesn't need annotation *)
      { expr with G.e = G.LetPattern (pat, annotate_protected_expr e) }
  | G.Conditional (cond, then_e, else_e) ->
      (* Check if this is a guard pattern with zero-protection *)
      (* Pattern: if (n <= 0) then ... else (if otherwise then DIV else ...) *)
      let else_protected = match guard_protects_zero cond with
        | Some _ ->
            (* Outer condition protects against zero, recursively annotate else branch *)
            (* This handles nested conditionals where otherwise contains the division *)
            annotate_protected_expr else_e
        | None ->
            (* Check if the else branch is a nested conditional with otherwise *)
            (* If the outer guard was a zero check, the inner otherwise is protected *)
            (match else_e.G.e with
             | G.Conditional (inner_cond, inner_then, inner_else) ->
                 (* Check if inner condition is "otherwise" identifier *)
                 (match inner_cond.G.e with
                  | G.N (G.Id (("otherwise", _), _)) ->
                      (* This is a nested guard pattern - don't annotate here, let outer handle it *)
                      else_e
                  | _ ->
                      (* Not otherwise, process normally *)
                      { else_e with G.e = G.Conditional (
                        annotate_protected_expr inner_cond,
                        annotate_protected_expr inner_then,
                        annotate_protected_expr inner_else) })
             | _ -> annotate_protected_expr else_e)
      in
      { expr with G.e = G.Conditional (
        annotate_protected_expr cond,
        annotate_protected_expr then_e,
        else_protected) }
  | G.StmtExpr stmt ->
      { expr with G.e = G.StmtExpr (annotate_protected_stmt stmt) }
  | G.Container (container_type, (t1, exprs, t2)) ->
      (* Recursively annotate all expressions inside lists/tuples *)
      let annotated_exprs = List.map annotate_protected_expr exprs in
      { expr with G.e = G.Container (container_type, (t1, annotated_exprs, t2)) }
  | _ -> expr

and annotate_protected_stmt (stmt : G.stmt) : G.stmt =
  match stmt.G.s with
  | G.ExprStmt (e, t) ->
      { stmt with G.s = G.ExprStmt (annotate_protected_expr e, t) }
  | G.Block (t1, stmts, t2) ->
      { stmt with G.s = G.Block (t1, List.map annotate_protected_stmt stmts, t2) }
  | G.Switch (tok, cond_opt, cases) ->
      (* Check if any prior case has empty pattern (0, [], "") *)
      (* If so, protect subsequent wildcard/default cases *)
      let annotated_cases = List.mapi (fun idx case_and_body ->
        match case_and_body with
        | G.CasesAndBody (case_list, body) ->
            (* Check if ANY prior case (idx < current) has empty pattern *)
            let has_prior_empty =
              (* Take only cases before current index *)
              let prior_cases = List.filteri (fun i _ -> i < idx) cases in
              List.exists (fun prior_case_and_body ->
                match prior_case_and_body with
                | G.CasesAndBody (prior_cases, _) ->
                    List.exists case_has_empty_pattern prior_cases
                | _ -> false
              ) prior_cases
            in
            (* Check if current case is a wildcard or default *)
            let is_wildcard_or_default = List.exists (fun c ->
              match c with
              | G.Case (_, G.PatWildcard _) -> true
              | G.Default _ -> true
              | _ -> false
            ) case_list in
            (* Protect body if prior empty pattern exists and this is wildcard/default *)
            let annotated_body =
              if has_prior_empty && is_wildcard_or_default then
                (* Annotate this body as protected since 0/[]/""  was already handled *)
                match body.G.s with
                | G.ExprStmt (e, t) ->
                    { body with G.s = G.ExprStmt (annotate_protected_expr e, t) }
                | _ -> annotate_protected_stmt body
              else
                annotate_protected_stmt body
            in
            G.CasesAndBody (case_list, annotated_body)
        | other -> other
      ) cases in
      { stmt with G.s = G.Switch (tok, cond_opt, annotated_cases) }
  | _ -> stmt

(******************************************************************************)
(* SQL Injection Detection Helpers *)
(******************************************************************************)

(* Check if a function name is a SQL query function *)
let is_sql_query_function (func_name : string) : bool =
  func_name = "query" || func_name = "query_" ||
  func_name = "execute" || func_name = "execute_" ||
  func_name = "rawExecute" || func_name = "rawSql" ||
  func_name = "executeRaw" || func_name = "executeSql"

(* Helper to check if string contains substring *)
let string_contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(* Check if an expression contains SQL keywords *)
let rec expr_contains_sql_keyword (expr : G.expr) : bool =
  match expr.G.e with
  | G.L (G.String (_, (text, _), _)) ->
      let upper_text = String.uppercase_ascii text in
      string_contains_substring upper_text "SELECT" ||
      string_contains_substring upper_text "INSERT" ||
      string_contains_substring upper_text "UPDATE" ||
      string_contains_substring upper_text "DELETE" ||
      string_contains_substring upper_text "WHERE" ||
      string_contains_substring upper_text "FROM"
  | G.Call (_, (_, args, _)) ->
      (* Check recursively in call arguments *)
      List.exists (fun arg ->
        match arg with
        | G.Arg e -> expr_contains_sql_keyword e
        | _ -> false
      ) args
  | _ -> false

(* Check if an expression contains string concatenation (++) *)
let rec expr_contains_concat (expr : G.expr) : bool =
  match expr.G.e with
  | G.Call (func, (_, args, _)) ->
      (match func.G.e with
       | G.N (G.Id ((op_name, _), _)) when op_name = "++" -> true
       | _ ->
           (* Recursively check arguments *)
           List.exists (fun arg ->
             match arg with
             | G.Arg e -> expr_contains_concat e
             | _ -> false
           ) args)
  | _ -> false

(* Check if an expression contains parameterization (?) *)
let rec expr_contains_placeholder (expr : G.expr) : bool =
  match expr.G.e with
  | G.L (G.String (_, (text, _), _)) ->
      String.contains text '?'
  | G.Call (_, (_, args, _)) ->
      List.exists (fun arg ->
        match arg with
        | G.Arg e -> expr_contains_placeholder e
        | _ -> false
      ) args
  | _ -> false

(* Check if an expression is wrapped in Only/Just (safe parameterization) *)
let expr_is_safe_parameter (expr : G.expr) : bool =
  match expr.G.e with
  | G.Call (func, _) ->
      (match func.G.e with
       | G.N (G.Id ((name, _), _)) when name = "Only" || name = "Just" -> true
       | _ -> false)
  | _ -> false

(* Analyze SQL query call for injection risk *)
let analyze_sql_call (_func_name : string) (args : G.expr list) : string option =
  match args with
  (* Pattern 1: query/execute with 2+ args *)
  | _ :: query_arg :: rest ->
      let has_sql = expr_contains_sql_keyword query_arg in
      let has_concat = expr_contains_concat query_arg in
      let has_placeholder = expr_contains_placeholder query_arg in

      (* Check if 3rd argument (if exists) is safe parameterization *)
      let has_safe_param = match rest with
        | param :: _ -> expr_is_safe_parameter param
        | [] -> false
      in

      (* Determine risk level *)
      if has_sql && has_concat && not has_placeholder then
        (* SQL keyword + concat + no placeholder = HIGH RISK *)
        Some "unsafe_sql_concat"
      else if has_sql && has_concat && has_placeholder && not has_safe_param then
        (* Has ?, but param might still have concat = MEDIUM RISK *)
        Some "potential_sql_concat"
      else if has_sql && has_concat && has_placeholder && has_safe_param then
        (* Safe: parameterized with Only/Just wrapper *)
        None
      else if has_concat then
        (* General concat in SQL context = LOW RISK *)
        Some "sql_string_building"
      else
        None
  | _ -> None

(******************************************************************************)
(* Conversion functions *)
(******************************************************************************)

let rec convert_node (env : env) (node : node) : G.any =
  match node.Tree_sitter_bindings.Tree_sitter_output_t.type_ with

  (* Literals *)
  | "string" ->
      let tok = make_tok env node in
      let text = get_text env node in
      G.E (G.L (G.String (fake_tok "\"", (text, tok), fake_tok "\"")) |> G.e)

  | "integer" | "integer_literal" | "float" ->
      let tok = make_tok env node in
      let text = get_text env node in
      if String.contains text '.' then
        let f_opt = try Some (float_of_string text) with _ -> None in
        G.E (G.L (G.Float (f_opt, tok)) |> G.e)
      else
        let i_opt = try Some (Int64.of_string text) with _ -> None in
        G.E (G.L (G.Int (i_opt, tok)) |> G.e)

  | "char" ->
      let tok = make_tok env node in
      let text = get_text env node in
      G.E (G.L (G.Char (text, tok)) |> G.e)

  (* Variables and names *)
  | "variable" | "name" | "qualified_variable" | "constructor" | "constructor_operator" ->
      let raw_text = get_text env node in
      let text = resolve_metavar_text env raw_text in
      let tok = make_tok ~override_text:text env node in

      (* Check if this is a metavariable in pattern context *)
      if is_metavar_in_context env raw_text then
        (* This is a metavariable: $VAR, $EXPR, $...ARGS *)
        if is_metavar_ellipsis text then
          (* Ellipsis metavariable: $...ARGS *)
          (* Keep the $ prefix in the AST for Semgrep matching *)
          G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
        else
          (* Regular metavariable: $VAR *)
          (* Keep the $ prefix in the AST for Semgrep matching *)
          G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
      else
        (* Regular identifier *)
        G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)

  (* Top-level splices - used for bare identifiers in patterns *)
  | "top_splice" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | _ -> G.E (G.OtherExpr (("top_splice", fake_tok ""), []) |> G.e))

  (* Template Haskell Splices: $(...) or $(expr) *)
  | "splice" | "exp_splice" ->
      let children = get_children node in
      let tok = make_tok env node in
      (* Filter out $ and parentheses *)
      let expr_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "$" && t <> "(" && t <> ")"
      ) children in
      (* Extract the splice expression *)
      let splice_expr = match expr_children with
        | [expr_node] -> convert_expr env expr_node
        | [] -> G.OtherExpr (("empty_splice", fake_tok ""), []) |> G.e
        | expr_nodes ->
            (* Multiple expressions - convert all *)
            let exprs = List.map (convert_expr env) expr_nodes in
            G.Seq exprs |> G.e
      in
      (* Represent as OtherExpr with splice annotation *)
      (* This allows semgrep to detect the presence of splices even if we can't evaluate them *)
      G.E (G.OtherExpr (("template_haskell_splice", tok), [G.E splice_expr]) |> G.e)

  (* Template Haskell Quotations: [|...|], [e|...|], [p|...|], [t|...|], [d|...|] *)
  | "quotation" | "quasiquote" | "exp_th_quoted_name" ->
      let children = get_children node in
      let tok = make_tok env node in
      let full_text = get_text env node in
      (* Extract quotation type from brackets: [|expr|], [e|expr|], etc. *)
      let quote_type =
        if String.length full_text >= 2 && full_text.[0] = '[' then
          (* Check for typed quotation: [e|, [p|, [t|, [d| *)
          if String.length full_text >= 3 && full_text.[2] = '|' then
            String.sub full_text 1 1  (* Extract the type letter *)
          else
            "expr"  (* Default expression quotation [|...|] *)
        else
          "unknown"
      in
      (* Extract the quoted content *)
      let content_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "[" && t <> "]" && t <> "|" && t <> "[|" && t <> "|]"
      ) children in
      (* Convert the quoted content as regular Haskell code *)
      let quoted_exprs = match content_children with
        | [] -> [G.OtherExpr (("empty_quote", fake_tok ""), []) |> G.e]
        | expr_nodes ->
            List.filter_map (fun node ->
              match convert_node env node with
              | G.E e -> Some e
              | G.S s -> Some (G.StmtExpr s |> G.e)
              | _ -> None
            ) expr_nodes
      in
      let quoted_content = match quoted_exprs with
        | [] -> G.OtherExpr (("empty_quote", fake_tok ""), []) |> G.e
        | [e] -> e
        | es -> G.Seq es |> G.e
      in
      (* Represent as OtherExpr with quotation type annotation *)
      let quote_tag = "template_haskell_quote_" ^ quote_type in
      G.E (G.OtherExpr ((quote_tag, tok), [G.E quoted_content]) |> G.e)

  (* Expressions *)
  | "apply" ->
      let children = get_children node in
      (match children with
       | func :: args ->
           let func_expr = convert_expr env func in

           (* BlockArguments: Detect if any arguments are lambda/do/case without parens *)
           let has_block_args = List.exists (fun arg ->
             let arg_type = arg.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
             arg_type = "lambda" || arg_type = "exp_lambda" ||
             arg_type = "do" || arg_type = "exp_do" ||
             arg_type = "case" || arg_type = "exp_case" ||
             arg_type = "lambda_case" || arg_type = "mdo"
           ) args in

           let arg_exprs = List.map (convert_expr env) args in
           let call_expr = List.fold_left (fun acc arg ->
             G.Call (acc, fb [G.Arg arg]) |> G.e
           ) func_expr arg_exprs in

           (* SQL Injection Context Analysis *)
           let final_expr =
             (* Extract function name from curried calls: query_ conn sql => extract "query_" *)
             let rec extract_func_name expr =
               match expr.G.e with
               | G.N (G.Id ((name, _), _)) -> Some name
               | G.Call (inner_func, _) -> extract_func_name inner_func
               | _ -> None
             in

             let func_name_opt = extract_func_name func_expr in

             (* Check if this is a SQL query function call *)
             match func_name_opt with
             | Some func_name when is_sql_query_function func_name ->
                 (* Collect ALL arguments from curried calls *)
                 let rec collect_all_args expr acc =
                   match expr.G.e with
                   | G.Call (inner_func, (_, args, _)) ->
                       let arg_exprs = List.filter_map (fun arg ->
                         match arg with
                         | G.Arg e -> Some e
                         | _ -> None
                       ) args in
                       collect_all_args inner_func (arg_exprs @ acc)
                   | _ -> acc
                 in

                 let all_args = collect_all_args call_expr [] in

                 (* Analyze SQL call for injection risk *)
                 (match analyze_sql_call func_name all_args with
                  | Some risk_tag ->
                      (* Annotate by prefixing function name with risk tag *)
                      (* This makes it matchable by Semgrep patterns *)
                      let rec annotate_call_with_risk expr =
                        match expr.G.e with
                        | G.Call (inner_func, (t1, args, t2)) ->
                            (* Recursively find the base function and annotate it *)
                            { expr with G.e = G.Call (annotate_call_with_risk inner_func, (t1, args, t2)) }
                        | G.N (G.Id ((name, tok), id_info)) when name = func_name ->
                            (* Found the function - prefix with risk tag *)
                            let annotated_name = risk_tag ^ "__" ^ name in
                            { expr with G.e = G.N (G.Id ((annotated_name, tok), id_info)) }
                        | _ -> expr
                      in
                      annotate_call_with_risk call_expr
                  | None ->
                      (* Safe SQL call *)
                      call_expr)
             | _ ->
                 (* Not a SQL function *)
                 call_expr
           in

           (* Annotate with block_arguments if detected *)
           if has_block_args then
             (* Wrap in OtherExpr to preserve block arguments annotation *)
             let tok = make_tok env node in
             G.E (G.OtherExpr (("block_arguments", tok), [G.E final_expr]) |> G.e)
           else
             G.E final_expr
       | [] ->
           G.E (G.OtherExpr (("empty_apply", fake_tok ""), []) |> G.e))

  | "infix" ->
      (* infix operator application: a + b or a `mod` b *)
      let children = get_children node in
      (match children with
       | left :: op :: right :: _ ->
           let op_text = get_text env op in
           let op_tok = make_tok env op in

           (* Special handling for $ operator: desugar f $ x => f(x) *)
           if op_text = "$" then
             (* Desugar dollar operator: function application *)
             let func_expr = convert_expr env left in
             let arg_expr = convert_expr env right in
             let call_expr = G.Call (func_expr, fb [G.Arg arg_expr]) |> G.e in

             (* SQL Injection Context Analysis for $ operator *)
             let final_expr =
               (* Extract function name from left side (might be curried: query_ conn) *)
               let rec extract_func_name expr =
                 match expr.G.e with
                 | G.N (G.Id ((name, _), _)) -> Some name
                 | G.Call (inner_func, _) -> extract_func_name inner_func
                 | _ -> None
               in

               let func_name_opt = extract_func_name func_expr in

               (* Check if this is a SQL query function call *)
               match func_name_opt with
               | Some func_name when is_sql_query_function func_name ->
                   (* Collect ALL arguments from curried calls *)
                   let rec collect_all_args expr acc =
                     match expr.G.e with
                     | G.Call (inner_func, (_, args, _)) ->
                         let arg_exprs = List.filter_map (fun arg ->
                           match arg with
                           | G.Arg e -> Some e
                           | _ -> None
                         ) args in
                         collect_all_args inner_func (arg_exprs @ acc)
                     | _ -> acc
                   in

                   let all_args = collect_all_args call_expr [] in

                   (* Analyze SQL call for injection risk *)
                   (match analyze_sql_call func_name all_args with
                    | Some risk_tag ->
                        (* Annotate by prefixing function name with risk tag *)
                        (* This makes it matchable by Semgrep patterns *)
                        let rec annotate_call_with_risk expr =
                          match expr.G.e with
                          | G.Call (inner_func, (t1, args, t2)) ->
                              (* Recursively find the base function and annotate it *)
                              { expr with G.e = G.Call (annotate_call_with_risk inner_func, (t1, args, t2)) }
                          | G.N (G.Id ((name, tok), id_info)) when name = func_name ->
                              (* Found the function - prefix with risk tag *)
                              let annotated_name = risk_tag ^ "__" ^ name in
                              { expr with G.e = G.N (G.Id ((annotated_name, tok), id_info)) }
                          | _ -> expr
                        in
                        annotate_call_with_risk call_expr
                    | None ->
                        (* Safe SQL call *)
                        call_expr)
               | _ ->
                   (* Not a SQL function *)
                   call_expr
             in
             G.E final_expr
           else
             (* Convert left and right for other operators *)
             let left_expr = convert_expr env left in
             let right_expr = convert_expr env right in

             (* Detect backtick operator: `func` *)
             let is_backtick = String.length op_text >= 2
                               && op_text.[0] = '`'
                               && op_text.[String.length op_text - 1] = '`' in

             if is_backtick then
               (* Backtick operator: extract function name from `name` *)
               let func_name = String.sub op_text 1 (String.length op_text - 2) in
               let func_tok = make_tok env op in
               let func_expr = G.N (G.Id ((func_name, func_tok), G.empty_id_info ())) |> G.e in
               (* Normal function call: func(left, right) *)
               G.E (G.Call (func_expr, fb [G.Arg left_expr; G.Arg right_expr]) |> G.e)
             else
               (* Regular operator symbol: +, *, etc. *)
               let op_expr = G.N (G.Id ((op_text, op_tok), G.empty_id_info ())) |> G.e in
               G.E (G.Call (op_expr, fb [G.Arg left_expr; G.Arg right_expr]) |> G.e)
       | [single] ->
           (* Malformed infix with single child - just convert it *)
           convert_node env single
       | [left; op] ->
           (* Partial infix - treat as application *)
           let left_expr = convert_expr env left in
           let op_expr = convert_expr env op in
           G.E (G.Call (op_expr, fb [G.Arg left_expr]) |> G.e)
       | [] ->
           G.E (G.OtherExpr (("empty_infix", fake_tok ""), []) |> G.e))

  | "literal" ->
      (* literal is a wrapper, convert its child *)
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | [] -> G.E (G.OtherExpr (("empty_literal", fake_tok ""), []) |> G.e)
       | children ->
           (* Multiple children - try first valid child *)
           let child_results = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           match child_results with
           | [] -> G.E (G.OtherExpr (("malformed_literal", fake_tok ""), []) |> G.e)
           | [e] -> G.E e
           | e :: _ -> G.E e  (* Take first valid expression *))

  (* Negation *)
  | "negation" | "exp_negation" ->
      let children = get_children node in
      (match children with
       | [operand] ->
           let expr = convert_expr env operand in
           let op_tok = fake_tok "-" in
           G.E (G.Call (G.N (G.Id (("-", op_tok), G.empty_id_info ())) |> G.e,
                        fb [G.Arg expr]) |> G.e)
       | _ -> G.E (G.OtherExpr (("negation", fake_tok ""), []) |> G.e))

  (* Function bindings and pattern matching *)
  | "bind" | "function" ->
      (* Extract first child as name, then split params from body *)
      let children = get_children node in
      (* Special case: if we're in a do-block, treat bind as monadic binding *)
      if env.in_do_block then
        (match children with
         | name_node :: rest ->
             let expr_nodes = List.filter (fun c ->
               let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
               t <> "=" && t <> "<-"
             ) rest in
             (match expr_nodes with
              | [expr_node] ->
                  let var_name = get_text env name_node in
                  let var_tok = make_tok env name_node in
                  let pattern = G.PatId ((var_name, var_tok), G.empty_id_info ()) in
                  let binding_expr = convert_expr env expr_node in
                  (* Use LetPattern for taint propagation in do-blocks *)
                  G.E (G.LetPattern (pattern, binding_expr) |> G.e)
              | _ -> G.E (G.OtherExpr (("bind_no_expr", fake_tok ""), []) |> G.e))
         | _ -> G.E (G.OtherExpr (("bind_empty", fake_tok ""), []) |> G.e))
      else
        (* Normal function binding - collect as function clause *)
        (match children with
         | name :: rest ->
             let name_tok = make_tok env name in
             let name_str = get_text env name in

             (* Look up type signature if available *)
            let frettype = match find_type_signature env name_str with
             | Some type_text ->
                 (* Parse the type signature into a proper type structure *)
                 (try
                   Some (Haskell_type_parser.parse_and_convert type_text)
                 with _ ->
                   (* Fallback to string if parsing fails *)
                   let type_tok = fake_tok type_text in
                   Some { G.t = G.TyN (G.Id ((type_text, type_tok), G.empty_id_info ()));
                          G.t_attrs = [] })
             | None -> None
           in

           (* Separate WHERE nodes from other children *)
           let where_nodes, rest_no_where = List.partition (fun c ->
             let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
             t = "where" || t = "local_binds"
           ) rest in

           (* Separate MATCH nodes (may contain guards) from other children *)
           let match_nodes, rest_no_match = List.partition (fun c ->
             c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "match"
           ) rest_no_where in

           (* Filter out = token from non-MATCH nodes - KEEP | for guard detection *)
           let rest_filtered = List.filter (fun c ->
             let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
             t <> "="
           ) rest_no_match in

           (* Split params from body - last child is body, rest are params *)
           let params, body_nodes = match rest_filtered with
             | [] -> ([], [])
             | [single] ->
                 (* Check if this single child is a patterns wrapper - if so, unwrap it *)
                 if single.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "patterns" then
                   (get_children single, [])  (* Extract children as params, body comes from match_nodes *)
                 else
                   ([], [single])  (* Single child is body *)
             | children ->
                 (* Same strategy as lambda: reverse, split, reverse back *)
                 let rev_children = List.rev children in
                 (match rev_children with
                  | body_node :: param_nodes ->
                      (List.rev param_nodes, [body_node])
                  | [] -> ([], []))
           in

           (* Extract guard branches from match nodes *)
           let guard_branches = if List.length match_nodes > 0 then
             (* We have match nodes - extract guards from them *)
             List.concat_map (fun match_node ->
               let match_children = get_children match_node in
               (* Filter out = token *)
               let match_children_filtered = List.filter (fun c ->
                 c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
               ) match_children in

               (* Check if we have guards node *)
               let guards_node = List.find_opt (fun c ->
                 c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "guards"
               ) match_children_filtered in

               (match guards_node with
                | Some guards ->
                    (* Pattern guards detected! Extract each guard branch *)
                    let guard_children = get_children guards in
                    let body_nodes = List.filter (fun c ->
                      c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "guards"
                    ) match_children_filtered in
                    let body_expr = match body_nodes with
                      | [b] -> convert_expr env b
                      | [] -> G.OtherExpr (("no_guard_body", fake_tok ""), []) |> G.e
                      | bs -> G.Seq (List.map (convert_expr env) bs) |> G.e
                    in

                    (* Convert each guard to a guard_branch *)
                    List.map (fun guard_node ->
                      match guard_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
                      | "boolean" ->
                          (* Boolean guard: | n < 0 *)
                          let guard_children = get_children guard_node in
                          let cond = match guard_children with
                            | [c] -> convert_expr env c
                            | _ -> G.OtherExpr (("guard_bool", fake_tok ""), []) |> G.e
                          in
                          (* Check if this is the "otherwise" guard - if so, treat as no guard *)
                          (match cond.G.e with
                           | G.N (G.Id (("otherwise", _), _)) ->
                               { guard = None; body = body_expr }  (* Otherwise = no guard *)
                           | _ ->
                               { guard = Some cond; body = body_expr })
                      | "pattern_guard" ->
                          (* Pattern guard: | x <- xs *)
                          let guard_children = get_children guard_node in
                          (match guard_children with
                           | pattern_node :: expr_node :: _ ->
                               let pat = convert_pattern env pattern_node in
                               let iter_expr = convert_expr env expr_node in
                               (* Create LetPattern as guard condition *)
                               let guard_expr = G.LetPattern (pat, iter_expr) |> G.e in
                               { guard = Some guard_expr; body = body_expr }
                           | _ ->
                               (* Fallback: no guard *)
                               { guard = None; body = body_expr })
                      | _ ->
                          (* Unknown guard type - no guard *)
                          { guard = None; body = body_expr }
                    ) guard_children
                | None ->
                    (* No guards - normal match with single body *)
                    let body_expr = match match_children_filtered with
                      | [child] ->
                          (match convert_node env child with
                           | G.E e -> e
                           | _ -> G.OtherExpr (("match_no_expr", fake_tok ""), []) |> G.e)
                      | [] -> G.OtherExpr (("empty_match", fake_tok ""), []) |> G.e
                      | children ->
                          let child_results = List.filter_map (fun child ->
                            match convert_node env child with
                            | G.E e -> Some e
                            | _ -> None
                          ) children in
                          (match child_results with
                           | [e] -> e
                           | es -> G.Seq es |> G.e)
                    in
                    [{ guard = None; body = body_expr }])
             ) match_nodes
           else if List.length body_nodes > 0 then
             (* No match nodes - use simple body *)
             let body_expr = match body_nodes with
               | [body] -> convert_expr env body
               | _ -> G.OtherExpr (("empty_body", fake_tok ""), []) |> G.e
             in

             (* Process WHERE bindings if present *)
             let body_expr_with_where = match where_nodes with
               | [] -> body_expr
               | where_nodes ->
                   (* Convert WHERE bindings to statements *)
                   let where_stmts = List.concat_map (fun where_node ->
                     match convert_node env where_node with
                     | G.Ss stmts -> stmts
                     | G.S stmt -> [stmt]
                     | _ -> []
                   ) where_nodes in
                   (* Wrap body with WHERE bindings as a block *)
                   match where_stmts with
                   | [] -> body_expr
                   | stmts ->
                       (* Create a block with bindings followed by the main body *)
                       G.StmtExpr (G.Block (fb (stmts @ [G.ExprStmt (body_expr, fake_tok ";") |> G.s])) |> G.s) |> G.e
             in
             [{ guard = None; body = body_expr_with_where }]
           else
             (* No body at all *)
             [{ guard = None; body = G.OtherExpr (("empty_body", fake_tok ""), []) |> G.e }]
           in

           (* Convert params to parameter list *)
           (* Each param node might contain multiple variables as children *)
           let param_list = List.concat_map (fun p ->
             match p.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
             | "patterns" ->
                 (* Multiple patterns - extract children *)
                 let pattern_children = get_children p in
                 List.map (fun child ->
                   (* Use convert_pattern to support complex patterns *)
                   let pattern = convert_pattern env child in
                   match pattern with
                   | G.PatId ((name, tok), _id_info) ->
                       (* Simple variable - convert to Param *)
                       G.Param (G.param_of_id (name, tok))
                   | _ ->
                       (* Complex pattern (literal, wildcard, constructor) *)
                       G.ParamPattern pattern
                 ) pattern_children
             | "variable" | "name" ->
                 (* Simple variable parameter *)
                 let tok = make_tok env p in
                 let name = get_text env p in
                 [G.Param (G.param_of_id (name, tok))]
             | _ ->
                 (* Complex pattern - use convert_pattern *)
                 let pattern = convert_pattern env p in
                 [G.ParamPattern pattern]
           ) params in

           (* Store clause with ALL guard branches *)
           add_function_clause env {
             name = name_str;
             name_tok = name_tok;
             params = param_list;
             branches = guard_branches;
             frettype = frettype;
           };

           (* Return a placeholder that will be filtered out later *)
           G.S (G.OtherStmt (G.OS_Pass, []) |> G.s)
         | _ ->
             G.S (G.OtherStmt (G.OS_Pass, []) |> G.s))

  | "match" ->
      (* Function match/body - convert children *)
      let children = get_children node in
      (* Filter out = token *)
      let children_filtered = List.filter (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
      ) children in

      (* Check if we have guards (pattern guards feature) *)
      let guards_node = List.find_opt (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "guards"
      ) children_filtered in

      (match guards_node with
       | Some guards ->
           (* Pattern guards detected! Extract guard conditions and body *)
           let guard_children = get_children guards in
           let body_nodes = List.filter (fun c ->
             c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "guards"
           ) children_filtered in
           let body_expr = match body_nodes with
             | [b] -> convert_expr env b
             | [] -> G.OtherExpr (("no_guard_body", fake_tok ""), []) |> G.e
             | bs -> G.Seq (List.map (convert_expr env) bs) |> G.e
           in

           (* Convert guards to conditional chain *)
           let cond_expr = List.fold_right (fun guard_node acc ->
             match guard_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
             | "boolean" ->
                 (* Boolean guard: | n < 0 *)
                 let guard_children = get_children guard_node in
                 let cond = match guard_children with
                   | [c] -> convert_expr env c
                   | _ -> G.OtherExpr (("guard_bool", fake_tok ""), []) |> G.e
                 in
                 G.Conditional (cond, body_expr, acc) |> G.e
             | "pattern_guard" ->
                 (* Pattern guard: | x <- xs *)
                 let guard_children = get_children guard_node in
                 (match guard_children with
                  | pattern_node :: expr_node :: _ ->
                      let pat = convert_pattern env pattern_node in
                      let iter_expr = convert_expr env expr_node in
                      (* Create LetPattern then conditional *)
                      G.Seq [
                        G.LetPattern (pat, iter_expr) |> G.e;
                        G.Conditional (G.L (G.Bool (true, fake_tok "true")) |> G.e, body_expr, acc) |> G.e
                      ] |> G.e
                  | _ -> acc)
             | _ -> acc
           ) guard_children (G.OtherExpr (("no_else", fake_tok ""), []) |> G.e) in

           G.E cond_expr

       | None ->
           (* No guards - normal match *)
           (match children_filtered with
            | [child] -> convert_node env child
            | [] -> G.E (G.OtherExpr (("empty_match", fake_tok ""), []) |> G.e)
            | children ->
                (* Multiple children - convert all *)
                let child_results = List.filter_map (fun child ->
                  match convert_node env child with
                  | G.E e -> Some e
                  | _ -> None
                ) children in
                match child_results with
                | [e] -> G.E e
                | es -> G.E (G.Seq es |> G.e)))

  (* Declarations *)
  (* Single declaration wrapper *)
  | "declaration" | "decl" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | _ ->
           let stmts = List.filter_map (fun c ->
             match convert_node env c with
             | G.S s -> Some s
             | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
             | _ -> None
           ) children in
           G.Ss stmts)

  (* Multiple declarations *)
  | "declarations" ->
      let children = get_children node in
      let stmts = List.filter_map (fun child ->
        match convert_node env child with
        | G.S stmt -> Some stmt
        | G.E expr ->
            (* Convert expr to expr statement *)
            Some (G.ExprStmt (expr, fake_tok ";") |> G.s)
        | _ -> None
      ) children in
      G.Ss stmts

  (* Module structure *)
  | "haskell" ->
      (* Root node - look for declarations child by type *)
      let children = get_children node in
      let decls = List.find_opt (fun c -> c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "declarations") children in
      (match decls with
       | Some d -> convert_node env d
       | None -> G.Ss [])

  | "header" ->
      (* Module header: module Foo where *)
      let children = get_children node in
      (match children with
       | name_node :: _ ->
           let module_name = get_text env name_node in
           let tok = make_tok env name_node in
           (* Create a module definition - dotted_ident is just a list of idents *)
           let module_def = G.ModuleDef {
             mbody = G.ModuleStruct (Some [(module_name, tok)], [])
           } in
           let ent = G.basic_entity (module_name, tok) in
           G.Ss [G.DefStmt (ent, module_def) |> G.s]
       | _ -> G.Ss [])

  | "imports" ->
      (* Import statements list *)
      let children = get_children node in
      let import_stmts = List.filter_map (fun child ->
        match child.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
        | "import" ->
            (* Parse import structure:
               import [qualified] Module [as Alias] [(...)] [hiding (...)]
            *)
            let full_text = get_text env child in
            let module_name = ref "" in
            let alias_name = ref None in
            let is_qualified = ref false in

            (* Simple parsing: extract module name and check for qualified/as *)
            let parts = String.split_on_char ' ' full_text in
            let rec parse_parts = function
              | "import" :: "qualified" :: mod_name :: rest ->
                  is_qualified := true;
                  module_name := mod_name;
                  parse_rest rest
              | "import" :: mod_name :: rest ->
                  module_name := mod_name;
                  parse_rest rest
              | _ -> ()
            and parse_rest = function
              | "as" :: alias :: _ ->
                  alias_name := Some alias
              | _ :: rest -> parse_rest rest
              | [] -> ()
            in
            parse_parts parts;

            if !module_name <> "" then
              let tok = make_tok env child in
              let mod_name = G.DottedName [(!module_name, tok)] in
              (* Create appropriate import based on structure *)
              let dir = match !alias_name with
                | Some alias ->
                    (* import qualified M as A -> ImportAs *)
                    let alias_full = ((alias, tok), G.empty_id_info ()) in
                    { G.d = G.ImportAs (tok, mod_name, Some alias_full); G.d_attrs = [] }
                | None when !is_qualified ->
                    (* import qualified M -> ImportAll but qualified *)
                    { G.d = G.ImportAll (tok, mod_name, tok); G.d_attrs = [] }
                | None ->
                    (* import M -> ImportAll *)
                    { G.d = G.ImportAll (tok, mod_name, tok); G.d_attrs = [] }
              in
              Some (G.DirectiveStmt dir |> G.s)
            else
              None
        | _ -> None
      ) children in
      G.Ss import_stmts

  (* Fixity declarations for operators *)
  | "fixity" | "infixl" | "infixr" ->
      (* Fixity declarations: infixl 6 +++ *)
      (* For now, just ignore them - they don't create executable code *)
      G.Ss []

  | "comment" | "haddock" | "cpp" | "pragma" ->
      (* Ignore comments and pragmas for now *)
      G.Ss []

  (* Wrappers - pass through to children *)
  | "expression" | "exp" | "pattern" | "parens" | "exp_parens" ->
      let children = get_children node in
      (* Filter out punctuation tokens: (, ), [, ], {, } *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "[" && t <> "]" && t <> "{" && t <> "}"
      ) children in
      (match children_filtered with
       | [child] -> convert_node env child
       | [] -> G.E (G.OtherExpr (("empty", fake_tok ""), []) |> G.e)
       | children ->
           (* Multiple children - try to convert all and merge *)
           let child_results = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           match child_results with
           | [] -> G.E (G.OtherExpr (("empty", fake_tok ""), []) |> G.e)
           | [e] -> G.E e
           | es -> G.E (G.Seq es |> G.e))

  (* Tuples *)
  | "tuple" | "exp_tuple" ->
      let children = get_children node in
      (* Filter out punctuation tokens: (, ), , *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> ","
      ) children in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children_filtered in

      (* Check if tuple contains ellipsis metavariable *)
      let has_ellipsis = List.exists (fun e ->
        match e.G.e with
        | G.Ellipsis _ -> true
        | G.N (G.Id ((name, _), _)) -> is_metavar_ellipsis name
        | _ -> false
      ) exprs in

      if has_ellipsis && env.is_pattern_mode then
        (* Pattern tuple with ellipsis: ($A, $...REST, $B) *)
        (* Semgrep handles this automatically via Ellipsis node *)
        G.E (G.Container (G.Tuple, fb exprs) |> G.e)
      else
        (* Regular tuple *)
        G.E (G.Container (G.Tuple, fb exprs) |> G.e)

  (* Lists *)
  | "list" | "exp_list" ->
      let children = get_children node in
      (* Filter out punctuation tokens: [, ], , *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "[" && t <> "]" && t <> ","
      ) children in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children_filtered in

      (* Check if list contains ellipsis metavariable *)
      let has_ellipsis = List.exists (fun e ->
        match e.G.e with
        | G.Ellipsis _ -> true
        | G.N (G.Id ((name, _), _)) -> is_metavar_ellipsis name
        | _ -> false
      ) exprs in

      if has_ellipsis && env.is_pattern_mode then
        (* Pattern list with ellipsis: [$A, $...REST, $B] *)
        (* Semgrep handles this automatically via Ellipsis node *)
        G.E (G.Container (G.List, fb exprs) |> G.e)
      else
        (* Regular list *)
        G.E (G.Container (G.List, fb exprs) |> G.e)

  (* List comprehensions: [expr | generators] *)
  | "list_comprehension" | "exp_list_comprehension" ->
      let children = get_children node in
      (* Filter out punctuation tokens: [, ], |, , *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "[" && t <> "]" && t <> "|" && t <> ","
      ) children in
      (* First child is the expression, rest are generators/guards *)
      (match children_filtered with
       | expr_node :: generator_nodes ->
           let result_expr = convert_expr env expr_node in
           (* Convert generators: x <- list becomes CompFor *)
           let comprehension_parts = List.filter_map (fun gen ->
             match gen.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
             | "generator" ->
                 (* x <- expr *)
                 let gen_children = get_children gen in
                 (match gen_children with
                  | pattern_node :: expr_node :: _ ->
                      let pat = convert_pattern env pattern_node in
                      let iter_expr = convert_expr env expr_node in
                      Some (G.CompFor (fake_tok "for", pat, fake_tok "<-", iter_expr))
                  | _ -> None)
             | "qualifiers" ->
                 (* ParallelListComp: multiple qualifiers separated by | *)
                 (* Process children recursively - they contain generators/guards *)
                 let qualifier_children = get_children gen in
                 let nested_parts = List.filter_map (fun qual_child ->
                   match qual_child.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
                   | "generator" ->
                       let gen_children = get_children qual_child in
                       (match gen_children with
                        | pattern_node :: expr_node :: _ ->
                            let pat = convert_pattern env pattern_node in
                            let iter_expr = convert_expr env expr_node in
                            Some (G.CompFor (fake_tok "for", pat, fake_tok "<-", iter_expr))
                        | _ -> None)
                   | _ -> None
                 ) qualifier_children in
                 (* Return first valid part, or None if empty *)
                 (match nested_parts with
                  | x :: _ -> Some x
                  | [] -> None)
             | "group" ->
                 (* TransformListComp: then group by key using func *)
                 let group_children = get_children gen in
                 (* Extract key and classifier expressions *)
                 let key_expr = List.find_opt (fun c ->
                   c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "key"
                 ) group_children in
                 let classifier_expr = List.find_opt (fun c ->
                   c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "classifier"
                 ) group_children in
                 (* Convert to CompFor with special annotation *)
                 (match key_expr, classifier_expr with
                  | Some key, Some classifier ->
                      let key_conv = convert_expr env key in
                      let classifier_conv = convert_expr env classifier in
                      (* Represent as CompFor with combined expression *)
                      Some (G.CompFor (fake_tok "group", G.PatWildcard (fake_tok "_"), fake_tok "by",
                            G.Seq [key_conv; classifier_conv] |> G.e))
                  | _ -> None)
             | "transform" ->
                 (* TransformListComp: then sortWith by expr using func *)
                 let transform_children = get_children gen in
                 (* Extract transformation expression *)
                 let trans_expr = List.find_opt (fun c ->
                   c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "transformation"
                 ) transform_children in
                 (match trans_expr with
                  | Some trans ->
                      let trans_conv = convert_expr env trans in
                      (* Represent as CompFor with special annotation *)
                      Some (G.CompFor (fake_tok "transform", G.PatWildcard (fake_tok "_"), fake_tok "using", trans_conv))
                  | _ -> None)
             | "guard" | "boolean_guard" ->
                 (* Predicate guard: even x *)
                 (* Extract children from guard node for more robust parsing *)
                 let guard_children = get_children gen in
                 if List.length guard_children > 0 then
                   (* Guard has children - filter punctuation and convert *)
                   let guard_children_filtered = List.filter (fun c ->
                     let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
                     t <> "|" && t <> "," && t <> ";"
                   ) guard_children in
                   (match guard_children_filtered with
                    | [] -> None  (* No valid children after filtering *)
                    | [guard_expr_node] ->
                        (* Single child - convert it *)
                        let guard_expr = convert_expr env guard_expr_node in
                        Some (G.CompIf (fake_tok "if", guard_expr))
                    | multiple_children ->
                        (* Multiple children - convert all and use first *)
                        let guard_exprs = List.filter_map (fun c ->
                          match convert_node env c with
                          | G.E e -> Some e
                          | _ -> None
                        ) multiple_children in
                        (match guard_exprs with
                         | [] -> None
                         | e :: _ -> Some (G.CompIf (fake_tok "if", e))))
                 else
                   (* Guard has no children - convert node directly *)
                   let guard_expr = convert_expr env gen in
                   Some (G.CompIf (fake_tok "if", guard_expr))
             | _ -> None
           ) generator_nodes in
           (* Comprehension expects (expr * for_or_if_comp list), wrapped in bracket *)
           G.E (G.Comprehension (G.List, fb (result_expr, comprehension_parts)) |> G.e)
       | _ -> G.E (G.Container (G.List, fb []) |> G.e))

  (* Conditional (if/then/else) *)
  | "conditional" | "if" | "exp_conditional" ->
      let children = get_children node in
      (* Filter out keyword tokens: "if", "then", "else", ";" *)
      let expr_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "if" && t <> "then" && t <> "else" && t <> ";"
      ) children in
      (match expr_children with
       | cond :: then_branch :: else_branch :: _ ->
           (* With else *)
           let cond_expr = convert_expr env cond in
           let then_expr = convert_expr env then_branch in
           let else_expr = convert_expr env else_branch in
           (* Check if condition protects a variable *)
           (* For positive guards (y > 0, y >= 1, y /= 0), protect the THEN branch *)
           let protected_then = match extract_protected_variable cond_expr with
             | Some _ -> annotate_protected_expr then_expr
             | None -> then_expr
           in
           (* For negative guards (y == 0, y <= 0, y < 1, null xs, i < 0), protect the ELSE branch *)
           let protected_else = match guard_protects_zero cond_expr with
             | Some _ -> annotate_protected_expr else_expr
             | None ->
                 (* Also check for empty list protection *)
                 (match guard_protects_empty cond_expr with
                  | Some _ -> annotate_protected_expr else_expr
                  | None ->
                      (* Also check for bounds protection *)
                      (match guard_protects_bounds cond_expr with
                       | Some _ -> annotate_protected_expr else_expr
                       | None -> else_expr))
           in
           G.E (G.Conditional (cond_expr, protected_then, protected_else) |> G.e)
       | [cond; then_branch] ->
           (* Without else - use empty expr *)
           let cond_expr = convert_expr env cond in
           let then_expr = convert_expr env then_branch in
           (* Check if condition protects a variable *)
           (* For positive guards (y > 0, y >= 1, y /= 0), protect the THEN branch *)
           let protected_then = match extract_protected_variable cond_expr with
             | Some _ -> annotate_protected_expr then_expr
             | None -> then_expr
           in
           (* Note: No else branch to protect here *)
           G.E (G.Conditional (cond_expr, protected_then, G.OtherExpr (("no_else", fake_tok ""), []) |> G.e) |> G.e)
       | _ -> G.E (G.OtherExpr (("conditional", fake_tok ""), []) |> G.e))

  (* Lambda expressions *)
  | "lambda" | "exp_lambda" ->
      let children = get_children node in
      (* Filter out punctuation tokens: backslash and arrow *)
      let children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "\\" && t <> "->" && t <> "arrow" && t <> "_arrow"
      ) children in
      (* Lambda: \x -> body or \x y -> body *)
      let params, body = match children with
        | [] -> ([], G.OtherExpr (("empty_lambda", fake_tok ""), []) |> G.e)
        | [body_node] -> ([], convert_expr env body_node)
        | children ->
            (* Split params from body - last child is body, rest are params *)
            let rev_children = List.rev children in
            (match rev_children with
             | body_node :: param_nodes ->
                 (List.rev param_nodes, convert_expr env body_node)
             | [] -> ([], G.OtherExpr (("empty_lambda", fake_tok ""), []) |> G.e))
      in
      let lambda_params = List.map (fun p ->
        let raw_name = get_text env p in
        let name = resolve_metavar_text env raw_name in
        let tok = make_tok ~override_text:name env p in
        G.Param (G.param_of_id (name, tok))
      ) params in
      let fdef = G.{ fkind = (G.Arrow, fake_tok ""); fparams = fb lambda_params; frettype = None; fbody = G.FBExpr body } in
      G.E (G.Lambda fdef |> G.e)

  (* Lambda case: backslash-case -> alternatives *)
  | "lambda_case" ->
      let children = get_children node in
      let tok = make_tok env node in
      (* Filter out keyword tokens: backslash, case, of *)
      let expr_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "\\" && t <> "case" && t <> "of"
      ) children in
      (* Extract alternatives - they may be in an "alternatives" wrapper node *)
      let alternatives = match expr_children with
        | [alternatives_node] when alternatives_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "alternatives" ->
            get_children alternatives_node
        | _ -> expr_children
      in
      (* Parse alternatives directly to extract pattern AND body *)
      let cases = List.map (fun alt ->
        if alt.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "alternative" then
          (* Extract pattern and body from alternative node *)
          let alt_children = get_children alt in
          let filtered = List.filter (fun c ->
            let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
            t <> "->" && t <> "arrow" && t <> "_arrow"
          ) alt_children in
          (match filtered with
           | pat_node :: body_or_nodes ->
               (* Extract pattern *)
               let pattern = convert_pattern env pat_node in
               (* Extract body (may be wrapped in match node with guards) *)
               let body_expr = match body_or_nodes with
                 | [body] -> convert_expr env body
                 | [] -> G.OtherExpr (("no_alt_body", fake_tok ""), []) |> G.e
                 | body :: _ -> convert_expr env body
               in
               G.CasesAndBody ([G.Case (tok, pattern)], G.ExprStmt (body_expr, fake_tok ";") |> G.s)
           | _ ->
               G.CasesAndBody ([G.Default tok], G.OtherStmt (G.OS_Todo, []) |> G.s))
        else
          (* Fallback for non-alternative nodes *)
          match convert_node env alt with
          | G.S s -> G.CasesAndBody ([G.Default tok], s)
          | G.E e -> G.CasesAndBody ([G.Default tok], G.ExprStmt (e, fake_tok ";") |> G.s)
          | G.Ss stmts -> G.CasesAndBody ([G.Default tok], G.Block (fb stmts) |> G.s)
          | _ -> G.CasesAndBody ([G.Default tok], G.OtherStmt (G.OS_Todo, []) |> G.s)
      ) alternatives in
      (* Create a Switch statement as the lambda body *)
      (* LambdaCase has an implicit parameter that we need to represent *)
      let param_name = "_case_param" in
      let param_tok = fake_tok param_name in
      let match_expr = G.N (G.Id ((param_name, param_tok), G.empty_id_info ())) |> G.e in
      let switch_stmt = G.Switch (tok, Some (G.Cond match_expr), cases) |> G.s in
      (* Wrap in a Lambda with one implicit parameter *)
      let fdef = G.{
        fkind = (G.Arrow, fake_tok "");
        fparams = fb [G.Param (G.param_of_id (param_name, param_tok))];
        frettype = None;
        fbody = G.FBStmt switch_stmt;
      } in
      G.E (G.Lambda fdef |> G.e)

  (* Operator sections: (+1), (>0), (2x) *)
  | "left_section" ->
      (* (expr op) -> \x -> expr op x *)
      let children = get_children node in
      (* Filter out punctuation tokens: (, ), [, ] *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "[" && t <> "]"
      ) children in
      (match children_filtered with
       | left_expr_node :: op_node :: _ ->
           let left_expr = convert_expr env left_expr_node in
           let op_name = get_text env op_node in
           let op_tok = make_tok env op_node in
           (* Create lambda: \x -> left_expr op x *)
           let param_name = "_x" in
           let param_tok = fake_tok param_name in
           let param_expr = G.N (G.Id ((param_name, param_tok), G.empty_id_info ())) |> G.e in
           let op_expr = G.N (G.Id ((op_name, op_tok), G.empty_id_info ())) |> G.e in
           let body = G.Call (op_expr, fb [G.Arg left_expr; G.Arg param_expr]) |> G.e in
           let fdef = G.{ fkind = (G.Arrow, fake_tok "");
                         fparams = fb [G.Param (G.param_of_id (param_name, param_tok))];
                         frettype = None;
                         fbody = G.FBExpr body } in
           G.E (G.Lambda fdef |> G.e)
       | _ -> G.E (G.OtherExpr (("left_section", fake_tok ""), []) |> G.e))

  | "right_section" ->
      (* (op expr) -> \x -> x op expr *)
      let children = get_children node in
      (* Filter out punctuation tokens: (, ), [, ] *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "[" && t <> "]"
      ) children in
      (match children_filtered with
       | op_node :: right_expr_node :: _ ->
           let right_expr = convert_expr env right_expr_node in
           let op_name = get_text env op_node in
           let op_tok = make_tok env op_node in
           (* Create lambda: \x -> x op right_expr *)
           let param_name = "_x" in
           let param_tok = fake_tok param_name in
           let param_expr = G.N (G.Id ((param_name, param_tok), G.empty_id_info ())) |> G.e in
           let op_expr = G.N (G.Id ((op_name, op_tok), G.empty_id_info ())) |> G.e in
           let body = G.Call (op_expr, fb [G.Arg param_expr; G.Arg right_expr]) |> G.e in
           let fdef = G.{ fkind = (G.Arrow, fake_tok "");
                         fparams = fb [G.Param (G.param_of_id (param_name, param_tok))];
                         frettype = None;
                         fbody = G.FBExpr body } in
           G.E (G.Lambda fdef |> G.e)
       | _ -> G.E (G.OtherExpr (("right_section", fake_tok ""), []) |> G.e))

  (* RequiredTypeArguments: (type a) explicit type arguments *)
  | "explicit_type" ->
      let children = get_children node in
      (* Extract the type expression after 'type' keyword *)
      let type_expr = match children with
        | type_node :: _ -> convert_expr env type_node
        | [] -> G.OtherExpr (("empty_explicit_type", fake_tok ""), []) |> G.e
      in
      let tok = make_tok env node in
      (* Represent as OtherExpr with explicit_type annotation *)
      G.E (G.OtherExpr (("explicit_type", tok), [G.E type_expr]) |> G.e)

  (* Type annotation in expression: (42 :: Int) *)
  | "signature" ->
      let children = get_children node in
      (* Check if this is expression signature or top-level *)
      let parent_is_expr = true in (* Simplified - assume expr context if we reach here *)
      if parent_is_expr && List.length children >= 2 then
        (* Expression signature: expr :: type *)
        let expr_node = List.hd children in
        let type_nodes = List.tl children in
        let expr = convert_expr env expr_node in
        (* Extract type text and parse it *)
        let type_text = String.concat " " (List.map (get_text env) type_nodes) in
        (* In pattern mode, resolve metavar names in type identifiers *)
        let resolve_name s = resolve_metavar_text env s in
        let type_parsed =
          try Haskell_type_parser.parse_and_convert_with_metavars type_text resolve_name
          with _ ->
            let resolved_text = resolve_metavar_text env type_text in
            { G.t = G.TyN (G.Id ((resolved_text, fake_tok resolved_text), G.empty_id_info ()));
              G.t_attrs = [] }
        in
        (* Create Cast expression: (Type) expr *)
        G.E (G.Cast (type_parsed, fake_tok "::", expr) |> G.e)
      else
        (* Top-level signature - handled by existing code below *)
        let children = get_children node in
        (match children with
         | name_node :: type_parts ->
             let func_name = get_text env name_node in
             let type_text =
               if List.length type_parts > 0 then
                 get_text env node
               else
                 ""
             in
             add_type_signature env func_name type_text;
             G.S (G.OtherStmt (G.OS_Pass, []) |> G.s)
         | _ -> G.S (G.OtherStmt (G.OS_Pass, []) |> G.s))

  (* Do blocks *)
  | "do" | "do_module" | "exp_do" ->
      let children = get_children node in
      (* Set flag to mark we're inside a do-block *)
      let env_in_do = { env with in_do_block = true } in
      let stmts = List.filter_map (fun child ->
        match convert_node env_in_do child with
        | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
        | G.S s -> Some s
        | _ -> None
      ) children in
      G.E (G.StmtExpr (G.Block (fb stmts) |> G.s) |> G.e)

  (* Case expressions *)
  | "case" | "exp_case" ->
      let children = get_children node in
      let tok = make_tok env node in
      (* Filter out keyword tokens: "case", "of" *)
      let expr_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "case" && t <> "of"
      ) children in
      (match expr_children with
       | expr :: rest ->
           let expr = convert_expr env expr in
           (* Extract alternatives - they may be in an "alternatives" wrapper node *)
           let alternatives = match rest with
             | [alternatives_node] when alternatives_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "alternatives" ->
                 get_children alternatives_node
             | _ -> rest
           in
           (* Parse alternatives directly to extract pattern AND body *)
           let cases = List.map (fun alt ->
             if alt.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "alternative" then
               (* Extract pattern and body from alternative node *)
               let alt_children = get_children alt in
               let filtered = List.filter (fun c ->
                 let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
                 t <> "->" && t <> "arrow" && t <> "_arrow"
               ) alt_children in
               (match filtered with
                | pat_node :: body_or_nodes ->
                    (* Extract pattern *)
                    let pattern = convert_pattern env pat_node in
                    (* Extract body (may be wrapped in match node with guards) *)
                    let body_expr = match body_or_nodes with
                      | [body] -> convert_expr env body
                      | [] -> G.OtherExpr (("no_alt_body", fake_tok ""), []) |> G.e
                      | body :: _ -> convert_expr env body
                    in
                    G.CasesAndBody ([G.Case (tok, pattern)], G.ExprStmt (body_expr, fake_tok ";") |> G.s)
                | _ ->
                    G.CasesAndBody ([G.Default tok], G.OtherStmt (G.OS_Todo, []) |> G.s))
             else
               (* Fallback for non-alternative nodes *)
               match convert_node env alt with
               | G.S s -> G.CasesAndBody ([G.Default tok], s)
               | G.E e -> G.CasesAndBody ([G.Default tok], G.ExprStmt (e, fake_tok ";") |> G.s)
               | G.Ss stmts -> G.CasesAndBody ([G.Default tok], G.Block (fb stmts) |> G.s)
               | _ -> G.CasesAndBody ([G.Default tok], G.OtherStmt (G.OS_Todo, []) |> G.s)
           ) alternatives in
           G.E (G.StmtExpr (G.Switch (tok, Some (G.Cond expr), cases) |> G.s) |> G.e)
       | _ -> G.E (G.OtherExpr (("case", fake_tok ""), []) |> G.e))

  (* Alternative (case branch) *)
  | "alternative" ->
      let children = get_children node in
      (* Filter out tokens: "->", but keep match/guards for processing *)
      let filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        (* Keep pattern nodes and expression nodes, filter out arrow tokens only *)
        t <> "->" && t <> "arrow" && t <> "_arrow"
      ) children in
      (match filtered with
       | pat :: body_or_nodes ->
           let _pattern = convert_pattern env pat in
           (* Body might be wrapped in "match" node - convert it (match now handles guards) *)
           let body_expr = match body_or_nodes with
             | [body] -> convert_expr env body
             | [] -> G.OtherExpr (("no_alt_body", fake_tok ""), []) |> G.e
             | body :: _ ->
                 (* Take first child - it might be a match node with guards *)
                 convert_expr env body
           in
           G.S (G.ExprStmt (body_expr, fake_tok ";") |> G.s)
       | _ -> G.S (G.OtherStmt (G.OS_Todo, []) |> G.s))

  (* Let bindings *)
  | "let_in" ->
      (* let x = expr in body *)
      (* Improved support for nested and multiple bindings *)
      let children = get_children node in
      (* Filter out "let", "in" tokens *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "let" && t <> "in"
      ) children in

      (* Helper function to process a single binding node *)
      let process_binding binding_node =
        let binding_type = binding_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        if binding_type = "bind" || binding_type = "function" then
          (* This is a "bind" node - extract name and expression *)
          let binding_children = get_children binding_node in
          (match binding_children with
           | name_node :: rest ->
               let raw_name = get_text env name_node in
               let var_name = resolve_metavar_text env raw_name in
               let var_tok = make_tok ~override_text:var_name env name_node in
               let pattern = G.PatId ((var_name, var_tok), G.empty_id_info ()) in
               (* Filter out = token and extract expression *)
               let expr_nodes = List.filter (fun c ->
                 c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
               ) rest in
               (match expr_nodes with
                | [expr_node] ->
                    let binding_expr = convert_expr env expr_node in
                    Some (pattern, binding_expr)
                | _ ->
                    (* No expression found - cannot process this binding *)
                    None)
           | [] -> None)
        else
          (* Not a bind node - extract manually *)
          let binding_children = get_children binding_node in
          let binding_children_filtered = List.filter (fun c ->
            c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
          ) binding_children in
          (match binding_children_filtered with
           | name_node :: expr_node :: _ ->
               let raw_name = get_text env name_node in
               let var_name = resolve_metavar_text env raw_name in
               let var_tok = make_tok ~override_text:var_name env name_node in
               let pattern = G.PatId ((var_name, var_tok), G.empty_id_info ()) in
               let binding_expr = convert_expr env expr_node in
               Some (pattern, binding_expr)
           | _ -> None)
      in

      (* Separate local_binds (which contain bindings) from body *)
      let local_binds_nodes = List.filter (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "local_binds"
      ) children_filtered in
      let body_nodes = List.filter (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "local_binds"
      ) children_filtered in

      (match body_nodes with
       | [body_node] ->
           let body_expr = convert_expr env body_node in
           (* Extract bindings from local_binds nodes *)
           let all_binding_nodes = List.concat_map (fun local_binds ->
             get_children local_binds
           ) local_binds_nodes in
           (* Process all bindings *)
           let bindings = List.filter_map process_binding all_binding_nodes in
           (* Create sequence of let bindings followed by body *)
           let let_exprs = List.map (fun (pat, expr) ->
             G.LetPattern (pat, expr) |> G.e
           ) bindings in
           G.E (G.Seq (let_exprs @ [body_expr]) |> G.e)
       | _ ->
           (* No body or multiple bodies - fallback *)
           G.E (G.OtherExpr (("malformed_let_in", fake_tok ""), []) |> G.e))

  | "let" | "exp_let" ->
      (* Let statement in do block - extract bindings and create LetPattern expressions *)
      let children = get_children node in
      (* Filter out "let" keyword token and get local_binds *)
      let local_binds_nodes = List.filter (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "local_binds"
      ) children in

      (* Helper function to process bindings (same as above) *)
      let process_binding_do binding_node =
        let binding_type = binding_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        if binding_type = "bind" || binding_type = "function" then
          let binding_children = get_children binding_node in
          (match binding_children with
           | name_node :: rest ->
               let var_name = get_text env name_node in
               let var_tok = make_tok env name_node in
               let pattern = G.PatId ((var_name, var_tok), G.empty_id_info ()) in
               let expr_nodes = List.filter (fun c ->
                 c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
               ) rest in
               (match expr_nodes with
                | [expr_node] ->
                    let binding_expr = convert_expr env expr_node in
                    Some (pattern, binding_expr)
                | _ -> None)
           | [] -> None)
        else
          let binding_children = get_children binding_node in
          let binding_children_filtered = List.filter (fun c ->
            c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "="
          ) binding_children in
          (match binding_children_filtered with
           | name_node :: expr_node :: _ ->
               let var_name = get_text env name_node in
               let var_tok = make_tok env name_node in
               let pattern = G.PatId ((var_name, var_tok), G.empty_id_info ()) in
               let binding_expr = convert_expr env expr_node in
               Some (pattern, binding_expr)
           | _ -> None)
      in

      (* Extract all bindings from local_binds nodes *)
      let all_binding_nodes = List.concat_map (fun local_binds ->
        get_children local_binds
      ) local_binds_nodes in
      let bindings = List.filter_map process_binding_do all_binding_nodes in

      (* Create LetPattern expressions wrapped in ExprStmt for do-notation *)
      let let_stmts = List.map (fun (pat, expr) ->
        G.ExprStmt (G.LetPattern (pat, expr) |> G.e, fake_tok ";") |> G.s
      ) bindings in

      (match let_stmts with
       | [] -> G.E (G.OtherExpr (("empty_let", fake_tok ""), []) |> G.e)
       | [s] -> G.S s
       | stmts -> G.Ss stmts)

  (* Guards (guarded right-hand side) *)
  | "guard" | "guarded_rhs" ->
      let children = get_children node in
      (match children with
       | guard :: body :: _ ->
           (* Check if guard contains pattern binding: pattern <- expr *)
           let guard_children = get_children guard in
           let has_pattern_binding = List.exists (fun c ->
             c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "arrow"
             || get_text env c = "<-"
           ) guard_children in

           if has_pattern_binding && List.length guard_children >= 3 then
             (* Pattern guard: pattern <- expr *)
             (* Assume structure: pattern, arrow, expr *)
             (match guard_children with
              | pattern_node :: _ :: expr_node :: _ ->
                  let pattern = convert_pattern env pattern_node in
                  let iter_expr = convert_expr env expr_node in
                  let body_expr = convert_expr env body in
                  (* Create LetPattern followed by body *)
                  G.E (G.Seq [
                    G.LetPattern (pattern, iter_expr) |> G.e;
                    body_expr
                  ] |> G.e)
              | _ ->
                  (* Fallback to regular guard *)
                  let guard_expr = convert_expr env guard in
                  let body_expr = convert_expr env body in
                  G.E (G.Conditional (guard_expr, body_expr, G.OtherExpr (("no_else", fake_tok ""), []) |> G.e) |> G.e))
           else
             (* Regular boolean guard: condition *)
             let guard_expr = convert_expr env guard in
             let body_expr = convert_expr env body in
             G.E (G.Conditional (guard_expr, body_expr, G.OtherExpr (("no_else", fake_tok ""), []) |> G.e) |> G.e)
       | _ -> G.E (G.OtherExpr (("guard", fake_tok ""), []) |> G.e))

  (* Prefix operator *)
  | "prefix" ->
      let children = get_children node in
      (match children with
       | op :: expr :: _ ->
           let op_tok = make_tok env op in
           let expr = convert_expr env expr in
           G.E (G.Call (G.IdSpecial (G.Op G.Not, op_tok) |> G.e, fb [G.Arg expr]) |> G.e)
       | _ -> G.E (G.OtherExpr (("prefix", fake_tok ""), []) |> G.e))

  (* Generator (do notation: x <- action) *)
  | "generator" ->
      let children = get_children node in
      (match children with
       | pattern_node :: expr_node :: _ ->
           let pat = convert_pattern env pattern_node in
           let expr = convert_expr env expr_node in
           let tok = make_tok env node in
           (* Convert to variable definition for better taint tracking *)
           (* x <- action becomes let x = action *)
           (match pat with
            | G.PatId (id, _) ->
                (* Simple pattern: convert to variable definition *)
                let var_def = G.VarDef {
                  G.vinit = Some expr;
                  vtype = None;
                  vtok = G.no_sc;
                } in
                G.S (G.DefStmt (G.basic_entity id, var_def) |> G.s)
            | _ ->
                (* Complex pattern: keep as ForEach for pattern matching *)
                G.S (G.For (tok, G.ForEach (pat, fake_tok "<-", expr), G.Block (fb []) |> G.s) |> G.s))
       | _ -> G.E (G.OtherExpr (("generator", fake_tok ""), []) |> G.e))

  (* Where bindings - local declarations *)
  | "where" | "local_binds" ->
      let children = get_children node in
      let stmts = List.filter_map (fun child ->
        match convert_node env child with
        | G.S stmt -> Some stmt
        | G.E expr -> Some (G.ExprStmt (expr, fake_tok ";") |> G.s)
        | _ -> None
      ) children in
      (match stmts with
       | [] -> G.Ss []
       | _ -> G.Ss stmts)

  (* Data type declarations *)
  | "data_type" | "newtype" ->
      let children = get_children node in
      (match children with
       | name_node :: rest ->
           let name_tok = make_tok env name_node in
           let name_str = get_text env name_node in
           (* Check for record fields in children *)
           let has_fields = List.exists (fun c ->
             c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "record"
             || c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "field"
           ) rest in
           (* Extract deriving clauses *)
           let deriving_attrs = List.filter_map (fun c ->
             if c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "deriving" then
               let deriving_text = get_text env c in
               let tok = make_tok env c in
               (* NamedAttr expects (tok, name, arguments) *)
               Some (G.NamedAttr (tok, G.Id (("deriving", tok), G.empty_id_info ()), fb [G.Arg (G.L (G.String (fake_tok "\"", (deriving_text, tok), fake_tok "\"")) |> G.e)]))
             else
               None
           ) rest in
           let ent = { (G.basic_entity (name_str, name_tok)) with G.attrs = deriving_attrs } in
           G.S (G.DefStmt (ent, G.TypeDef {
             tbody = if has_fields then
               (* Record type with fields *)
               G.OtherTypeKind (("record", fake_tok ""), [])
             else
               (* Regular data type *)
               G.OtherTypeKind (("data", fake_tok ""), []);
           }) |> G.s)
       | _ -> G.Ss [])

  (* Type aliases *)
  | "type_synonym" ->
      let children = get_children node in
      (match children with
       | name_node :: _ ->
           let name_tok = make_tok env name_node in
           let name_str = get_text env name_node in
           let ent = G.basic_entity (name_str, name_tok) in
           G.S (G.DefStmt (ent, G.TypeDef {
             tbody = G.OtherTypeKind (("type_alias", fake_tok ""), []);
           }) |> G.s)
       | _ -> G.Ss [])

  (* Type signatures handled above in line 538 - removed duplicate *)

  (* Type class declarations *)
  | "class" | "class_decl" ->
      let children = get_children node in
      (match children with
       | name_node :: rest ->
           let name_tok = make_tok env name_node in
           let name_str = get_text env name_node in
           (* Convert class methods (they are children) *)
           let methods = List.filter_map (fun child ->
             match convert_node env child with
             | G.S s -> Some s
             | _ -> None
           ) rest in
           let ent = G.basic_entity (name_str, name_tok) in
           (* Create class with methods as body *)
           let class_def = G.DefStmt (ent, G.TypeDef {
             tbody = G.OtherTypeKind (("class", fake_tok ""),
               List.map (fun s -> G.S s) methods);
           }) |> G.s in
           G.S class_def
       | _ -> G.Ss [])

  (* Instance declarations *)
  | "instance" | "instance_decl" ->
      let children = get_children node in
      (* Instance: instance Eq Int where x == y = ... *)
      (match children with
       | class_name :: type_name :: rest ->
           let class_str = get_text env class_name in
           let type_str = get_text env type_name in
           let tok = make_tok env node in
           (* Convert instance methods *)
           let methods = List.filter_map (fun child ->
             match convert_node env child with
             | G.S s -> Some s
             | _ -> None
           ) rest in
           (* Create instance as a type definition *)
           let instance_name = class_str ^ " " ^ type_str in
           let ent = G.basic_entity (instance_name, tok) in
           let instance_def = G.DefStmt (ent, G.TypeDef {
             tbody = G.OtherTypeKind (("instance", fake_tok ""),
               List.map (fun s -> G.S s) methods);
           }) |> G.s in
           G.S instance_def
       | _ ->
           (* Fallback: just convert children *)
           let stmts = List.filter_map (fun c ->
             match convert_node env c with
             | G.S s -> Some s
             | _ -> None
           ) children in
           G.Ss stmts)

  (* Record expressions: Person { name = "John", age = 30 } *)
  | "record" | "record_expression" | "exp_record" ->
      let children = get_children node in
      (* Check if there's a base expression (record update) or just fields (record construction) *)
      let base_expr_opt, field_nodes =
        match children with
        | first :: rest ->
            (* Check if first child looks like an expression vs field *)
            if first.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "field_update"
               || first.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "field" then
              (None, children)
            else
              (Some (convert_expr env first), rest)
        | [] -> (None, [])
      in
      (* Extract field updates: field = expr *)
      let field_stmts = List.filter_map (fun field_node ->
        let field_children = get_children field_node in
        match field_children with
        | field_name_node :: expr_node :: _ ->
            let field_name = get_text env field_name_node in
            let field_tok = make_tok env field_name_node in
            let field_expr = convert_expr env expr_node in
            (* Create field as DefStmt *)
            let ent = G.basic_entity (field_name, field_tok) in
            let field_stmt = G.DefStmt (ent, G.VarDef { G.vinit = Some field_expr; G.vtype = None; G.vtok = Some (fake_tok "=") }) |> G.s in
            Some (G.F field_stmt)
        | _ -> None
      ) field_nodes in
      (match base_expr_opt with
       | Some base_expr ->
           (* Record update: base { fields } *)
           (* This is like a function call to update *)
           G.E (G.OtherExpr (("record_update", fake_tok ""), [G.E base_expr; G.E (G.Record (fb field_stmts) |> G.e)]) |> G.e)
       | None ->
           (* Record construction: { fields } *)
           G.E (G.Record (fb field_stmts) |> G.e))

  (* Record field projection: person.name *)
  | "projection" ->
      let children = get_children node in
      (match children with
       | expr_node :: field_node :: _ ->
           let base_expr = convert_expr env expr_node in
           let field_name = get_text env field_node in
           let field_tok = make_tok env field_node in
           (* DotAccess: expr.field *)
           G.E (G.DotAccess (base_expr, field_tok, G.FN (G.Id ((field_name, field_tok), G.empty_id_info ()))) |> G.e)
       | _ -> G.E (G.OtherExpr (("projection", fake_tok ""), []) |> G.e))

  (* Linear Types: a %1 -> b *)
  | "linear_function" ->
      let children = get_children node in
      (* Structure: param, modifier (with multiplicity), result *)
      let param_expr, multiplicity_text, result_expr = match children with
        | param :: modifier :: result :: _ ->
            (* Extract multiplicity from modifier → literal → integer *)
            let mult_text = match get_children modifier with
              | literal_node :: _ ->
                  (match get_children literal_node with
                   | int_node :: _ -> get_text env int_node
                   | [] -> "1")
              | [] -> "1"
            in
            (convert_expr env param, mult_text, convert_expr env result)
        | _ -> (G.OtherExpr (("empty_linear", fake_tok ""), []) |> G.e, "1", G.OtherExpr (("empty_linear", fake_tok ""), []) |> G.e)
      in
      (* Represent as function application with multiplicity annotation *)
      let mult_tok = fake_tok ("%" ^ multiplicity_text) in
      let linear_arrow = G.N (G.Id (("linear_arrow_" ^ multiplicity_text, mult_tok), G.empty_id_info ())) |> G.e in
      G.E (G.Call (linear_arrow, fb [G.Arg param_expr; G.Arg result_expr]) |> G.e)

  (* MultiWayIf: if | guard -> expr | guard -> expr *)
  | "multi_way_if" ->
      let children = get_children node in
      (* Each child is a "match" node with guards *)
      let branches = List.filter_map (fun match_node ->
        if match_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "match" then
          let match_children = get_children match_node in
          (* Find guards node and body *)
          let guards_node = List.find_opt (fun c ->
            c.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "guards"
          ) match_children in
          let body_nodes = List.filter (fun c ->
            c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "guards"
          ) match_children in
          let body_expr = match body_nodes with
            | [b] -> convert_expr env b
            | [] -> G.OtherExpr (("no_mwif_body", fake_tok ""), []) |> G.e
            | bs -> G.Seq (List.map (convert_expr env) bs) |> G.e
          in
          (match guards_node with
           | Some guards ->
               let guard_children = get_children guards in
               let guard_expr = match guard_children with
                 | [g] -> convert_expr env g
                 | gs -> G.Seq (List.map (convert_expr env) gs) |> G.e
               in
               Some (guard_expr, body_expr)
           | None -> Some (G.L (G.Bool (true, fake_tok "true")) |> G.e, body_expr))
        else
          None
      ) children in
      (* Build nested conditionals *)
      let result = List.fold_right (fun (guard, body) acc ->
        G.Conditional (guard, body, acc) |> G.e
      ) branches (G.OtherExpr (("no_else", fake_tok ""), []) |> G.e) in
      G.E result

  (* RecursiveDo: mdo { ... } *)
  | "mdo" ->
      let children = get_children node in
      (* mdo is like do but with recursive bindings *)
      let stmts = List.filter_map (fun child ->
        match convert_node env child with
        | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
        | G.S s -> Some s
        | _ -> None
      ) children in
      (* Annotate with "mdo" in the block *)
      let mdo_comment = G.OtherStmt (G.OS_Todo, [G.E (G.N (G.Id (("mdo_recursive_do", fake_tok "mdo"), G.empty_id_info ())) |> G.e)]) |> G.s in
      G.E (G.StmtExpr (G.Block (fb (mdo_comment :: stmts)) |> G.s) |> G.e)

  (* Pattern Synonyms *)
  | "pattern_synonym" ->
      let children = get_children node in
      (* Pattern synonym: pattern Name = expr or pattern Name <- expr where ... *)
      let name_node, rest = match children with
        | n :: r -> (n, r)
        | [] -> (node, [])
      in
      let name_str = get_text env name_node in
      let name_tok = make_tok env name_node in

      (* Convert the pattern definition (RHS) *)
      let body_exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) rest in

      let body = match body_exprs with
        | [] -> G.OtherExpr (("empty_pattern_syn", fake_tok ""), []) |> G.e
        | [e] -> e
        | es -> G.Seq es |> G.e
      in

      (* Create as a type definition (pattern synonym) *)
      let ent = G.basic_entity (name_str, name_tok) in
      G.S (G.DefStmt (ent, G.TypeDef {
        tbody = G.OtherTypeKind (("pattern_synonym", fake_tok ""), [G.E body]);
      }) |> G.s)

  (* Type Arrow: -> (used in function types and lambdas) *)
  | "->" ->
      let tok = make_tok env node in
      (* Type arrows should be represented as operators *)
      G.E (G.N (G.Id (("->", tok), G.empty_id_info ())) |> G.e)

  (* Type Constructor Application: type application with spaces *)
  | "type_context" | "type_forall" | "type_arrow" ->
      let children = get_children node in
      (match children with
       | [] -> G.E (G.OtherExpr (("empty_type_expr", fake_tok ""), []) |> G.e)
       | [child] -> convert_node env child
       | _ ->
           (* Multiple type components - convert all *)
           let exprs = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match exprs with
            | [] -> G.E (G.OtherExpr (("empty_type_expr", fake_tok ""), []) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Assignment/Equality: = (used in definitions and guards) *)
  | "=" ->
      let tok = make_tok env node in
      (* Assignments are represented as identifiers/operators *)
      G.E (G.N (G.Id (("=", tok), G.empty_id_info ())) |> G.e)

  (* Guard Operator: | (used in pattern guards) *)
  | "|" ->
      let tok = make_tok env node in
      (* Pipe is represented as an operator *)
      G.E (G.N (G.Id (("|", tok), G.empty_id_info ())) |> G.e)

  (* Module Qualification Dot: . *)
  | "." ->
      let tok = make_tok env node in
      (* Dot operator for module qualification *)
      G.E (G.N (G.Id ((".", tok), G.empty_id_info ())) |> G.e)

  (* Lambda Arrow: for lambda expressions *)
  | "lambda_arrow" | "\\" ->
      let tok = make_tok env node in
      (* Lambda is represented as a special identifier *)
      G.E (G.N (G.Id (("\\", tok), G.empty_id_info ())) |> G.e)

  (* Composite operators that tree-sitter can produce *)
  (* These are combinations like "->." or similar *)
  | "->." | "arrow" | "_arrow" ->
      let tok = make_tok env node in
      let op_text = get_text env node in
      G.E (G.N (G.Id ((op_text, tok), G.empty_id_info ())) |> G.e)

  (* Bind operator: <- and >> and >>= *)
  | "<-" | ">>" | ">>=" | "bind_op" ->
      let tok = make_tok env node in
      let op_text = get_text env node in
      G.E (G.N (G.Id ((op_text, tok), G.empty_id_info ())) |> G.e)

  (* Type annotation: :: *)
  | "::" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("::", tok), G.empty_id_info ())) |> G.e)

  (* Comma: , (for tuples, lists, etc.) *)
  | "," ->
      let tok = make_tok env node in
      G.E (G.N (G.Id ((",", tok), G.empty_id_info ())) |> G.e)

  (* Semicolon: ; *)
  | ";" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id ((";", tok), G.empty_id_info ())) |> G.e)

  (* Other common operators that might be leaf nodes *)
  | "operator" | "operator_id" | "operator_symbol" | "qualified_operator" ->
      let tok = make_tok env node in
      let op_text = get_text env node in
      G.E (G.N (G.Id ((op_text, tok), G.empty_id_info ())) |> G.e)

  (* Module identifiers: module_id (qualified module names like Foundation.Authorization) *)
  | "module_id" ->
      let tok = make_tok env node in
      let module_text = get_text env node in
      G.E (G.N (G.Id ((module_text, tok), G.empty_id_info ())) |> G.e)

  (* As-pattern operator: @ (for pattern binding like xs@(x:rest)) *)
  | "@" | "at_operator" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("@", tok), G.empty_id_info ())) |> G.e)

  (* Parentheses: ( and ) as leaf nodes (typically filtered, but handling just in case) *)
  | "(" | ")" ->
      let tok = make_tok env node in
      let paren_text = get_text env node in
      G.E (G.N (G.Id ((paren_text, tok), G.empty_id_info ())) |> G.e)

  (* Square brackets: [ and ] *)
  | "[" | "]" ->
      let tok = make_tok env node in
      let bracket_text = get_text env node in
      G.E (G.N (G.Id ((bracket_text, tok), G.empty_id_info ())) |> G.e)

  (* Curly braces: { and } *)
  | "{" | "}" ->
      let tok = make_tok env node in
      let brace_text = get_text env node in
      G.E (G.N (G.Id ((brace_text, tok), G.empty_id_info ())) |> G.e)

  (* Label notation for record fields: label *)
  | "label" ->
      let tok = make_tok env node in
      let label_text = get_text env node in
      G.E (G.N (G.Id ((label_text, tok), G.empty_id_info ())) |> G.e)

  (* Single and double quotes (for quoted expressions or characters) *)
  | "'''" | "''" | "'" | "quote" ->
      let tok = make_tok env node in
      let quote_text = get_text env node in
      G.E (G.N (G.Id ((quote_text, tok), G.empty_id_info ())) |> G.e)

  (* Backticks: ` (for infix notation) *)
  | "`" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("`", tok), G.empty_id_info ())) |> G.e)

  (* Colon: : (for list cons and type annotations) *)
  | ":" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id ((":", tok), G.empty_id_info ())) |> G.e)

  (* Exclamation mark: ! (for strict fields) *)
  | "!" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("!", tok), G.empty_id_info ())) |> G.e)

  (* Question mark: ? (for implicit parameters) *)
  | "?" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("?", tok), G.empty_id_info ())) |> G.e)

  (* Hash/Pound: # (for magic hashes in GHC) *)
  | "#" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("#", tok), G.empty_id_info ())) |> G.e)

  (* At sign in different contexts *)
  | "at_sign" | "at" ->
      let tok = make_tok env node in
      let at_text = get_text env node in
      G.E (G.N (G.Id ((at_text, tok), G.empty_id_info ())) |> G.e)

  (* Quasi-quote body: for Template Haskell quasi-quotations *)
  | "quasiquote_body" ->
      let tok = make_tok env node in
      let children = get_children node in
      (match children with
       | [] ->
           let body_text = get_text env node in
           G.E (G.N (G.Id ((body_text, tok), G.empty_id_info ())) |> G.e)
       | _ ->
           (* If there are children, try to convert them *)
           let child_exprs = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match child_exprs with
            | [] -> G.E (G.OtherExpr (("quasiquote", tok), []) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Range/ellipsis operator: .. (for ranges like [1..10] or record updates {..}) *)
  | ".." ->
      let tok = make_tok env node in
      G.E (G.N (G.Id (("..", tok), G.empty_id_info ())) |> G.e)

  (* Arithmetic Sequence: [1,3..10] *)
  | "arithmetic_sequence" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Unboxed tuples: (#,#) *)
  | "unboxed_tuple" | "prefix_unboxed_tuple" ->
      let children = get_children node in
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "#" && t <> ","
      ) children in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children_filtered in
      let tok = make_tok env node in
      G.E (G.OtherExpr (("unboxed_tuple", tok), [G.E (G.Container (G.Tuple, fb exprs) |> G.e)]) |> G.e)

  (* Unboxed sums: (# a | b #) *)
  | "unboxed_sum" | "prefix_unboxed_sum" ->
      let children = get_children node in
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "#" && t <> "|"
      ) children in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children_filtered in
      let tok = make_tok env node in
      G.E (G.OtherExpr (("unboxed_sum", tok), List.map (fun e -> G.E e) exprs) |> G.e)

  (* Unboxed unit: (#) *)
  | "unboxed_unit" ->
      let tok = make_tok env node in
      G.E (G.OtherExpr (("unboxed_unit", tok), []) |> G.e)

  (* Foreign imports/exports *)
  | "foreign_import" | "foreign_export" ->
      let children = get_children node in
      let stmts = List.filter_map (fun c ->
        match convert_node env c with
        | G.S s -> Some s
        | _ -> None
      ) children in
      (match stmts with
       | [] -> G.Ss []
       | _ -> G.Ss stmts)

  | "foreign" ->
      let children = get_children node in
      let stmts = List.filter_map (fun c ->
        match convert_node env c with
        | G.S s -> Some s
        | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
        | _ -> None
      ) children in
      G.Ss stmts

  (* Type families *)
  | "type_family" | "data_family" ->
      let children = get_children node in
      (match children with
       | name_node :: _ ->
           let name_tok = make_tok env name_node in
           let name_str = get_text env name_node in
           let ent = G.basic_entity (name_str, name_tok) in
           G.S (G.DefStmt (ent, G.TypeDef {
             tbody = G.OtherTypeKind (("type_family", fake_tok ""), []);
           }) |> G.s)
       | _ -> G.Ss [])

  (* Type instances *)
  | "type_instance" ->
      let children = get_children node in
      let stmts = List.filter_map (fun c ->
        match convert_node env c with
        | G.S s -> Some s
        | _ -> None
      ) children in
      G.Ss stmts

  (* GADT *)
  | "gadt_constructor" | "gadt_constructors" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Type family injectivity *)
  | "type_family_injectivity" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Role annotations *)
  | "role_annotation" | "type_role" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Deriving strategy *)
  | "deriving_strategy" | "deriving_instance" ->
      let children = get_children node in
      let stmts = List.filter_map (fun c ->
        match convert_node env c with
        | G.S s -> Some s
        | _ -> None
      ) children in
      G.Ss stmts

  (* Associated types *)
  | "associated_type" ->
      let children = get_children node in
      (match children with
       | name_node :: _ ->
           let name_tok = make_tok env name_node in
           let name_str = get_text env name_node in
           let ent = G.basic_entity (name_str, name_tok) in
           G.S (G.DefStmt (ent, G.TypeDef {
             tbody = G.OtherTypeKind (("associated_type", fake_tok ""), []);
           }) |> G.s)
       | _ -> G.Ss [])

  (* Quantified types *)
  | "quantified_type" | "forall_required" ->
      let children = get_children node in
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "forall" && t <> "." && t <> "->"
      ) children in
      (match children_filtered with
       | [child] -> convert_node env child
       | [] -> G.E (G.OtherExpr (("empty_quantified_type", fake_tok ""), []) |> G.e)
       | children ->
           let child_results = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match child_results with
            | [] -> G.E (G.OtherExpr (("empty_quantified_type", fake_tok ""), []) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Kind annotations *)
  | "kind_signature" | "kind_application" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | [] -> G.E (G.OtherExpr (("empty_kind", fake_tok ""), []) |> G.e)
       | children ->
           let exprs = List.filter_map (fun c ->
             match convert_node env c with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match exprs with
            | [] -> G.E (G.OtherExpr (("empty_kind", fake_tok ""), []) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Type applications *)
  | "type_application" ->
      let children = get_children node in
      (match children with
       | func :: args ->
           let func_expr = convert_expr env func in
           let arg_exprs = List.map (convert_expr env) args in
           let result = List.fold_left (fun acc arg ->
             G.Call (acc, fb [G.Arg arg]) |> G.e
           ) func_expr arg_exprs in
           G.E result
       | [] -> G.E (G.OtherExpr (("empty_type_app", fake_tok ""), []) |> G.e))

  (* Implicit parameters and variables *)
  | "implicit_parameter" | "implicit_variable" | "implicit" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | _ ->
           let tok = make_tok env node in
           G.E (G.OtherExpr (("implicit_param", tok), []) |> G.e))

  (* Default declarations *)
  | "default" ->
      let children = get_children node in
      (match children with
       | [] -> G.Ss []
       | _ ->
           let stmts = List.filter_map (fun c ->
             match convert_node env c with
             | G.S s -> Some s
             | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
             | _ -> None
           ) children in
           G.Ss stmts)

  (* Module exports *)
  | "export" | "exports" | "module_export" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Import-related *)
  | "import_list" | "import_name" | "import_package" ->
      let children = get_children node in
      let exprs = List.filter_map (fun c ->
        match convert_node env c with
        | G.E e -> Some e
        | _ -> None
      ) children in
      G.E (G.Container (G.List, fb exprs) |> G.e)

  (* Module identifiers and qualifications *)
  | "qualified" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | _ ->
           let tok = make_tok env node in
           let text = get_text env node in
           G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e))

  (* Visibility and roles *)
  | "representational" | "nominal" | "phantom" ->
      let tok = make_tok env node in
      let text = get_text env node in
      G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)

  (* Strategy keywords *)
  | "stock" | "via" ->
      let tok = make_tok env node in
      let text = get_text env node in
      G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)

  (* Catch-all for operator symbols and special characters *)
  (* Matches: ||, &&, //, <|>, <*>, <$>, =>, **, etc. *)
  (* Also composite operators like ||], etc. *)
  | "||" | "&&" | "//" | "<|>" | "<*>" | "<$>" | "=>" | "**"
  | "<<" | "~=" | "<>" | "||]" | "||}" | "||)" ->
      let tok = make_tok env node in
      G.E (G.N (G.Id ((node.Tree_sitter_bindings.Tree_sitter_output_t.type_, tok), G.empty_id_info ())) |> G.e)

  (* Private tree-sitter nodes (starting with underscore) - pass through children *)
  | "_exp" | "_pat" | "_body" | "_matches" | "_guards" | "_ops" | "_field_spec"
  | "_do" | "_data" | "_newtype" | "_instance" | "_gadt" | "_local_decl"
  | "_where" | "_where_binds" | "_bind_matches" | "_statements" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | [] -> G.E (G.OtherExpr ((node.Tree_sitter_bindings.Tree_sitter_output_t.type_, fake_tok ""), []) |> G.e)
       | children ->
           let child_results = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match child_results with
            | [] -> G.E (G.OtherExpr ((node.Tree_sitter_bindings.Tree_sitter_output_t.type_, fake_tok ""), []) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Literal tokens and prefixes that appear as private nodes *)
  | "_binary_literal" | "_hex_literal" | "_octal_literal" | "_integer_literal" ->
      let tok = make_tok env node in
      let text = get_text env node in
      let i_opt = try Some (Int64.of_string text) with _ -> None in
      G.E (G.L (G.Int (i_opt, tok)) |> G.e)

  (* Operators appearing as private nodes *)
  | "_exp_op" | "_operator_alias" | "_operator_minus" | "_operator_hash_head" ->
      let tok = make_tok env node in
      let op_text = get_text env node in
      G.E (G.N (G.Id ((op_text, tok), G.empty_id_info ())) |> G.e)

  (* Error nodes and other special tree-sitter nodes *)
  (* FIX_ERROR_HANDLER_V2_TIMESTAMP_20250105 *)
  | "ERROR" | "error_sentinel" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_node env child
       | [] ->
           let tok = make_tok env node in
           let text = get_text env node in
           G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
       | children ->
           let child_results = List.filter_map (fun child ->
             match convert_node env child with
             | G.E e -> Some e
             | _ -> None
           ) children in
           (match child_results with
            | [] ->
                let tok = make_tok env node in
                let text = get_text env node in
                G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
            | [e] -> G.E e
            | es -> G.E (G.Seq es |> G.e)))

  (* Generic operator catch-all for any remaining operator-like node types *)
  (* This handles composite operators created by tree-sitter *)
  | node_type when String.length node_type > 0 &&
                   (String.contains node_type '<' || String.contains node_type '>' ||
                    String.contains node_type '|' || String.contains node_type '&' ||
                    String.contains node_type '-' || String.contains node_type '+' ||
                    String.contains node_type '*' || String.contains node_type '/' ||
                    String.contains node_type '=' || String.contains node_type '!' ||
                    String.contains node_type ':' || String.contains node_type '.' ||
                    String.contains node_type '~' || String.contains node_type '^' ||
                    String.contains node_type '%' || String.contains node_type '$') ->
      let tok = make_tok env node in
      G.E (G.N (G.Id ((node_type, tok), G.empty_id_info ())) |> G.e)

  (* Fallback *)
  | _ ->
      (* Try to convert children *)
      let children = get_children node in
      let node_type = node.Tree_sitter_bindings.Tree_sitter_output_t.type_ in

      if List.length children > 0 then begin
        let child_results = List.filter_map (fun child ->
          match convert_node env child with
          | G.E e -> Some (G.ExprStmt (e, fake_tok ";") |> G.s)
          | G.S s -> Some s
          | G.Ss ss -> if ss = [] then None else Some (G.Block (fb ss) |> G.s)
          | _ -> None
        ) children in

        if child_results <> [] then
          G.Ss child_results
        else
          (* Unhandled node type with children - extract text content as identifier *)
          let text = String.trim (get_text env node) in
          let tok = make_tok env node in

          (* Check if text is a valid identifier *)
          if String.length text > 0 &&
             ((text.[0] >= 'a' && text.[0] <= 'z') ||
              (text.[0] >= 'A' && text.[0] <= 'Z') ||
              text.[0] = '_' || text.[0] = '$') &&
             String.length text <= 256 then
            (* Text is a valid identifier - return as Name *)
            G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
          else
            (* Can't extract valid identifier - mark as unhandled with normalized name *)
            let safe_node_type = normalize_node_type node_type in
            G.E (G.OtherExpr (("unhandled_" ^ safe_node_type, tok), []) |> G.e)
      end else
        (* Unhandled leaf node - try to get text *)
        let text = String.trim (get_text env node) in
        let tok = make_tok env node in

        if String.length text > 0 &&
           ((text.[0] >= 'a' && text.[0] <= 'z') ||
            (text.[0] >= 'A' && text.[0] <= 'Z') ||
            text.[0] = '_' || text.[0] = '$') &&
           String.length text <= 256 then
          (* Text is a valid identifier - return as Name *)
          G.E (G.N (G.Id ((text, tok), G.empty_id_info ())) |> G.e)
        else
          (* Can't extract valid identifier - mark as unhandled with normalized name *)
          let safe_node_type = normalize_node_type node_type in
          G.E (G.OtherExpr (("unhandled_" ^ safe_node_type, tok), []) |> G.e)

and convert_expr (env : env) (node : node) : G.expr =
  match convert_node env node with
  | G.E expr -> expr
  | G.S stmt ->
      (* Statement in expression context - wrap as StmtExpr *)
      G.StmtExpr stmt |> G.e
  | G.Ss stmts ->
      (* Multiple statements in expression context - wrap as block *)
      G.StmtExpr (G.Block (fb stmts) |> G.s) |> G.e
  | _ ->
      (* Unexpected conversion result - create descriptive error *)
      let node_type = node.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
      G.OtherExpr (("unexpected_conversion_result:" ^ node_type, fake_tok ""), []) |> G.e

and convert_pattern (env : env) (node : node) : G.pattern =
  match node.Tree_sitter_bindings.Tree_sitter_output_t.type_ with
  (* Literal wrapper - unwrap and convert child *)
  | "literal" ->
      let children = get_children node in
      (match children with
       | [child] -> convert_pattern env child
       | _ -> G.PatWildcard (make_tok env node))
  (* Bang patterns: strict evaluation !x *)
  | "strict" | "bang_pattern" | "strict_pattern" ->
      let children = get_children node in
      (* Filter out "!" token children, keep only the actual pattern *)
      let pattern_children = List.filter (fun c ->
        c.Tree_sitter_bindings.Tree_sitter_output_t.type_ <> "!"
      ) children in
      (match pattern_children with
       | [child] ->
           (* Extract inner pattern *)
           let inner_pattern = convert_pattern env child in
           (* Wrap with type annotation for strict *)
           let strict_type = { G.t = G.TyN (G.Id (("strict", fake_tok "!"), G.empty_id_info ())); G.t_attrs = [] } in
           G.OtherPat (("strict", make_tok env node), [G.T strict_type; G.P inner_pattern])
       | _ ->
           (* Fallback: try text parsing for !x *)
           let text = get_text env node in
           if String.length text > 0 && text.[0] = '!' then
             let var_name = String.sub text 1 (String.length text - 1) in
             let tok = make_tok env node in
             let inner = G.PatId ((var_name, tok), G.empty_id_info ()) in
             let strict_type = { G.t = G.TyN (G.Id (("strict", fake_tok "!"), G.empty_id_info ())); G.t_attrs = [] } in
             G.OtherPat (("strict", tok), [G.T strict_type; G.P inner])
           else
             G.PatWildcard (make_tok env node))

  (* View patterns: (view -> pattern) *)
  | "view_pattern" | "exp_view" | "pat_view" ->
      let children = get_children node in
      (match children with
       | view_node :: _ :: pattern_node :: _ ->
           (* Extract view function and result pattern *)
           let view_expr = convert_expr env view_node in
           let result_pattern = convert_pattern env pattern_node in
           (* Represent as OtherPat with view metadata *)
           G.OtherPat (("view_pattern", make_tok env node), [
             G.E view_expr;
             G.P result_pattern
           ])
       | _ ->
           (* Fallback: text parsing for (view -> pattern) *)
           let text = get_text env node in
           (try
             (* Find -> in text *)
             let rec find_arrow idx =
               if idx >= String.length text - 1 then raise Not_found
               else if text.[idx] = '-' && text.[idx + 1] = '>' then idx
               else find_arrow (idx + 1)
             in
             let arrow_idx = find_arrow 0 in
             let view_text = String.sub text 1 (arrow_idx - 1) |> String.trim in
             let pattern_text = String.sub text (arrow_idx + 2) (String.length text - arrow_idx - 3) |> String.trim in
             let view_tok = fake_tok view_text in
             let view_expr = G.N (G.Id ((view_text, view_tok), G.empty_id_info ())) |> G.e in
             let result_pattern = G.PatId ((pattern_text, fake_tok pattern_text), G.empty_id_info ()) in
             G.OtherPat (("view_pattern", make_tok env node), [
               G.E view_expr;
               G.P result_pattern
             ])
           with Not_found ->
             G.PatWildcard (make_tok env node)))

  (* Infix patterns: x:xs, a:b:c, etc. *)
  | "infix" ->
      let children = get_children node in
      (match children with
       | left :: op :: right :: _ ->
           (* Infix pattern for list cons (:) or custom operators *)
           let op_text = get_text env op in
           let op_tok = make_tok env op in
           let left_pat = convert_pattern env left in
           let right_pat = convert_pattern env right in
           (* Constructor pattern with operator as name *)
           G.PatConstructor (G.Id ((op_text, op_tok), G.empty_id_info ()), [left_pat; right_pat])
       | _ -> G.PatWildcard (make_tok env node))

  (* Wrappers - pass through to children *)
  | "pattern" | "parens" | "pat_parens" ->
      let children = get_children node in
      (* Filter out punctuation tokens like (, ), [, ] *)
      let pattern_children = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "(" && t <> ")" && t <> "[" && t <> "]" && t <> "{" && t <> "}"
      ) children in
      (match pattern_children with
       | [child] -> convert_pattern env child
       | _ -> G.PatWildcard (make_tok env node))
  | "integer" | "integer_literal" ->
      let tok = make_tok env node in
      let text = get_text env node in
      let i_opt = try Some (Int64.of_string text) with _ -> None in
      G.PatLiteral (G.Int (i_opt, tok))
  | "string" ->
      let tok = make_tok env node in
      let text = get_text env node in
      G.PatLiteral (G.String (fake_tok "\"", (text, tok), fake_tok "\""))
  | "variable" | "name" ->
      let raw_text = get_text env node in
      let text = resolve_metavar_text env raw_text in
      let tok = make_tok ~override_text:text env node in

      (* Check if this is a metavariable in pattern context *)
      if is_metavar_in_context env raw_text then
        (* Metavariable pattern: $VAR matches any value *)
        G.PatId ((text, tok), G.empty_id_info ())  (* Keep $ prefix *)
      else if String.length text > 0 && text.[0] = '!' then
        (* Bang pattern in text: !x *)
        let var_name = String.sub text 1 (String.length text - 1) in
        let inner = G.PatId ((var_name, tok), G.empty_id_info ()) in
        let strict_type = { G.t = G.TyN (G.Id (("strict", fake_tok "!"), G.empty_id_info ())); G.t_attrs = [] } in
        G.OtherPat (("strict", tok), [G.T strict_type; G.P inner])
      else if text = "_" then
        G.PatWildcard tok
      else
        G.PatId ((text, tok), G.empty_id_info ())
  | "constructor" | "constructor_operator" ->
      (* Pattern constructeur: Just, Nothing, Node, etc. *)
      let tok = make_tok env node in
      let name = get_text env node in
      let name_id = G.Id ((name, tok), G.empty_id_info ()) in
      let children = get_children node in
      if List.length children = 0 then
        (* Simple constructor without args: Nothing, Leaf *)
        G.PatConstructor (name_id, [])
      else
        (* Constructor with args: Just x, Node l r *)
        let arg_patterns = List.map (convert_pattern env) children in
        G.PatConstructor (name_id, arg_patterns)
  | "pat_apply" | "pattern_apply" ->
      (* Application pattern: constructeur avec args *)
      let children = get_children node in
      (match children with
       | constr :: args ->
           let constr_name = get_text env constr in
           let constr_tok = make_tok env constr in
           let constr_id = G.Id ((constr_name, constr_tok), G.empty_id_info ()) in
           let arg_patterns = List.map (convert_pattern env) args in
           G.PatConstructor (constr_id, arg_patterns)
       | _ -> G.PatWildcard (make_tok env node))
  | "type_binder" ->
      (* TypeAbstractions: @a type binders in patterns *)
      let children = get_children node in
      (* Extract the type variable after @ *)
      let type_var = match children with
        | var_node :: _ ->
            let var_name = get_text env var_node in
            var_name
        | [] ->
            (* No children - parse from text *)
            let text = get_text env node in
            if String.length text > 0 && text.[0] = '@' then
              String.sub text 1 (String.length text - 1)
            else
              text
      in
      let tok = make_tok env node in
      (* Represent as OtherPat with type_binder annotation *)
      G.OtherPat (("type_binder", tok), [G.E (G.N (G.Id ((type_var, tok), G.empty_id_info ())) |> G.e)])
  | "as" ->
      (* As-pattern: xs@(x:_) - tree-sitter uses text heavily due to wrapper nodes *)
      (* Grammar: field('bind', $.variable), field('pattern', $.pattern) *)
      (* STRATEGY: Use text parsing as primary since tree-sitter has complex wrappers *)
      let tok = make_tok env node in
      let text = get_text env node in
      (try
        let at_idx = String.index text '@' in
        let bind_name = String.sub text 0 at_idx |> String.trim in
        let pattern_text = String.sub text (at_idx + 1) (String.length text - at_idx - 1) |> String.trim in

        (* Helper to parse pattern text recursively *)
        let rec parse_pattern_text text =
          if text = "_" then
            G.PatWildcard (fake_tok "_")
          else if String.contains text '@' then
            (* Nested as-pattern: ys@(y:_) *)
            (try
              let at_idx = String.index text '@' in
              let inner_bind = String.sub text 0 at_idx |> String.trim in
              let inner_pattern_text = String.sub text (at_idx + 1) (String.length text - at_idx - 1) |> String.trim in
              let inner_pattern = parse_pattern_text inner_pattern_text in
              G.PatAs (inner_pattern, ((inner_bind, fake_tok inner_bind), G.empty_id_info ()))
            with Not_found | Invalid_argument _ ->
              G.PatId ((text, fake_tok text), G.empty_id_info ()))
          else if String.length text > 0 && text.[0] = '(' then
            (* Complex pattern like (x:_) or (Just x) - unwrap parens *)
            let inner_text = String.sub text 1 (String.length text - 2) in
            if String.contains inner_text ':' then
              (* List cons pattern (x:xs) or (x:_) *)
              let colon_idx = String.index inner_text ':' in
              let head_name = String.sub inner_text 0 colon_idx |> String.trim in
              let tail_name = String.sub inner_text (colon_idx + 1) (String.length inner_text - colon_idx - 1) |> String.trim in
              let head_pat = parse_pattern_text head_name in
              let tail_pat = parse_pattern_text tail_name in
              G.PatConstructor (G.Id ((":", fake_tok ":"), G.empty_id_info ()), [head_pat; tail_pat])
            else if String.contains inner_text ' ' then
              (* Constructor with args like (Just x) or (Node l r) *)
              let parts = String.split_on_char ' ' inner_text |> List.filter (fun s -> s <> "") in
              (match parts with
               | constr :: arg_names ->
                   let arg_patterns = List.map parse_pattern_text arg_names in
                   G.PatConstructor (G.Id ((constr, fake_tok constr), G.empty_id_info ()), arg_patterns)
               | _ -> G.PatId ((inner_text, fake_tok inner_text), G.empty_id_info ()))
            else
              (* Simple pattern in parens *)
              parse_pattern_text inner_text
          else
            (* Simple variable pattern *)
            G.PatId ((text, fake_tok text), G.empty_id_info ())
        in

        (* Parse the pattern part *)
        let pattern = parse_pattern_text pattern_text in
        G.PatAs (pattern, ((bind_name, tok), G.empty_id_info ()))
      with Not_found | Invalid_argument _ ->
        (* No @ found - shouldn't happen for "as" node, fallback to wildcard *)
        G.PatWildcard tok)
  (* List patterns: [], [x], [x, y], etc. *)
  | "list" | "exp_list" | "pat_list" ->
      let children = get_children node in
      (* Filter out punctuation tokens: [, ], , *)
      let children_filtered = List.filter (fun c ->
        let t = c.Tree_sitter_bindings.Tree_sitter_output_t.type_ in
        t <> "[" && t <> "]" && t <> ","
      ) children in
      (match children_filtered with
       | [] ->
           (* Empty list pattern: [] *)
           let tok = make_tok env node in
           G.PatConstructor (G.Id (("[]", tok), G.empty_id_info ()), [])
       | patterns ->
           (* Non-empty list pattern: [x], [x, y], etc. *)
           (* Convert to nested cons pattern: x:(y:[]) *)
           let elem_patterns = List.map (convert_pattern env) patterns in
           (* Build nested cons from right to left *)
           let empty_tok = fake_tok "[]" in
           let empty_list = G.PatConstructor (G.Id (("[]", empty_tok), G.empty_id_info ()), []) in
           List.fold_right (fun elem acc ->
             G.PatConstructor (G.Id ((":", fake_tok ":"), G.empty_id_info ()), [elem; acc])
           ) elem_patterns empty_list)
  | _ ->
      (* Fallback: wildcard *)
      G.PatWildcard (make_tok env node)

(******************************************************************************)
(* Clause grouping *)
(******************************************************************************)

(* Helper: Check if a parameter is a literal 0 *)
let is_literal_zero (param : G.parameter) : bool =
  match param with
  | G.ParamPattern (G.PatLiteral (G.Int (Some 0L, _))) -> true
  | _ -> false

(* Helper: Check if a parameter is an empty list pattern [] *)
let is_empty_list_pattern (param : G.parameter) : bool =
  match param with
  | G.ParamPattern (G.PatConstructor (G.Id (("[]", _), _), [])) -> true
  | _ -> false

(* Helper: Check if a parameter is an empty string pattern "" *)
let is_empty_string_pattern (param : G.parameter) : bool =
  match param with
  | G.ParamPattern (G.PatLiteral (G.String (_, ("", _), _))) -> true
  | G.ParamPattern (G.PatLiteral (G.String (_, ("\"\"", _), _))) -> true  (* String with quotes included *)
  | _ -> false

(* Helper: Check if any prior clause has empty pattern (0, [], "") in given parameter position *)
let has_prior_empty_pattern (clauses : clause_info list) (current_idx : int) (param_pos : int) : bool =
  let rec check_clauses idx = function
    | [] -> false
    | clause :: rest ->
        if idx >= current_idx then
          false  (* We've reached the current clause, stop *)
        else
          (* Check if this clause has empty pattern (0, [], or "") at param_pos *)
          let has_empty =
            if param_pos < List.length clause.params then
              let param = List.nth clause.params param_pos in
              is_literal_zero param ||
              is_empty_list_pattern param ||
              is_empty_string_pattern param
            else
              false
          in
          has_empty || check_clauses (idx + 1) rest
  in
  check_clauses 0 clauses
[@@warning "-32"]

(* Group multiple clauses into a single function with pattern matching *)
let group_clauses_into_defstmt (clauses : clause_info list) : G.stmt =
  match clauses with
  | [] -> G.OtherStmt (G.OS_Pass, []) |> G.s
  | first :: rest ->
      (* If only one clause, create simple function *)
      if rest = [] then
        (* Extract body from branches - for now, take first branch *)
        let body_expr = match first.branches with
          | [] -> G.OtherExpr (("empty_clause_branches", fake_tok ""), []) |> G.e
          | [{guard = None; body}] -> body  (* Simple case: no guards *)
          | branches ->
              (* Multiple branches with guards - build conditional chain *)
              (* Check if ANY guard protects against zero, empty list, or bounds *)
              let any_guard_protects = List.exists (fun branch ->
                match branch.guard with
                | Some g ->
                    Option.is_some (guard_protects_zero g) ||
                    Option.is_some (guard_protects_empty g) ||
                    Option.is_some (guard_protects_bounds g)
                | None -> false
              ) branches in
              (* Build conditional chain with protection annotation if needed *)
              List.fold_right (fun branch acc ->
                match branch.guard with
                | None ->
                    (* Otherwise branch - protect if any prior guard was a zero check *)
                    if any_guard_protects then
                      annotate_protected_expr branch.body
                    else
                      branch.body
                | Some guard_expr ->
                    (* Guarded branch - check if guard protects the variable *)
                    (* For guards like "y > 0", the THEN branch is protected *)
                    let protected_body = match extract_protected_variable guard_expr with
                      | Some _ -> annotate_protected_expr branch.body
                      | None -> branch.body
                    in
                    G.Conditional (guard_expr, protected_body, acc) |> G.e
              ) branches (G.OtherExpr (("no_else_branch", fake_tok ""), []) |> G.e)
        in
        (* ALWAYS apply annotate_protected_expr to detect safe divisions like div by literals *)
        (* This allows is_expr_guaranteed_nonzero to detect safe divisors *)
        let final_body_expr = annotate_protected_expr body_expr in
        let func_def = G.FuncDef {
          fkind = (G.Function, fake_tok "");
          fparams = fb first.params;
          frettype = first.frettype;
          fbody = G.FBExpr final_body_expr;
        } in
        let ent = G.basic_entity (first.name, first.name_tok) in
        G.DefStmt (ent, func_def) |> G.s
      else
        (* Multiple clauses - create pattern matching *)
        let all_clauses = first :: rest in
          (* Regular pattern matching - create Match expression *)
          let cases = List.map (fun clause ->
            (* Extract patterns from parameters *)
            let patterns = List.map (fun param ->
              match param with
              | G.Param p ->
                  (* Simple variable - convert to pattern *)
                  (match p.G.pname with
                   | Some (name, tok) -> G.PatId ((name, tok), G.empty_id_info ())
                   | None -> G.PatWildcard (fake_tok "_"))
              | G.ParamPattern pat -> pat
              | _ -> G.PatWildcard (fake_tok "_")
            ) clause.params in
            (* Extract body from branches *)
            let body_expr = match clause.branches with
              | [] -> G.OtherExpr (("empty_clause_branches", fake_tok ""), []) |> G.e
              | [{guard = None; body}] -> body
              | branches ->
                  (* Check if ANY guard protects against zero, empty list, or bounds *)
                  let any_guard_protects = List.exists (fun branch ->
                    match branch.guard with
                    | Some g ->
                        Option.is_some (guard_protects_zero g) ||
                        Option.is_some (guard_protects_empty g) ||
                        Option.is_some (guard_protects_bounds g)
                    | None -> false
                  ) branches in
                  (* Build conditional chain with protection annotation if needed *)
                  List.fold_right (fun branch acc ->
                    match branch.guard with
                    | None ->
                        (* Otherwise branch - protect if any prior guard was a zero check *)
                        if any_guard_protects then
                          annotate_protected_expr branch.body
                        else
                          branch.body
                    | Some guard_expr ->
                        (* Guarded branch - check if guard protects the variable *)
                        (* For guards like "y > 0", the THEN branch is protected *)
                        let protected_body = match extract_protected_variable guard_expr with
                          | Some _ -> annotate_protected_expr branch.body
                          | None -> branch.body
                        in
                        G.Conditional (guard_expr, protected_body, acc) |> G.e
                  ) branches (G.OtherExpr (("no_else_branch", fake_tok ""), []) |> G.e)
            in
            (* ALWAYS annotate to detect safe divisions (literals, denominator, etc.) *)
            (* This also handles protection from pattern matching empty patterns (0, [], "") *)
            let final_body_expr = annotate_protected_expr body_expr in
            (* Create a case with these patterns *)
            G.CasesAndBody (
              List.map (fun pat -> G.Case (fake_tok "", pat)) patterns,
              G.ExprStmt (final_body_expr, fake_tok ";") |> G.s
            )
          ) all_clauses in
          (* Create a dummy expr to match on (we don't have the actual match target) *)
          let match_expr = G.OtherExpr (("match_target", fake_tok ""), []) |> G.e in
          let match_stmt = G.Switch (fake_tok "case", Some (G.Cond match_expr), cases) |> G.s in
          let func_def = G.FuncDef {
            fkind = (G.Function, fake_tok "");
            fparams = fb first.params;  (* Use first clause params as signature *)
            frettype = first.frettype;
            fbody = G.FBStmt match_stmt;
          } in
          let ent = G.basic_entity (first.name, first.name_tok) in
          G.DefStmt (ent, func_def) |> G.s

(* Process stored clauses and create grouped function definitions *)
let process_function_clauses (env : env) : G.stmt list =
  (* Get all unique function names *)
  let names = get_all_function_names env in
  (* For each name, get clauses (in reverse order) and group them *)
  List.map (fun name ->
    let clauses = List.rev (get_clauses_for_function env name) in
    group_clauses_into_defstmt clauses
  ) names

(******************************************************************************)
(* Entry point *)
(******************************************************************************)

let program (env : env) (root : node) : G.program =
  (* No need to reset - env is fresh for each file *)

  (* Convert AST - this stores clauses in env *)
  let initial_stmts = match convert_node env root with
    | G.Ss stmts -> stmts
    | G.S stmt -> [stmt]
    | G.E expr -> [G.ExprStmt (expr, fake_tok ";") |> G.s]
    | _ -> []
  in

  (* Process function clauses in stable order *)
  let function_defs = process_function_clauses env in

  (* Filter out OS_Pass placeholders and combine with functions *)
  let non_function_stmts = List.filter (fun s ->
    match s.G.s with
    | G.OtherStmt (G.OS_Pass, []) -> false
    | _ -> true
  ) initial_stmts in

  (* Combine: functions first (in declaration order), then rest *)
  function_defs @ non_function_stmts
