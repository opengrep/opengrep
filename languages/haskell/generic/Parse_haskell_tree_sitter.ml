(* Haskell parser using tree-sitter *)

open Fpath_.Operators
module CST = Tree_sitter_haskell.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Haskell parser using tree-sitter-haskell (auto-generated) and converting
 * to AST_generic for Semgrep pattern matching.
 *
 * This parser:
 * 1. Uses auto-generated tree-sitter Parse.ml to parse into CST.haskell
 * 2. Converts CST.haskell to AST_generic program
 *
 * The CST is auto-generated from semgrep-haskell grammar and includes
 * metavariable support ($VAR, ellipsis, etc.)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* CST to AST_generic conversion *)
(*****************************************************************************)

(* Full CST to AST_generic conversion using Haskell_to_generic module *)

let convert_program_raw (env : Haskell_to_generic_raw.env) (root_node : Tree_sitter_bindings.Tree_sitter_output_t.node) : G.program =
  Haskell_to_generic_raw.program env root_node

(*****************************************************************************)
(* Metavariable preprocessing *)
(*****************************************************************************)

let preprocess_metavariables (pattern : string) :
    string * (string, string) Hashtbl.t =
  let re =
    Str.regexp "\\$\\.\\.\\([A-Z_][A-Z0-9_]*\\)\\|\\$[A-Z_][A-Z0-9_]*"
  in
  let buffer = Buffer.create (String.length pattern) in
  let mapping = Hashtbl.create 16 in
  let rec aux idx =
    if idx >= String.length pattern then ()
    else
      match
        (try Some (Str.search_forward re pattern idx) with Not_found -> None)
      with
      | None ->
          Buffer.add_substring buffer pattern idx (String.length pattern - idx)
      | Some pos ->
          Buffer.add_substring buffer pattern idx (pos - idx);
          let matched = Str.matched_string pattern in
          let placeholder, original =
            if String.length matched >= 4
               && String.sub matched 0 4 = "$..."
            then
              let name = Str.matched_group 1 pattern in
              ("__semgrep_ellipsis_" ^ name, matched)
            else
              let name =
                String.sub matched 1 (String.length matched - 1)
              in
              ("__semgrep_metavar_" ^ name, matched)
          in
          Hashtbl.replace mapping placeholder original;
          Buffer.add_string buffer placeholder;
          aux (pos + String.length matched)
  in
  aux 0;
  (Buffer.contents buffer, mapping)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (file : Fpath.t) :
    (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t =
  (* Parse using tree-sitter-haskell C parser via DLS-safe helpers *)
  let input_tree = Tree_sitter_haskell.Parse.parse_source_file !!file in
  let root_node = Tree_sitter_run.Tree_sitter_parsing.root input_tree in
  let src = Tree_sitter_run.Tree_sitter_parsing.src input_tree in

  (* Extract errors from tree-sitter *)
  let errors = Tree_sitter_run.Run.extract_errors src root_node in

  (* Convert tree-sitter node directly to AST_generic (bypassing CST) *)
  let program =
    if root_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "ERROR" then
      None
    else begin
      let env = {
        Haskell_to_generic_raw.file;
        conv = H.line_col_to_pos file;
        src;
        type_signatures = Hashtbl.create 16;
        function_clauses = Hashtbl.create 16;
        is_pattern_mode = false;
        metavar_map = Hashtbl.create 1;
        in_do_block = false;
      } in
      try
        let ast = convert_program_raw env root_node in
        Some ast
      with _ ->
        None
    end
  in

  let stat = {
    Tree_sitter_run.Parsing_result.total_line_count = 0;
    error_line_count = List.length errors;
    error_count = List.length errors;
  } in
  { Tree_sitter_run.Parsing_result.program; errors; extras = []; stat }

let parse_pattern (str_input : string) :
    (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t =
  let preprocessed_pattern, metavar_map = preprocess_metavariables str_input in

  (* Wrap pattern in a module context for tree-sitter to parse correctly *)
  (* Pattern: "x = <pattern>" where x is a dummy function *)
  let wrapped_input = "module Pattern where\nx = " ^ preprocessed_pattern in

  (* Parse wrapped pattern using DLS-safe helpers *)
  let input_tree = Tree_sitter_haskell.Parse.parse_source_string
    ~src_file:"<pattern>" wrapped_input in
  let root_node = Tree_sitter_run.Tree_sitter_parsing.root input_tree in
  let src = Tree_sitter_run.Tree_sitter_parsing.src input_tree in

  (* Extract errors *)
  let errors = Tree_sitter_run.Run.extract_errors src root_node in

  (* Convert to AST_generic *)
  let result =
    if root_node.Tree_sitter_bindings.Tree_sitter_output_t.type_ = "ERROR" then
      None
    else begin
      let file = Fpath.v "<pattern>" in
      let env = {
        Haskell_to_generic_raw.file;
        conv = H.line_col_to_pos_pattern preprocessed_pattern;
        src;
        type_signatures = Hashtbl.create 16;
        function_clauses = Hashtbl.create 16;
        is_pattern_mode = true;
        metavar_map;
        in_do_block = false;
      } in
      try
        let program = convert_program_raw env root_node in
        (* Extract the pattern expression from the wrapped "x = <pattern>" *)
        (* Program structure: [DefStmt(x, FuncDef{fbody=FBExpr(pattern_expr)})] *)
        match program with
        | [stmt] ->
            (match stmt.G.s with
             | G.DefStmt (_, G.FuncDef { G.fbody = G.FBExpr pattern_expr; _ }) ->
                 Some (G.E pattern_expr)
             | _ -> Some (G.Ss program))  (* Fallback to full program *)
        | _ -> Some (G.Ss program)  (* Fallback to full program *)
      with _ ->
        None
    end
  in

  let stat = {
    Tree_sitter_run.Parsing_result.total_line_count = 0;
    error_line_count = List.length errors;
    error_count = List.length errors;
  } in
  { Tree_sitter_run.Parsing_result.program = result; errors; extras = []; stat }
