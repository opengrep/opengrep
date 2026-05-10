(* Haskell parser using tree-sitter.
 *
 * Mirrors the structure of the canonical tree-sitter front-ends in this
 * repo (Python, Clojure, Ruby, OCaml, …): parse the source with the
 * generated tree-sitter wrapper, hand the typed CST to the
 * language-specific mapper (Haskell_to_generic), and wrap everything in
 * the standard Parse_tree_sitter_helpers.wrap_parser.
 *)

open Fpath_.Operators
module CST = Tree_sitter_haskell.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic

(*****************************************************************************)
(* Metavariable preprocessing *)
(*****************************************************************************)

(* Preprocessor for $-prefixed Semgrep metavariables.
 *
 * Two output conventions are needed depending on the parse strategy:
 *
 *  [Lowercase] — default. Emits `__semgrep_metavar_<NAME>`, which is a
 *   valid Haskell lowercase identifier (`variable` in the grammar).
 *   Works anywhere a value-level name fits, including wrapped-expression
 *   mode (`module Pattern where\nx = <pattern>`).
 *
 *  [Uppercase] — required for positions that the Haskell grammar only
 *   accepts in uppercase: module names (`module_id`), data constructors,
 *   type constructors/names. Emits `SemgrepMv<NAME>` for regular metavars
 *   and `SemgrepEllipsis<NAME>` for `$...`. Still maps back to the
 *   original `$NAME` via the same hashtable, so the translator's
 *   metavariable resolver recovers the metavar identity.
 *)
type placeholder_case = Lower | Upper

let preprocess_metavariables_with_case (case : placeholder_case)
    (pattern : string) : string * (string, string) Hashtbl.t =
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
              let ph = match case with
                | Lower -> "__semgrep_ellipsis_" ^ name
                | Upper -> "SemgrepEllipsis" ^ name
              in
              (ph, matched)
            else
              let name =
                String.sub matched 1 (String.length matched - 1)
              in
              let ph = match case with
                | Lower -> "__semgrep_metavar_" ^ name
                | Upper -> "SemgrepMv" ^ name
              in
              (ph, matched)
          in
          Hashtbl.replace mapping placeholder original;
          Buffer.add_string buffer placeholder;
          aux (pos + String.length matched)
  in
  aux 0;
  (Buffer.contents buffer, mapping)

(*****************************************************************************)
(* Entry points                                                              *)
(*****************************************************************************)

let parse (file : Fpath.t) :
    (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t =
  H.wrap_parser
    (fun () -> Tree_sitter_haskell.Parse.file !!file)
    (fun cst _extras ->
      let env : Haskell_to_generic.env = {
        H.file;
        conv = H.line_col_to_pos file;
        extra = {
          Haskell_to_generic.is_pattern_mode = false;
          metavar_map = Hashtbl.create 1;
        };
      } in
      Haskell_to_generic.program env cst)

(* Try to parse `candidate` as a full Haskell source and convert it via
 * the typed walker. Returns None if tree-sitter reports any error or
 * the conversion fails. `preprocessed` is the text whose line/col
 * offsets we want to map back to. *)
let try_parse_as_program ~preprocessed ~metavar_map ~candidate :
    (G.program * Tree_sitter_run.Tree_sitter_error.t list) option =
  let parsing_result =
    Tree_sitter_haskell.Parse.string ~src_file:"<pattern>" candidate
  in
  if parsing_result.errors <> [] then None
  else
    match parsing_result.program with
    | None -> None
    | Some cst ->
        let env : Haskell_to_generic.env = {
          H.file = Fpath.v "<pattern>";
          conv = H.line_col_to_pos_pattern preprocessed;
          extra = {
            Haskell_to_generic.is_pattern_mode = true;
            metavar_map;
          };
        } in
        (try Some (Haskell_to_generic.program env cst, parsing_result.errors)
         with _ -> None)

(* Extract the RHS of `x = <pattern>` from a wrapped program. The typed
 * walker nests the binding inside ModuleDef(ModuleStruct(...)). *)
let rec extract_wrapped_rhs (program : G.program) : G.expr option =
  List.find_map (fun stmt ->
    match stmt.G.s with
    | G.DefStmt (ent, G.FuncDef { G.fbody = G.FBExpr expr; _ }) ->
        (match ent.G.name with
         | G.EN (G.Id (("x", _), _)) -> Some expr
         | _ -> None)
    | G.DefStmt (_ent, G.ModuleDef { G.mbody = G.ModuleStruct (_, body) }) ->
        extract_wrapped_rhs body
    | _ -> None
  ) program

(* parse_pattern cascades through three parsing strategies, in order of
 * specificity. The first strategy whose tree-sitter parse is error-free
 * wins. This lets users write either expression patterns (matched via
 * wrap), top-level decl patterns (matched bare), or full programs.
 *
 * Strategy 1 — bare top-decl: feed the pattern directly. Tree-sitter's
 *   `haskell` rule accepts `terminated($, $._topdecl)`, so things like
 *   `import qualified $M`, `class $C $T where $BODY`, `password = "..."`,
 *   `$A +++ $B = $E`, `module $M where ...` all parse at this level.
 *
 * Strategy 2 — wrapped expression: wrap the pattern in
 *   `module Pattern where\nx = <pattern>`. Required for expression-only
 *   patterns like `putStrLn $X`, `$X == $X`, `\$X -> $BODY`, or operator
 *   sections that are not valid top-decls.
 *)
let parse_pattern (str_input : string) :
    (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t =
  (* Lowercase preprocessing for the expression-wrap strategy. *)
  let pp_lower, map_lower = preprocess_metavariables_with_case Lower str_input in
  (* Uppercase preprocessing for the bare top-decl strategy — needed so
   * patterns like `module $M where` tokenize a `module_id` and not a
   * lowercase variable. *)
  let pp_upper, map_upper = preprocess_metavariables_with_case Upper str_input in

  let wrapped = "module Pattern where\nx = " ^ pp_lower in

  (* Decide which strategy to try first based on the pattern's surface
   * form. Patterns that look like top-level declarations should try the
   * bare strategy first; everything else tries wrapped first so the
   * pattern lands in expression context. *)
  let looks_like_stmt (s : string) : bool =
    let trimmed =
      let i = ref 0 in
      while !i < String.length s && (s.[!i] = ' ' || s.[!i] = '\n' || s.[!i] = '\t')
      do incr i done;
      String.sub s !i (String.length s - !i)
    in
    let starts kw =
      let l = String.length kw in
      String.length trimmed >= l
      && String.sub trimmed 0 l = kw
      && (String.length trimmed = l
          || let c = trimmed.[l] in c = ' ' || c = '\n' || c = '\t')
    in
    let has_toplevel_eq_binding =
      not (starts "let" || starts "case" || starts "if"
           || starts "do" || starts "\\")
      && (let n = String.length trimmed in
          let rec scan i depth =
            if i >= n then false
            else match trimmed.[i] with
              | '(' | '[' | '{' -> scan (i+1) (depth+1)
              | ')' | ']' | '}' -> scan (i+1) (depth-1)
              | '=' when depth = 0 && i + 1 < n && trimmed.[i+1] <> '='
                     && (i = 0
                         || (let p = trimmed.[i-1] in
                             p <> '=' && p <> '<' && p <> '>'
                             && p <> '/' && p <> '!')) ->
                  true
              | _ -> scan (i+1) depth
          in
          scan 0 0)
    in
    starts "module" || starts "import" || starts "class" || starts "instance"
    || starts "data" || starts "newtype" || starts "type" || starts "foreign"
    || starts "infixl" || starts "infixr" || starts "infix"
    || has_toplevel_eq_binding
  in

  let try_bare_upper () =
    try_parse_as_program ~preprocessed:pp_upper
      ~metavar_map:map_upper ~candidate:pp_upper
  in
  let try_bare_lower () =
    try_parse_as_program ~preprocessed:pp_lower
      ~metavar_map:map_lower ~candidate:pp_lower
  in
  let try_wrapped () =
    try_parse_as_program ~preprocessed:pp_lower
      ~metavar_map:map_lower ~candidate:wrapped
  in
  let cascade steps =
    let rec go = function
      | [] -> None
      | step :: rest ->
          (match step () with
           | Some outcome -> Some outcome
           | None -> go rest)
    in
    go steps
  in

  let outcome =
    if looks_like_stmt str_input then
      cascade
        [ (fun () ->
            match try_bare_upper () with
            | Some (program, errs) when program <> [] ->
                Some (G.Ss program, errs)
            | _ -> None);
          (fun () ->
            match try_bare_lower () with
            | Some (program, errs) when program <> [] ->
                Some (G.Ss program, errs)
            | _ -> None);
          (fun () ->
            match try_wrapped () with
            | Some (program, errs) ->
                (match extract_wrapped_rhs program with
                 | Some e -> Some (G.E e, errs)
                 | None -> Some (G.Ss program, errs))
            | None -> None); ]
    else
      cascade
        [ (fun () ->
            match try_wrapped () with
            | Some (program, errs) ->
                (match extract_wrapped_rhs program with
                 | Some e -> Some (G.E e, errs)
                 | None -> Some (G.Ss program, errs))
            | None -> None);
          (fun () ->
            match try_bare_upper () with
            | Some (program, errs) when program <> [] ->
                Some (G.Ss program, errs)
            | _ -> None);
          (fun () ->
            match try_bare_lower () with
            | Some (program, errs) when program <> [] ->
                Some (G.Ss program, errs)
            | _ -> None); ]
  in

  (* If every strategy failed, run the bare pattern once more to collect
   * tree-sitter's error report — we don't want to return `Ss []` because
   * the matcher would treat that as "match anything". *)
  let result, errors = match outcome with
    | Some (ast, errs) -> (Some ast, errs)
    | None ->
        let parsing_result =
          Tree_sitter_haskell.Parse.string ~src_file:"<pattern>" pp_lower
        in
        (None, parsing_result.errors)
  in

  let stat = {
    Tree_sitter_run.Parsing_result.total_line_count = 0;
    error_line_count = List.length errors;
    error_count = List.length errors;
  } in
  { Tree_sitter_run.Parsing_result.program = result; errors; extras = []; stat }
