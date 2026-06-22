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

(* Preprocess `$VAR` and `$...VAR` metavariables outside of string
 * literals. We deliberately skip the contents of double-quoted strings
 * so that patterns like [foo "$VAR"] keep their literal `$VAR` inside
 * the string for the matcher to recognize as a string metavariable
 * (this matches Python's behavior). *)
let preprocess_metavariables_with_case (case : placeholder_case)
    (pattern : string) : string * (string, string) Hashtbl.t =
  let n = String.length pattern in
  let buffer = Buffer.create n in
  let mapping = Hashtbl.create 16 in
  let is_meta_start_char c =
    (c >= 'A' && c <= 'Z') || c = '_'
  in
  let is_meta_body_char c =
    (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
  in
  let i = ref 0 in
  while !i < n do
    let c = pattern.[!i] in
    if c = '"' then begin
      (* Copy a string literal verbatim, including the surrounding
       * quotes, handling backslash escapes. *)
      Buffer.add_char buffer '"';
      incr i;
      let in_string = ref true in
      while !in_string && !i < n do
        let ch = pattern.[!i] in
        if ch = '\\' && !i + 1 < n then begin
          Buffer.add_char buffer ch;
          Buffer.add_char buffer pattern.[!i + 1];
          i := !i + 2
        end else if ch = '"' then begin
          Buffer.add_char buffer '"';
          incr i;
          in_string := false
        end else begin
          Buffer.add_char buffer ch;
          incr i
        end
      done
    end else if c = '<' && !i + 3 < n
                && pattern.[!i + 1] = '.' && pattern.[!i + 2] = '.'
                && pattern.[!i + 3] = '.' then begin
      (* Deep-ellipsis open `<...`: rewrite to a marker application whose
       * single argument is the deep-matched expression. The matching
       * `...>` becomes the closing parens. *)
      Buffer.add_string buffer "(__semgrep_deep__ (";
      Hashtbl.replace mapping "__semgrep_deep__" "<...>";
      i := !i + 4
    end else if c = '.' && !i + 3 < n
                && pattern.[!i + 1] = '.' && pattern.[!i + 2] = '.'
                && pattern.[!i + 3] = '>' then begin
      (* Deep-ellipsis close `...>`. *)
      Buffer.add_string buffer "))";
      i := !i + 4
    end else if c = '.' && !i + 2 < n
                && pattern.[!i + 1] = '.' && pattern.[!i + 2] = '.'
                && (!i + 3 >= n || pattern.[!i + 3] <> '.') then begin
      (* A bare `...` ellipsis is not valid Haskell syntax, so tree-sitter
       * cannot parse it. Rewrite it to a placeholder identifier (like the
       * metavariable rewrites) that the walker turns back into
       * G.Ellipsis. Exactly three dots, so `..` ranges and `{..}` record
       * wildcards are left untouched. *)
      let ph = "__semgrep_dots__" in
      Hashtbl.replace mapping ph "...";
      Buffer.add_string buffer ph;
      i := !i + 3
    end else if c = '$' && !i + 1 < n then begin
      (* Try to match $...NAME or $NAME *)
      let after_dollar = !i + 1 in
      if after_dollar + 2 < n
         && pattern.[after_dollar] = '.'
         && pattern.[after_dollar + 1] = '.'
         && pattern.[after_dollar + 2] = '.'
         && after_dollar + 3 < n
         && is_meta_start_char pattern.[after_dollar + 3]
      then begin
        let name_start = after_dollar + 3 in
        let name_end = ref name_start in
        while !name_end < n && is_meta_body_char pattern.[!name_end] do
          incr name_end
        done;
        let name = String.sub pattern name_start (!name_end - name_start) in
        let ph = match case with
          | Lower -> "__semgrep_ellipsis_" ^ name
          | Upper -> "SemgrepEllipsis" ^ name
        in
        let original = String.sub pattern !i (!name_end - !i) in
        Hashtbl.replace mapping ph original;
        Buffer.add_string buffer ph;
        i := !name_end
      end else if is_meta_start_char pattern.[after_dollar] then begin
        let name_end = ref (after_dollar + 1) in
        while !name_end < n && is_meta_body_char pattern.[!name_end] do
          incr name_end
        done;
        let name = String.sub pattern after_dollar (!name_end - after_dollar) in
        let ph = match case with
          | Lower -> "__semgrep_metavar_" ^ name
          | Upper -> "SemgrepMv" ^ name
        in
        let original = String.sub pattern !i (!name_end - !i) in
        Hashtbl.replace mapping ph original;
        Buffer.add_string buffer ph;
        i := !name_end
      end else begin
        Buffer.add_char buffer c;
        incr i
      end
    end else begin
      Buffer.add_char buffer c;
      incr i
    end
  done;
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
 * walker nests the binding inside ModuleDef(ModuleStruct(...)) and now
 * emits zero-param value bindings as VarDef instead of FuncDef (so the
 * matcher's constant propagation kicks in), so we accept either shape. *)
let rec extract_wrapped_rhs (program : G.program) : G.expr option =
  List.find_map (fun stmt ->
    match stmt.G.s with
    | G.DefStmt (ent, G.FuncDef { G.fbody = G.FBExpr expr; _ }) ->
        (match ent.G.name with
         | G.EN (G.Id (("x", _), _)) -> Some expr
         | _ -> None)
    | G.DefStmt (ent, G.VarDef { G.vinit = Some expr; _ }) ->
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
    || starts "default" || starts "deriving" || starts "pattern"
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

  (* Patterns starting with a Haskell keyword [module / class / instance
   * / data / newtype / type / foreign / infix] need uppercase
   * preprocessing because the ident slot in those decls only accepts
   * Module-style or Type-style identifiers. Other top-decl patterns
   * like [$F $X = $E] are function definitions whose first ident must
   * be lowercase, so bare-lower wins. *)
  let trimmed_starts =
    let s = str_input in
    let i = ref 0 in
    while !i < String.length s && (s.[!i] = ' ' || s.[!i] = '\n' || s.[!i] = '\t')
    do incr i done;
    fun kw ->
      let l = String.length kw in
      String.length s - !i >= l
      && String.sub s !i l = kw
      && (String.length s - !i = l
          || let c = s.[!i + l] in c = ' ' || c = '\n' || c = '\t')
  in
  let needs_upper_first =
    trimmed_starts "module" || trimmed_starts "import"
    || trimmed_starts "class" || trimmed_starts "instance"
    || trimmed_starts "data" || trimmed_starts "newtype"
    || trimmed_starts "type" || trimmed_starts "foreign"
    || trimmed_starts "infixl" || trimmed_starts "infixr"
    || trimmed_starts "infix"
    || trimmed_starts "default" || trimmed_starts "deriving"
    || trimmed_starts "pattern"
  in

  let outcome =
    if looks_like_stmt str_input then
      let bare_upper_step () =
        match try_bare_upper () with
        | Some (program, errs) when program <> [] ->
            Some (G.Ss program, errs)
        | _ -> None
      in
      let bare_lower_step () =
        match try_bare_lower () with
        | Some (program, errs) when program <> [] ->
            Some (G.Ss program, errs)
        | _ -> None
      in
      let wrapped_step () =
        match try_wrapped () with
        | Some (program, errs) ->
            (match extract_wrapped_rhs program with
             | Some e -> Some (G.E e, errs)
             | None -> Some (G.Ss program, errs))
        | None -> None
      in
      cascade
        (if needs_upper_first then
           [ bare_upper_step; bare_lower_step; wrapped_step ]
         else
           [ bare_lower_step; bare_upper_step; wrapped_step ])
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
