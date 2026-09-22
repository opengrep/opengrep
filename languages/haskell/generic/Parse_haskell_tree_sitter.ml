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
type placeholder_case = Lower | Upper | Smart

(* Keywords / tokens after which a metavariable stands for a module,
 * type or constructor name, hence must be spelled uppercase to parse. *)
let upper_context_tokens =
  [ "module"; "import"; "qualified"; "as"; "hiding"; "data"; "newtype";
    "type"; "class"; "instance"; "deriving"; "family"; "::"; "->"; "=>";
    "|"; "forall" ]

(* [Smart] mode decides the case of each metavariable from its position:
 * uppercase after the tokens above, after a `.` (qualified name), or in
 * constructor position (`data T = $C ...`); lowercase elsewhere. This is
 * what makes mixed patterns such as `data $T = $C { $F :: String }`
 * parse, where all-upper and all-lower both fail. *)
let smart_wants_upper (pattern : string) (dollar_pos : int) : bool =
  let is_space c = c = ' ' || c = '\t' || c = '\n' in
  (* previous non-space token, and the first word of the line *)
  let j = ref (dollar_pos - 1) in
  while !j >= 0 && (pattern.[!j] = ' ' || pattern.[!j] = '\t') do decr j done;
  if !j >= 0 && pattern.[!j] = '.' then true
  else begin
    let tok_end = !j + 1 in
    while !j >= 0 && not (is_space pattern.[!j]) do decr j done;
    let prev = String.sub pattern (!j + 1) (tok_end - !j - 1) in
    let line_start =
      let k = ref dollar_pos in
      while !k > 0 && pattern.[!k - 1] <> '\n' do decr k done; !k
    in
    let first_word =
      let k = ref line_start in
      while !k < String.length pattern && is_space pattern.[!k] do incr k done;
      let b = !k in
      while !k < String.length pattern && not (is_space pattern.[!k]) do incr k done;
      String.sub pattern b (!k - b)
    in
    let prev = if prev = "" then "" else
      (* strip a leading `(` as in `($T` *)
      if prev.[0] = '(' then String.sub prev 1 (String.length prev - 1) else prev
    in
    List.mem prev upper_context_tokens
    || ((first_word = "data" || first_word = "newtype") && prev = "=")
    || (first_word = "import"
        && not (String.contains (String.sub pattern line_start (dollar_pos - line_start)) '('))
  end

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
      let ph = match case with
        | Lower | Smart -> "__semgrep_dots__"
        | Upper -> "SemgrepDots"
      in
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
        let upper = match case with
          | Lower -> false | Upper -> true
          | Smart -> smart_wants_upper pattern !i
        in
        let ph =
          if upper then "SemgrepEllipsis" ^ name
          else "__semgrep_ellipsis_" ^ name
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
        let upper = match case with
          | Lower -> false | Upper -> true
          | Smart -> smart_wants_upper pattern !i
        in
        let ph =
          if upper then "SemgrepMv" ^ name
          else "__semgrep_metavar_" ^ name
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
(* Source preprocessing                                                      *)
(*****************************************************************************)

(* The rewrites below keep the line count so tree-sitter's row numbers
 * still address the original file. When a rewrite drops characters at
 * the start of a line (bird tracks), the number of dropped characters is
 * recorded per row so columns can be shifted back (see [parse]). *)

(* Literate Haskell (`.lhs`). Three conventions are recognised:
 *  - bird tracks: only lines starting with `>` are code; the `>` and one
 *    following space are dropped (the grammar does not accept top-level
 *    declarations indented by the space the standard unlit leaves behind);
 *  - LaTeX: only lines between `\begin{code}` and `\end{code}` are code;
 *  - Markdown (markdown-unlit): only lines inside a ```haskell fenced
 *    block are code (fences starting with ``` or ~~~ end a block).
 * Prose lines are blanked. A file using none of them is left untouched. *)
let unlit (src : string) : string * int array =
  let lines = Array.of_list (String.split_on_char '\n' src) in
  let n = Array.length lines in
  let shift = Array.make n 0 in
  let is_bird l = String.length l > 0 && l.[0] = '>' in
  let starts_with pre l =
    String.length l >= String.length pre
    && String.sub l 0 (String.length pre) = pre
  in
  let is_fence l = starts_with "```" l || starts_with "~~~" l in
  (* markdown-unlit: a block is code when its info string names haskell
   * and does not carry the `ignore` marker (used for snippets that are
   * not meant to compile). *)
  let is_haskell_fence l =
    is_fence l
    && (let rest = String.trim (String.sub l 3 (String.length l - 3)) in
        (starts_with "haskell" rest || starts_with "{.haskell" rest)
        && not (List.mem "ignore" (String.split_on_char ' ' rest)))
  in
  let has_bird = Array.exists is_bird lines in
  let has_latex = Array.exists (starts_with "\\begin{code}") lines in
  let has_markdown = Array.exists is_haskell_fence lines in
  if not (has_bird || has_latex || has_markdown) then (src, shift)
  else begin
    let in_code = ref false in
    let out = Array.mapi (fun i l ->
      if has_latex && starts_with "\\begin{code}" l then (in_code := true; "")
      else if has_latex && starts_with "\\end{code}" l then (in_code := false; "")
      else if has_markdown && not !in_code && is_haskell_fence l then
        (in_code := true; "")
      else if has_markdown && !in_code && is_fence l then (in_code := false; "")
      else if !in_code then l
      else if has_bird && is_bird l then begin
        let drop = if String.length l > 1 && l.[1] = ' ' then 2 else 1 in
        shift.(i) <- drop;
        String.sub l drop (String.length l - drop)
      end
      else "") lines
    in
    (String.concat "\n" (Array.to_list out), shift)
  end

(* C preprocessor directives are common in real-world Haskell and the
 * grammar handles them poorly (an `#else` can swallow the code after it,
 * and keeping every branch yields duplicate module headers or imports).
 * Approximate the preprocessor: directive lines are blanked, the first
 * branch of each `#if`/`#ifdef`/`#ifndef` is kept and every `#elif` /
 * `#else` branch is blanked. A shebang line is blanked too. *)
let blank_cpp_directives (src : string) : string =
  let directive l =
    let n = String.length l in
    if n = 0 || l.[0] <> '#' then None
    else if n >= 2 && l.[1] = '!' then Some "shebang"
    else begin
      let i = ref 1 in
      while !i < n && (l.[!i] = ' ' || l.[!i] = '\t') do incr i done;
      let j = ref !i in
      while !j < n && l.[!j] >= 'a' && l.[!j] <= 'z' do incr j done;
      match String.sub l !i (!j - !i) with
      | ("if" | "ifdef" | "ifndef" | "elif" | "else" | "endif"
        | "define" | "undef" | "include" | "error" | "warning"
        | "line" | "pragma") as d -> Some d
      | _ -> None
    end
  in
  if not (String.contains src '#') then src
  else begin
    (* Stack of enclosing conditionals: [true] while inside a kept branch. *)
    let stack = ref [] in
    let keeping () = List.for_all (fun k -> k) !stack in
    String.split_on_char '\n' src
    |> List.map (fun l ->
        match directive l with
        | Some ("if" | "ifdef" | "ifndef") -> stack := true :: !stack; ""
        | Some ("elif" | "else") ->
            (match !stack with
             | _ :: rest -> stack := false :: rest
             | [] -> ());
            ""
        | Some "endif" ->
            (match !stack with _ :: rest -> stack := rest | [] -> ());
            ""
        | Some _ -> ""
        | None -> if keeping () then l else "")
    |> String.concat "\n"
  end

(* Returns the source to hand to tree-sitter plus the per-row column
 * shift to apply when mapping positions back to the original file. *)
let preprocess_source (file : Fpath.t) (src : string) : string * int array =
  let src, shift =
    if Fpath.has_ext ".lhs" file then unlit src
    else (src, [||])
  in
  (blank_cpp_directives src, shift)

(*****************************************************************************)
(* Entry points                                                              *)
(*****************************************************************************)

let parse (file : Fpath.t) :
    (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t =
  let src = UFile.read_file file in
  let pre, shift = preprocess_source file src in
  H.wrap_parser
    (fun () ->
      if pre == src then Tree_sitter_haskell.Parse.file !!file
      else Tree_sitter_haskell.Parse.string ~src_file:!!file pre)
    (fun cst _extras ->
      let conv = H.line_col_to_pos file in
      let conv (line, col) =
        let row = line - 1 in
        let delta =
          if row >= 0 && row < Array.length shift then shift.(row) else 0
        in
        conv (line, col + delta)
      in
      let env : Haskell_to_generic.env = {
        H.file;
        conv;
        extra = {
          Haskell_to_generic.is_pattern_mode = false;
          metavar_map = Hashtbl.create 1;
          col_shift = shift;
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
            col_shift = [||];
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
(* Two ellipsis placements the grammar cannot express once `...` has
 * been turned into an identifier:
 *  - a `...` alone on the line after a `where` (`instance Show $T where`
 *    followed by `...`): the line is dropped, and the walker injects an
 *    ellipsis body for the empty `where` block;
 *  - `...` used as a record field (`$C { ..., $F :: T, ... }`): it becomes
 *    the field `... :: ...`, which the walker turns into a TyEllipsis. *)
let rewrite_pattern_ellipses (pattern : string) : string =
  let lines = String.split_on_char '\n' pattern in
  let ends_with_where l =
    let t = String.trim l in
    let n = String.length t in
    n >= 5 && String.sub t (n - 5) 5 = "where"
    && (n = 5 || t.[n - 6] = ' ' || t.[n - 6] = ')')
  in
  let rec drop_body_dots prev = function
    | [] -> []
    | l :: rest when String.trim l = "..." && prev ->
        "" :: drop_body_dots false rest
    | l :: rest -> l :: drop_body_dots (ends_with_where l) rest
  in
  let lines = drop_body_dots false lines in
  let pattern = String.concat "\n" lines in
  (* `{ ..., f :: T }` / `{ f :: T, ... }` / `{ ... }` *)
  let buf = Buffer.create (String.length pattern) in
  let n = String.length pattern in
  let prev_sig i =
    let j = ref (i - 1) in
    while !j >= 0 && (pattern.[!j] = ' ' || pattern.[!j] = '\n') do decr j done;
    if !j >= 0 then pattern.[!j] else ' '
  in
  let next_sig i =
    let j = ref i in
    while !j < n && (pattern.[!j] = ' ' || pattern.[!j] = '\n') do incr j done;
    if !j < n then pattern.[!j] else ' '
  in
  let i = ref 0 in
  while !i < n do
    if !i + 2 < n && pattern.[!i] = '.' && pattern.[!i + 1] = '.'
       && pattern.[!i + 2] = '.'
       && (!i + 3 >= n || pattern.[!i + 3] <> '.')
       && (let p = prev_sig !i in p = '{' || p = ',')
       && (let q = next_sig (!i + 3) in q = '}' || q = ',')
    then (Buffer.add_string buf "... :: ..."; i := !i + 3)
    else (Buffer.add_char buf pattern.[!i]; incr i)
  done;
  Buffer.contents buf

let parse_pattern (str_input : string) :
    (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t =
  let str_input = rewrite_pattern_ellipses str_input in
  (* Mixed-case preprocessing: each metavariable's case follows its
   * syntactic position (see [smart_wants_upper]). *)
  let pp_smart, map_smart = preprocess_metavariables_with_case Smart str_input in
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
    (* `name :: T` with a plain (non-metavariable) lowercase name or a
     * parenthesised operator is unambiguously a type signature, hence a
     * top-level declaration. `$F :: $T` and `(e :: T)` stay expressions
     * (a cast) so they keep matching inline annotations. *)
    let looks_like_signature =
      let n = String.length trimmed in
      let rec find_dcolon i depth =
        if i + 1 >= n then None
        else match trimmed.[i] with
          | '(' | '[' | '{' -> find_dcolon (i + 1) (depth + 1)
          | ')' | ']' | '}' -> find_dcolon (i + 1) (depth - 1)
          | ':' when depth = 0 && trimmed.[i + 1] = ':' -> Some i
          | _ -> find_dcolon (i + 1) depth
      in
      match find_dcolon 0 0 with
      | None -> false
      | Some pos ->
          let head = String.trim (String.sub trimmed 0 pos) in
          let hn = String.length head in
          hn > 0
          && (let c = head.[0] in
              ((c >= 'a' && c <= 'z') || c = '_')
              && String.for_all (fun ch ->
                   (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
                   || (ch >= '0' && ch <= '9') || ch = '_' || ch = '\'') head
              || (c = '(' && head.[hn - 1] = ')'
                  && not (String.contains head '$')))
    in
    starts "module" || starts "import" || starts "class" || starts "instance"
    || starts "data" || starts "newtype" || starts "type" || starts "foreign"
    || starts "infixl" || starts "infixr" || starts "infix"
    || starts "default" || starts "deriving" || starts "pattern"
    || has_toplevel_eq_binding || looks_like_signature
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
  let try_bare_smart () =
    try_parse_as_program ~preprocessed:pp_smart
      ~metavar_map:map_smart ~candidate:pp_smart
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
      let bare_smart_step () =
        match try_bare_smart () with
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
           [ bare_upper_step; bare_lower_step; bare_smart_step; wrapped_step ]
         else
           [ bare_lower_step; bare_upper_step; bare_smart_step; wrapped_step ])
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
            | _ -> None);
          (fun () ->
            match try_bare_smart () with
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
