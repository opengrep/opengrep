type token_name = string

module T = Vbnet_token
module Tokenize = Vbnet_tokenize
module G = AST_generic
module RT = Raw_tree
module DLS = Domain.DLS

type token = T.t

let token_match = T.token_match

let token_ghost = T.token_ghost

type 'a parsing_result = {
  next : token list;
  value : 'a
  }

type 'a parser =
  (* next *) token list ->
  ('a parsing_result) Seq.t

(* parser state *)

let partial = DLS.new_key (fun () -> false)

let last_pos = DLS.new_key (fun () -> -1)

let last_token : T.t option DLS.key = DLS.new_key (fun _ -> None)

let reset_parser_state () =
  DLS.set partial false;
  DLS.set last_pos (-1);
  DLS.set last_token None

(* run parser *)

let run (p : 'a parser) (ts : token list): 'a parsing_result list =
  reset_parser_state ();
  p ts |> Seq.take 1 |> List.of_seq

type error = Fpath.t * string * Pos.t

let error_from_token_opt (f : Fpath.t) (tok_opt : T.t option) : error option =
  match tok_opt with
  | None -> None
  | Some t ->
      match Tok.loc_of_tok t.tok with
      | Result.Ok loc -> Some (f, t.content, loc.pos)
      | _ -> None

 (* Can't use Parsing_Result2.t because of subproject dependencies *)
type 'a result =
  | Ok of 'a * Parsing_stat.t
  | Partial of 'a * Parsing_stat.t * error option
  | Fail of Parsing_stat.t * error option

(* FIXME: works for entire files only! *)
let tokenize_and_parse_string (p : 'a parser) ?(filepath=Fpath.v "<pattern>")
    (s : string) : 'a result =
  let ts = Tokenize.tokenize ~filepath s in
  let line_count = List.filter T.token_line_terminator ts |> List.length in
  let stat =
    Parsing_stat.{
      filename = Fpath.filename filepath;
      total_line_count = line_count;
      error_line_count = 0;
      have_timeout = false;
      commentized = 0;
      problematic_lines = [];
      ast_stat = None;
    }
  in
  match run p ts with
  | { value; next = [] } :: _ when DLS.get partial ->
      Partial (value,
               stat,
               error_from_token_opt filepath (DLS.get last_token))
  | { value; next = [] } :: _ ->
      Ok (value, stat)
  | _ -> Fail (stat,
               error_from_token_opt filepath (DLS.get last_token))


let ( let/ ) xs f = Seq.concat_map f xs

let empty = Seq.empty

let single a = Seq.cons a Seq.empty

let is_empty = Seq.is_empty

let cut s = Seq.take 1 s

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun next ->
    let/ { next = next'; value = v } = p next in
    f v next'

let ( let* ) p f = bind p f

let pure (value : 'a) : 'a parser =
  fun next ->
    single { next; value }

let fail : 'a parser =
  fun _next -> empty

let rec token (t : token_name) : token parser =
  fun next ->
    match next with
    | w :: ws when token_ghost w ->
        token t ws
    | w :: ws when token_match t w ->
        let loc = Tok.unsafe_loc_of_tok w.tok in
        if loc.pos.bytepos > DLS.get last_pos then
          begin
            DLS.set last_pos loc.pos.bytepos;
            DLS.set last_token (Some w)
          end;
        single { next = ws; value = w }
    | _ -> empty

let pure_token (t : token_name) : unit parser =
  let* _ = token t in
  pure ()

let rec token_type (t : T.token_kind) : token parser =
  fun next ->
    match next with
    | w :: ws when token_ghost w ->
        token_type t ws
    | w :: ws when w.kind = t ->
        single { next = ws; value = w }
    | _ -> empty

let choice (ps : 'a parser list) : 'a parser =
  fun next ->
    let/ p = List.to_seq ps in
    p next

let look_ahead (t : token_name) : unit parser =
  fun next ->
    match next with
    | w :: _ when token_match t w ->
        single { next; value = () }
    | [] when String.equal t "EOF" ->
        single { next; value = () }
    | _ -> empty

let look_ahead_not (t : token_name) : unit parser =
  fun next ->
    match next with
    | w :: _ when token_match t w ->
        empty
    | [] when String.equal t "EOF" ->
        empty
    | _ ->
        single { next; value = () }

let optional (p : 'a parser) : 'a option parser =
  choice
    [ begin
        let* a = p in
        pure (Some a)
      end
    ; pure None
    ]

(* lists don't backtrack for perf reasons *)
let rec list_of_aux (p : 'a parser) (acc : 'a list) : 'a list parser =
  fun next ->
    match Seq.uncons (p next) with
    | None -> pure (List.rev acc) next
    | Some (r, _) ->
        list_of_aux p (r.value :: acc) r.next

let list_of (p : 'a parser) : 'a list parser =
  fun next ->
    list_of_aux p [] next

let ne_list_of (p : 'a parser) : 'a list parser =
  let* x = p in
  let* xs = list_of p in
  pure (x :: xs)

let stop_on_partial : unit parser =
  fun _next ->
    DLS.set partial true;
    single { next = []; value = () }

let any_of_raw (r : G.any RT.t) : G.any =
  G.Raw r

let raw_of_any (a : G.any) : G.any RT.t =
  RT.Any a

let xToken (t : token) : G.any =
  G.Raw (RT.Token (t.content, t.tok))

let xRule (_rule_name : string) (_prod_idex : int) (rs : G.any list) : G.any =
  RT.Tuple (List.map raw_of_any rs) |> any_of_raw

let xGroup (rs : G.any list) : G.any =
  RT.List (List.map raw_of_any rs) |> any_of_raw

let xOptional (r : G.any option) : G.any =
  RT.Option (Option.map raw_of_any r) |> any_of_raw

let xList (rs : G.any list) : G.any =
  RT.List (List.map raw_of_any rs) |> any_of_raw

(* helpers *)

let fb = Tok.unsafe_fake_bracket

let ident_of_token (t : T.t) : G.ident =
  t.content, t.tok

let expr_of_id (id : G.ident) =
  G.N (G.Id (id, G.empty_id_info ~case_insensitive:true ())) |> G.e

let make_name (ident : G.ident) (type_args : G.type_arguments option) : G.name =
  match type_args with
  | None -> G.Id (ident, G.empty_id_info ~case_insensitive:true ())
  | Some _ ->
      G.IdQualified
        { name_last = (ident, type_args);
          name_middle = None;
          name_top = None;
          name_info = G.empty_id_info ~case_insensitive:true () }

let rec split_last (xs : 'a list) : 'a list * 'a =
  match xs with
  | [] -> failwith "impossible"
  | [x] -> [], x
  | x :: xs' -> let rs, r = split_last xs' in (x :: rs), r

let idents_of_qualifier (q : G.qualifier option) : (G.ident * G.type_arguments option) list =
  match q with
  | Some (G.QDots xs) -> xs
  | _ -> []

let idents_of_name (n : G.name) : (G.ident * G.type_arguments option) list =
  match n with
  | Id (i, _) -> [i, None]
  | IdQualified r -> (idents_of_qualifier r.name_middle) @ [r.name_last]

let collapse_names (ns : G.name list) : G.name =
  match ns with
  | [] -> failwith "impossible"
  | [n] -> n
  | _ ->
      let ns, n = List.concat_map idents_of_name ns |> split_last in
      G.IdQualified
        { name_last = n;
          name_middle = Some (QDots ns);
          name_top = None;
          name_info = G.empty_id_info ~case_insensitive:true ()
        }

let add_attrs_to_type (t : G.type_) (attrs : G.attribute list) : G.type_=
  { t with t_attrs = attrs @ t.t_attrs }

let add_attrs_to_stmt (s : G.stmt) (attrs : G.attribute list) : G.stmt =
  match s with
  | { G.s = G.DefStmt (entity, def); _ } ->
      { s with
        G.s = G.DefStmt ({ entity with G.attrs = attrs @ entity.G.attrs }, def) }
  | _ -> s

let stmt_of_stmts (xs : G.stmt list) =
  match xs with
  | [s] -> s
  | _ -> G.Block (fb xs) |> G.s

let raw_token (t : T.t) : G.any RT.t =
  RT.Token (t.content, t.tok)

let s_range (tl : Tok.t) (tr : Tok.t) (stmt : G.stmt) : G.stmt =
  let l = Tok.unsafe_loc_of_tok tl in
  let r = Tok.unsafe_loc_of_tok tr in
  stmt.s_range <- Some (l, r);
  stmt

(* parser *)

let rec compilation_unit : G.program parser = fun __n -> (
  (* compilation_unit -> toplevel '<EOF>' *)
  (* ... or stop on partial *)
  let* stmts = toplevel in
  let* _ = choice [
      pure_token "<EOF>";
      stop_on_partial
    ]
  in
  pure stmts
) __n |> cut

and toplevel : G.stmt list parser = fun __n -> (
  (* toplevel -> option_statement* imports_statement* attributes_statement* toplevel_declaration* *)
  let* options = list_of option_statement in
  let* imports = list_of imports_statement in
  let* attrs = list_of attributes_statement in
  let* decls = list_of toplevel_declaration in
  pure (options @ List.concat imports @ List.concat attrs @ decls)
) __n

and option_statement_mandatory : G.ident parser = fun __n -> (
  (* option_statement_mandatory -> 'Explicit' *)
  let* t = choice [ token "EXPLICIT"; token "STRICT"; token "COMPARE"; token "INFER" ] in
  pure (ident_of_token t)
) __n

and option_statement_optional : G.ident parser = fun __n -> (
  let* t = choice [ token "ON"; token "OFF"; token "TEXT"; token "BINARY" ] in
  pure (ident_of_token t)
) __n

and option_statement : G.stmt parser = fun __n -> (
  (* option_statement -> 'Option' option_statement_mandatory option_statement_optional? *)
  let* option = token "OPTION" in
  let* mandatory = option_statement_mandatory in
  let* opt = optional option_statement_optional in
  let directive =
    G.{ d = G.Pragma
              (ident_of_token option,
               List.map (fun x -> G.I x) (mandatory :: Option.to_list opt));
        d_attrs = []
      }
  in
  pure (G.DirectiveStmt directive |> G.s)
) __n

and imports_statement : G.stmt list parser = fun __n -> (
  (* imports_statement -> 'Imports' imports_clause (',' imports_clause)* *)
  let* imports = token "IMPORTS" in
  let* clause = imports_clause imports in
  let* clauses = list_of
    begin
      let* _comma = token "," in
      imports_clause imports
    end
  in
  pure (clause :: clauses)
) __n

and imports_clause (imports : T.t) : G.stmt parser = fun __n -> (
  choice [
    begin
      (* imports_clause -> simple_imports_clause *)
      simple_imports_clause imports
    end;
    begin
      (* imports_clause -> '<' x_name '=' '<STRING>' '>' *)
      let* _lt = token "<" in
      let* name = x_name in
      let* _eq = token "=" in
      let* ns = token "<STRING>" in
      let* _gt = token ">" in
      let id = ident_of_token imports in
      let lit = G.L (G.String (fb (T.extract_string_content ns, ns.tok))) |> G.e in
      let directive =
        G.{ d = G.Pragma (id, [G.Name name; G.E lit]);
            d_attrs = []
          }
      in
      pure (G.DirectiveStmt directive |> G.s)
    end;
  ]
) __n

and simple_imports_clause (imports : T.t) : G.stmt parser = fun __n -> (
  (* simple_imports_clause -> import_alias_clause? name *)
  let* alias = optional import_alias_clause in
  let* n = name in
  let dotted_name = G.DottedName (List.map fst (idents_of_name n)) in
  let directive =
    G.{ d = G.ImportAs (imports.tok, dotted_name, alias);
        d_attrs = []}
  in
  pure (G.DirectiveStmt directive |> G.s)
) __n

and import_alias_clause : G.alias parser = fun __n -> (
  (* import_alias_clause -> identifier_token '=' *)
  let* t = token "<IDENT>" in
  let* _eq = token "=" in
  pure (ident_of_token t, G.empty_id_info ~case_insensitive:true ())
) __n

and attributes_statement : G.stmt list parser = fun __n -> (
  (* attributes_statement -> attribute_list+ *)
  let* attrs = ne_list_of attribute_list in
  let attrs = List.concat attrs in
  let make_stmt (a : G.attribute) : G.stmt =
    let directive =
      G.{ d = G.OtherDirective (("attribute", Tok.unsafe_fake_tok "attribute"), [G.At a]);
          d_attrs = []
        }
    in
    G.DirectiveStmt directive |> G.s
  in
  pure (List.map make_stmt attrs)
) __n

and attribute_list : G.attribute list parser = fun __n -> (
  (* attribute_list -> '<' attribute (',' attribute)* '>' *)
  let* _lt = token "<" in
  let* attr = attribute in
  let* attrs = list_of
    begin
      let* _comma = token "," in
      attribute
    end
  in
  let* _gt = token ">" in
  pure (attr :: attrs)
) __n

and toplevel_attribute_list : G.attribute list parser = fun __n -> (
  (* toplevel_attribute_list -> '<' attribute (',' attribute)* '>' *)
  let* _lt = token "<" in
  let* attr = attribute in
  let* attrs = list_of
    begin
      let* _comma = token "," in
      attribute
    end
  in
  let* _gt = token ">" in
  pure (attr :: attrs)
) __n

and attribute : G.attribute parser = fun __n -> (
  (* attribute -> attribute_target? type argument_list? *)
  let* target_opt = optional attribute_target in
  let* name = qualified_name in
  let name =
    match target_opt with
    | None -> name
    | Some target -> collapse_names [target; name]
  in
  let* args = optional argument_list in
  let args =
    match args with
    | None -> fb []
    | Some args -> args
  in
  pure (G.NamedAttr (Tok.unsafe_fake_tok "", name, args))
) __n

and toplevel_attribute : G.attribute parser = fun __n -> (
  (* toplevel_attribute -> attribute_target? type argument_list? *)
  let* target = attribute_target in
  let* name = qualified_name in
  let name = collapse_names [target; name] in
  let* args = optional argument_list in
  let args =
    match args with
    | None -> fb []
    | Some args -> args
  in
  pure (G.NamedAttr (Tok.unsafe_fake_tok "", name, args))
) __n

and attribute_target : G.name parser = fun __n -> (
  (* attribute_target -> ('Assembly' | 'Module') :' *)
  let* t = choice [ token "ASSEMBLY"; token "MODULE" ] in
  let* _colon = token ":" in
  pure (make_name (ident_of_token t) None)
) __n

and argument_list : G.arguments parser = fun __n -> (
  (* argument_list -> '(' (argument? (',' argument?)* )? ')' *)
  (* NOTE: arguments can be skipped, as in foo(1,2,,4) *)
  let* lparen = token "(" in
  let* args_opt = optional
    begin
      let* arg = optional argument in
      let arg =
        match arg with
        | Some arg -> arg
        | None -> G.Arg (G.L (G.Undefined lparen.tok) |> G.e)
      in
      let* args = list_of
        begin
          let* comma = token "," in
          let* arg = optional argument in
          match arg with
          | Some arg -> pure arg
          | None -> pure (G.Arg (G.L (G.Undefined comma.tok) |> G.e))
        end
      in
      pure (arg :: args)
    end
  in
  let* rparen = token ")" in
  let args =
    match args_opt with
    | None -> []
    | Some args -> args
  in
  pure (lparen.tok, args, rparen.tok)
) __n

and argument : G.argument parser = fun __n -> (
  (* argument -> (identifier_or_keyword ':=')? expression ('To' expression)? *)
  let* id_opt = optional
    begin
      let* id =
        choice [
          identifier_name;
          let* t = token "<KEYWORD>" in pure (ident_of_token t)
        ]
      in
      let* _colon_eq = token ":=" in
      pure id
    end
  in
  let* expr = expression in
  (* TODO: use separate construct to represent range args *)
  (* let* to_expression_opt1 = optional
    begin
      let* to1 = token "TO" in
      let* expression1 = expression in
      pure (xGroup([xToken(to1); G.E expression1]))
    end
     in *)
  match id_opt with
  | None -> pure (G.Arg expr)
  | Some ident -> pure (G.ArgKwd (ident, expr))
) __n

and escaped_identifier_content : G.ident (* =  string G.wrap *) parser = fun __n -> (
  choice [
    begin
      (* escaped_identifier_content -> '<KEYWORD>' *)
      let* t = token "<KEYWORD>" in
      pure (t.content, t.tok)
    end;
    begin
      (* escaped_identifier_content -> '<IDENT>' *)
      let* t = token "<IDENT>" in
      pure (t.content, t.tok)
    end;
    begin
      (* escaped_identifier_content -> '<OPERATOR>' *)
      let* t = token "<OPERATOR>" in
      pure (t.content, t.tok)
    end;
    begin
      (* escaped_identifier_content -> '<INT>' *)
      let* t = token "<INT>" in
      pure (t.content, t.tok)
    end;
    begin
      (* escaped_identifier_content -> '<STRING>' *)
      let* t = token "<STRING>" in
      pure (t.content, t.tok)
    end;
  ]
) __n

and identifier_name : G.ident (* = string G.wrap *) parser = fun __n -> (
  choice [
    begin
      (* identifier_name -> '<IDENT>' *)
      let* t = token "<IDENT>" in
      pure (t.content, t.tok)
    end;
    begin
      (* identifier_name -> '[' escaped_identifier_content+ ']' *)
      (* TODO: Make it all happen in the lexer? *)
      let* lbrack = token "[" in
      let* ids = ne_list_of escaped_identifier_content in
      let* rbrack = token "]" in
      let str = lbrack.content ^ String.concat "" (List.map fst ids) ^ rbrack.content in
      let tok = Tok.combine_toks lbrack.tok (List.map snd ids @ [rbrack.tok]) in
      pure (str, tok)
    end;
  ]
) __n

and single_line_statement : G.stmt list parser = fun __n -> (
  choice [
    begin
      (* single_line_statement -> single_line_if_statement *)
      let* s = single_line_if_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> add_remove_handler_statement *)
      let* s = add_remove_handler_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> raise_event_statement *)
      let* s = raise_event_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> local_declaration_statement *)
      local_declaration_statement
    end;
    begin
      (* single_line_statement -> erase_statement *)
      let* s = erase_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> error_statement *)
      let* s = error_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> continue_statement *)
      let* s = continue_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> call_statement *)
      let* s = call_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> go_to_statement *)
      let* s = go_to_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> on_error_statement *)
      let* s = on_error_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> re_dim_statement *)
      re_dim_statement
    end;
    begin
      (* single_line_statement -> resume_statement *)
      let* s = resume_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> return_statement *)
      let* s = return_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> stop_or_end_statement *)
      let* s = stop_or_end_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> throw_statement *)
      let* s = throw_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> yield_statement *)
      let* s = yield_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> exit_statement *)
      let* s = exit_statement in
      pure [s]
    end;
    begin
      (* single_line_statement -> assignment_statement *)
      let* s = assignment_statement in
      pure [s]
    end;
  ]
) __n

and single_line_statements : G.stmt list parser = fun __n -> (
  (* single_line_statements -> single_line_statement (':' single_line_statement)* *)
  let* stmt = single_line_statement in
  let* stmts = list_of
    begin
      let* _colon = token ":" in
      single_line_statement
    end
  in
  pure (stmt @ List.concat stmts)
) __n

and multi_line_statement : G.stmt parser = fun __n -> (
  choice [
    begin
      (* multi_line_statement -> select_case_block *)
      select_case_block
    end;
    begin
      (* multi_line_statement -> multi_line_if_block *)
      multi_line_if_block
    end;
    begin
      (* multi_line_statement -> for_block *)
      for_block
    end;
    begin
      (* multi_line_statement -> do_block *)
      do_block
    end;
    begin
      (* multi_line_statement -> while_block *)
      while_block
    end;
    begin
      (* multi_line_statement -> try_block *)
      try_block
    end;
    begin
      (* multi_line_statement -> with_block *)
      with_block
    end;
    begin
      (* multi_line_statement -> sync_lock_block *)
      sync_lock_block
    end;
    begin
      (* multi_line_statement -> using_block *)
      using_block
    end;
  ]
) __n

and statements_block_item : G.stmt list parser = fun __n -> (
  choice [
    begin
      (* statements_block_item -> multi_line_statement *)
      let* s = multi_line_statement in
      pure [s]
    end;
    begin
      (* statements_block_item -> (identifier_label | numeric_label) ':' statements_block_item *)
      let* label = choice [ identifier_label; numeric_label ] in
      let* _colon = token ":" in
      let* stmts = statements_block_item in
      let make_label s =
        G.Label (label, s) |> G.s
      in
      match stmts with
      | [] -> failwith "Impossible"
      | [s] -> pure [make_label s]
      | s :: ss -> pure (make_label s :: ss)
    end;
    begin
      (* statements_block_item -> single_line_statements *)
      single_line_statements
    end;
  ]
) __n

and statements_block : G.stmt parser = fun __n -> (
  choice [
    begin
      (* statements_block -> statements_block_item (@lookahead('<LINE_TERMINATOR>') statements_block_item)* *)
      let* stmt = statements_block_item in
      let* stmts = list_of
        begin
          let* _ = look_ahead "<LINE_TERMINATOR>" in
          statements_block_item
        end
      in
      pure (stmt_of_stmts (stmt @ List.concat stmts))
    end;
    begin
      (* statements_block ->  *)
      pure (stmt_of_stmts [])
    end;
  ]
) __n

and select_case_block : G.stmt parser = fun __n -> (
  (* select_case_block -> select_statement case_block* end_select_statement *)
  let* select = token "SELECT" in
  let* _case = optional (token "CASE") in
  let* expr = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* case_blocks = list_of case_block in
  let* end_select = end_select_statement in
  pure (G.Switch (select.tok, Some (G.Cond expr), case_blocks)
        |> G.s
        |> s_range select.tok end_select.tok)
) __n

and case_block : G.case_and_body parser = fun __n -> (
  choice [
    begin
      (* case_block -> case_statement statements_block *)
      let* cases = case_statement in
      let* stmt = statements_block in
      pure (G.CasesAndBody (cases, stmt))
    end;
    begin
      (* case_block -> '... *)
      let* dot_dot_dot = token "..." in
      pure (G.CaseEllipsis dot_dot_dot.tok)
    end
  ]
) __n

and case_statement_terminator : unit parser = fun __n -> (
  choice [
    begin
      (* case_statement_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure ()
    end;
    begin
      (* case_statement_terminator -> ':' *)
      let* _ = token ":" in
      pure ()
    end;
  ]
) __n

and case_statement : G.case list parser = fun __n -> (
  (* case_statement -> 'Case' case_clause (',' case_clause)* case_statement_terminator *)
  let* case = token "CASE" in
  let* clause = case_clause case in
  let* clauses = list_of
    begin
      let* _comma = token "," in
      case_clause case
    end
  in
  let* _ = case_statement_terminator in
  pure (clause :: clauses)
) __n

and case_clause (case : T.t) : G.case parser = fun __n -> (
  choice [
    begin
      (* case_clause -> 'Else' *)
      let* else_ = token "ELSE" in
      pure (G.Default else_.tok)
    end;
    begin
      (* case_clause -> range_or_expression_case_clause *)
      range_or_expression_case_clause case
    end;
    begin
      (* case_clause -> relational_case_clause *)
      relational_case_clause
    end;
  ]
) __n

and range_or_expression_case_clause (case : T.t) : G.case parser = fun __n -> (
  (* range_or_expression_case_clause -> expression ('To' expression)? *)
  let* expr = expression in
  let* to_expr_opt = optional
    begin
      let* to_ = token "TO" in
      let* to_expr = expression in
      pure (to_, to_expr)
    end
  in
  match to_expr_opt with
  | None ->
      pure (G.CaseEqualExpr (case.tok, expr))
  | Some (to_, to_expr) ->
      let func = G.IdSpecial (G.Op G.Range, to_.tok) |> G.e in
      pure (G.OtherCase (("RANGE_CASE", to_.tok), [G.E (G.Call (func, fb [G.Arg expr; G.Arg to_expr]) |> G.e)]))
) __n

and relational_case_clause_op : (G.special * T.t) parser = fun __n -> (
  choice [
    begin
      (* relational_case_clause_op -> '=' *)
      let* t = token "=" in
      pure (G.Op G.Eq, t)
    end;
    begin
      (* relational_case_clause_op -> '>' *)
      let* t = token ">" in
      pure (G.Op G.Gt, t)
    end;
    begin
      (* relational_case_clause_op -> '>=' *)
      let* t = token ">=" in
      pure (G.Op G.GtE, t)
    end;
    begin
      (* relational_case_clause_op -> '<' *)
      let* t = token "<" in
      pure (G.Op G.Lt, t)
    end;
    begin
      (* relational_case_clause_op -> '<=' *)
      let* t = token "<=" in
      pure (G.Op G.LtE, t)
    end;
    begin
      (* relational_case_clause_op -> '<>' *)
      let* t = token "<>" in
      pure (G.Op G.NotEq, t)
    end;
  ]
) __n

and relational_case_clause : G.case parser = fun __n -> (
  (* relational_case_clause -> 'Is'? relational_case_clause_op expression *)
  let* _is = optional (token "IS") in
  let* op = relational_case_clause_op in
  let* expr = expression in
  match op with
  | (G.Op Eq, t) -> pure (G.CaseEqualExpr (t.tok, expr))
  | (op, t) -> pure (G.OtherCase (("RELATIONAL_CASE", t.tok), [G.E (G.IdSpecial (op, t.tok) |> G.e); G.E expr]))
) __n

and single_line_if_statement : G.stmt parser = fun __n -> (
  (* single_line_if_statement -> 'If' expression 'Then' @lookahead_not('<LINE_TERMINATOR>') single_line_statements (@lookahead_not('<LINE_TERMINATOR>') 'Else' single_line_statements)? *)
  let* _ = optional (token ":") in
  let* if_ = token "IF" in
  let* expr = expression in
  let* _ = token "THEN" in
  let* _ = look_ahead_not "<LINE_TERMINATOR>" in
  let* then_stmts = single_line_statements in
  let* else_stmts = optional
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* _ = token "ELSE" in
      single_line_statements
    end
  in
  pure (G.If (if_.tok,
              G.Cond expr,
              stmt_of_stmts then_stmts,
              Option.map stmt_of_stmts else_stmts) |> G.s)
) __n

and make_else_if_blocks (else_if_blocks : (Tok.t * G.expr * G.stmt) list)
    (else_block : G.stmt option) : G.stmt option =
  match else_if_blocks with
  | [] ->
      else_block
  | (t, condition, stmt) :: eibs ->
      Some (G.If (t, G.Cond condition, stmt, make_else_if_blocks eibs else_block) |> G.s)

and multi_line_if_block : G.stmt parser = fun __n -> (
  (* multi_line_if_block -> 'If' expression 'Then'? case_statement_terminator statements_block else_if_block* else_block? ':'? 'End' 'If' *)
  let* if1 = token "IF" in
  let* condition = expression in
  let* _then = optional (token "THEN") in
  let* _ = case_statement_terminator in (* ensure multi-line *)
  let* stmt = statements_block in
  let* else_if_blocks = list_of else_if_block in
  let* else_block_opt = optional else_block in
  let* _colon = optional (token ":") in
  let* _end = token "END" in
  let* if2 = token "IF" in
  let else_ = make_else_if_blocks else_if_blocks else_block_opt in
  pure (G.If (if1.tok, G.Cond condition, stmt, else_)
        |> G.s
        |> s_range if1.tok if2.tok)
) __n

and else_if_or_elseif : Tok.t parser = fun __n -> (
  choice [
    begin
      (* else_if_or_elseif -> 'ElseIf' *)
      let* t = token "ELSEIF" in
      pure t.tok
    end;
    begin
      (* else_if_or_elseif -> 'Else' @lookahead_not('<LINE_TERMINATOR>') 'If' *)
      let* else_ = token "ELSE" in
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* if_ = token "IF" in
      pure (Tok.combine_toks else_.tok [if_.tok])
    end;
  ]
) __n

and else_if_block : (Tok.t * G.expr * G.stmt) parser = fun __n -> (
  (* else_if_block -> ':'? else_if_or_elseif expression 'Then'? case_statement_terminator statements_block *)
  let* _ = optional (token ":") in
  let* else_if_or_elseif = else_if_or_elseif in
  let* condition = expression in
  let* _then = optional (token "THEN") in
  let* _ = case_statement_terminator in
  let* stmt = statements_block in
  pure (else_if_or_elseif, condition, stmt)
) __n

and else_block : G.stmt parser = fun __n -> (
  (* else_block -> ':'? 'Else' ':'? statements_block *)
  let* _ = optional (token ":") in
  let* _else = token "ELSE" in
  let* _ = optional (token ":") in
  statements_block
) __n

and after_next : unit parser = fun __n -> (
  choice [
    begin
      (* after_next -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure ()
    end;
    begin
      (* after_next -> @lookahead(':') *)
      let* _ = look_ahead ":" in
      pure ()
    end;
  ]
) __n

and for_block : G.stmt parser = fun __n -> (
  (* for_block -> 'For' for_header case_statement_terminator statements_block case_statement_terminator 'Next' (@lookahead_not('<LINE_TERMINATOR>') identifier_name)? after_next *)
  let* for_ = token "FOR" in
  let* header = for_header in
  let* _ = case_statement_terminator in
  let* stmt = statements_block in
  let* _ = case_statement_terminator in
  let* next = token "NEXT" in
  let* counter = optional
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      identifier_name
    end
  in
  let* _ = after_next in
  let end_token =
    (match counter with
    | None -> next.tok
    | Some (_, t) -> t)
  in
  pure (G.For (for_.tok, header, stmt)
        |> G.s
        |> s_range for_.tok end_token)
) __n

and for_header : G.for_header parser = fun __n -> (
  choice [
    begin
      (* for_header -> identifier_name simple_as_clause? '=' expression 'To' expression ('Step' expression)? *)
      let* id = identifier_name in
      let* _as = optional simple_as_clause in
      let* _eq = token "=" in
      let* from_ = expression in
      let* to_tok = token "TO" in
      let* to_ = expression in
      let* step_opt = optional
        begin
          let* _ = token "STEP" in
          expression
        end
      in
      match step_opt with
      | None ->
          let range =
            G.Call (G.IdSpecial (G.Op G.Range, to_tok.tok) |> G.e,
                    fb [G.Arg from_; G.Arg to_]) |> G.e
          in
          pure (G.ForEach (G.PatId (id, G.empty_id_info ~case_insensitive:true ()), to_tok.tok, range))
      | Some step ->
          let range =
            G.Call (G.IdSpecial (G.Op G.Range, to_tok.tok) |> G.e,
                    fb [G.Arg from_; G.Arg to_; G.Arg step]) |> G.e
          in
          pure (G.ForEach (G.PatId (id, G.empty_id_info ~case_insensitive:true ()), to_tok.tok, range))
    end;
    begin
      (* for_header -> 'Each' identifier_name simple_as_clause? 'In' expression *)
      let* _each = token "EACH" in
      let* id = identifier_name in
      let* _as = optional simple_as_clause in
      let* in_ = token "IN" in
      let* expr = expression in
      pure (G.ForEach (G.PatId (id, G.empty_id_info ~case_insensitive:true ()), in_.tok, expr))
    end;
  ]
) __n

and do_block : G.stmt parser = fun __n -> (
  (* do_block -> 'Do' do_header? case_statement_terminator statements_block case_statement_terminator 'Loop' (@lookahead_not('<LINE_TERMINATOR>') do_header)? *)
  let* do_ = token "DO" in
  let* header_opt = optional do_header in
  let* _ = case_statement_terminator in
  let* stmt = statements_block in
  let* _ = case_statement_terminator in
  let* loop = token "LOOP" in
  let* footer_opt = optional
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      do_header
    end
  in
  match header_opt, footer_opt with
  | Some e, _ ->
      pure (G.While (do_.tok, G.Cond e, stmt)
            |> G.s
            |> s_range do_.tok loop.tok)
  | _, Some e ->
      pure (G.DoWhile (do_.tok, stmt, e) |> G.s)
  | None, None ->
      (* Should not happen *)
      pure (G.Block (do_.tok, [stmt], loop.tok) |> G.s)
) __n

and do_header : G.expr parser = fun __n -> (
  choice [
    begin
      (* do_header -> 'While' expression *)
      let* _while = token "WHILE" in
      expression
    end;
    begin
      (* do_header -> 'Until' expression *)
      let* until = token "UNTIL" in
      let* expr = expression in
      pure (G.Call (G.IdSpecial (G.Op G.Not, until.tok) |> G.e, fb [G.Arg expr]) |> G.e)
    end;
  ]
) __n

and while_block : G.stmt parser = fun __n -> (
  (* while_block -> 'While' expression case_statement_terminator statements_block case_statement_terminator ':'? 'End' 'While' *)
  let* while1 = token "WHILE" in
  let* expr = expression in
  let* _ = case_statement_terminator in
  let* stmt = statements_block in
  let* _ = case_statement_terminator in
  let* _ = token "END" in
  let* while2 = token "WHILE" in
  pure (G.While (while1.tok, G.Cond expr, stmt)
        |> G.s
        |> s_range while1.tok while2.tok)
) __n

and soft_terminator : unit parser = fun __n -> (
  let* _ = choice [
    begin
      (* soft_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      look_ahead "<LINE_TERMINATOR>"
    end;
    begin
      (* soft_terminator -> @lookahead(':') *)
      look_ahead ":"
    end;
  ]
  in
  pure ()
) __n

and try_block : G.stmt parser = fun __n -> (
  (* try_block -> 'Try' statements_block catch_block* finally_block? ':'? 'End' 'Try' *)
  let* _ = optional (token ":") in
  let* try1 = token "TRY" in
  let* stmt = statements_block in
  let* catch_blocks = list_of catch_block in
  let* finally = optional finally_block in
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* try2 = token "TRY" in
  pure (G.Try (try1.tok, stmt, catch_blocks, None, finally)
        |> G.s
        |> s_range try1.tok try2.tok)
) __n

and catch_block : G.catch parser = fun __n -> (
  (* catch_block -> 'Catch' identifier_name? simple_as_clause? catch_filter_clause? soft_terminator statements_block *)
  let* _ = optional (token ":") in
  let* catch_ = token "CATCH" in
  let* id_opt = optional identifier_name in
  let* typ_opt = optional simple_as_clause in
  let* filter_opt = optional catch_filter_clause in
  let* _ = soft_terminator in
  let* stmt = statements_block in
  let base_pat =
    match id_opt with
    | None -> G.PatWildcard catch_.tok
    | Some id -> G.PatId (id, G.empty_id_info ~case_insensitive:true ())
  in
  let typed_pat =
    match typ_opt with
    | None -> base_pat
    | Some typ -> G.PatTyped (base_pat, typ)
  in
  let filter_pat =
    match filter_opt with
    | None -> typed_pat
    | Some expr -> G.PatWhen (typed_pat, expr)
  in
    pure (catch_.tok, G.CatchPattern filter_pat, stmt)
) __n

and simple_as_clause : G.type_ parser = fun __n -> (
  (* simple_as_clause -> 'As' attribute_list* type *)
  let* _as = token "AS" in
  let* attrs = list_of attribute_list in
  let* typ = type_ in
  pure (add_attrs_to_type typ (List.concat attrs))
) __n

and catch_filter_clause : G.expr parser = fun __n -> (
  (* catch_filter_clause -> 'When' expression *)
  let* _ = optional (token ":") in
  let* _when = token "WHEN" in
   expression
) __n

and finally_block : G.finally parser = fun __n -> (
  (* finally_block -> 'Finally' statements_block *)
  let* finally = token "FINALLY" in
  let* stmt = statements_block in
  pure (finally.tok, stmt)
) __n

(* NOTE: we can use the name "with", because it's a reserved keyword *)
and make_with_block_var (_ : unit) : G.name =
  G.Id (("with", Tok.unsafe_fake_tok "with"),
        G.empty_id_info ~case_insensitive:true ~hidden:true ())

and with_block : G.stmt parser = fun __n -> (
  (* with_block -> 'With' expression @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'With' *)
  let* with1 = token "WITH" in
  let* expr = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* stmt = statements_block in
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* with2 = token "WITH" in
  let entity =
    G.{ name = G.EN (make_with_block_var ());
        attrs = [];
        tparams = None
      }
  in
  let def =
    G.{ vinit = Some expr;
        vtype = None;
        vtok = None
      }
  in
  let varDef = G.DefStmt (entity, G.VarDef def) |> G.s in
  pure (G.Block (fb [varDef; stmt])
        |> G.s
        |> s_range with1.tok with2.tok)
) __n

and sync_lock_block : G.stmt parser = fun __n -> (
  (* sync_lock_block -> 'SyncLock' expression @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'SyncLock' *)
  let* sync_lock1 = token "SYNCLOCK" in
  let* expr = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* stmt = statements_block in
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* sync_lock2 = token "SYNCLOCK" in
  pure (G.OtherStmtWithStmt
          (G.OSWS_With, [G.Tk sync_lock1.tok; G.E expr], stmt)
        |> G.s
           |> s_range sync_lock1.tok sync_lock2.tok)
) __n

and using_block : G.stmt parser = fun __n -> (
  (* using_block -> 'Using' using_header @lookahead('<LINE_TERMINATOR>') statements_block end_using_statement *)
  let* using = token "USING" in
  let* header = using_header in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* stmt = statements_block in
  let* end_using = end_using_statement in
  pure (G.WithUsingResource (using.tok, header, stmt)
        |> G.s
        |> s_range using.tok end_using.tok)
) __n

and using_header : G.stmt list parser = fun __n -> (
  (* using_header -> using_header_item (',' using_header_item)* *)
  let* item = using_header_item in
  let* items = list_of
    begin
      let* _comma = token "," in
      using_header_item
    end
  in
  pure (item @ List.concat items)
) __n

and using_header_item : G.stmt list parser = fun __n -> (
  choice [
    begin
      (* using_header_item -> 'New'? variable_declarator *)
      let* _new_opt1 = optional (token "NEW") in
      variable_declarator []
    end;
    begin
      (* using_header_item -> access_expression *)
      let* expr = access_expression in
      pure [G.ExprStmt (expr, Tok.unsafe_sc) |> G.s]
    end;
  ]
) __n

and erase_statement : G.stmt parser = fun __n -> (
  (* erase_statement -> 'Erase' expression (',' expression)* *)
  (* NOTE: like "free" in C, so making it a function. *)
  let* erase = token "ERASE" in
  let* expr = expression in
  let* exprs = list_of
    begin
      let* _comma1 = token "," in
      expression
    end
  in
  let func = G.N (make_name (ident_of_token erase) None) |> G.e in
  let call = G.Call (func, fb (List.map (fun x -> G.Arg x) (expr :: exprs))) |> G.e in
  pure (G.ExprStmt (call, Tok.unsafe_sc) |> G.s)
) __n

and error_statement : G.stmt parser = fun __n -> (
  (* error_statement -> 'Error' expression *)
  let* error = token "ERROR" in
  let* expr = expression in
  pure (G.RawStmt (RT.Tuple
    [raw_token error; RT.Any (G.E expr)]) |> G.s)
) __n

and continue_statement : G.stmt parser = fun __n -> (
  (* continue_statement -> 'Continue' continue_what *)
  let* _ = optional (token ":") in
  let* cont = token "CONTINUE" in
  let* _ = choice [ token "DO"; token "FOR"; token "WHILE" ] in
  pure (G.Continue (cont.tok, G.LNone, Tok.unsafe_fake_tok "") |> G.s)
) __n

and call_statement : G.stmt parser = fun __n -> (
  (* call_statement -> 'Call' expression *)
  let* _call = token "CALL" in
  let* expr = expression in
  match expr with
  | { G.e = G.Call _; _} -> pure (G.ExprStmt (expr, Tok.unsafe_sc) |> G.s)
  | _ -> pure (G.ExprStmt (G.Call (expr, fb []) |> G.e, Tok.unsafe_sc) |> G.s)
) __n

and on_error_statement : G.stmt parser = fun __n -> (
  let* _ = optional (token ":") in
  let* on = token "ON" in
  let* error = token "ERROR" in
  choice [
    begin
      let* goto = token "GOTO" in
      let* t = choice [ token "<IDENT>"; token "<INT>" ] in
      pure (G.RawStmt (RT.Tuple
        [raw_token on; raw_token error; raw_token goto; raw_token t]) |> G.s)
    end;
    begin
      let* resume = token "RESUME" in
      let* next = token "NEXT" in
      pure (G.RawStmt (RT.Tuple
        [raw_token on; raw_token error; raw_token resume; raw_token next]) |> G.s)
    end;
  ]
) __n

and raise_event_statement : G.stmt parser = fun __n -> (
  (* raise_event_statement -> 'RaiseEvent' access_expression *)
  let* t = token "RAISEEVENT" in
  let* expr = access_expression in
  let func = G.N (make_name (ident_of_token t) None) |> G.e in
  let call = G.Call (func, fb [G.Arg expr]) |> G.e in
  pure (G.ExprStmt (call, Tok.unsafe_sc) |> G.s)
) __n

and resume_statement : G.stmt parser = fun __n -> (
  let* _ = optional (token ":") in
  let* resume = token "RESUME" in
  let* label = optional (choice [ token "<IDENT>"; token "<INT>" ]) in
  pure (G.RawStmt (RT.Tuple [RT.Token (resume.content, resume.tok);
    RT.Option (Option.map raw_token label)]) |> G.s)
) __n

and return_terminator : unit parser = fun __n -> (
  choice [
    begin
      (* return_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure ()
    end;
    begin
      (* return_terminator -> @lookahead(',') *)
      let* _ = look_ahead "," in
      pure ()
    end;
    begin
      (* return_terminator -> @lookahead(')') *)
      let* _ = look_ahead ")" in
      pure ()
    end;
    begin
      (* return_terminator -> @lookahead('}') *)
      let* _ = look_ahead "}" in
      pure ()
    end;
    begin
      (* return_terminator -> @lookahead('End') *)
      let* _ = look_ahead "END" in
      pure ()
    end;
    begin
      (* return_terminator -> @lookahead(':') *)
      let* _ = look_ahead ":" in
      pure ()
    end;
  ]
) __n

and return_statement : G.stmt parser = fun __n -> (
  let* _colon = optional (token ":") in
  let* return = token "RETURN" in
  choice [
    begin
      (* return_statement -> ':'? 'Return' return_terminator *)
      let* _ = return_terminator in
      pure (G.Return (return.tok, None, Tok.unsafe_fake_tok "") |> G.s)
    end;
    begin
      (* return_statement -> ':'? 'Return' @lookahead_not('<LINE_TERMINATOR>') expression *)
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* expr = expression in
      pure (G.Return (return.tok, Some expr, Tok.unsafe_fake_tok "") |> G.s)
    end;
  ]
) __n

and stop_or_end_statement : G.stmt parser = fun __n -> (
  (* stop_or_end_statement -> ':'? ('End' | 'Stop') @lookahead('<LINE_TERMINATOR>') *)
  let* _ = optional (token ":") in
  let* t = choice [token "END"; token "STOP"] in
  let* _ = return_terminator in
  let func = G.N (make_name (ident_of_token t) None) |> G.e in
  let call = G.Call (func, fb []) |> G.e in
  pure (G.ExprStmt (call, Tok.unsafe_sc) |> G.s)
) __n

and yield_statement : G.stmt parser = fun __n -> (
  (* yield_statement -> 'Yield' expression *)
  let* _ = optional (token ":") in
  let* yield = token "YIELD" in
  let* expr = expression in
  (* TODO: What does the 3rd arg of G.Yield do? *)
  let e = G.Yield (yield.tok, Some expr, false) |> G.e in
  pure (G.ExprStmt (e, Tok.unsafe_fake_tok "") |> G.s)
) __n

and throw_statement : G.stmt parser = fun __n -> (
  (* throw_statement -> 'Throw' expression? *)
  let* _ = optional (token ":") in
  let* throw = token "THROW" in
  let* expr_opt = optional expression in
  match expr_opt with
  | Some expr ->
      pure (G.Throw (throw.tok, expr, Tok.unsafe_fake_tok "") |> G.s)
  | None ->
      (* Syntax for re-throwing eceptions only *)
      let expr = G.L (G.Undefined throw.tok) |> G.e in
      pure (G.Throw (throw.tok, expr, Tok.unsafe_fake_tok "") |> G.s)
) __n

and toplevel_declaration : G.stmt parser = fun __n -> (
  (* toplevel_declaration -> attribute_list* modifier* toplevel_kw_declaration *)
  let* attrs = list_of attribute_list in
  let* modifiers = list_of modifier in
  let attrs = modifiers @ List.concat attrs in
  toplevel_kw_declaration attrs
) __n

and toplevel_kw_declaration (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  choice [
    begin
      (* toplevel_kw_declaration -> namespace_block *)
      namespace_block
    end;
    begin
      (* toplevel_kw_declaration -> class_block *)
      class_block attrs
    end;
    begin
      (* toplevel_kw_declaration -> interface_block *)
      interface_block attrs
    end;
    begin
      (* toplevel_kw_declaration -> module_block *)
      module_block attrs
    end;
    begin
      (* toplevel_kw_declaration -> structure_block *)
      structure_block attrs
    end;
    begin
      (* toplevel_kw_declaration -> enum_block *)
      enum_block attrs
    end;
    begin
      (* toplevel_kw_declaration -> function_block *)
      function_block G.Function attrs
    end;
    begin
      (* toplevel_kw_declaration -> sub_block *)
      sub_block G.Function attrs
    end;
    begin
      (* toplevel_kw_declaration -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.ExprStmt (G.Ellipsis dot_dot_dot.tok |> G.e, Tok.unsafe_sc) |> G.s)
    end;
    begin
      (* toplevel_kw_declaration -> '<...' expression '...>' *)
      let* left = token "<..." in
      let* expr = expression in
      let* right = token "...>" in
      pure (G.ExprStmt (G.DeepEllipsis (left.tok, expr, right.tok) |> G.e, Tok.unsafe_sc) |> G.s)
    end;
  ]
) __n

and end_add_handler_statement : unit parser = fun __n -> (
  (* end_add_handler_statement -> ':'? 'End' 'AddHandler' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "ADDHANDLER" in
  pure ()
) __n

and end_class_statement : unit parser = fun __n -> (
  choice [
    begin
      (* end_class_statement -> ':'? 'End' 'Class' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "CLASS" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_structure_statement : unit parser = fun __n -> (
  choice [
    begin
      (* end_class_statement -> ':'? 'End' 'Class' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "STRUCTURE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_enum_statement : unit parser = fun __n -> (
  (* end_enum_statement -> ':'? 'End' 'Enum' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "ENUM" in
  pure ()
) __n

and end_event_statement : unit parser = fun __n -> (
  (* end_event_statement -> ':'? 'End' 'Event' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "EVENT" in
  pure ()
) __n

and end_function_statement : unit parser = fun __n -> (
  (* end_function_statement -> ':'? 'End' 'Function' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "FUNCTION" in
  pure ()
) __n

and end_interface_statement : unit parser = fun __n -> (
  choice [
    begin
      (* end_interface_statement -> ':'? 'End' 'Interface' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "INTERFACE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_module_statement : unit parser = fun __n -> (
  choice [
    begin
      (* end_module_statement -> ':'? 'End' 'Module' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "MODULE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_namespace_statement : unit parser = fun __n -> (
  choice [
    begin
      (* end_namespace_statement -> ':'? 'End' 'Namespace' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "NAMESPACE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_operator_statement : unit parser = fun __n -> (
  (* end_operator_statement -> ':'? 'End' 'Operator' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "OPERATOR" in
  pure ()
) __n

and end_property_statement : unit parser = fun __n -> (
  (* end_property_statement -> ':'? 'End' 'Property' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "PROPERTY" in
  pure ()
) __n

and end_raise_event_statement : unit parser = fun __n -> (
  (* end_raise_event_statement -> ':'? 'End' 'RaiseEvent' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "RAISEEVENT" in
  pure ()
) __n

and end_remove_handler_statement : unit parser = fun __n -> (
  (* end_remove_handler_statement -> ':'? 'End' 'RemoveHandler' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "REMOVEHANDLER" in
  pure ()
) __n

and end_select_statement : T.t parser = fun __n -> (
  (* end_select_statement -> ':'? 'End' 'Select' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  token "SELECT"
) __n

and end_sub_statement : unit parser = fun __n -> (
  (* end_sub_statement -> ':'? 'End' 'Sub' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  let* _ = token "SUB" in
  pure ()
) __n

and end_using_statement : T.t parser = fun __n -> (
  (* end_using_statement -> ':'? 'End' 'Using' *)
  let* _ = optional (token ":") in
  let* _ = token "END" in
  token "USING"
) __n

and enum_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* enum_block -> 'Enum' identifier_token as_clause? enum_block_item* end_enum_statement *)
  let* _enum = token "ENUM" in
  let* qname = qualified_name in
  let* _typ = optional as_clause in (* TODO: nowhere to put this in the AST *)
  let* items = list_of enum_block_item in
  let* _ = end_enum_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs;
        tparams = None }
  in
  let def =
    G.TypeDef
      G.{ tbody = G.OrType items }
  in
    pure (G.DefStmt (entity, def) |> G.s)
) __n

and enum_block_item : G.or_type_element parser = fun __n -> (
  (* enum_block_item -> attribute_list* single_line_statement ','? @lookahead('<LINE_TERMINATOR>') *)
  let* _attrs = list_of attribute_list in (* TODO: nowhere to put these in the AST *)
  let* id = identifier_name in
  let* init = optional
    begin
      let* _ = token "=" in
      expression
    end
  in
  let* _ = optional (token ",") in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  pure (G.OrEnum (id, init))
) __n

and as_clause : (G.expr, G.type_) Either.t parser = fun __n -> (
  choice [
    begin
      (* as_clause -> as_new_clause *)
      let* expr = as_new_clause in
      pure (Either.Left expr)
    end;
    begin
      (* as_clause -> simple_as_clause *)
      let* typ = simple_as_clause in
      pure (Either.Right typ)
    end;
  ]
) __n

and as_new_clause : G.expr parser = fun __n -> (
  (* as_new_clause -> 'As' new_expression *)
  let* _as = token "AS" in
  new_expression
) __n

and new_expression : G.expr parser = fun __n -> (
  (* TODO: merge these two to avoid backtracking *)
  choice [
    begin
      (* new_expression -> new_object_expression *)
      new_object_expression
    end;
    begin
      (* new_expression -> array_creation_expression *)
      array_creation_expression
    end;
  ]
) __n

and new_object_expression : G.expr parser = fun __n -> (
  let* new_ = token "NEW" in
  let* _attrs = list_of attribute_list in (* attributes here seem syntactically illegal *)
  let* typ_and_args = optional
    begin
      let* typ = type_ in
      let* args = optional argument_list in
      pure (typ, args)
    end
  in
  let* object_initializer = optional object_member_initializer in
  match typ_and_args, object_initializer with
  | _, Some (with_, cbody) ->
      let class_def =
        G.{ ckind = (G.Class, with_.tok);
            cextends = [];
            cimplements = [];
            cmixins = [];
            cparams = fb [];
            cbody }
      in
      let anon_class_val =
        G.AnonClass class_def |> G.e
      in
      (match typ_and_args with
       | None ->
           pure anon_class_val
       | Some (typ, _constructor_args) ->
           (* NOTE: it is syntactically invalid to have constructor arguments here,
            * so we simply ignore them. *)
           (* NOTE: in the AST there is no way to express the initializing constructor,
            * so we have to do with passing the created object as an argument. *)
           pure (G.New (new_.tok, typ, G.empty_id_info ~case_insensitive:true (), fb [G.Arg anon_class_val]) |> G.e))
  | Some (typ, Some args), None ->
      pure (G.New (new_.tok, typ, G.empty_id_info(), args) |> G.e)
  | Some (typ, None), None ->
      pure (G.New (new_.tok, typ, G.empty_id_info(), fb []) |> G.e)
  | None, None ->
      (* This should not happen in a syntactically correct file *)
      pure (G.L (G.Null new_.tok) |> G.e)
) __n

and object_member_initializer : (T.t * G.field list G.bracket) parser = fun __n -> (
  (* object_member_initializer -> 'With' '{' (field_initializer (',' field_initializer)* )? '}' *)
  let* with_ = token "WITH" in
  let* lbrace = token "{" in
  let* fields_opt = optional
    begin
      let* field = field_initializer in
      let* fields = list_of
        begin
          let* _comma1 = token "," in
          field_initializer
        end
      in
      pure (field :: fields)
    end
  in
  let* rbrace = token "}" in
  let fields =
    Option.to_list fields_opt
    |> List.concat
    |> List.mapi (|>)
  in
  pure (with_, (lbrace.tok, fields, rbrace.tok))
) __n

and field_initializer : (int -> G.field) parser = fun __n -> (
  (* field_initializer -> 'Key'? ('.' identifier_name '=')? expression *)
  let* key_opt = optional (token "KEY") in
  let* id_opt = optional
    begin
      let* _dot = token "." in
      let* id = identifier_name in
      let* _eq = token "=" in
      pure id
    end
  in
  let* expr = expression in
  let attrs =
    match key_opt with
    | None -> []
    | Some key -> [G.NamedAttr (key.tok, make_name (ident_of_token key) None, fb [])]
  in
  pure (fun i ->
    let id =
      match id_opt with
      | Some id -> id
      | None -> let prop = "Property" ^ string_of_int (i + 1) in
                (prop, Tok.unsafe_fake_tok prop)
    in
    let entity = G.{ name = EN (make_name id None); attrs; tparams = None } in
    let defn = G.VarDef G.{ vinit = Some expr; vtype = None; vtok = None } in
    G.F (G.DefStmt (entity, defn) |> G.s)
)) __n

and array_creation_expression : G.expr parser = fun __n -> (
  (* array_creation_expression -> 'New' attribute_list* type argument_list? array_rank_specifier* collection_initializer *)
  let* new_ = token "NEW" in
  let* attrs = list_of attribute_list in (* this seems syntactically illegal *)
  let* typ = type_ in
  let typ = add_attrs_to_type typ (List.concat attrs) in
  let* _args = optional argument_list in
  let* _array_rank = list_of array_rank_specifier in
  let* init = collection_initializer in
  (* TODO: add dimensions to the type *)
  pure (G.New (new_.tok, typ, G.empty_id_info ~case_insensitive:true (), fb [G.Arg init]) |> G.e)
) __n

and array_rank_specifier : int G.bracket parser = fun __n -> (
  (* array_rank_specifier -> '(' ','* ')' *)
  let* lparen = token "(" in
  let* commas = list_of (token ",") in
  let* rparen = token ")" in
  pure (lparen.tok, List.length commas, rparen.tok)
) __n

and collection_initializer : G.expr parser = fun __n -> (
  (* collection_initializer -> '{' (expression (',' expression)* )? '}' *)
  let* lbrace = token "{" in
  let* exps_opt = optional
    begin
      let* exp = expression in
      let* exps = list_of
        begin
          let* _comma1 = token "," in
          expression
        end
      in
      pure (exp :: exps)
    end
  in
  let* rbrace = token "}" in
  let exps = List.concat (Option.to_list exps_opt) in
  pure (G.Container (G.Array, (lbrace.tok, exps, rbrace.tok)) |> G.e)
) __n

(* TODO: what are those? *)
(*
and object_creation_expression : G.any parser = fun __n -> (
  (* object_creation_expression -> 'New' attribute_list* type argument_list? object_creation_initializer? *)
  let* new_ = token "NEW" in
  let* attribute_lists1 = list_of attribute_list in
  let* _type_1 = type_ in
  let* argument_list_opt1 = optional argument_list in
  let* object_creation_initializer_opt1 = optional object_creation_initializer in
  pure (xRule "object_creation_expression" 0 [xToken(new1); xList(attribute_lists1); xOptional(argument_list_opt1); xOptional(object_creation_initializer_opt1)])
) __n

and object_creation_initializer : G.any parser = fun __n -> (
  choice [
    begin
      (* object_creation_initializer -> object_collection_initializer *)
      let* object_collection_initializer1 = object_collection_initializer in
      pure (xRule "object_creation_initializer" 0 [object_collection_initializer1])
    end;
    begin
      (* object_creation_initializer -> object_member_initializer *)
      let* object_member_initializer1 = object_member_initializer in
      pure (xRule "object_creation_initializer" 1 [object_member_initializer1])
    end;
  ]
) __n

and object_collection_initializer : G.any parser = fun __n -> (
  (* object_collection_initializer -> 'From' collection_initializer *)
  let* from1 = token "FROM" in
  let* collection_initializer1 = collection_initializer in
  pure (xRule "object_collection_initializer" 0 [xToken(from1); collection_initializer1])
) __n
*)

and equals_value : G.expr parser = fun __n -> (
  (* equals_value -> '=' expression *)
  let* _eq = token "=" in
  expression
) __n

and event_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* event_block -> event_statement (event_accessor_elem* end_event_statement)? *)
  let* id, stmt = event_statement attrs in
  let* elems = optional
    begin
      let* elems = list_of (event_accessor_elem id) in
      let* _ = end_event_statement in
      pure elems
    end
  in
  let stmt =
    G.DefStmt stmt |> G.s
  in
  match elems with
  | None | Some [] -> pure stmt
  | Some xs -> pure (G.Block (fb (stmt :: xs)) |> G.s)
) __n

and event_statement (attrs : G.attribute list) : (G.ident * G.definition) parser = fun __n -> (
  (* event_statement -> 'Custom'? 'Event' identifier_token parameter_list? simple_as_clause? implements_clause? *)
  let* _ = optional (token ":") in
  let* _ = optional (token "CUSTOM") in
  let* event = token "EVENT" in
  let event_attr = G.NamedAttr (event.tok, make_name (ident_of_token event) None, fb []) in
  let* id = identifier_name in
  let* params = optional parameter_list in
  let* as_clause = optional simple_as_clause in
  (* TODO: implements *)
  let* _implements = optional implements_clause in
  let entity =
    G.{ name = G.EN (make_name id None);
        attrs = event_attr :: attrs;
        tparams = None }
  in
  let def =
    G.FuncDef
      { fkind = (G.Function, event.tok);
        fparams =
          (match params with
          | None -> fb []
          | Some ps -> ps);
        frettype = as_clause;
        fbody = G.FBDecl Tok.unsafe_sc
      }
  in
  pure (id, (entity, def))
) __n

and event_accessor_elem (id : G.ident) : G.stmt parser = fun __n -> (
  (* event_accessor_elem -> attribute_list* event_accessor_block *)
  let* attrs = list_of attribute_list in
  event_accessor_block id (List.concat attrs)
) __n

and event_accessor_block ((event_name_str, _) : G.ident) (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  let* _ = optional (token ":") in
  let* tok, params, stmt = choice
    [
      begin
        (* event_accessor_block -> 'AddHandler' parameter_list statements_block end_add_handler_statement *)
        let* t = token "ADDHANDLER" in
        let* params = parameter_list in
        let* stmt = statements_block in
        let* _ = end_add_handler_statement in
        pure (t, params, stmt)
      end;
      begin
        (* event_accessor_block -> 'RaiseEvent' parameter_list statements_block end_raise_event_statement *)
        let* t = token "RAISEEVENT" in
        let* params = parameter_list in
        let* stmt = statements_block in
        let* _ = end_raise_event_statement in
        pure (t, params, stmt)
      end;
      begin
        (* event_accessor_block -> 'RemoveHandler' parameter_list statements_block end_remove_handler_statement *)
        let* t = token "REMOVEHANDLER" in
        let* params = parameter_list in
        let* stmt = statements_block in
        let* _ = end_remove_handler_statement in
        pure (t, params, stmt)
      end;
    ]
  in
  let entity =
    G.{ name = G.EN (make_name (event_name_str ^ "_" ^ tok.content, tok.tok) None);
        attrs = attrs;
        tparams = None
      }
  in
  let def =
    G.FuncDef
      { fkind = (G.Function, tok.tok);
        fparams = params;
        frettype = None;
        fbody = G.FBStmt stmt
      }
  in
  pure (G.DefStmt (entity, def) |> G.s)

) __n

and parameter_list : G.parameters parser = fun __n -> (
  (* parameter_list -> '(' (parameter (',' parameter)* )? ')' *)
  let* lparen = token "(" in
  let* params_opt = optional
    begin
      let* param = parameter in
      let* params = list_of
        begin
          let* _comma = token "," in
          parameter
        end
      in
      pure (param :: params)
    end
  in
  let* rparen = token ")" in
  match params_opt with
  | None -> pure (lparen.tok, [], rparen.tok)
  | Some params -> pure (lparen.tok, params, rparen.tok)
) __n

and parameter : G.parameter parser = fun __n -> (
  choice [
    begin
      (* parameter -> attribute_list* modifier* modified_identifier simple_as_clause? equals_value? *)
      let* attrs = list_of attribute_list in
      let* modifiers = list_of modifier in
      let pattrs = modifiers @ List.concat attrs in
      let* name = identifier_name in
      let* _ = optional (token "?") in
      (* TODO *)
      let* _ = optional
        begin
          let* _ = token "(" in
          let* _ = list_of (token ",") in
          let* _ = token ")" in
          pure ()
        end
      in
      let* ptype = optional simple_as_clause in
      let* pdefault = optional equals_value in
      let p =
        G.{ pname = Some name;
            ptype;
            pdefault;
            pattrs;
            pinfo = G.empty_id_info ~case_insensitive:true () }
      in
      pure (G.Param p)
    end;
    begin
      (* parameter -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.ParamEllipsis dot_dot_dot.tok)
    end;
    (* TODO: Do we want this? *)
    (*
    begin
      (* parameter -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "parameter" 2 [xToken(lt_dot_dot_dot1); G.E expression1; xToken(dot_dot_dot_gt1)])
    end;
    *)
  ]
) __n

(* TODO *)
and modified_identifier : G.any parser = fun __n -> (
  (* modified_identifier -> identifier_token '?'? argument_list? array_rank_specifier* *)
  let* identifier_token1 = identifier_token in
  let* qmark_opt1 = optional (token "?") in
  let* _argument_list_opt1 = optional argument_list in
  let* _array_rank_specifiers1 = list_of array_rank_specifier in
  pure (xRule "modified_identifier" 0 [identifier_token1; xOptional(Option.map (fun x -> xToken x) qmark_opt1)])
) __n

and implements_clause : G.any parser = fun __n -> (
  (* implements_clause -> ':'? 'Implements' type_list_elem (',' type_list_elem)* *)
  let* colon_opt1 = optional (token ":") in
  let* implements1 = token "IMPLEMENTS" in
  let* _type_list_elem1 = type_list_elem in
  let* comma_type_list_elems1 = list_of 
    begin
      let* comma1 = token "," in
      let* _type_list_elem1 = type_list_elem in
      pure (xGroup([xToken(comma1)]))
    end
  in
  pure (xRule "implements_clause" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(implements1); xList(comma_type_list_elems1)])
) __n

and type_list_elem : G.type_argument parser = fun __n -> (
  choice [
    begin
      (* type_list_elem -> type *)
      let* typ = type_ in
      pure (G.TA typ)
    end;
    (* TODO: do we need these?*)
    (*
    begin
      (* type_list_elem -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "type_list_elem" 1 [xToken(dot_dot_dot1)])
    end;
    begin
      (* type_list_elem -> '<...' type '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* type_1 = type_ in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "type_list_elem" 2 [xToken(lt_dot_dot_dot1); type_1; xToken(dot_dot_dot_gt1)])
    end;
    *)
  ]
) __n

and qualified_name : G.name parser = fun __n -> (
  (* qualified_name -> simple_name ('.' identifier_or_keyword)* *)
  let* name = simple_name in
  let* names = list_of
    begin
      let* _dot = token "." in
      identifier_or_keyword
    end
  in
  pure (collapse_names (name :: names))
) __n

and simple_name : G.name parser = fun __n -> (
  let* id = identifier_name in
  let* type_args = optional type_argument_list in
  pure (make_name id type_args)
) __n

and type_argument_list : G.type_arguments parser = fun __n -> (
  (* type_argument_list -> '(' 'Of' type_list_elem? (',' type_list_elem?)* ')' *)
  let* lparen = token "(" in
  let* _of1 = token "OF" in
  let* elem = optional type_list_elem in
  let* elems = list_of
    begin
      let* _comma1 = token "," in
      optional type_list_elem
    end
  in
  let* rparen = token ")" in
  let args = List.filter_map (fun x -> x) (elem :: elems) in
  pure (lparen.tok, args, rparen.tok)
) __n

and field_declaration (attrs : G.attribute list) : G.field list parser = fun __n -> (
  (* field_declaration -> attribute_list* modifier+ variable_declarator (',' variable_declarator)* *)
  let* decl = variable_declarator attrs in
  let* decls = list_of
    begin
      let* _comma1 = token "," in
      variable_declarator attrs
    end
  in
  pure (List.map (fun x -> G.F x) (decl @ List.concat decls))
) __n

and variable_declarator_identifier_dimension : (G.expr option * G.expr) parser = fun __n -> (
  let* opt = optional
    begin
      let* e1 = expression in
      let* e2_opt = optional (let* _ = token "TO" in expression) in
      pure (e1, e2_opt)
    end
  in
  match opt with
  | None -> pure (None, G.L (G.Undefined (Tok.unsafe_fake_tok "-1")) |> G.e)
  | Some (e, None) -> pure (None, e)
  | Some (e1, Some e2) -> pure (Some e1, e2)
) __n

and variable_declarator_identifier : (G.name * int) parser = fun __n -> (
  let* id = identifier_name in
  let* dims_opt = optional
    begin
      let* _lparen = token "(" in
      let* dim = variable_declarator_identifier_dimension in
      let* dims = list_of
        begin
          let* _comma = token "," in
          variable_declarator_identifier_dimension
        end
      in
      let* _rparen = token ")" in
      pure (List.length (dim :: dims))
    end
  in
  match dims_opt with
  | None -> pure (make_name id None, 0)
  | Some n -> pure (make_name id None, n)
) __n

and make_variable_declaration (attrs : G.attribute list) (as_clause : (G.expr, G.type_) Either.t option)
    (equals_value : G.expr option) ((var, arr) : G.name * int) : G.stmt =
  let rec add_array_type (typ : G.type_) (n : int) =
    match n with
    | 0 -> typ
    | _ ->
        let typ' =
          G.{ t = TyArray (fb None, typ);
              t_attrs = [] }
        in
        add_array_type typ' (n - 1)
  in
  let entity =
    G.{ name = G.EN var;
        attrs;
        tparams = None}
  in
  let def =
    match as_clause, equals_value with
    | _, Some expr
    | Some (Either.Left expr), _ ->
        G.{ vinit = Some expr;
            vtype = None;
            vtok = None}
    | Some (Either.Right typ), _ ->
        G.{ vinit = None;
            vtype = Some (add_array_type typ arr);
            vtok = None}
    | _ ->
        G.{ vinit = None;
            vtype = None;
            vtok = None}
  in
  G.DefStmt (entity, G.VarDef def) |> G.s

and variable_declarator (attrs : G.attribute list) : G.stmt list parser = fun __n -> (
  (* variable_declarator -> modified_identifier (',' modified_identifier)* as_clause? equals_value? *)
  match attrs with
  | [] ->
      (* At least one attr such as "Dim" or "Const" is needed *)
      fail
  | _ ->
      let* id = variable_declarator_identifier in
      let* ids = list_of
        begin
          let* _comma = token "," in
          variable_declarator_identifier
        end
      in
      let* as_clause = optional as_clause in
      let* equals_value = optional equals_value in
      pure (List.map (make_variable_declaration attrs as_clause equals_value) (id :: ids))
) __n

and implements_statement : G.type_ list parser = fun __n -> (
  (* implements_statement -> ':'? 'Implements' type (',' type)* *)
  let* _ = optional (token ":") in
  let* _implements = token "IMPLEMENTS" in
  let* typ = type_ in
  let* typs = list_of
    begin
      let* _ = token "," in
      type_
    end
  in
  pure (typ :: typs)
) __n

and inherits_statement : G.class_parent list parser = fun __n -> (
  (* inherits_statement -> ':'? 'Inherits' type (',' type)* *)
  let* _ = optional (token ":") in
  let* _inherits = token "INHERITS" in
  let* typ = type_ in
  let typ = (typ, None) in
  let* typs = list_of
    begin
      let* _ = token "," in
      let* typ = type_ in
      pure (typ, None)
    end
  in
  pure (typ :: typs)
) __n

and declare_statement (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* declare_statement -> 'Declare' text_encoding? function_or_sub identifier_token 'Lib' literal_expression ('Alias' literal_expression)? parameter_list simple_as_clause? *)
  let* declare = token "DECLARE" in
  let* encoding_attr_opt = optional
    begin
      let* t = choice [ token "ANSI"; token "UNICODE"; token "AUTO" ] in
      pure (G.NamedAttr (t.tok, make_name (t.content, t.tok) None, fb []))
    end
  in
  let* _ = choice [ token "FUNCTION"; token "SUB" ] in
  let* id = identifier_name in
  let* lib = token "LIB" in
  let* lib_id = literal_expression in
  let declare_attr =
    G.NamedAttr (lib.tok, make_name ("Declare", lib.tok) None, fb [G.Arg lib_id])
  in
  let* alias_attr_opt = optional
    begin
      let* alias = token "ALIAS" in
      let* alias_expr = literal_expression in
      pure (G.NamedAttr (lib.tok, make_name ("Alias", alias.tok) None, fb [G.Arg alias_expr]))
    end
  in
  let* params = optional parameter_list in
  let* as_clause = optional simple_as_clause in
  let entity =
    G.{ name = G.EN (make_name id None);
        attrs = declare_attr
                :: Option.to_list encoding_attr_opt
                @ Option.to_list alias_attr_opt
                @ attrs;
        tparams = None }
  in
  let def =
    G.FuncDef
      G.{ fkind = (G.Function, declare.tok);
          fparams =
            (match params with
            | None -> fb []
            | Some p -> p);
          frettype = as_clause;
          fbody = G.FBDecl Tok.unsafe_sc
        }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and literal_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* literal_expression -> 'False' *)
      let* t = token "FALSE" in
      pure (G.L (G.Bool (false, t.tok)) |> G.e)
    end;
    begin
      (* literal_expression -> 'Nothing' *)
      let* t = token "NOTHING" in
      pure (G.L (G.Null t.tok) |> G.e)
    end;
    begin
      (* literal_expression -> 'True' *)
      let* t = token "TRUE" in
      pure (G.L (G.Bool (true, t.tok)) |> G.e)
    end;
    begin
      (* literal_expression -> token "<CHAR>" *)
      let* t = token "<CHAR>" in
      pure (G.L (G.Char (T.extract_string_content t, t.tok)) |> G.e)
    end;
    begin
      (* literal_expression -> date_literal_token *)
      let* t = token "<DATE>" in
      (* Using G.Atom because no other literal kind fits, while we
       * will not inspect its interanal structure anyway *)
      pure (G.L (G.Atom (t.tok, (t.content, t.tok))) |> G.e)
    end;
    begin
      (* literal_expression -> token "<FLOAT>" *)
      let* t = token "<FLOAT>" in
      pure (G.L (G.Float (Float.of_string_opt t.content, t.tok)) |> G.e)
    end;
    begin
      (* literal_expression -> token "<INT>" *)
      let* t = token "<INT>" in
      pure (G.L (G.Int (Some (T.extract_int_content t), t.tok)) |> G.e)
    end;
    begin
      (* literal_expression -> token "<STRING>" *)
      let* t = token "<STRING>" in
      pure (G.L (G.String (fb (T.extract_string_content t, t.tok))) |> G.e)
    end
  ]
) __n

and type_parameter_list : G.type_parameters parser = fun __n -> (
  (* type_parameter_list -> '(' 'Of' type_parameter (',' type_parameter)* ')' *)
  let* lparen = token "(" in
  let* _of = token "OF" in
  let* tparam = type_parameter in
  let* tparams = list_of
    begin
      let* _comma = token "," in
      type_parameter
    end
  in
  let* rparen = token ")" in
  pure (lparen.tok, tparam :: tparams, rparen.tok)
) __n

and type_parameter : G.type_parameter parser = fun __n -> (
  choice [
    begin
      let* dot_dot_dot = token "..." in
      pure (G.TParamEllipsis dot_dot_dot.tok)
    end;
    begin
      (* type_parameter -> ('In' | 'Out')? identifier_token type_parameter_constraint_clause? *)
      let* variance = optional variance in
      let* id = identifier_name in
      let* constr_ = optional type_parameter_constraint_clause in
      let attrs =
        match constr_ with
        | None -> []
        | Some a -> [G.OtherAttribute
                       (("constraint", Tok.unsafe_fake_tok "constraint"), [a])]
      in
        pure
          (G.TP { tp_id = id;
                  tp_attrs = attrs;
                  tp_bounds = [];
                  tp_default = None;
                  tp_variance = variance })
    end;
  ]
) __n

and variance : G.variance G.wrap parser = fun __n -> (
  choice [
    begin
      let* t = token "IN" in
      pure (G.Contravariant, t.tok)
    end;
    begin
      let* t = token "OUT" in
      pure (G.Covariant, t.tok)
    end;
  ]
) __n

(* TODO: this is unused *)
and type_parameter_constraint_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* type_parameter_constraint_clause -> type_parameter_multiple_constraint_clause *)
      let* type_parameter_multiple_constraint_clause1 = type_parameter_multiple_constraint_clause in
      pure (xRule "type_parameter_constraint_clause" 0 [type_parameter_multiple_constraint_clause1])
    end;
    begin
      (* type_parameter_constraint_clause -> type_parameter_single_constraint_clause *)
      let* type_parameter_single_constraint_clause1 = type_parameter_single_constraint_clause in
      pure (xRule "type_parameter_constraint_clause" 1 [type_parameter_single_constraint_clause1])
    end;
  ]
) __n

(* TODO: this is unused *)
and type_parameter_multiple_constraint_clause : G.any parser = fun __n -> (
  (* type_parameter_multiple_constraint_clause -> 'As' '{' constraint (',' constraint)* '}' *)
  let* as1 = token "AS" in
  let* lbrace1 = token "{" in
  let* constraint_1 = constraint_ in
  let* comma_constraint_s1 = list_of 
    begin
      let* comma1 = token "," in
      let* constraint_1 = constraint_ in
      pure (xGroup([xToken(comma1); constraint_1]))
    end
  in
  let* rbrace1 = token "}" in
  pure (xRule "type_parameter_multiple_constraint_clause" 0 [xToken(as1); xToken(lbrace1); constraint_1; xList(comma_constraint_s1); xToken(rbrace1)])
) __n

(* TODO: this is unused *)
and constraint_ : G.any parser = fun __n -> (
  choice [
    begin
      (* constraint -> special_constraint *)
      let* special_constraint1 = special_constraint in
      pure (xRule "constraint" 0 [special_constraint1])
    end;
    begin
      (* constraint -> type_constraint *)
      let* type_constraint1 = type_constraint in
      pure (xRule "constraint" 1 [type_constraint1])
    end;
  ]
) __n

(* TODO: this is unused *)
and special_constraint : G.any parser = fun __n -> (
  choice [
    begin
      (* special_constraint -> class_constraint *)
      let* class_constraint1 = class_constraint in
      pure (xRule "special_constraint" 0 [class_constraint1])
    end;
    begin
      (* special_constraint -> new_constraint *)
      let* new_constraint1 = new_constraint in
      pure (xRule "special_constraint" 1 [new_constraint1])
    end;
    begin
      (* special_constraint -> structure_constraint *)
      let* structure_constraint1 = structure_constraint in
      pure (xRule "special_constraint" 2 [structure_constraint1])
    end;
  ]
) __n

(* TODO: this is unused *)
and class_constraint : G.any parser = fun __n -> (
  (* class_constraint -> 'Class' *)
  let* class1 = token "CLASS" in
  pure (xRule "class_constraint" 0 [xToken(class1)])
) __n

(* TODO: this is unused *)
and new_constraint : G.any parser = fun __n -> (
  (* new_constraint -> 'New' *)
  let* new1 = token "NEW" in
  pure (xRule "new_constraint" 0 [xToken(new1)])
) __n

(* TODO: this is unused *)
and structure_constraint : G.any parser = fun __n -> (
  (* structure_constraint -> 'Structure' *)
  let* structure1 = token "STRUCTURE" in
  pure (xRule "structure_constraint" 0 [xToken(structure1)])
) __n

(* TODO: this is unused *)
and type_constraint : G.any parser = fun __n -> (
  (* type_constraint -> type *)
  let* _type_1 = type_ in
  pure (xRule "type_constraint" 0 [])
) __n

(* TODO: this is unused *)
and type_parameter_single_constraint_clause : G.any parser = fun __n -> (
  (* type_parameter_single_constraint_clause -> 'As' constraint *)
  let* as1 = token "AS" in
  let* constraint_1 = constraint_ in
  pure (xRule "type_parameter_single_constraint_clause" 0 [xToken(as1); constraint_1])
) __n

and function_statement (where_am_i : G.function_kind) (attrs : G.attribute list)
    : G.definition parser = fun __n -> (
  (* function_statement -> 'Function' identifier_token type_parameter_list? parameter_list? simple_as_clause? handles_clause? implements_clause? *)
  let* func = token "FUNCTION" in
  let* id = identifier_name in
  let* type_params = optional type_parameter_list in
  let* params = optional parameter_list in
  let* as_clause = optional simple_as_clause in
  (* TODO: handles, implements *)
  let* _handles = optional handles_clause in
  let* _implements = optional implements_clause in
  let entity =
    G.{ name = G.EN (make_name id None);
        attrs;
        tparams = type_params
      }
  in
  let def =
    G.FuncDef
      G.{ fkind = (where_am_i, func.tok);
          fparams =
            (match params with
            | None -> fb []
            | Some p -> p);
          frettype = as_clause;
          fbody = G.FBDecl Tok.unsafe_sc
        }
  in
  pure (entity, def)
) __n

and handles_clause : G.any parser = fun __n -> (
  (* handles_clause -> 'Handles' handles_clause_item (',' handles_clause_item)* *)
  let* handles1 = token "HANDLES" in
  let* handles_clause_item1 = handles_clause_item in
  let* comma_handles_clause_items1 = list_of 
    begin
      let* comma1 = token "," in
      let* handles_clause_item1 = handles_clause_item in
      pure (xGroup([xToken(comma1); handles_clause_item1]))
    end
  in
  pure (xRule "handles_clause" 0 [xToken(handles1); handles_clause_item1; xList(comma_handles_clause_items1)])
) __n

and handles_clause_item : G.any parser = fun __n -> (
  (* handles_clause_item -> event_container '.' identifier_name *)
  let* event_container1 = event_container in
  let* dot1 = token "." in
  let* identifier_name1 = identifier_name in
  pure (xRule "handles_clause_item" 0 [event_container1; xToken(dot1); G.E (expr_of_id identifier_name1)])
) __n

and event_container : G.any parser = fun __n -> (
  choice [
    begin
      (* event_container -> keyword_event_container *)
      let* keyword_event_container1 = keyword_event_container in
      pure (xRule "event_container" 0 [keyword_event_container1])
    end;
    begin
      (* event_container -> with_events_event_container *)
      let* with_events_event_container1 = with_events_event_container in
      pure (xRule "event_container" 1 [with_events_event_container1])
    end;
    begin
      (* event_container -> with_events_property_event_container *)
      let* with_events_property_event_container1 = with_events_property_event_container in
      pure (xRule "event_container" 2 [with_events_property_event_container1])
    end;
  ]
) __n

and keyword_event_container : G.any parser = fun __n -> (
  choice [
    begin
      (* keyword_event_container -> 'Me' *)
      let* me1 = token "ME" in
      pure (xRule "keyword_event_container" 0 [xToken(me1)])
    end;
    begin
      (* keyword_event_container -> 'MyBase' *)
      let* myBase1 = token "MYBASE" in
      pure (xRule "keyword_event_container" 1 [xToken(myBase1)])
    end;
    begin
      (* keyword_event_container -> 'MyClass' *)
      let* myClass1 = token "MYCLASS" in
      pure (xRule "keyword_event_container" 2 [xToken(myClass1)])
    end;
  ]
) __n

and with_events_event_container : G.any parser = fun __n -> (
  (* with_events_event_container -> identifier_token *)
  let* identifier_token1 = identifier_token in
  pure (xRule "with_events_event_container" 0 [identifier_token1])
) __n

and with_events_property_event_container : G.any parser = fun __n -> (
  (* with_events_property_event_container -> with_events_event_container '.' identifier_name *)
  let* with_events_event_container1 = with_events_event_container in
  let* dot1 = token "." in
  let* identifier_name1 = identifier_name in
  pure (xRule "with_events_property_event_container" 0 [with_events_event_container1; xToken(dot1); G.E (expr_of_id identifier_name1)])
) __n

and sub_statement (where_am_i : G.function_kind) (attrs : G.attribute list)
    : G.definition parser = fun __n -> (
  (* sub_statement -> 'Sub' identifier_token type_parameter_list? parameter_list? simple_as_clause? handles_clause? implements_clause? *)
  let* func = token "SUB" in
  let* id = identifier_name in
  let* type_params = optional type_parameter_list in
  let* params = optional parameter_list in
  let* as_clause = optional simple_as_clause in
  let* _handles = optional handles_clause in
  let* _implements = optional implements_clause in
  let entity =
    G.{ name = G.EN (make_name id None);
        attrs;
        tparams = type_params
      }
  in
  let def =
    G.FuncDef
      G.{ fkind = (where_am_i, func.tok);
          fparams =
            (match params with
            | None -> fb []
            | Some p -> p);
          frettype = as_clause;
          fbody = G.FBDecl Tok.unsafe_sc
        }
  in
  pure (entity, def)
) __n

and operator_statement_operator : G.ident parser = fun __n -> (
  let* t = choice
    [
      (* operator_statement_operator -> 'CType' *)
      token "CTYPE";
      (* operator_statement_operator -> 'IsTrue' *)
      token "ISTRUE";
      (* operator_statement_operator -> 'IsFalse' *)
      token "ISFALSE";
      (* operator_statement_operator -> 'Not' *)
      token "NOT";
      (* operator_statement_operator -> '+' *)
      token "+";
      (* operator_statement_operator -> '-' *)
      token "-";
      (* operator_statement_operator -> '*' *)
      token "*";
      (* operator_statement_operator -> '/' *)
      token "/";
      (* operator_statement_operator -> '^' *)
      token "^";
      (* operator_statement_operator -> '\\' *)
      token "\\";
      (* operator_statement_operator -> '&' *)
      token "&";
      (* operator_statement_operator -> '<<' *)
      token "<<";
      (* operator_statement_operator -> '>>' *)
      token ">>";
      (* operator_statement_operator -> 'Mod' *)
      token "MOD";
      (* operator_statement_operator -> 'Or' *)
      token "OR";
      (* operator_statement_operator -> 'Xor' *)
      token "XOR";
      (* operator_statement_operator -> 'And' *)
      token "AND";
      (* operator_statement_operator -> 'Like' *)
      token "LIKE";
      (* operator_statement_operator -> '=' *)
      token "=";
      (* operator_statement_operator -> '<>' *)
      token "<>";
      (* operator_statement_operator -> '<' *)
      token "<";
      (* operator_statement_operator -> '<=' *)
      token "<=";
      (* operator_statement_operator -> '>=' *)
      token ">=";
      (* operator_statement_operator -> '>' *)
      token ">"
    ]
  in
  pure (ident_of_token t)
) __n

and constructor_block (attrs : G.attribute list) (class_name : G.name) : G.stmt parser = fun __n -> (
  (* constructor_block -> 'Sub' 'New' parameter_list? @lookahead('<LINE_TERMINATOR>') statements_block end_sub_statement *)
  let* sub = token "SUB" in
  let* new_ = token "NEW" in
  let ctor_attr = G.KeywordAttr (G.Ctor, new_.tok) in
  let* params = optional parameter_list in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* stmt = statements_block in
  let* _ = end_sub_statement in
  let entity =
    G.{ name = G.EN class_name;
        attrs = ctor_attr :: attrs;
        tparams = None
      }
  in
  let def =
    G.FuncDef
      G.{ fkind = (G.Method, sub.tok);
          fparams =
            (match params with
            | None -> fb []
            | Some p -> p);
          frettype = None;
          fbody = G.FBStmt stmt
        }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and function_block (where_am_i : G.function_kind) (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* function_block -> function_statement @lookahead('<LINE_TERMINATOR>') (statements_block end_function_statement)? *)
  let* (entity, kind) as fs = function_statement where_am_i attrs in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* body_opt = optional
    begin
      let* stmt = statements_block in
      let* _ = end_function_statement in
      pure stmt
    end
  in
  match body_opt, kind with
  | Some body, FuncDef d ->
      pure (G.DefStmt (entity, G.FuncDef { d with G.fbody = FBStmt body }) |> G.s)
  | _ ->
      pure (G.DefStmt fs |> G.s)
) __n

and sub_block (where_am_i : G.function_kind) (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* sub_block -> sub_statement @lookahead('<LINE_TERMINATOR>') (statements_block end_sub_statement)? *)
  let* (entity, kind) as fs = sub_statement where_am_i attrs in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* body_opt = optional
    begin
      let* stmt = statements_block in
      let* _ = end_sub_statement in
      pure stmt
    end
  in
  match body_opt, kind with
  | Some body, FuncDef d ->
      pure (G.DefStmt (entity, G.FuncDef { d with G.fbody = FBStmt body }) |> G.s)
  | _ ->
      pure (G.DefStmt fs |> G.s)
) __n

and operator_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* operator_block -> 'Operator' operator_statement_operator parameter_list? simple_as_clause? statements_block end_operator_statement *)
  let* func = token "OPERATOR" in
  let* operator = operator_statement_operator in
  let* params = optional parameter_list in
  let* as_clause = optional simple_as_clause in
  let* stmt = statements_block in
  let* _ = end_operator_statement in
  let entity =
    G.{ name = G.EN (make_name operator None);
        attrs;
        tparams = None
      }
  in
  let def =
    G.FuncDef
      G.{ fkind = (G.Method, func.tok);
          fparams =
            (match params with
            | None -> fb []
            | Some p -> p);
          frettype = as_clause;
          fbody = G.FBStmt stmt
        }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and namespace_block : G.stmt parser = fun __n -> (
  (* namespace_block -> 'Namespace' qualified_name ('.' identifier_name)* toplevel end_namespace_statement *)
  let* _ns = token "NAMESPACE" in
  let* qname = qualified_name in
  let* decls = list_of toplevel_declaration in
  let* _ = end_namespace_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs = []; (* namespaces don't have attributes *)
        tparams = None (* ...or type params *) }
  in
  let def =
    G.ModuleDef G.{ mbody = G.ModuleStruct (None, decls) }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and property_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* property_block -> property_statement (property_accessor_block* end_property_statement)? *)
  let* _property = token "PROPERTY" in
  let* id = identifier_name in
  let* _params = optional parameter_list in (* TODO: params *)
  let* as_clause = optional as_clause in
  let typ = match as_clause with
    | Some (Either.Right typ) -> Some typ
    | _ -> None
  in
  let* init = optional equals_value in
  let* _implements = optional implements_clause in (* TODO: implements *)
  let* accessors_opt = optional
    begin
      let* accessor = list_of (property_accessor_block id typ) in
      let* _ = end_property_statement in
      pure accessor
    end
  in
  let entity =
    G.{ name = G.EN (make_name id None);
        attrs = attrs;
        tparams = None }
  in
  let def =
    G.VarDef
      { vinit =
          (match as_clause, init with
          | Some (Either.Left e), _ -> Some e
          | _ -> init);
        vtype =
          (match as_clause with
          | Some (Either.Right typ) -> Some typ
          | _ -> None);
        vtok = None;
      }
  in
  let var_stmt = G.DefStmt (entity, def) |> G.s in
  match accessors_opt with
  | None | Some [] -> pure var_stmt
  | Some accessors -> pure (G.Block (fb (var_stmt :: accessors)) |> G.s)
) __n

and property_accessor_block ((property_name_str, _) : G.ident)
    (typ : G.type_ option) : G.stmt parser = fun __n -> (
  let* attrs = list_of attribute_list in
  let* modifiers = list_of modifier in
  choice [
    begin
      (* property_accessor_block -> attribute_list* modifier* 'Get' statements_block ':'? 'End' 'Get' *)
      let* get = token "GET" in
      let* stmt = statements_block in
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "GET" in
      let entity =
        G.{ name = G.EN (make_name ("GET_" ^ property_name_str,  get.tok) None);
            attrs = modifiers @ List.concat attrs;
            tparams = None }
      in
      let def =
        G.FuncDef
          { fkind = (G.Method, get.tok);
            fparams = fb [];
            frettype = typ;
            fbody = G.FBStmt stmt
          }
      in
      pure (G.DefStmt (entity, def) |> G.s)
    end;
    begin
      (* property_accessor_block -> attribute_list* modifier* 'Set' parameter_list? statements_block ':'? 'End' 'Set' *)
      let* set = token "SET" in
      let* params = optional parameter_list in
      let* stmt = statements_block in
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "SET" in
      let entity =
        G.{ name = G.EN (make_name ("SET_" ^ property_name_str, set.tok) None);
            attrs = modifiers @ List.concat attrs;
            tparams = None }
      in
      let def =
        G.FuncDef
          { fkind = (G.Method, set.tok);
            fparams =
              (match params with
              | Some p -> p
              | None -> fb []);
            frettype = typ;
            fbody = G.FBStmt stmt
          }
      in
      pure (G.DefStmt (entity, def) |> G.s)
    end;
    begin
      (* property_accessor_block -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.ExprStmt (G.Ellipsis dot_dot_dot.tok |> G.e, Tok.unsafe_sc) |> G.s)
    end;
    begin
      (* property_accessor_block -> '<...' expression '...>' *)
      let* lt = token "<..." in
      let* expr = expression in
      let* gt = token "...>" in
      pure (G.ExprStmt (G.DeepEllipsis (lt.tok, expr, gt.tok) |> G.e, Tok.unsafe_sc) |> G.s)
    end;
  ]
) __n

and class_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* class_block -> 'Class' qualified_name type_parameter_list? inherits_statement* implements_statement* class_block_declaration* end_class_statement *)
  let* class_ = token "CLASS" in
  let* qname = qualified_name in
  let* tparams = optional type_parameter_list in
  let* inherits = list_of inherits_statement in
  let* implements = list_of implements_statement in
  let* decls = list_of (class_block_declaration qname) in
  let* _ = end_class_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs;
        tparams }
  in
  let def =
    G.ClassDef
      G.{ ckind = (G.Class, class_.tok);
          cextends = List.concat inherits;
          cimplements = List.concat implements;
          cmixins = [];
          cparams = fb [];
          cbody = fb (List.concat decls)
          }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and class_block_declaration (class_name : G.name) : G.field list parser = fun __n -> (
  let* attrs = list_of attribute_list in
  let* modifiers = list_of modifier in
  let attrs = modifiers @ List.concat attrs in
  choice [
    begin
      (* class_block_declaration -> attribute_list* method_modifier* class_block_kw_declaration *)
      let* s = class_block_kw_declaration attrs class_name in
      pure [G.F s]
    end;
    begin
      (* class_block_declaration -> field_declaration *)
      field_declaration attrs
    end;
  ]
) __n

and class_block_kw_declaration (attrs : G.attribute list) (class_name : G.name) : G.stmt parser = fun __n -> (
  choice [
    begin
      (* class_block_kw_declaration -> function_block *)
      function_block G.Method attrs
    end;
    begin
      (* class_block_kw_declaration -> sub_block *)
      sub_block G.Method attrs
    end;
    begin
      (* class_block_kw_declaration -> constructor_block *)
      constructor_block attrs class_name
    end;
    begin
      (* class_block_kw_declaration -> property_block *)
      property_block attrs
    end;
    begin
      (* class_block_kw_declaration -> class_block *)
      class_block attrs
    end;
    begin
      (* class_block_kw_declaration -> module_block *)
      module_block attrs
    end;
    begin
      (* class_block_kw_declaration -> interface_block *)
      interface_block attrs
    end;
    begin
      (* class_block_kw_declaration -> structure_block *)
      structure_block attrs
    end;
    begin
      (* class_block_kw_declaration -> event_block *)
      event_block attrs
    end;
    begin
      (* class_block_kw_declaration -> operator_block *)
      operator_block attrs
    end;
    begin
      (* class_block_kw_declaration -> enum_block *)
      enum_block attrs
    end;
    begin
      (* class_block_kw_declaration -> declare_statement *)
      declare_statement attrs
    end;
    begin
      (* toplevel_kw_declaration -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.ExprStmt (G.Ellipsis dot_dot_dot.tok |> G.e, Tok.unsafe_sc) |> G.s)
    end;
    begin
      (* toplevel_kw_declaration -> '<...' expression '...>' *)
      let* left = token "<..." in
      let* expr = expression in
      let* right = token "...>" in
      pure (G.ExprStmt (G.DeepEllipsis (left.tok, expr, right.tok) |> G.e, Tok.unsafe_sc) |> G.s)
    end;
  ]
) __n

and interface_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* interface_block -> interface_statement inherits_statement* implements_statement* class_block_declaration* end_interface_statement *)
  let* interface = token "INTERFACE" in
  let* qname = qualified_name in
  let* tparams = optional type_parameter_list in
  let* inherits = list_of inherits_statement in
  let* implements = list_of implements_statement in
  let* decls = list_of (class_block_declaration qname) in
  let* _ = end_interface_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs;
        tparams }
  in
  let def =
    G.ClassDef
      G.{ ckind = (G.Interface, interface.tok);
          cextends = List.concat inherits;
          cimplements = List.concat implements;
          cmixins = [];
          cparams = fb [];
          cbody = fb (List.concat decls) }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and module_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* module_block -> module_statement inherits_statement* implements_statement* class_block_declaration* end_module_statement *)
  let static = G.KeywordAttr (G.Static, Tok.unsafe_fake_tok "STATIC") in
  let* interface = token "MODULE" in
  let* qname = qualified_name in
  let* tparams = optional type_parameter_list in
  let* inherits = list_of inherits_statement in
  let* implements = list_of implements_statement in
  let* decls = list_of (class_block_declaration qname) in
  let* _ = end_module_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs = static :: attrs;
        tparams }
  in
  let add_static = function
    | G.F s -> G.F (add_attrs_to_stmt s [static])
  in
  let def =
    G.ClassDef
      G.{ ckind = (G.Object, interface.tok);
          cextends = List.concat inherits;
          cimplements = List.concat implements;
          cmixins = [];
          cparams = fb [];
          cbody = fb (List.concat decls |> List.map add_static) }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

(* TODO: structures are represented the same way as classes, so we cannot distinguish them
 * by matching. Is it fixable? *)
and structure_block (attrs : G.attribute list) : G.stmt parser = fun __n -> (
  (* structure_block -> structure_statement inherits_statement* implements_statement* class_block_declaration* end_structure_statement *)
  let* class_ = token "STRUCTURE" in
  let* qname = qualified_name in
  let* tparams = optional type_parameter_list in
  let* inherits = list_of inherits_statement in
  let* implements = list_of implements_statement in
  let* decls = list_of (class_block_declaration qname) in
  let* _ = end_structure_statement in
  let entity =
    G.{ name = G.EN qname;
        attrs;
        tparams }
  in
  let def =
    G.ClassDef
      G.{ ckind = (G.Class, class_.tok);
          cextends = List.concat inherits;
          cimplements = List.concat implements;
          cmixins = [];
          cparams = fb [];
          cbody = fb (List.concat decls)
          }
  in
  pure (G.DefStmt (entity, def) |> G.s)
) __n

and add_remove_handler_statement : G.stmt parser = fun __n -> (
  let* t = choice [ token "ADDHANDLER"; token "REMOVEHANDLER" ] in
  let* expr1 = expression in
  let* _comma = token "," in
  let* expr2 = expression in
  let func = G.N (make_name (ident_of_token t) None) |> G.e in
  let call = G.Call (func, fb [G.Arg expr1; G.Arg expr2]) |> G.e in
  pure (G.ExprStmt (call, Tok.unsafe_sc) |> G.s)
) __n

and assignment_statement_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* assignment_statement_operator -> '=' *)
      let* t = token "=" in
      pure (G.Eq, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '+=' *)
      let* t = token "+=" in
      pure (G.Plus, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '-=' *)
      let* t = token "-=" in
      pure (G.Minus, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '*=' *)
      let* t = token "*=" in
      pure (G.Mult, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '/=' *)
      let* t = token "/=" in
      pure (G.Div, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '\\=' *)
      let* t = token "\\=" in
      pure (G.FloorDiv, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '^=' *)
      let* t = token "^=" in
      pure (G.Pow, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '<<=' *)
      let* t = token "<<=" in
      pure (G.LSL, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '>>=' *)
      let* t = token ">>=" in
      pure (G.LSR, t.tok)
    end;
    begin
      (* assignment_statement_operator -> '&=' *)
      let* t = token "&=" in
      pure (G.Plus, t.tok)
    end;
  ]
) __n

and assignment_statement : G.stmt parser = fun __n -> (
  (* assignment_statement -> await_expression (assignment_statement_operator expression)? *)
  let* lhs = await_expression in
  let* op_rhs_opt = optional
    begin
      let* op = assignment_statement_operator in
      let* rhs = expression in
      pure (op, rhs)
    end
  in
  let expr =
    match op_rhs_opt with
    | None -> lhs
    | Some (op, rhs) -> G.AssignOp (lhs, op, rhs) |> G.e
  in
  pure (G.ExprStmt (expr, Tok.unsafe_sc) |> G.s)
) __n

and exit_statement : G.stmt parser = fun __n -> (
  let* exit = token "EXIT" in
  choice [
    begin
      let* _exit_what = choice
          [token "DO";
           token "FOR";
           token "SELECT";
           token "TRY";
           token "WHILE"] in
      pure (G.Break (exit.tok, G.LNone, Tok.unsafe_sc) |> G.s)
    end;
    begin
      let* _exit_what = choice
          [token "FUNCTION";
           token "OPERATOR"; (* not in the language reference... *)
           token "PROPERTY";
           token "SUB"] in
      pure (G.Return (exit.tok, None, Tok.unsafe_sc) |> G.s)
    end;
  ]
) __n

and go_to_statement : G.stmt parser = fun __n -> (
  (* go_to_statement -> 'GoTo' label *)
  let* _ = optional (token ":") in
  let* goto = token "GOTO" in
  let* lbl = choice [ token "<IDENT>"; token "<INT>" ]
  in
  pure (G.Goto (goto.tok, ident_of_token lbl, Tok.unsafe_sc) |> G.s)
) __n

and identifier_label : G.label parser = fun __n -> (
  (* identifier_label -> identifier_token *)
  let* id = token "<IDENT>" in
  pure (ident_of_token id)
) __n

and numeric_label : G.label parser = fun __n -> (
  (* numeric_label -> integer_literal_token *)
  let* id = token "<INT>" in
  pure (ident_of_token id)
) __n

and local_declaration_statement : G.stmt list parser = fun __n -> (
  (* local_declaration_statement -> modifier+ variable_declarator (',' variable_declarator)* *)
  let* _ = optional (token ":") in
  let* attrs = list_of attribute_list in
  let* modifiers = ne_list_of modifier in
  let attrs = modifiers @ List.concat attrs in
  let* decl = variable_declarator attrs in
  let* decls = list_of
    begin
      let* _comma1 = token "," in
      variable_declarator attrs
    end
  in
  pure (decl @ List.concat decls)
) __n

and re_dim_statement : G.stmt list parser = fun __n -> (
  (* re_dim_statement -> 'ReDim' 'Preserve'? redim_clause (',' redim_clause)* *)
  let* redim = token "REDIM" in
  let* preserve_opt = optional (token "PRESERVE") in
  let* clause = redim_clause in
  let* clauses = list_of
    begin
      let* _comma = token "," in
      redim_clause
    end
  in
  let clauses = clause :: clauses in
  let ident =
    match preserve_opt with
    | None -> redim.content, redim.tok
    | Some preserve -> redim.content ^ preserve.content, Tok.combine_toks redim.tok [preserve.tok]
  in
  let func = G.N (make_name ident None) |> G.e in
  let call args = G.Call (func, args) |> G.e in
  let stmt args = G.ExprStmt (call args, Tok.unsafe_sc) |> G.s in
  pure (List.map stmt clauses)
) __n

and redim_clause : G.arguments parser = fun __n -> (
  (* redim_clause -> access_expression *)
  let dim_to_arg d =
    match d with
    | None, expr -> G.Arg expr
    | Some expr1, expr2 -> G.Arg (G.Container (G.Tuple, fb [expr1; expr2]) |> G.e)
  in
  let* name = qualified_name in
  let* lparen = token "(" in
  let* dim = variable_declarator_identifier_dimension in
  let* dims = list_of
    begin
      let* _comma = token "," in
      variable_declarator_identifier_dimension
    end
  in
  let* rparen = token ")" in
  pure (lparen.tok, G.Arg (G.N name |> G.e) :: List.map dim_to_arg (dim :: dims), rparen.tok)
) __n

and aggregation : G.any parser = fun __n -> (
  choice [
    begin
      (* aggregation -> 'Group' *)
      let* group1 = token "GROUP" in
      pure (xRule "aggregation" 0 [xToken(group1)])
    end;
    begin
      (* aggregation -> function_aggregation *)
      let* function_aggregation1 = function_aggregation in
      pure (xRule "aggregation" 1 [function_aggregation1])
    end;
  ]
) __n

and function_aggregation : G.any parser = fun __n -> (
  (* function_aggregation -> identifier_token ('(' expression ')')? *)
  let* identifier_token1 = identifier_token in
  let* lparen_expression_rparen_opt1 = optional 
    begin
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xGroup([xToken(lparen1); G.E expression1; xToken(rparen1)]))
    end
  in
  pure (xRule "function_aggregation" 0 [identifier_token1; xOptional(lparen_expression_rparen_opt1)])
) __n

and expression_terminator : unit parser = fun __n -> (
  choice [
    begin
      (* expression_terminator -> @lookahead('<PUNCTUATION>') *)
      let* _ = look_ahead "<PUNCTUATION>" in
      pure ()
    end;
    begin
      (* expression_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure ()
    end;
  ]
) __n

and expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* expression -> literal_expression expression_terminator *)
      let* e = literal_expression in
      let* _ = expression_terminator in
      pure e
    end;
    begin
      (* expression -> binary_logical_xor_expression *)
      binary_logical_xor_expression
    end;
  ]
) __n

and xor_operator : G.operator G.wrap parser = fun __n -> (
  (* xor_operator -> 'Xor' *)
  let* t = token "XOR" in
  pure (G.BitXor, t.tok) (* FIXME: This plays the role of G.Xor as well *)
) __n

and or_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* or_operator -> 'Or' *)
      let* t = token "OR" in (* NOTE: Or in VB.NET is not short-circuiting *)
      pure (G.Or, t.tok)
    end;
    begin
      (* or_operator -> 'OrElse' *)
      let* t = token "ORELSE" in
      pure (G.Or, t.tok)
    end;
  ]
) __n

and and_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* and_operator -> 'And' *)
      let* t = token "AND" in
      pure (G.And, t.tok)
    end;
    begin
      (* and_operator -> 'AndAlso' *)
      let* t = token "ANDALSO" in
      pure (G.And, t.tok)
    end;
  ]
) __n

and not_operator : G.operator G.wrap parser = fun __n -> (
  (* not_operator -> 'Not' *)
  let* t = token "NOT" in
  pure (G.Not, t.tok)
) __n

and relational_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* relational_operator -> '=' *)
      let* t = token "=" in
      pure (G.Eq, t.tok)
    end;
    begin
      (* relational_operator -> '<>' *)
      let* t = token "<>" in
      pure (G.NotEq, t.tok)
    end;
    begin
      (* relational_operator -> @lookahead_not('<LINE_TERMINATOR>') '<' *)
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* t = token "<" in
      pure (G.Lt, t.tok)
    end;
    begin
      (* relational_operator -> '>' *)
      let* t = token ">" in
      pure (G.Gt, t.tok)
    end;
    begin
      (* relational_operator -> '<=' *)
      let* t = token "<=" in
      pure (G.LtE, t.tok)
    end;
    begin
      (* relational_operator -> '>=' *)
      let* t = token ">=" in
      pure (G.GtE, t.tok)
    end;
    begin
      (* relational_operator -> 'Like' *)
      let* t = token "LIKE" in
      pure (G.RegexpMatch, t.tok)
    end;
    begin
      (* relational_operator -> 'Is' *)
      let* t = token "IS" in
      pure (G.PhysEq, t.tok)
    end;
    begin
      (* relational_operator -> 'IsNot' *)
      let* t = token "ISNOT" in
      pure (G.NotPhysEq, t.tok)
    end;
  ]
) __n

and shift_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* shift_operator -> '<<' *)
      let* t = token "<<" in
      pure (G.LSL, t.tok)
    end;
    begin
      (* shift_operator -> '>>' *)
      let* t = token ">>" in
      pure (G.LSR, t.tok)
    end;
  ]
) __n

and concatenation_operator : G.operator G.wrap parser = fun __n -> (
  (* concatenation_operator -> '&' *)
  let* t = token "&" in
  pure (G.Plus, t.tok)
) __n

and additive_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* additive_operator -> '+' *)
      let* t = token "+" in
      pure (G.Plus, t.tok)
    end;
    begin
      (* additive_operator -> '-' *)
      let* t = token "-" in
      pure (G.Minus, t.tok)
    end;
  ]
) __n

and multiplicative_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* multiplicative_operator -> '*' *)
      let* t = token "*" in
      pure (G.Mult, t.tok)
    end;
    begin
      (* multiplicative_operator -> '/' *)
      let* t = token "/" in
      pure (G.Div, t.tok)
    end;
    begin
      (* multiplicative_operator -> '\\' *)
      let* t = token "\\" in
      pure (G.FloorDiv, t.tok)
    end;
    begin
      (* multiplicative_operator -> 'Mod' *)
      let* t = token "MOD" in
      pure (G.Mod, t.tok)
    end;
  ]
) __n

and unary_operator : G.operator G.wrap parser = fun __n -> (
  choice [
    begin
      (* unary_operator -> '+' *)
      let* t = token "+" in
      pure (G.Plus, t.tok)
    end;
    begin
      (* unary_operator -> '-' *)
      let* t = token "-" in
      pure (G.Minus, t.tok)
    end;
  ]
) __n

and exponentiation_operator : G.operator G.wrap parser = fun __n -> (
  (* exponentiation_operator -> '^' *)
  let* t = token "^" in
  pure (G.Pow, t.tok)
) __n

and await_operator : G.tok parser = fun __n -> (
  (* await_operator -> 'Await' *)
  let* t = token "AWAIT" in
  pure t.tok
) __n

and combine_infix_operator (e : G.expr) (es : (G.operator G.wrap * G.expr) list) : G.expr =
  List.fold_left
    (fun prev_e ((op, op_tok), next_e) ->
      G.Call (G.IdSpecial (G.Op op, op_tok) |> G.e, fb [ G.Arg prev_e; G.Arg next_e ]) |> G.e)
    e es

and binary_logical_xor_expression : G.expr parser = fun __n -> (
  (* binary_logical_xor_expression -> binary_logical_or_expression (xor_operator binary_logical_or_expression)* *)
  let* e = binary_logical_or_expression in
  let* es = list_of
    begin
      let* op = xor_operator in
      let* en = binary_logical_or_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and binary_logical_or_expression : G.expr parser = fun __n -> (
  (* binary_logical_or_expression -> binary_logical_and_expression (or_operator binary_logical_and_expression)* *)
  let* e = binary_logical_and_expression in
  let* es = list_of
    begin
      let* op = or_operator in
      let* en = binary_logical_and_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and binary_logical_and_expression : G.expr parser = fun __n -> (
  (* binary_logical_and_expression -> logical_not_expression (and_operator logical_not_expression)* *)
  let* e = logical_not_expression in
  let* es = list_of
    begin
      let* op = and_operator in
      let* en = logical_not_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and logical_not_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* logical_not_expression -> not_operator logical_not_expression *)
      let* (op, op_tok) = not_operator in
      let* e = logical_not_expression in
      pure (G.Call (G.IdSpecial (G.Op op, op_tok) |> G.e,
                    fb [ G.Arg e ]) |> G.e)
    end;
    begin
      (* logical_not_expression -> binary_relational_expression *)
      binary_relational_expression
    end;
  ]
) __n

and binary_relational_expression : G.expr parser = fun __n -> (
  (* binary_relational_expression -> binary_shift_expression (relational_operator logical_not_expression)? *)
  let* e = binary_shift_expression in
  let* es = optional
    begin
      let* op = relational_operator in
      let* en = logical_not_expression in
      pure (op, en)
    end
  in
  match es with
  | Some ((op, op_tok), en) ->
      pure (G.Call (G.IdSpecial (G.Op op, op_tok) |> G.e,
                    fb [ G.Arg e; G.Arg en ]) |> G.e)
  | None ->
      pure e
) __n

and binary_shift_expression : G.expr parser = fun __n -> (
  (* binary_shift_expression -> binary_concatenation_expression (shift_operator binary_concatenation_expression)* *)
  let* e = binary_concatenation_expression in
  let* es = list_of
    begin
      let* op = shift_operator in
      let* en = binary_concatenation_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and binary_concatenation_expression : G.expr parser = fun __n -> (
  (* binary_concatenation_expression -> binary_additive_expression (concatenation_operator binary_additive_expression)* *)
  let* e = binary_additive_expression in
  let* es = list_of
    begin
      let* op = concatenation_operator in
      let* en = binary_additive_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and binary_additive_expression : G.expr parser = fun __n -> (
  (* binary_additive_expression -> binary_multiplicative_expression (additive_operator binary_multiplicative_expression)* *)
  let* e = binary_multiplicative_expression in
  let* es = list_of
    begin
      let* op = additive_operator in
      let* en = binary_multiplicative_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and binary_multiplicative_expression : G.expr parser = fun __n -> (
  (* binary_multiplicative_expression -> unary_expression (multiplicative_operator unary_expression)* *)
  let* e = unary_expression in
  let* es = list_of
    begin
      let* op = multiplicative_operator in
      let* en = unary_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and unary_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* unary_expression -> unary_operator unary_expression *)
      let* (op, op_tok) = unary_operator in
      let* e = unary_expression in
      pure (G.Call (G.IdSpecial (G.Op op, op_tok) |> G.e,
                    fb [ G.Arg e ]) |> G.e)
    end;
    begin
      (* unary_expression -> address_of_expression *)
      address_of_expression
    end;
    begin
      (* unary_expression -> binary_exponentiation_expression *)
      binary_exponentiation_expression
    end;
  ]
) __n

and binary_exponentiation_expression : G.expr parser = fun __n -> (
  (* binary_exponentiation_expression -> await_expression (exponentiation_operator unary_expression)* *)
  let* e = await_expression in
  let* es = list_of
    begin
      let* op = exponentiation_operator in
      let* en = unary_expression in
      pure (op, en)
    end
  in
  pure (combine_infix_operator e es)
) __n

and await_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* await_expression -> await_operator await_expression *)
      let* t = await_operator in
      let* e = await_expression in
      pure (G.Await (t, e) |> G.e)
    end;
    begin
      (* await_expression -> access_expression *)
      access_expression
    end;
  ]
) __n

and access_expression : G.expr parser = fun __n -> (
  (* access_expression -> primary_expression (@lookahead_not('<LINE_TERMINATOR>') accessor)* *)
  let* expr = primary_expression in
  let* accessors = list_of
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      accessor
    end
  in
  pure (List.fold_left (|>) expr accessors)
) __n

and identifier_or_keyword : G.name parser = fun __n -> (
  choice [
    begin
      (* identifier_or_keyword -> identifier_name type_argument_list? *)
      let* id = identifier_name in
      let* type_args = optional type_argument_list in
      pure (make_name id type_args)
    end;
    begin
      (* identifier_or_keyword -> '<KEYWORD>' type_argument_list? *)
      let* id = token "<KEYWORD>" in
      let* type_args = optional type_argument_list in
      pure (make_name (id.content, id.tok) type_args)
    end;
  ]
) __n

and collection_index_access : G.expr G.bracket parser = fun __n -> (
  let* lparen = token "(" in
  let* expr = expression in
  let* exprs = list_of
    begin
      let* _comma = token "," in
       expression
    end
  in
  let* rparen = token ")" in
  match exprs with
  | [] -> pure (lparen.tok, expr, rparen.tok)
  | _ -> pure (lparen.tok, G.Container (G.Tuple, fb (expr :: exprs)) |> G.e, rparen.tok)
) __n

and elvis_wrap (tok : T.t) (e : G.expr) : G.expr =
   G.Call (G.IdSpecial (G.Op G.Elvis, tok.tok) |> G.e, fb [G.Arg e]) |> G.e

and accessor : (G.expr -> G.expr) parser = fun __n -> (
  choice [
    begin
      (* accessor -> '.' 'Item' collection_index_access *)
      (* NOTE: this is a special case: Item property *usually* works like array access*)
      let* _dot = token "." in
      let* _item = token "ITEM" in
      let* args = collection_index_access in
      pure (fun e -> G.ArrayAccess (e, args) |> G.e)
    end;
    begin
      (* accessor -> '.' accessor_body *)
      let* dot = token "." in
      let* rhs = accessor_body in
      match rhs with
      | Either.Left name ->
          pure (fun e -> G.DotAccess (e, dot.tok, G.FN name) |> G.e)
      | Either.Right dot_dot_dot ->
          pure (fun e -> G.DotAccessEllipsis (e, dot_dot_dot.tok) |> G.e)
    end;
    begin
      (* accessor -> '.' '(' ')' *)
      (* TODO: access default property *)
      let* _dot = token "." in
      let* _lparen = token "(" in
      let* _rparen = token ")" in
      pure (fun e -> e)
    end;
    begin
      (* accessor -> '.' collection_index_access *)
      let* _dot = token "." in
      let* args = collection_index_access in
      pure (fun e -> G.ArrayAccess (e, args) |> G.e)
    end;
    begin
      (* accessor -> '!' ('<IDENT>' | '<KEYWORD>') *)
      (* NOTE: according to Copilot: the obj!expr syntax is a quirky and
       * under-documented feature of VB.NET, and it's not formally described
       * in the main Visual Basic Language Specification or the standard
       * Microsoft Learn pages. *)
      let* _bang = token "!" in
      let* rhs = choice [token "<IDENT>"; token "<KEYWORD>"] in
      pure (fun e -> G.Call (e, fb [G.Arg (G.L (G.String (fb (rhs.content, rhs.tok))) |> G.e)]) |> G.e)
    end;
    begin
      (* accessor -> '?.' accessor_body *)
      let* qmark_dot = token "?." in
      let* rhs = accessor_body in
      match rhs with
      | Either.Left name ->
          pure (fun e -> G.DotAccess (elvis_wrap qmark_dot e, qmark_dot.tok, G.FN name) |> G.e)
      | Either.Right dot_dot_dot ->
          pure (fun e -> G.DotAccessEllipsis (elvis_wrap qmark_dot e, dot_dot_dot.tok) |> G.e)
    end;
    begin
      (* accessor -> '?' collection_index_access *)
      let* qmark = token "?" in
      let* args = collection_index_access in
      pure (fun e -> G.ArrayAccess (elvis_wrap qmark e, args) |> G.e)
    end;
    (* NOTE: XML props: https://learn.microsoft.com/en-us/dotnet/visual-basic/language-reference/xml-axis/  *)
    begin
      (* accessor -> '.@' x_name *)
      let* dot_at = token ".@" in
      let* name = x_name in
      pure (fun e -> G.DotAccess (e, dot_at.tok, G.FN name) |> G.e)
    end;
    begin
      (* accessor -> '.@<' x_name '>' *)
      let* dot_at_lt = token ".@<" in
      let* name = x_name in
      let* _gt = token ">" in
      pure (fun e -> G.DotAccess (e, dot_at_lt.tok, G.FN name) |> G.e)
    end;
    begin
      (* accessor -> '.' '<' x_name '>' *)
      let* dot = token "." in
      let* lt = token "<" in
      let tok = Tok.combine_toks dot.tok [lt.tok] in
      let* name = x_name in
      let* _gt = token ">" in
      pure (fun e -> G.DotAccess (e, tok, G.FN name) |> G.e)
    end;
    begin
      (* accessor -> '...' '<' x_name '>' *)
      let* dot_dot_dot = token "..." in
      let* lt = token "<" in
      let tok = Tok.combine_toks dot_dot_dot.tok [lt.tok] in
      let* name = x_name in
      let* _gt = token ">" in
      pure (fun e -> G.DotAccess (e, tok, G.FN name) |> G.e)
    end;
    begin
      (* accessor -> argument_list *)
      let* args = argument_list in
      pure (fun e -> G.Call (e, args) |> G.e)
    end;
  ]
) __n

and accessor_body : (G.name, T.t) Either.t parser = fun __n -> (
  choice [
    begin
      (* accessor_body -> identifier_or_keyword *)
      let* id = identifier_or_keyword in
      pure (Either.Left id)
    end;
    begin
      (* accessor_body -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (Either.Right dot_dot_dot)
    end;
  ]
) __n

and adjust_range_of_parenthesized_expr (l : G.tok) (e : G.expr) (r : G.tok) : G.expr =
  e.e_range <- Some (Tok.unsafe_loc_of_tok l, Tok.unsafe_loc_of_tok r);
  e

and primary_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* primary_expression -> '(' expression ')' *)
      let* l = token "(" in
      let* e = expression in
      let* r = token ")" in
      pure (adjust_range_of_parenthesized_expr l.tok e r.tok)
    end;
    begin
      (* primary_expression -> literal_expression *)
      literal_expression
    end;
    begin
      (* primary_expression -> conditional_expression *)
      conditional_expression
    end;
    begin
      (* primary_expression -> get_type_expression *)
      get_type_expression
    end;
    begin
      (* primary_expression -> cast_expression *)
      cast_expression
    end;
    begin
      (* primary_expression -> collection_initializer *)
      collection_initializer
    end;
    begin
      (* primary_expression -> get_xml_namespace_expression *)
      get_xml_namespace_expression
    end;
    begin
      (* primary_expression -> instance_expression *)
      instance_expression
    end;
    begin
      (* primary_expression -> interpolated_string_expression *)
      interpolated_string_expression
    end;
    begin
      (* primary_expression -> lambda_expression *)
      lambda_expression
    end;
    begin
      (* primary_expression -> name_of_expression *)
      name_of_expression
    end;
    begin
      (* primary_expression -> new_expression *)
      new_expression
    end;
    begin
      (* primary_expression -> predefined_cast_expression *)
      predefined_cast_expression
    end;
    begin
      (* primary_expression -> query_expression *)
      let* query = query_expression in
      pure (G.RawExpr (RT.Any query) |> G.e)
    end;
    begin
      (* primary_expression -> tuple_expression *)
      tuple_expression
    end;
    begin
      (* primary_expression -> type_of_expression *)
      type_of_expression
    end;
    begin
      (* primary_expression -> identifier_expression *)
      identifier_expression
    end;
    begin
      (* primary_expression -> xml_cdata *)
      xml_cdata
    end;
    begin
      (* primary_expression -> x_expression *)
      let* xml = x_expression in
      pure (G.Xml xml |> G.e)
    end;
    begin
      (* primary_expression -> x_tag_embed_expression *)
      let* (_, expr, _) = x_tag_embed_expression in
      pure expr
    end;
    begin
      (* primary_expression -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.Ellipsis dot_dot_dot.tok |> G.e)
    end;
    begin
      (* primary_expression -> '<...' expression '...>' *)
      let* lt = token "<..." in
      let* expr = expression in
      let* gt = token "...>" in
      pure (DeepEllipsis (lt.tok, expr, gt.tok) |> G.e)
    end;
  ]
) __n

and identifier_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* identifier_expression -> '.' identifier_or_keyword *)
      let* dot = token "." in
      let* accessor = identifier_or_keyword in
      let var_name = make_with_block_var () in
      let lhs = G.N var_name |> G.e in
      let rhs = G.FN accessor in
      pure (G.DotAccess (lhs, dot.tok, rhs) |> G.e)
    end;
    begin
      (* identifier_expression -> identifier_name type_argument_list? *)
      let* id = identifier_name in
      let* type_args = optional type_argument_list in
      pure (G.N (make_name id type_args) |> G.e)
    end
  ]
) __n |> cut

and get_type_expression : G.expr parser = fun __n -> (
  (* get_type_expression -> 'GetType' '(' qualified_name '?' ')' *)
  let* getType = token "GETTYPE" in
  let* lparen = token "(" in
  let* expr = access_expression in
  let* _qmark = optional (token "?") in
  let* rparen = token ")" in
  pure
    (G.Call
      (G.IdSpecial (G.Typeof, getType.tok) |> G.e,
      (lparen.tok, [ G.Arg expr ], rparen.tok)) |> G.e)
) __n

and cast_expression : G.expr parser = fun __n -> (
  choice [
    c_type_expression;
    direct_cast_expression;
    try_cast_expression
  ]
) __n

and c_type_expression : G.expr parser = fun __n -> (
  (* c_type_expression -> 'CType' '(' expression ',' type ')' *)
  let* t = token "CTYPE" in
  let* _lparen = token "(" in
  let* expr = expression in
  let* _comma = token "," in
  let* typ = type_ in
  let* _rparen = token ")" in
  pure (G.Cast (typ, t.tok, expr) |> G.e)
) __n

and direct_cast_expression : G.expr parser = fun __n -> (
  (* direct_cast_expression -> 'DirectCast' '(' expression ',' type ')' *)
  let* t = token "DIRECTCAST" in
  let* _lparen = token "(" in
  let* expr = expression in
  let* _comma = token "," in
  let* typ = type_ in
  let* _rparen = token ")" in
  pure (G.Cast (typ, t.tok, expr) |> G.e)
) __n

and try_cast_expression : G.expr parser = fun __n -> (
  (* try_cast_expression -> 'TryCast' '(' expression ',' type ')' *)
  let* t = token "TRYCAST" in
  let* _lparen = token "(" in
  let* expr = expression in
  let* _comma = token "," in
  let* typ = type_ in
  let* _rparen = token ")" in
  pure (G.Cast (typ, t.tok, expr) |> G.e)
) __n

and get_xml_namespace_expression : G.expr parser = fun __n -> (
  (* get_xml_namespace_expression -> 'GetXmlNamespace' '(' xml_prefix_name? ')' *)
  let* get_ns = token "GETXMLNAMESPACE" in
  let* lparen = token "(" in
  let* ns_opt = optional x_name in
  let* rparen = token ")" in
  pure (G.Call (G.N (make_name (ident_of_token get_ns) None) |> G.e,
        (lparen.tok,
         ns_opt
         |> Option.map (fun n -> G.Arg (G.N n |> G.e))
         |> Option.to_list,
         rparen.tok)) |> G.e)
) __n

and instance_expression : G.expr parser = fun __n -> (
  choice [
    begin
      (* instance_expression -> 'Me' *)
      let* t = token "ME" in
      pure (G.IdSpecial (G.This, t.tok) |> G.e)
    end;
    begin
      (* instance_expression -> 'MyBase' *)
      let* t = token "MYBASE" in
      pure (G.IdSpecial (G.Super, t.tok) |> G.e)
    end;
    begin
      (* instance_expression -> 'MyClass' *)
      (* NOTE: MyClass is not directly expressible in the generic AST at the moment,
       * While "This" is the closest, it can still cause problems in x-function tainting*)
      let* t = token "MYCLASS" in
      pure (G.IdSpecial (G.This, t.tok) |> G.e)
    end;
  ]
) __n

and interpolated_string_expression : G.expr parser = fun __n -> (
  (* interpolated_string_expression -> '$"' interpolated_string_content* '"' *)
  let* dollar_dquote = token "$\"" in
  let* args = list_of interpolated_string_content in
  let* dquote = token "\"" in
  let func =
    G.IdSpecial (G.ConcatString G.InterpolatedConcat, dollar_dquote.tok) |> G.e
  in
  pure (G.Call (func, (dollar_dquote.tok, args, dquote.tok)) |> G.e)
) __n

and interpolated_string_content : G.argument parser = fun __n -> (
  choice [
    begin
      (* interpolated_string_content -> '<STRING_SEGMENT>' *)
      let* seg = token "<STRING_SEGMENT>" in
      pure (G.Arg (G.L (G.String (fb (seg.content, seg.tok))) |> G.e))
    end;
    begin
      (* interpolated_string_content -> expression interpolation_alignment_clause? (':' interpolation_format_char* )? *)
      let* expr = expression in
      let* alignment_opt = optional interpolation_alignment_clause in
      let* format_opt = optional
        begin
          let* _colon = token ":" in
          list_of interpolation_format_char
        end
      in
      let combine_format (ts_opt : T.t list option) : G.expr option =
        match ts_opt with
        | Some ((x :: xs) as ts) ->
            let t = Tok.combine_toks x.T.tok (List.map (fun x -> x.T.tok) xs) in
            let s = String.concat "" (List.map (fun x -> x.T.content) ts) in
            Some (G.L (G.String (fb (s, t))) |> G.e)
        | _ ->
            None
      in
        match alignment_opt, format_opt with
        | None, None ->
            pure (G.Arg expr)
        | _ ->
            let args =
              [Some expr; alignment_opt; combine_format format_opt]
              |> List.filter_map (fun opt -> opt)
              |> List.map (fun expr -> G.Arg expr)
            in
            let func =
              G.IdSpecial (G.InterpolatedElement, Tok.unsafe_fake_tok "") |> G.e
            in
            pure (G.Arg (G.Call (func, fb args) |> G.e))
    end;
  ]
) __n

and interpolation_alignment_clause : G.expr parser = fun __n -> (
  (* interpolation_alignment_clause -> ',' unary_expression *)
  let* _comma = token "," in
  unary_expression
) __n

and interpolation_format_char : T.t parser = fun __n -> (
  choice [
    begin
      (* interpolation_format_char -> @lookahead_not('\\"') @lookahead_not('$\\"') '<OPERATOR>' *)
      let* _ = look_ahead_not "\"" in
      let* _ = look_ahead_not "$\"" in
      token "<OPERATOR>"
    end;
    begin
      (*terpolation_format_char -> '.' *)
      token "."
    end;
    begin
      (*terpolation_format_char -> '<IDENT>' *)
      token "<IDENT>"
    end;
    begin
      (*terpolation_format_char -> '<KEYWORD>' *)
      token "<KEYWORD>"
    end;
    begin
      (*terpolation_format_char -> '<INT>' *)
      token "<INT>"
    end;
    begin
      (*terpolation_format_char -> '<FLOAT>' *)
      token "<FLOAT>"
    end;
  ]
) __n

and lambda_expression : G.expr parser = fun __n -> (
  choice [ multi_line_lambda_expression; single_line_lambda_expression ]
) __n

and lambda_modifier : G.attribute parser = fun __n -> (
  choice [
    begin
      (* lambda_modifier -> 'Async' *)
      let* t = token "ASYNC" in
      pure (G.KeywordAttr (G.Async, t.tok))
    end;
    begin
      (* lambda_modifier -> 'Iterator' *)
      let* t = token "ITERATOR" in
      pure (G.KeywordAttr (G.Async, t.tok))
    end;
  ]
) __n

and single_line_lambda_expression : G.expr parser = fun __n -> (
  let* _modifiers = list_of lambda_modifier in
  choice [
    begin
      (* single_line_lambda_expression -> lambda_modifier* 'Function' parameter_list expression *)
      let* fun_ = token "FUNCTION" in
      let* params = optional parameter_list in
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* expr = expression in
      let fdef =
        G.{ fkind = (G.Arrow, fun_.tok);
            fparams =
              (match params with
              | Some p -> p
              | None -> fb []);
            frettype = None;
            fbody = G.FBExpr expr }
      in
      pure (G.Lambda fdef |> G.e)
    end;
    begin
      (* single_line_lambda_expression -> lambda_modifier* 'Sub' parameter_list single_line_statement *)
      let* sub = token "SUB" in
      let* params = optional parameter_list in
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* stmts = single_line_statement in
      let fdef =
        G.{ fkind = (G.Arrow, sub.tok);
            fparams =
              (match params with
              | Some p -> p
              | None -> fb []);
            frettype = None;
            fbody = G.FBStmt (stmt_of_stmts stmts) }
      in
      pure (G.Lambda fdef |> G.e)
    end;
  ]
) __n

and multi_line_lambda_expression : G.expr parser = fun __n -> (
  let* _modifiers = list_of lambda_modifier in
  choice [
    begin
      (* multi_line_lambda_expression -> lambda_modifier* 'Function' parameter_list simple_as_clause? @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'Function' *)
      let* fun_ = token "FUNCTION" in
      let* params = optional parameter_list in
      (* let* _simple_as_clause_opt1 = optional simple_as_clause in *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      let* stmt = statements_block in
      let* _ = optional (token ":") in
      let* _end = token "END" in
      let* _function = token "FUNCTION" in
      let fdef =
        G.{ fkind = (G.Arrow, fun_.tok);
            fparams =
              (match params with
              | Some p -> p
              | None -> fb []);
            frettype = None;
            fbody = G.FBStmt stmt }
      in
      pure (G.Lambda fdef |> G.e)
    end;
    begin
      (* multi_line_lambda_expression -> lambda_modifier* 'Sub' parameter_list @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'Sub' *)
      let* sub = token "SUB" in
      let* params = optional parameter_list in
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      let* stmt = statements_block in
      let* _ = optional (token ":") in
      let* _end = token "END" in
      let* _sub = token "SUB" in
      let fdef =
        G.{ fkind = (G.Arrow, sub.tok);
            fparams =
              (match params with
              | Some p -> p
              | None -> fb []);
            frettype = None;
            fbody = G.FBStmt stmt }
      in
      pure (G.Lambda fdef |> G.e)
    end;
  ]
) __n

and name_of_expression : G.expr parser = fun __n -> (
  (* name_of_expression -> 'NameOf' '(' expression ')' *)
  let* nameOf = token "NAMEOF" in
  let* lparen = token "(" in
  let* exp = expression in
  let* rparen = token ")" in
  pure (G.Call (expr_of_id (ident_of_token nameOf),
               (lparen.tok, [G.Arg exp], rparen.tok)) |> G.e)
) __n

and predefined_cast_expression : G.expr parser = fun __n -> (
  (* predefined_cast_expression -> ('CBool' | ... | 'CUShort') '(' expression ')' *)
  let* typ_tok = choice [
      token "CBOOL";
      token "CBYTE";
      token "CCHAR";
      token "CDATE";
      token "CDBL";
      token "CDEC";
      token "CINT";
      token "CLNG";
      token "COBJ";
      token "CSBYTE";
      token "CLNG";
      token "CSBYTE";
      token "CSHORT";
      token "CSNG";
      token "CSTR";
      token "CUINT";
      token "CULNG";
      token "CUSHORT";
    ]
  in
  let* _lparen = token "(" in
  let* exp = expression in
  let* _rparen = token ")" in
  let t = {
    G.t = TyN (make_name (ident_of_token typ_tok) None);
    G.t_attrs = [] }
  in
  pure (G.Cast (t, typ_tok.tok, exp) |> G.e)
) __n

and query_expression : G.any parser = fun __n -> (
  (* query_expression -> query_clause+ *)
  let* query_clauses1 = ne_list_of query_clause in
  pure (xRule "query_expression" 0 [xList(query_clauses1)])
) __n

and query_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* query_clause -> from_clause *)
      let* from_clause1 = from_clause in
      pure (xRule "query_clause" 0 [from_clause1])
    end;
    begin
      (* query_clause -> aggregate_clause *)
      let* aggregate_clause1 = aggregate_clause in
      pure (xRule "query_clause" 1 [aggregate_clause1])
    end;
    begin
      (* query_clause -> distinct_clause *)
      let* distinct_clause1 = distinct_clause in
      pure (xRule "query_clause" 2 [distinct_clause1])
    end;
    begin
      (* query_clause -> group_by_clause *)
      let* group_by_clause1 = group_by_clause in
      pure (xRule "query_clause" 3 [group_by_clause1])
    end;
    begin
      (* query_clause -> join_clause *)
      let* join_clause1 = join_clause in
      pure (xRule "query_clause" 4 [join_clause1])
    end;
    begin
      (* query_clause -> let_clause *)
      let* let_clause1 = let_clause in
      pure (xRule "query_clause" 5 [let_clause1])
    end;
    begin
      (* query_clause -> order_by_clause *)
      let* order_by_clause1 = order_by_clause in
      pure (xRule "query_clause" 6 [order_by_clause1])
    end;
    begin
      (* query_clause -> partition_clause *)
      let* partition_clause1 = partition_clause in
      pure (xRule "query_clause" 7 [partition_clause1])
    end;
    begin
      (* query_clause -> partition_while_clause *)
      let* partition_while_clause1 = partition_while_clause in
      pure (xRule "query_clause" 8 [partition_while_clause1])
    end;
    begin
      (* query_clause -> select_clause *)
      let* select_clause1 = select_clause in
      pure (xRule "query_clause" 9 [select_clause1])
    end;
    begin
      (* query_clause -> where_clause *)
      let* where_clause1 = where_clause in
      pure (xRule "query_clause" 10 [where_clause1])
    end;
  ]
) __n

and aggregate_clause : G.any parser = fun __n -> (
  (* aggregate_clause -> 'Aggregate' collection_range_variable (',' collection_range_variable)* query_clause* 'Into' aggregation_range_variable (',' aggregation_range_variable)* *)
  let* aggregate1 = token "AGGREGATE" in
  let* collection_range_variable1 = collection_range_variable in
  let* comma_collection_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* collection_range_variable1 = collection_range_variable in
      pure (xGroup([xToken(comma1); collection_range_variable1]))
    end
  in
  let* query_clauses1 = list_of query_clause in
  let* into1 = token "INTO" in
  let* aggregation_range_variable1 = aggregation_range_variable in
  let* comma_aggregation_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* aggregation_range_variable1 = aggregation_range_variable in
      pure (xGroup([xToken(comma1); aggregation_range_variable1]))
    end
  in
  pure (xRule "aggregate_clause" 0 [xToken(aggregate1); collection_range_variable1; xList(comma_collection_range_variables1); xList(query_clauses1); xToken(into1); aggregation_range_variable1; xList(comma_aggregation_range_variables1)])
) __n

and collection_range_variable : G.any parser = fun __n -> (
  (* collection_range_variable -> modified_identifier simple_as_clause? 'In' expression *)
  let* modified_identifier1 = modified_identifier in
  let* _simple_as_clause_opt1 = optional simple_as_clause in
  let* in1 = token "IN" in
  let* expression1 = expression in
  pure (xRule "collection_range_variable" 0 [modified_identifier1; xToken(in1); G.E expression1])
) __n

and aggregation_range_variable : G.any parser = fun __n -> (
  (* aggregation_range_variable -> variable_name_equals? aggregation *)
  let* variable_name_equals_opt1 = optional variable_name_equals in
  let* aggregation1 = aggregation in
  pure (xRule "aggregation_range_variable" 0 [xOptional(variable_name_equals_opt1); aggregation1])
) __n

and variable_name_equals : G.any parser = fun __n -> (
  (* variable_name_equals -> modified_identifier simple_as_clause? '=' *)
  let* modified_identifier1 = modified_identifier in
  let* _simple_as_clause_opt1 = optional simple_as_clause in
  let* eq1 = token "=" in
  pure (xRule "variable_name_equals" 0 [modified_identifier1; xToken(eq1)])
) __n

and distinct_clause : G.any parser = fun __n -> (
  (* distinct_clause -> 'Distinct' *)
  let* distinct1 = token "DISTINCT" in
  pure (xRule "distinct_clause" 0 [xToken(distinct1)])
) __n

and from_clause : G.any parser = fun __n -> (
  (* from_clause -> 'From' collection_range_variable (',' collection_range_variable)* *)
  let* from1 = token "FROM" in
  let* collection_range_variable1 = collection_range_variable in
  let* comma_collection_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* collection_range_variable1 = collection_range_variable in
      pure (xGroup([xToken(comma1); collection_range_variable1]))
    end
  in
  pure (xRule "from_clause" 0 [xToken(from1); collection_range_variable1; xList(comma_collection_range_variables1)])
) __n

and group_by_clause : G.any parser = fun __n -> (
  (* group_by_clause -> 'Group' (expression_range_variable (',' expression_range_variable)* )? 'By' expression_range_variable (',' expression_range_variable)* 'Into' aggregation_range_variable (',' aggregation_range_variable)* *)
  let* group1 = token "GROUP" in
  let* expression_range_variable_comma_expression_range_variab_opt1 = optional 
    begin
      let* expression_range_variable1 = expression_range_variable in
      let* comma_expression_range_variables1 = list_of 
        begin
          let* comma1 = token "," in
          let* expression_range_variable1 = expression_range_variable in
          pure (xGroup([xToken(comma1); expression_range_variable1]))
        end
      in
      pure (xGroup([expression_range_variable1; xList(comma_expression_range_variables1)]))
    end
  in
  let* by1 = token "BY" in
  let* expression_range_variable1 = expression_range_variable in
  let* comma_expression_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* expression_range_variable1 = expression_range_variable in
      pure (xGroup([xToken(comma1); expression_range_variable1]))
    end
  in
  let* into1 = token "INTO" in
  let* aggregation_range_variable1 = aggregation_range_variable in
  let* comma_aggregation_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* aggregation_range_variable1 = aggregation_range_variable in
      pure (xGroup([xToken(comma1); aggregation_range_variable1]))
    end
  in
  pure (xRule "group_by_clause" 0 [xToken(group1); xOptional(expression_range_variable_comma_expression_range_variab_opt1); xToken(by1); expression_range_variable1; xList(comma_expression_range_variables1); xToken(into1); aggregation_range_variable1; xList(comma_aggregation_range_variables1)])
) __n

and expression_range_variable : G.any parser = fun __n -> (
  (* expression_range_variable -> variable_name_equals? expression *)
  let* variable_name_equals_opt1 = optional variable_name_equals in
  let* expression1 = expression in
  pure (xRule "expression_range_variable" 0 [xOptional(variable_name_equals_opt1); G.E expression1])
) __n

and join_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* join_clause -> group_join_clause *)
      let* group_join_clause1 = group_join_clause in
      pure (xRule "join_clause" 0 [group_join_clause1])
    end;
    begin
      (* join_clause -> simple_join_clause *)
      let* simple_join_clause1 = simple_join_clause in
      pure (xRule "join_clause" 1 [simple_join_clause1])
    end;
  ]
) __n

and group_join_clause : G.any parser = fun __n -> (
  (* group_join_clause -> 'Group' 'Join' collection_range_variable (',' collection_range_variable)* join_clause* 'On' join_condition ('And' join_condition)* 'Into' aggregation_range_variable (',' aggregation_range_variable)* *)
  let* group1 = token "GROUP" in
  let* join1 = token "JOIN" in
  let* collection_range_variable1 = collection_range_variable in
  let* comma_collection_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* collection_range_variable1 = collection_range_variable in
      pure (xGroup([xToken(comma1); collection_range_variable1]))
    end
  in
  let* join_clauses1 = list_of join_clause in
  let* on1 = token "ON" in
  let* join_condition1 = join_condition in
  let* and_join_conditions1 = list_of 
    begin
      let* and1 = token "AND" in
      let* join_condition1 = join_condition in
      pure (xGroup([xToken(and1); join_condition1]))
    end
  in
  let* into1 = token "INTO" in
  let* aggregation_range_variable1 = aggregation_range_variable in
  let* comma_aggregation_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* aggregation_range_variable1 = aggregation_range_variable in
      pure (xGroup([xToken(comma1); aggregation_range_variable1]))
    end
  in
  pure (xRule "group_join_clause" 0 [xToken(group1); xToken(join1); collection_range_variable1; xList(comma_collection_range_variables1); xList(join_clauses1); xToken(on1); join_condition1; xList(and_join_conditions1); xToken(into1); aggregation_range_variable1; xList(comma_aggregation_range_variables1)])
) __n

and join_condition : G.any parser = fun __n -> (
  (* join_condition -> expression 'Equals' expression *)
  let* expression1 = expression in
  let* equals1 = token "EQUALS" in
  let* expression2 = expression in
  pure (xRule "join_condition" 0 [G.E expression1; xToken(equals1); G.E expression2])
) __n

and simple_join_clause : G.any parser = fun __n -> (
  (* simple_join_clause -> 'Join' collection_range_variable (',' collection_range_variable)* join_clause* 'On' join_condition ('And' join_condition)* *)
  let* join1 = token "JOIN" in
  let* collection_range_variable1 = collection_range_variable in
  let* comma_collection_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* collection_range_variable1 = collection_range_variable in
      pure (xGroup([xToken(comma1); collection_range_variable1]))
    end
  in
  let* join_clauses1 = list_of join_clause in
  let* on1 = token "ON" in
  let* join_condition1 = join_condition in
  let* and_join_conditions1 = list_of 
    begin
      let* and1 = token "AND" in
      let* join_condition1 = join_condition in
      pure (xGroup([xToken(and1); join_condition1]))
    end
  in
  pure (xRule "simple_join_clause" 0 [xToken(join1); collection_range_variable1; xList(comma_collection_range_variables1); xList(join_clauses1); xToken(on1); join_condition1; xList(and_join_conditions1)])
) __n

and let_clause : G.any parser = fun __n -> (
  (* let_clause -> 'Let' expression_range_variable (',' expression_range_variable)* *)
  let* let1 = token "LET" in
  let* expression_range_variable1 = expression_range_variable in
  let* comma_expression_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* expression_range_variable1 = expression_range_variable in
      pure (xGroup([xToken(comma1); expression_range_variable1]))
    end
  in
  pure (xRule "let_clause" 0 [xToken(let1); expression_range_variable1; xList(comma_expression_range_variables1)])
) __n

and order_by_clause : G.any parser = fun __n -> (
  (* order_by_clause -> 'Order' 'By' ordering (',' ordering)* *)
  let* order1 = token "ORDER" in
  let* by1 = token "BY" in
  let* ordering1 = ordering in
  let* comma_orderings1 = list_of 
    begin
      let* comma1 = token "," in
      let* ordering1 = ordering in
      pure (xGroup([xToken(comma1); ordering1]))
    end
  in
  pure (xRule "order_by_clause" 0 [xToken(order1); xToken(by1); ordering1; xList(comma_orderings1)])
) __n

and ordering : G.any parser = fun __n -> (
  (* ordering -> expression ascending_ordering? *)
  let* expression1 = expression in
  let* ascending_ordering_opt1 = optional ascending_ordering in
  pure (xRule "ordering" 0 [G.E expression1; xOptional(ascending_ordering_opt1)])
) __n

and ascending_ordering : G.any parser = fun __n -> (
  choice [
    begin
      (* ascending_ordering -> 'Ascending' *)
      let* ascending1 = token "ASCENDING" in
      pure (xRule "ascending_ordering" 0 [xToken(ascending1)])
    end;
    begin
      (* ascending_ordering -> 'Descending' *)
      let* descending1 = token "DESCENDING" in
      pure (xRule "ascending_ordering" 1 [xToken(descending1)])
    end;
  ]
) __n

and partition_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* partition_clause -> skip_clause *)
      let* skip_clause1 = skip_clause in
      pure (xRule "partition_clause" 0 [skip_clause1])
    end;
    begin
      (* partition_clause -> take_clause *)
      let* take_clause1 = take_clause in
      pure (xRule "partition_clause" 1 [take_clause1])
    end;
  ]
) __n

and skip_clause : G.any parser = fun __n -> (
  (* skip_clause -> 'Skip' expression *)
  let* skip1 = token "SKIP" in
  let* expression1 = expression in
  pure (xRule "skip_clause" 0 [xToken(skip1); G.E expression1])
) __n

and take_clause : G.any parser = fun __n -> (
  (* take_clause -> 'Take' expression *)
  let* take1 = token "TAKE" in
  let* expression1 = expression in
  pure (xRule "take_clause" 0 [xToken(take1); G.E expression1])
) __n

and partition_while_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* partition_while_clause -> skip_while_clause *)
      let* skip_while_clause1 = skip_while_clause in
      pure (xRule "partition_while_clause" 0 [skip_while_clause1])
    end;
    begin
      (* partition_while_clause -> take_while_clause *)
      let* take_while_clause1 = take_while_clause in
      pure (xRule "partition_while_clause" 1 [take_while_clause1])
    end;
  ]
) __n

and skip_while_clause : G.any parser = fun __n -> (
  (* skip_while_clause -> 'Skip' 'While' expression *)
  let* skip1 = token "SKIP" in
  let* while1 = token "WHILE" in
  let* expression1 = expression in
  pure (xRule "skip_while_clause" 0 [xToken(skip1); xToken(while1); G.E expression1])
) __n

and take_while_clause : G.any parser = fun __n -> (
  (* take_while_clause -> 'Take' 'While' expression *)
  let* take1 = token "TAKE" in
  let* while1 = token "WHILE" in
  let* expression1 = expression in
  pure (xRule "take_while_clause" 0 [xToken(take1); xToken(while1); G.E expression1])
) __n

and select_clause : G.any parser = fun __n -> (
  (* select_clause -> 'Select' expression_range_variable (',' expression_range_variable)* *)
  let* select1 = token "SELECT" in
  let* expression_range_variable1 = expression_range_variable in
  let* comma_expression_range_variables1 = list_of 
    begin
      let* comma1 = token "," in
      let* expression_range_variable1 = expression_range_variable in
      pure (xGroup([xToken(comma1); expression_range_variable1]))
    end
  in
  pure (xRule "select_clause" 0 [xToken(select1); expression_range_variable1; xList(comma_expression_range_variables1)])
) __n

and where_clause : G.any parser = fun __n -> (
  (* where_clause -> 'Where' expression *)
  let* where1 = token "WHERE" in
  let* expression1 = expression in
  pure (xRule "where_clause" 0 [xToken(where1); G.E expression1])
) __n

and conditional_expression : G.expr parser = fun __n -> (
  (* conditional_expression -> 'If' '(' expression ',' expression (',' expression)? ')' *)
  let* _if1 = token "IF" in
  let* _lparen = token "(" in
  let* expression1 = expression in
  let* _comma1 = token "," in
  let* expression2 = expression in
  let* expression3_opt = optional
    begin
      let* _comma2 = token "," in
      expression
    end
  in
  let* rparen = token ")" in
  match expression3_opt with
  | Some expression3 ->
      pure (G.Conditional (expression1, expression2, expression3) |> G.e)
  | None ->
      (* Ugly to point to rparen as the Null token, but it's safer than fake *)
      pure (G.Conditional (expression1, expression2, G.L (G.Null rparen.tok) |> G.e) |> G.e)
) __n

and tuple_expression_argument : (int -> G.expr) parser = fun __n -> (
  (* argument -> (identifier_or_keyword ':=')? expression ('To' expression)? *)
  let* idork_opt = optional
    begin
      let* idork = identifier_or_keyword in
      let* colon_eq = token ":=" in
      pure (idork, colon_eq)
    end
  in
  let* exp = expression in
  match idork_opt with
  | Some (idork, colon_eq) ->
      pure (fun _ -> G.AssignOp
                       (G.N idork |> G.e, (G.Eq, colon_eq.tok), exp) |> G.e)
  | None ->
    pure (fun i -> G.AssignOp
                     ((let item = "Item" ^ string_of_int (i + 1) in
                      G.N (make_name (item, Tok.unsafe_fake_tok item) None) |> G.e),
                     (G.Eq, Tok.unsafe_fake_tok "="), exp) |> G.e)
) __n

and tuple_expression : G.expr parser = fun __n -> (
  (* tuple_expression -> '(' argument (',' argument)+ ')' *)
  let* lparen = token "(" in
  let* arg = tuple_expression_argument in
  let* args = ne_list_of
    begin
      let* _comma1 = token "," in
      tuple_expression_argument
    end
  in
  let* rparen = token ")" in
  let body = List.mapi (|>) (arg :: args) in
  pure (G.Container (G.Tuple, (lparen.tok, body, rparen.tok)) |> G.e)
) __n

and is_or_is_not : (bool * T.t) parser = fun __n -> (
  choice [
    begin
      (* is_or_is_not -> 'Is' *)
      let* is = token "IS" in
      pure (true, is)
    end;
    begin
      (* is_or_is_not -> 'IsNot' *)
      let* is_not = token "ISNOT" in
      pure (false, is_not)
    end;
  ]
) __n

and type_of_expression : G.expr parser = fun __n -> (
  (* type_of_expression -> 'TypeOf' await_expression is_or_is_not type *)
  let* typeOf = token "TYPEOF" in
  let* expr = await_expression in
  let* (is_or_not, is_t) = is_or_is_not in
  let* typ = type_ in
  let is_expr =
    G.Call (G.IdSpecial (G.Instanceof, typeOf.tok) |> G.e,
            fb [ G.Arg expr; G.ArgType typ ]) |> G.e
  in
  if is_or_not
    then pure is_expr
  else
    pure (G.Call (G.IdSpecial (G.Op G.Not, is_t.tok) |> G.e,
                  fb [ G.Arg is_expr ]) |> G.e)
) __n

and address_of_expression : G.expr parser = fun __n -> (
  (* address_of_expression -> 'AddressOf' unary_expression *)
  let* t = token "ADDRESSOF" in
  let* e = unary_expression in
  pure (G.Ref (t.tok, e) |> G.e)
) __n

and type_ : G.type_ parser = fun __n -> (
  (* type -> base_type type_modifier* *)
  let* base = base_type in
  let* modifiers = list_of type_modifier in
  pure (List.fold_left (|>) base modifiers)
) __n

and base_type : G.type_ parser = fun __n -> (
  choice [
    begin
      (* base_type -> name *)
      let* n = name in
      pure { G.t = TyN n; G.t_attrs = [] }
    end;
    begin
      (* base_type -> tuple_type *)
      tuple_type
    end;
  ]
) __n

and type_modifier : (G.type_ -> G.type_) parser = fun __n -> (
  choice [
    begin
      (* type_modifier -> array_rank_specifier *)
      let* (l, _rank, r) = array_rank_specifier in
      pure (fun base_type ->
        G.{ t = G.TyArray ((l, None, r), base_type);
            t_attrs = [] })
    end;
    begin
      (* type_modifier -> '?' *)
      let* qmark = token "?" in
      pure (fun base_type ->
        G.{ t = G.TyQuestion (base_type, qmark.tok);
            t_attrs = [] })
    end;
  ]
) __n

and tuple_type : G.type_ parser = fun __n -> (
  (* tuple_type -> '(' tuple_element (',' tuple_element)+ ')' *)
  let* lparen = token "(" in
  let* elem = tuple_element in
  let* elems = ne_list_of
    begin
      let* _comma1 = token "," in
      tuple_element
    end
  in
  let* rparen = token ")" in
  let fields = List.mapi (|>) (elem :: elems) in
  pure ( { G.t = G.TyRecordAnon
                   ((G.Object, Tok.unsafe_fake_tok "tuple"),
                    (lparen.tok, fields, rparen.tok));
           G.t_attrs = [] })
) __n

and tuple_element : (int -> G.field) parser = fun __n -> (
  let* id_opt = optional
    begin
      let* id = identifier_name in
      let* _as = token "AS" in
      pure id
    end
  in
  let* typ = type_ in
  match id_opt with
  | Some id ->
      let entity =
        { G.name = G.EN (make_name id None);
          G.attrs = [];
          G.tparams = None }
      in
      let def_kind = G.VarDef
        { G.vinit = None;
          G.vtype = Some typ;
          G.vtok = None }
      in
      pure (fun _ -> G.F (G.DefStmt (entity, def_kind) |> G.s))
  | None ->
      pure (fun i ->
        let entity =
          { G.name = G.EN (make_name (let n = "Item" ^ string_of_int (i + 1) in (n, Tok.unsafe_fake_tok n)) None);
            G.attrs = [];
            G.tparams = None }
        in
        let def_kind = G.VarDef
          { G.vinit = None;
            G.vtype = Some typ;
            G.vtok = None }
        in
        G.F (G.DefStmt (entity, def_kind) |> G.s))
) __n

and name : G.name parser = fun __n -> (
  (* name -> base_name ('.' name_reference)* *)
  let* base = base_name in
  let* refs = list_of
    begin
      let* _dot = token "." in
      identifier_or_keyword
    end
  in
  pure (collapse_names (base :: refs))
) __n

and base_name : G.name parser = fun __n -> (
  (* base_name -> identifier_name type_argument_list? *)
  let* id = identifier_name in
  let* type_args = optional type_argument_list in
  pure (make_name id type_args)
) __n

and xml_cdata : G.expr parser = fun __n -> (
  (* xml_cdata -> '<CDATA>' *)
  let* t = token "<CDATA>" in
  pure (G.L (G.String (fb (T.extract_string_content t, t.tok))) |> G.e)
) __n

and x_expression : G.xml parser = fun __n -> (
  choice [
    begin
      (* x_expression -> x_element *)
      x_element
    end;
    begin
      (* x_expression -> x_declaration *)
      x_declaration
    end;
    begin
      (* x_expression -> x_tag_single *)
      let* (lt, name, xml_attrs, gt) = x_tag_single in
      pure
        G.{ xml_kind = G.XmlSingleton (lt.tok, name, gt.tok);
            xml_attrs;
            xml_body = []}
    end;
    begin
      (* x_expression -> x_tag_comment *)
      x_tag_comment
    end;
  ]
) __n

and x_element : G.xml parser = fun __n -> (
  (* x_element -> x_tag_start x_body_element* x_tag_end *)
  let* (lt, name, xml_attrs, rt) = x_tag_start in
  let* xml_body = list_of x_body_element in
  let* tag_end_token = x_tag_end in
  pure
    G.{ xml_kind = G.XmlClassic (lt.tok, name, rt.tok, tag_end_token);
        xml_attrs;
        xml_body }
) __n

(* TODO: don't ignore the declaration *)
and x_declaration : G.xml parser = fun __n -> (
  (* x_declaration -> '<?' x_tag_inside '?>' x_expression *)
  let* _lt = token "<?" in
  let* _tag = x_tag_inside in
  let* _gt = token "?>" in
  x_expression
) __n

and x_non_xml_operator : T.t parser = fun __n -> (
  choice [
    begin
      (* x_non_xml_operator -> '!' *)
      token "!"
    end;
    begin
      (* x_non_xml_operator -> '\\'' *)
      token "'"
    end;
    begin
      (* x_non_xml_operator -> '#' *)
      token "#"
    end;
    begin
      (* x_non_xml_operator -> '$\\'' *)
      token "$'"
    end;
    begin
      (* x_non_xml_operator -> '%>' *)
      token "%>"
    end;
    begin
      (* x_non_xml_operator -> '&' *)
      token "&"
    end;
    begin
      (* x_non_xml_operator -> '&=' *)
      token "&="
    end;
    begin
      (* x_non_xml_operator -> '*' *)
      token "*"
    end;
    begin
      (* x_non_xml_operator -> '*=' *)
      token "*="
    end;
    begin
      (* x_non_xml_operator -> '+' *)
      token "+"
    end;
    begin
      (* x_non_xml_operator -> '+=' *)
      token "+="
    end;
    begin
      (* x_non_xml_operator -> '-' *)
      token "-"
    end;
    begin
      (* x_non_xml_operator -> '-=' *)
      token "-="
    end;
    begin
      (* x_non_xml_operator -> '.' *)
      token "."
    end;
    begin
      (* x_non_xml_operator -> '/' *)
      token "/"
    end;
    begin
      (* x_non_xml_operator -> '/=' *)
      token "/="
    end;
    begin
      (* x_non_xml_operator -> '/>' *)
      token "/>"
    end;
    begin
      (* x_non_xml_operator -> ':' *)
      token ":"
    end;
    begin
      (* x_non_xml_operator -> ':=' *)
      token ":="
    end;
    begin
      (* x_non_xml_operator -> '<<' *)
      token "<<"
    end;
    begin
      (* x_non_xml_operator -> '<<=' *)
      token "<<="
    end;
    begin
      (* x_non_xml_operator -> '<=' *)
      token "<="
    end;
    begin
      (* x_non_xml_operator -> '<>' *)
      token "<>"
    end;
    begin
      (* x_non_xml_operator -> '=' *)
      token "="
    end;
    begin
      (* x_non_xml_operator -> '>' *)
      token ">"
    end;
    begin
      (* x_non_xml_operator -> '>=' *)
      token ">="
    end;
    begin
      (* x_non_xml_operator -> '>>' *)
      token ">>"
    end;
    begin
      (* x_non_xml_operator -> '>>=' *)
      token ">>="
    end;
    begin
      (* x_non_xml_operator -> '?' *)
      token "?"
    end;
    begin
      (* x_non_xml_operator -> '?.' *)
      token "?."
    end;
    begin
      (* x_non_xml_operator -> '?>' *)
      token "?>"
    end;
    begin
      (* x_non_xml_operator -> '@' *)
      token "@"
    end;
    begin
      (* x_non_xml_operator -> '.@' *)
      token ".@"
    end;
    begin
      (* x_non_xml_operator -> '.@<' *)
      token ".@"
    end;
    begin
      (* x_non_xml_operator -> '\\' *)
      token "\\"
    end;
    begin
      (* x_non_xml_operator -> '\\=' *)
      token "\\="
    end;
    begin
      (* x_non_xml_operator -> '^' *)
      token "^"
    end;
    begin
      (* x_non_xml_operator -> '^=' *)
      token "^="
    end;
    begin
      (* x_non_xml_operator -> ';' *)
      token ";"
    end;
  ]
) __n

and x_body_element : G.xml_body parser = fun __n -> (
  choice [
    begin
      (* x_body_element -> x_expression *)
      let* xml = x_expression in
      pure (G.XmlXml xml)
    end;
    begin
      (* x_body_element -> x_tag_embed_expression *)
      let* (l, expr, r) = x_tag_embed_expression in
      pure (G.XmlExpr (l, Some expr, r))
    end;
    begin
      let* t = choice [
        begin
          (* x_body_element -> '<IDENT>' *)
          token "<IDENT>"
        end;
        begin
          (* x_body_element -> '<KEYWORD>' *)
          token "<KEYWORD>"
        end;
        begin
          (* x_body_element -> '<PUNCTUATION>' *)
          token "<PUNCTUATION>"
        end;
        begin
          (* x_body_element -> '<INT>' *)
          token "<INT>"
        end;
        begin
          (* x_body_element -> '<FLOAT>' *)
          token "<FLOAT>"
        end;
        begin
          (* x_body_element -> '<CHAR>' *)
          token "<CHAR>"
        end;
        begin
          (* x_body_element -> '<STRING>' *)
          token "<STRING>"
        end;
        begin
          (* x_body_element -> '<STRING_SEGMENT>' *)
          token "<STRING_SEGMENT>"
        end;
        begin
          (* x_body_element -> '<DATE>' *)
          token "<DATE>"
        end;
        begin
          (* x_body_element -> '<CDATA>' *)
          token "<CDATA>"
        end;
        begin
          (* x_body_element -> '<OTHER>' *)
          token "<OTHER>"
        end;
        begin
          (* x_body_element -> x_non_xml_operator *)
          x_non_xml_operator
        end;
      ]
      in
      pure (G.XmlText (t.content, t.tok))
    end
  ]
) __n

and x_tag_start : (T.t * G.name * G.xml_attribute list * T.t) parser = fun __n -> (
  (* x_tag_start -> '<' x_tag_inside '>' *)
  let* lt = token "<" in
  let* (name, attrs) = x_tag_inside in
  let* gt = token ">" in
  pure (lt, name, attrs, gt)
) __n

and x_tag_end : G.tok parser = fun __n -> (
  (* x_tag_end -> '</' x_tag_inside '>' *)
  let* lt_slash = token "</" in
  let* toks = x_name_as_tokens in
  let* gt = token ">" in
  pure (Tok.combine_toks lt_slash.tok (List.map (fun t -> t.T.tok) (toks @ [gt])))
) __n

and x_tag_single : (T.t * G.name * G.xml_attribute list * T.t) parser = fun __n -> (
  (* x_tag_single -> '<' x_tag_inside '/>' *)
  let* lt = token "<" in
  let* (name, attrs) = x_tag_inside in
  let* gt = token "/>" in
  pure (lt, name, attrs, gt)
) __n

and x_tag_comment : G.xml parser = fun __n -> (
  (* x_tag_comment -> '<!--' x_body_element* '-->' *)
  let* lt = token "<!--" in
  let* _ = list_of x_body_element in
  let* gt = token "-->" in
  pure
    G.{ xml_kind = G.XmlFragment (lt.tok, gt.tok);
        xml_attrs = [];
        xml_body = [] }
) __n

and x_tag_inside : (G.name * G.xml_attribute list) parser = fun __n -> (
  (* x_tag_inside -> x_name x_attr* *)
  let* name = x_name in
  let* params = list_of x_attr in
  pure (name, params)
) __n

and x_tag_embed_expression : G.expr G.bracket parser = fun __n -> (
  (* x_tag_embed_expression -> '<%=' expression '%>' *)
  let* lt = token "<%=" in
  let* expr = expression in
  let* gt = token "%>" in
  pure (lt.tok, expr, gt.tok)
) __n

and x_name : G.name parser = fun __n -> (
  (* x_name -> '<IDENT>' (':' '<IDENT>')* *)
  let* id = choice [ token "<IDENT>"; token "<KEYWORD>" ] in
  let name = make_name (ident_of_token id) None in
  let* names = list_of
    begin
      let* _colon = token ":" in
      let* id = choice [ token "<IDENT>"; token "<KEYWORD>" ] in
      pure (make_name (ident_of_token id) None)
    end
  in
  pure (collapse_names (name :: names))
) __n |> cut

and x_name_as_tokens : T.t list parser = fun __n -> (
  let* id = choice [ token "<IDENT>"; token "<KEYWORD>" ] in
  let* ids = list_of
    begin
      let* colon = token ":" in
      let* id = choice [ token "<IDENT>"; token "<KEYWORD>" ] in
      pure [colon; id]
    end
  in
  pure (id :: List.concat ids)
) __n |> cut

and x_attr : G.xml_attribute parser = fun __n -> (
  choice [
    begin
      (* x_attr -> x_name ('=' x_attr_value)? *)
      let* name = x_name in
      let* attr_value_opt = optional
        begin
          let* eq = token "=" in
          let* attr_value = x_attr_value in
          pure (eq, attr_value)
        end
      in
      (* XML attributes always require a value, but for some reason the Roslyn
       * grammar makes the value optional. We handle this case using the
       * G.XmlAttrExpr constructor, which coincidentally gives us attribute
       * definitions with metavariables, like <tag $ATTR>. *)
      match attr_value_opt with
      | Some (eq, attr_value) ->
          (* TODO: for some reason geenric AST wants an ident here, and not a name,
           * even though we can have namespaced-qualified ids here. *)
          let id = name |> idents_of_name |> List.hd |> fst in
          pure (G.XmlAttr (id, eq.tok, attr_value))
      | None ->
          pure (G.XmlAttrExpr (fb (G.N name |> G.e)))
    end;
    begin
      (* x_attr -> x_tag_embed_expression *)
      let* expr = x_tag_embed_expression in
      pure (G.XmlAttrExpr expr)
    end;
    begin
      (* x_attr -> '...' *)
      let* dot_dot_dot = token "..." in
      pure (G.XmlEllipsis dot_dot_dot.tok)
    end
  ]
) __n

and x_attr_value : G.a_xml_attr_value parser = fun __n -> (
  choice [
    begin
      let* (_, expr, _) = x_tag_embed_expression in
      pure expr
    end;
    begin
      (* x_attr_value -> '<IDENT>' *)
      let* t = token "<IDENT>" in
      pure (G.N (make_name (ident_of_token t) None) |> G.e)
    end;
    begin
      (* x_attr_value -> literal_expression *)
      literal_expression
    end;
  ]
) __n

and method_modifier : G.attribute parser = fun __n -> (
  choice [
    begin
      (* method_modifier -> 'Async' *)
      let* t = token "ASYNC" in
      pure (G.KeywordAttr (G.Async, t.tok))
    end;
    begin
      (* method_modifier -> 'Iterator' *)
      let* t = token "ITERATOR" in
      pure (G.KeywordAttr (G.Generator, t.tok))
    end;
    begin
      (* method_modifier -> modifier *)
      modifier
    end;
  ]
) __n

and modifier : G.attribute parser = fun __n -> (
  choice [
    begin
      (* modifier -> 'Const' *)
      let* t = token "CONST" in
      pure (G.KeywordAttr (G.Const, t.tok))
    end;
    begin
      (* modifier -> 'Default' *)
      (* NOTE: an equivalent of operator() in C++ *)
      let* t = token "DEFAULT" in
      pure (G.KeywordAttr (G.DefaultImpl, t.tok))
    end;
    begin
      (* modifier -> 'Delegate' *)
      let* t = token "DELEGATE" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Dim' *)
      let* t = token "DIM" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Friend' *)
      let* t = token "FRIEND" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'MustInherit' *)
      let* t = token "MUSTINHERIT" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'MustOverride' *)
      let* t = token "MUSTOVERRIDE" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Narrowing' *)
      let* t = token "NARROWING" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'NotInheritable' *)
      let* t = token "NOTINHERITABLE" in
      pure (G.KeywordAttr (G.SealedClass, t.tok))
    end;
    begin
      (* modifier -> 'NotOverridable' *)
      let* t = token "NOTOVERRIDABLE" in
      pure (G.KeywordAttr (G.Final, t.tok))
    end;
    begin
      (* modifier -> 'Optional' *)
      let* t = token "OPTIONAL" in
      pure (G.KeywordAttr (G.Optional, t.tok))
    end;
    begin
      (* modifier -> 'Overloads' *)
      let* t = token "OVERLOADS" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Overridable' *)
      let* t = token "OVERRIDABLE" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Overrides' *)
      let* t = token "OVERRIDES" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Partial' *)
      let* t = token "PARTIAL" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Private' *)
      let* t = token "PRIVATE" in
      pure (G.KeywordAttr (G.Private, t.tok))
    end;
    begin
      (* modifier -> 'Protected' *)
      let* t = token "PROTECTED" in
      pure (G.KeywordAttr (G.Protected, t.tok))
    end;
    begin
      (* modifier -> 'Public' *)
      let* t = token "PUBLIC" in
      pure (G.KeywordAttr (G.Public, t.tok))
    end;
    begin
      (* modifier -> 'ReadOnly' *)
      (* NOTE: Const is compile-time, ReadOnly is construction-time *)
      let* t = token "READONLY" in
      pure (G.KeywordAttr (G.Const, t.tok))
    end;
    begin
      (* modifier -> 'Shadows' *)
      let* t = token "SHADOWS" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Shared' *)
      let* t = token "SHARED" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Static' *)
      let* t = token "STATIC" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'Widening' *)
      let* t = token "WIDENING" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'WithEvents' *)
      let* t = token "WITHEVENTS" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'WriteOnly' *)
      let* t = token "WRITEONLY" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'ByRef' *)
      let* t = token "BYREF" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'ByVal' *)
      let* t = token "BYVAL" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
    begin
      (* modifier -> 'ParamArray' *)
      let* t = token "PARAMARRAY" in
      pure (G.OtherAttribute ((t.content, t.tok), []))
    end;
  ]
) __n

and character_literal_token : G.any parser = fun __n -> (
  (* character_literal_token -> '<CHAR>' *)
  let* lt_CHAR_gt1 = token "<CHAR>" in
  pure (xRule "character_literal_token" 0 [xToken(lt_CHAR_gt1)])
) __n

and date_literal_token : G.any parser = fun __n -> (
  (* date_literal_token -> '<DATE>' *)
  let* lt_DATE_gt1 = token "<DATE>" in
  pure (xRule "date_literal_token" 0 [xToken(lt_DATE_gt1)])
) __n

and decimal_literal_token : G.any parser = fun __n -> (
  (* decimal_literal_token -> 'DECIMAL_LITERAL' *)
  let* dECIMAL_LITERAL1 = token "DECIMAL_LITERAL" in
  pure (xRule "decimal_literal_token" 0 [xToken(dECIMAL_LITERAL1)])
) __n

and floating_literal_token : G.any parser = fun __n -> (
  (* floating_literal_token -> '<FLOAT>' *)
  let* lt_FLOAT_gt1 = token "<FLOAT>" in
  pure (xRule "floating_literal_token" 0 [xToken(lt_FLOAT_gt1)])
) __n

and identifier_token : G.any parser = fun __n -> (
  (* identifier_token -> '<IDENT>' *)
  let* lt_IDENT_gt1 = token "<IDENT>" in
  pure (xRule "identifier_token" 0 [xToken(lt_IDENT_gt1)])
) __n

and integer_literal_token : G.any parser = fun __n -> (
  (* integer_literal_token -> '<INT>' *)
  let* lt_INT_gt1 = token "<INT>" in
  pure (xRule "integer_literal_token" 0 [xToken(lt_INT_gt1)])
) __n

and string_literal_token : G.any parser = fun __n -> (
  (* string_literal_token -> '<STRING>' *)
  let* lt_STRING_gt1 = token "<STRING>" in
  pure (xRule "string_literal_token" 0 [xToken(lt_STRING_gt1)])
) __n

and opengrep_assignment_statement : G.any parser = fun __n -> (
  (* assignment_statement -> await_expression (assignment_statement_operator expression)? *)
  let* lhs = await_expression in
  let* op_rhs_opt = optional
    begin
      let* op = assignment_statement_operator in
      let* rhs = expression in
      pure (op, rhs)
    end
  in
  match op_rhs_opt with
  | None -> pure (G.E lhs)
  | Some (op, rhs) -> pure (G.E (G.AssignOp (lhs, op, rhs) |> G.e))
) __n

let opengrep_pattern : G.any parser =
  let* content = choice [
    begin
      opengrep_assignment_statement
    end;
    begin
      let* expr = expression in
      pure (G.E expr)
    end;
    begin
      let* stmt = toplevel_declaration in
      pure (G.S stmt)
    end;
    begin
      let* stmt = statements_block in
      pure (G.S stmt)
    end;
    begin
      let* typ = type_ in
      pure (G.T typ)
    end;
    begin
      let* stmts = imports_statement in
      match stmts with
      | [stmt] -> pure (G.S stmt)
      | _ -> pure (G.Ss stmts)
    end;
    begin
      let* _ = token "<" in
      let* attr = attribute in
      let* _ = token "> in" in
      pure (G.At attr)
    end;
    begin
      let* t = toplevel in
      pure (G.Ss t)
    end;
  ]
  in
  let* _ = token "<EOF>" in
  pure content

(* Entry points *)

let parse_file (file : Fpath.t) : G.program result =
  let content = UFile.read_file file in
  tokenize_and_parse_string compilation_unit ~filepath:file content

let parse_pattern (s : string) : G.any =
  let ts = Tokenize.tokenize s in
  match run opengrep_pattern ts with
  | { value; next = [] } :: _ -> value
  | _ -> raise Parsing.Parse_error
