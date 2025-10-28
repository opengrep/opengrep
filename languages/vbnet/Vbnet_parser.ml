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

let partial = DLS.new_key (fun () -> false)

let reset_parser_state () =
  DLS.set partial false

let run (p : 'a parser) (ts : token list): 'a parsing_result list =
  reset_parser_state ();
  p ts |> Seq.take 1 |> List.of_seq

(* Can't use Parsing_Result2.t because of subproject dependencies *)
type 'a result =
  | Ok of 'a * Parsing_stat.t
  | Partial of 'a * Parsing_stat.t
  | Fail of Parsing_stat.t

(* FIXME: works for entire files only! *)
let tokenize_and_parse_string (p : 'a parser) ?(filepath=Fpath.v "<pattern>") (s : string) : 'a result =
  let ts = Tokenize.tokenize ~filepath s in
  let line_count = List.filter T.token_line_terminator ts |> List.length in
  let stat =
    Parsing_stat.({
      filename = Fpath.filename filepath;
      total_line_count = line_count;
      error_line_count = 0;
      have_timeout = false;
      commentized = 0;
      problematic_lines = [];
      ast_stat = None;
    })
  in
  match run p ts with
  | { value; next = [] } :: _ when DLS.get partial -> Partial (value, stat)
  | { value; next = [] } :: _ -> Ok (value, stat)
  | _ -> Fail stat

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

(* parser *)

let rec compilation_unit : G.any parser = fun __n -> (
  (* compilation_unit -> toplevel '<EOF>' *)
  (* ... or stop on partial *)
  let* toplevel1 = toplevel in
  let* _ = choice [
      pure_token "<EOF>";
      stop_on_partial
    ]
  in
  pure (xRule "compilation_unit" 0 [toplevel1])
) __n |> cut

and toplevel : G.any parser = fun __n -> (
  (* toplevel -> option_statement* imports_statement* attributes_statement* toplevel_declaration* *)
  let* option_statements1 = list_of option_statement in
  let* imports_statements1 = list_of imports_statement in
  let* attributes_statements1 = list_of attributes_statement in
  let* toplevel_declarations1 = list_of toplevel_declaration in
  pure (xRule "toplevel" 0 [xList(option_statements1); xList(imports_statements1); xList(attributes_statements1); xList(toplevel_declarations1)])
) __n

and option_statement_mandatory : G.any parser = fun __n -> (
  choice [
    begin
      (* option_statement_mandatory -> 'Explicit' *)
      let* explicit1 = token "EXPLICIT" in
      pure (xRule "option_statement_mandatory" 0 [xToken(explicit1)])
    end;
    begin
      (* option_statement_mandatory -> 'Strict' *)
      let* strict1 = token "STRICT" in
      pure (xRule "option_statement_mandatory" 1 [xToken(strict1)])
    end;
    begin
      (* option_statement_mandatory -> 'Compare' *)
      let* compare1 = token "COMPARE" in
      pure (xRule "option_statement_mandatory" 2 [xToken(compare1)])
    end;
    begin
      (* option_statement_mandatory -> 'Infer' *)
      let* infer1 = token "INFER" in
      pure (xRule "option_statement_mandatory" 3 [xToken(infer1)])
    end;
  ]
) __n

and option_statement_optional : G.any parser = fun __n -> (
  choice [
    begin
      (* option_statement_optional -> 'On' *)
      let* on1 = token "ON" in
      pure (xRule "option_statement_optional" 0 [xToken(on1)])
    end;
    begin
      (* option_statement_optional -> 'Off' *)
      let* off1 = token "OFF" in
      pure (xRule "option_statement_optional" 1 [xToken(off1)])
    end;
    begin
      (* option_statement_optional -> 'Text' *)
      let* text1 = token "TEXT" in
      pure (xRule "option_statement_optional" 2 [xToken(text1)])
    end;
    begin
      (* option_statement_optional -> 'Binary' *)
      let* binary1 = token "BINARY" in
      pure (xRule "option_statement_optional" 3 [xToken(binary1)])
    end;
  ]
) __n

and option_statement : G.any parser = fun __n -> (
  (* option_statement -> 'Option' option_statement_mandatory option_statement_optional? *)
  let* option1 = token "OPTION" in
  let* option_statement_mandatory1 = option_statement_mandatory in
  let* option_statement_optional_opt1 = optional option_statement_optional in
  pure (xRule "option_statement" 0 [xToken(option1); option_statement_mandatory1; xOptional(option_statement_optional_opt1)])
) __n

and imports_statement : G.any parser = fun __n -> (
  (* imports_statement -> 'Imports' imports_clause (',' imports_clause)* *)
  let* imports1 = token "IMPORTS" in
  let* imports_clause1 = imports_clause in
  let* comma_imports_clauses1 = list_of 
    begin
      let* comma1 = token "," in
      let* imports_clause1 = imports_clause in
      pure (xGroup([xToken(comma1); imports_clause1]))
    end
  in
  pure (xRule "imports_statement" 0 [xToken(imports1); imports_clause1; xList(comma_imports_clauses1)])
) __n

and imports_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* imports_clause -> simple_imports_clause *)
      let* simple_imports_clause1 = simple_imports_clause in
      pure (xRule "imports_clause" 0 [simple_imports_clause1])
    end;
    begin
      (* imports_clause -> '<' x_name '=' '<STRING>' '>' *)
      let* lt1 = token "<" in
      let* x_name1 = x_name in
      let* eq1 = token "=" in
      let* lt_STRING_gt1 = token "<STRING>" in
      let* gt1 = token ">" in
      pure (xRule "imports_clause" 1 [xToken(lt1); x_name1; xToken(eq1); xToken(lt_STRING_gt1); xToken(gt1)])
    end;
  ]
) __n

and simple_imports_clause : G.any parser = fun __n -> (
  (* simple_imports_clause -> import_alias_clause? name *)
  let* import_alias_clause_opt1 = optional import_alias_clause in
  let* name1 = name in
  pure (xRule "simple_imports_clause" 0 [xOptional(import_alias_clause_opt1); name1])
) __n

and import_alias_clause : G.any parser = fun __n -> (
  (* import_alias_clause -> identifier_token '=' *)
  let* identifier_token1 = identifier_token in
  let* eq1 = token "=" in
  pure (xRule "import_alias_clause" 0 [identifier_token1; xToken(eq1)])
) __n

and attributes_statement : G.any parser = fun __n -> (
  (* attributes_statement -> attribute_list+ *)
  let* attribute_lists1 = ne_list_of attribute_list in
  pure (xRule "attributes_statement" 0 [xList(attribute_lists1)])
) __n

and attribute_list : G.any parser = fun __n -> (
  (* attribute_list -> '<' attribute (',' attribute)* '>' *)
  let* lt1 = token "<" in
  let* attribute1 = attribute in
  let* comma_attributes1 = list_of 
    begin
      let* comma1 = token "," in
      let* attribute1 = attribute in
      pure (xGroup([xToken(comma1); attribute1]))
    end
  in
  let* gt1 = token ">" in
  pure (xRule "attribute_list" 0 [xToken(lt1); attribute1; xList(comma_attributes1); xToken(gt1)])
) __n

and attribute : G.any parser = fun __n -> (
  (* attribute -> attribute_target? type argument_list? *)
  let* attribute_target_opt1 = optional attribute_target in
  let* type_1 = type_ in
  let* argument_list_opt1 = optional argument_list in
  pure (xRule "attribute" 0 [xOptional(attribute_target_opt1); type_1; xOptional(argument_list_opt1)])
) __n

and attribute_target : G.any parser = fun __n -> (
  choice [
    begin
      (* attribute_target -> 'Assembly' ':' *)
      let* assembly1 = token "ASSEMBLY" in
      let* colon1 = token ":" in
      pure (xRule "attribute_target" 0 [xToken(assembly1); xToken(colon1)])
    end;
    begin
      (* attribute_target -> 'Module' ':' *)
      let* module1 = token "MODULE" in
      let* colon1 = token ":" in
      pure (xRule "attribute_target" 1 [xToken(module1); xToken(colon1)])
    end;
  ]
) __n

and argument_list : G.any parser = fun __n -> (
  (* argument_list -> '(' (argument? (',' argument?)* )? ')' *)
  let* lparen1 = token "(" in
  let* argument_opt_comma_argument_opts_opt1 = optional 
    begin
      let* argument_opt1 = optional argument in
      let* comma_argument_opts1 = list_of 
        begin
          let* comma1 = token "," in
          let* argument_opt1 = optional argument in
          pure (xGroup([xToken(comma1); xOptional(argument_opt1)]))
        end
      in
      pure (xGroup([xOptional(argument_opt1); xList(comma_argument_opts1)]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "argument_list" 0 [xToken(lparen1); xOptional(argument_opt_comma_argument_opts_opt1); xToken(rparen1)])
) __n

and argument : G.any parser = fun __n -> (
  (* argument -> (identifier_or_keyword ':=')? expression ('To' expression)? *)
  let* identifier_or_keyword_colon_eq_opt1 = optional 
    begin
      let* identifier_or_keyword1 = identifier_or_keyword in
      let* colon_eq1 = token ":=" in
      pure (xGroup([identifier_or_keyword1; xToken(colon_eq1)]))
    end
  in
  let* expression1 = expression in
  let* to_expression_opt1 = optional 
    begin
      let* to1 = token "TO" in
      let* expression1 = expression in
      pure (xGroup([xToken(to1); G.E expression1]))
    end
  in
  pure (xRule "argument" 0 [xOptional(identifier_or_keyword_colon_eq_opt1); G.E expression1; xOptional(to_expression_opt1)])
) __n

and escaped_identifier_content : G.any parser = fun __n -> (
  choice [
    begin
      (* escaped_identifier_content -> '<KEYWORD>' *)
      let* lt_KEYWORD_gt1 = token "<KEYWORD>" in
      pure (xRule "escaped_identifier_content" 0 [xToken(lt_KEYWORD_gt1)])
    end;
    begin
      (* escaped_identifier_content -> '<IDENT>' *)
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xRule "escaped_identifier_content" 1 [xToken(lt_IDENT_gt1)])
    end;
    begin
      (* escaped_identifier_content -> '<OPERATOR>' *)
      let* lt_OPERATOR_gt1 = token "<OPERATOR>" in
      pure (xRule "escaped_identifier_content" 2 [xToken(lt_OPERATOR_gt1)])
    end;
    begin
      (* escaped_identifier_content -> '<INT>' *)
      let* lt_INT_gt1 = token "<INT>" in
      pure (xRule "escaped_identifier_content" 3 [xToken(lt_INT_gt1)])
    end;
    begin
      (* escaped_identifier_content -> '<STRING>' *)
      let* lt_STRING_gt1 = token "<STRING>" in
      pure (xRule "escaped_identifier_content" 4 [xToken(lt_STRING_gt1)])
    end;
  ]
) __n

and identifier_name : G.any parser = fun __n -> (
  choice [
    begin
      (* identifier_name -> '<IDENT>' *)
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xRule "identifier_name" 0 [xToken(lt_IDENT_gt1)])
    end;
    begin
      (* identifier_name -> '[' escaped_identifier_content+ ']' *)
      let* lbrack1 = token "[" in
      let* escaped_identifier_contents1 = ne_list_of escaped_identifier_content in
      let* rbrack1 = token "]" in
      pure (xRule "identifier_name" 1 [xToken(lbrack1); xList(escaped_identifier_contents1); xToken(rbrack1)])
    end;
  ]
) __n

and single_line_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* single_line_statement -> single_line_if_statement *)
      let* single_line_if_statement1 = single_line_if_statement in
      pure (xRule "single_line_statement" 0 [single_line_if_statement1])
    end;
    begin
      (* single_line_statement -> add_remove_handler_statement *)
      let* add_remove_handler_statement1 = add_remove_handler_statement in
      pure (xRule "single_line_statement" 1 [add_remove_handler_statement1])
    end;
    begin
      (* single_line_statement -> raise_event_statement *)
      let* raise_event_statement1 = raise_event_statement in
      pure (xRule "single_line_statement" 2 [raise_event_statement1])
    end;
    begin
      (* single_line_statement -> local_declaration_statement *)
      let* local_declaration_statement1 = local_declaration_statement in
      pure (xRule "single_line_statement" 3 [local_declaration_statement1])
    end;
    begin
      (* single_line_statement -> erase_statement *)
      let* erase_statement1 = erase_statement in
      pure (xRule "single_line_statement" 4 [erase_statement1])
    end;
    begin
      (* single_line_statement -> error_statement *)
      let* error_statement1 = error_statement in
      pure (xRule "single_line_statement" 5 [error_statement1])
    end;
    begin
      (* single_line_statement -> continue_statement *)
      let* continue_statement1 = continue_statement in
      pure (xRule "single_line_statement" 6 [continue_statement1])
    end;
    begin
      (* single_line_statement -> call_statement *)
      let* call_statement1 = call_statement in
      pure (xRule "single_line_statement" 7 [call_statement1])
    end;
    begin
      (* single_line_statement -> go_to_statement *)
      let* go_to_statement1 = go_to_statement in
      pure (xRule "single_line_statement" 8 [go_to_statement1])
    end;
    begin
      (* single_line_statement -> on_error_go_to_statement *)
      let* on_error_go_to_statement1 = on_error_go_to_statement in
      pure (xRule "single_line_statement" 9 [on_error_go_to_statement1])
    end;
    begin
      (* single_line_statement -> on_error_resume_next_statement *)
      let* on_error_resume_next_statement1 = on_error_resume_next_statement in
      pure (xRule "single_line_statement" 10 [on_error_resume_next_statement1])
    end;
    begin
      (* single_line_statement -> print_statement *)
      let* print_statement1 = print_statement in
      pure (xRule "single_line_statement" 11 [print_statement1])
    end;
    begin
      (* single_line_statement -> re_dim_statement *)
      let* re_dim_statement1 = re_dim_statement in
      pure (xRule "single_line_statement" 12 [re_dim_statement1])
    end;
    begin
      (* single_line_statement -> resume_statement *)
      let* resume_statement1 = resume_statement in
      pure (xRule "single_line_statement" 13 [resume_statement1])
    end;
    begin
      (* single_line_statement -> return_statement *)
      let* return_statement1 = return_statement in
      pure (xRule "single_line_statement" 14 [return_statement1])
    end;
    begin
      (* single_line_statement -> stop_or_end_statement *)
      let* stop_or_end_statement1 = stop_or_end_statement in
      pure (xRule "single_line_statement" 15 [stop_or_end_statement1])
    end;
    begin
      (* single_line_statement -> throw_statement *)
      let* throw_statement1 = throw_statement in
      pure (xRule "single_line_statement" 16 [throw_statement1])
    end;
    begin
      (* single_line_statement -> yield_statement *)
      let* yield_statement1 = yield_statement in
      pure (xRule "single_line_statement" 17 [yield_statement1])
    end;
    begin
      (* single_line_statement -> exit_statement *)
      let* exit_statement1 = exit_statement in
      pure (xRule "single_line_statement" 18 [exit_statement1])
    end;
    begin
      (* single_line_statement -> assignment_statement *)
      let* assignment_statement1 = assignment_statement in
      pure (xRule "single_line_statement" 19 [assignment_statement1])
    end;
  ]
) __n

and single_line_statements : G.any parser = fun __n -> (
  (* single_line_statements -> single_line_statement (':' single_line_statement)* *)
  let* single_line_statement1 = single_line_statement in
  let* colon_single_line_statements1 = list_of 
    begin
      let* colon1 = token ":" in
      let* single_line_statement1 = single_line_statement in
      pure (xGroup([xToken(colon1); single_line_statement1]))
    end
  in
  pure (xRule "single_line_statements" 0 [single_line_statement1; xList(colon_single_line_statements1)])
) __n

and multi_line_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* multi_line_statement -> select_case_block *)
      let* select_case_block1 = select_case_block in
      pure (xRule "multi_line_statement" 0 [select_case_block1])
    end;
    begin
      (* multi_line_statement -> multi_line_if_block *)
      let* multi_line_if_block1 = multi_line_if_block in
      pure (xRule "multi_line_statement" 1 [multi_line_if_block1])
    end;
    begin
      (* multi_line_statement -> for_block *)
      let* for_block1 = for_block in
      pure (xRule "multi_line_statement" 2 [for_block1])
    end;
    begin
      (* multi_line_statement -> do_block *)
      let* do_block1 = do_block in
      pure (xRule "multi_line_statement" 3 [do_block1])
    end;
    begin
      (* multi_line_statement -> while_block *)
      let* while_block1 = while_block in
      pure (xRule "multi_line_statement" 4 [while_block1])
    end;
    begin
      (* multi_line_statement -> try_block *)
      let* try_block1 = try_block in
      pure (xRule "multi_line_statement" 5 [try_block1])
    end;
    begin
      (* multi_line_statement -> with_block *)
      let* with_block1 = with_block in
      pure (xRule "multi_line_statement" 6 [with_block1])
    end;
    begin
      (* multi_line_statement -> sync_lock_block *)
      let* sync_lock_block1 = sync_lock_block in
      pure (xRule "multi_line_statement" 7 [sync_lock_block1])
    end;
    begin
      (* multi_line_statement -> using_block *)
      let* using_block1 = using_block in
      pure (xRule "multi_line_statement" 8 [using_block1])
    end;
  ]
) __n

and statements_block_item : G.any parser = fun __n -> (
  choice [
    begin
      (* statements_block_item -> multi_line_statement *)
      let* multi_line_statement1 = multi_line_statement in
      pure (xRule "statements_block_item" 0 [multi_line_statement1])
    end;
    begin
      (* statements_block_item -> identifier_label ':' @lookahead('<LINE_TERMINATOR>') *)
      let* identifier_label1 = identifier_label in
      let* colon1 = token ":" in
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "statements_block_item" 1 [identifier_label1; xToken(colon1)])
    end;
    begin
      (* statements_block_item -> numeric_label ':' @lookahead('<LINE_TERMINATOR>') *)
      let* numeric_label1 = numeric_label in
      let* colon1 = token ":" in
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "statements_block_item" 2 [numeric_label1; xToken(colon1)])
    end;
    begin
      (* statements_block_item -> single_line_statements *)
      let* single_line_statements1 = single_line_statements in
      pure (xRule "statements_block_item" 3 [single_line_statements1])
    end;
  ]
) __n

and statements_block : G.any parser = fun __n -> (
  choice [
    begin
      (* statements_block -> statements_block_item (@lookahead('<LINE_TERMINATOR>') statements_block_item)* *)
      let* statements_block_item1 = statements_block_item in
      let* lookahead_lt_LINE_TERMINATOR_gt_statements_block_items1 = list_of 
        begin
          let* _ = look_ahead "<LINE_TERMINATOR>" in
          let* statements_block_item1 = statements_block_item in
          pure (xGroup([statements_block_item1]))
        end
      in
      pure (xRule "statements_block" 0 [statements_block_item1; xList(lookahead_lt_LINE_TERMINATOR_gt_statements_block_items1)])
    end;
    begin
      (* statements_block ->  *)
      pure (xRule "statements_block" 1 [])
    end;
  ]
) __n

and select_case_block : G.any parser = fun __n -> (
  (* select_case_block -> select_statement case_block* end_select_statement *)
  let* select_statement1 = select_statement in
  let* case_blocks1 = list_of case_block in
  let* end_select_statement1 = end_select_statement in
  pure (xRule "select_case_block" 0 [select_statement1; xList(case_blocks1); end_select_statement1])
) __n

and select_statement : G.any parser = fun __n -> (
  (* select_statement -> 'Select' 'Case'? expression @lookahead('<LINE_TERMINATOR>') *)
  let* select1 = token "SELECT" in
  let* case_opt1 = optional (token "CASE") in
  let* expression1 = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  pure (xRule "select_statement" 0 [xToken(select1); xOptional(Option.map (fun x -> xToken x) case_opt1); G.E expression1])
) __n

and case_block : G.any parser = fun __n -> (
  (* case_block -> case_statement statements_block *)
  let* case_statement1 = case_statement in
  let* statements_block1 = statements_block in
  pure (xRule "case_block" 0 [case_statement1; statements_block1])
) __n

and case_statement_terminator : G.any parser = fun __n -> (
  choice [
    begin
      (* case_statement_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "case_statement_terminator" 0 [])
    end;
    begin
      (* case_statement_terminator -> ':' *)
      let* colon1 = token ":" in
      pure (xRule "case_statement_terminator" 1 [xToken(colon1)])
    end;
  ]
) __n

and case_statement : G.any parser = fun __n -> (
  (* case_statement -> 'Case' case_clause (',' case_clause)* case_statement_terminator *)
  let* case1 = token "CASE" in
  let* case_clause1 = case_clause in
  let* comma_case_clauses1 = list_of 
    begin
      let* comma1 = token "," in
      let* case_clause1 = case_clause in
      pure (xGroup([xToken(comma1); case_clause1]))
    end
  in
  let* case_statement_terminator1 = case_statement_terminator in
  pure (xRule "case_statement" 0 [xToken(case1); case_clause1; xList(comma_case_clauses1); case_statement_terminator1])
) __n

and case_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* case_clause -> 'Else' *)
      let* else1 = token "ELSE" in
      pure (xRule "case_clause" 0 [xToken(else1)])
    end;
    begin
      (* case_clause -> range_or_expression_case_clause *)
      let* range_or_expression_case_clause1 = range_or_expression_case_clause in
      pure (xRule "case_clause" 1 [range_or_expression_case_clause1])
    end;
    begin
      (* case_clause -> relational_case_clause *)
      let* relational_case_clause1 = relational_case_clause in
      pure (xRule "case_clause" 2 [relational_case_clause1])
    end;
  ]
) __n

and range_or_expression_case_clause : G.any parser = fun __n -> (
  (* range_or_expression_case_clause -> expression ('To' expression)? *)
  let* expression1 = expression in
  let* to_expression_opt1 = optional 
    begin
      let* to1 = token "TO" in
      let* expression1 = expression in
      pure (xGroup([xToken(to1); G.E expression1]))
    end
  in
  pure (xRule "range_or_expression_case_clause" 0 [G.E expression1; xOptional(to_expression_opt1)])
) __n

and relational_case_clause_op : G.any parser = fun __n -> (
  choice [
    begin
      (* relational_case_clause_op -> '=' *)
      let* eq1 = token "=" in
      pure (xRule "relational_case_clause_op" 0 [xToken(eq1)])
    end;
    begin
      (* relational_case_clause_op -> '>' *)
      let* gt1 = token ">" in
      pure (xRule "relational_case_clause_op" 1 [xToken(gt1)])
    end;
    begin
      (* relational_case_clause_op -> '>=' *)
      let* gt_eq1 = token ">=" in
      pure (xRule "relational_case_clause_op" 2 [xToken(gt_eq1)])
    end;
    begin
      (* relational_case_clause_op -> '<' *)
      let* lt1 = token "<" in
      pure (xRule "relational_case_clause_op" 3 [xToken(lt1)])
    end;
    begin
      (* relational_case_clause_op -> '<=' *)
      let* lt_eq1 = token "<=" in
      pure (xRule "relational_case_clause_op" 4 [xToken(lt_eq1)])
    end;
    begin
      (* relational_case_clause_op -> '<>' *)
      let* lt_gt1 = token "<>" in
      pure (xRule "relational_case_clause_op" 5 [xToken(lt_gt1)])
    end;
  ]
) __n

and relational_case_clause : G.any parser = fun __n -> (
  (* relational_case_clause -> 'Is'? relational_case_clause_op expression *)
  let* is_opt1 = optional (token "IS") in
  let* relational_case_clause_op1 = relational_case_clause_op in
  let* expression1 = expression in
  pure (xRule "relational_case_clause" 0 [xOptional(Option.map (fun x -> xToken x) is_opt1); relational_case_clause_op1; G.E expression1])
) __n

and single_line_if_statement : G.any parser = fun __n -> (
  (* single_line_if_statement -> 'If' expression 'Then' @lookahead_not('<LINE_TERMINATOR>') single_line_statements (@lookahead_not('<LINE_TERMINATOR>') 'Else' single_line_statements)? *)
  let* if1 = token "IF" in
  let* expression1 = expression in
  let* then1 = token "THEN" in
  let* _ = look_ahead_not "<LINE_TERMINATOR>" in
  let* single_line_statements1 = single_line_statements in
  let* lookahead_not_lt_LINE_TERMINATOR_gt_else_single_line_st_opt1 = optional 
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* else1 = token "ELSE" in
      let* single_line_statements1 = single_line_statements in
      pure (xGroup([xToken(else1); single_line_statements1]))
    end
  in
  pure (xRule "single_line_if_statement" 0 [xToken(if1); G.E expression1; xToken(then1); single_line_statements1; xOptional(lookahead_not_lt_LINE_TERMINATOR_gt_else_single_line_st_opt1)])
) __n

and multi_line_if_block : G.any parser = fun __n -> (
  (* multi_line_if_block -> 'If' expression 'Then'? case_statement_terminator statements_block else_if_block* else_block? ':'? 'End' 'If' *)
  let* if1 = token "IF" in
  let* expression1 = expression in
  let* then_opt1 = optional (token "THEN") in
  let* case_statement_terminator1 = case_statement_terminator in
  let* statements_block1 = statements_block in
  let* else_if_blocks1 = list_of else_if_block in
  let* else_block_opt1 = optional else_block in
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* if2 = token "IF" in
  pure (xRule "multi_line_if_block" 0 [xToken(if1); G.E expression1; xOptional(Option.map (fun x -> xToken x) then_opt1); case_statement_terminator1; statements_block1; xList(else_if_blocks1); xOptional(else_block_opt1); xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(if2)])
) __n

and else_if_or_elseif : G.any parser = fun __n -> (
  choice [
    begin
      (* else_if_or_elseif -> 'ElseIf' *)
      let* elseIf1 = token "ELSEIF" in
      pure (xRule "else_if_or_elseif" 0 [xToken(elseIf1)])
    end;
    begin
      (* else_if_or_elseif -> 'Else' @lookahead_not('<LINE_TERMINATOR>') 'If' *)
      let* else1 = token "ELSE" in
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* if1 = token "IF" in
      pure (xRule "else_if_or_elseif" 1 [xToken(else1); xToken(if1)])
    end;
  ]
) __n

and else_if_block : G.any parser = fun __n -> (
  (* else_if_block -> ':'? else_if_or_elseif expression 'Then'? case_statement_terminator statements_block *)
  let* colon_opt1 = optional (token ":") in
  let* else_if_or_elseif1 = else_if_or_elseif in
  let* expression1 = expression in
  let* then_opt1 = optional (token "THEN") in
  let* case_statement_terminator1 = case_statement_terminator in
  let* statements_block1 = statements_block in
  pure (xRule "else_if_block" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); else_if_or_elseif1; G.E expression1; xOptional(Option.map (fun x -> xToken x) then_opt1); case_statement_terminator1; statements_block1])
) __n

and else_block : G.any parser = fun __n -> (
  (* else_block -> ':'? 'Else' ':'? statements_block *)
  let* colon_opt1 = optional (token ":") in
  let* else1 = token "ELSE" in
  let* colon_opt2 = optional (token ":") in
  let* statements_block1 = statements_block in
  pure (xRule "else_block" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(else1); xOptional(Option.map (fun x -> xToken x) colon_opt2); statements_block1])
) __n

and after_next : G.any parser = fun __n -> (
  choice [
    begin
      (* after_next -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "after_next" 0 [])
    end;
    begin
      (* after_next -> @lookahead(':') *)
      let* _ = look_ahead ":" in
      pure (xRule "after_next" 1 [])
    end;
  ]
) __n

and for_block : G.any parser = fun __n -> (
  (* for_block -> 'For' for_header case_statement_terminator statements_block case_statement_terminator 'Next' (@lookahead_not('<LINE_TERMINATOR>') identifier_name)? after_next *)
  let* for1 = token "FOR" in
  let* for_header1 = for_header in
  let* case_statement_terminator1 = case_statement_terminator in
  let* statements_block1 = statements_block in
  let* case_statement_terminator2 = case_statement_terminator in
  let* next1 = token "NEXT" in
  let* lookahead_not_lt_LINE_TERMINATOR_gt_identifier_name_opt1 = optional 
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* identifier_name1 = identifier_name in
      pure (xGroup([identifier_name1]))
    end
  in
  let* after_next1 = after_next in
  pure (xRule "for_block" 0 [xToken(for1); for_header1; case_statement_terminator1; statements_block1; case_statement_terminator2; xToken(next1); xOptional(lookahead_not_lt_LINE_TERMINATOR_gt_identifier_name_opt1); after_next1])
) __n

and for_header : G.any parser = fun __n -> (
  choice [
    begin
      (* for_header -> identifier_name simple_as_clause? '=' expression 'To' expression ('Step' expression)? *)
      let* identifier_name1 = identifier_name in
      let* simple_as_clause_opt1 = optional simple_as_clause in
      let* eq1 = token "=" in
      let* expression1 = expression in
      let* to1 = token "TO" in
      let* expression2 = expression in
      let* step_expression_opt1 = optional 
        begin
          let* step1 = token "STEP" in
          let* expression1 = expression in
          pure (xGroup([xToken(step1); G.E expression1]))
        end
      in
      pure (xRule "for_header" 0 [identifier_name1; xOptional(simple_as_clause_opt1); xToken(eq1); G.E expression1; xToken(to1); G.E expression2; xOptional(step_expression_opt1)])
    end;
    begin
      (* for_header -> 'Each' identifier_name simple_as_clause? 'In' expression *)
      let* each1 = token "EACH" in
      let* identifier_name1 = identifier_name in
      let* simple_as_clause_opt1 = optional simple_as_clause in
      let* in1 = token "IN" in
      let* expression1 = expression in
      pure (xRule "for_header" 1 [xToken(each1); identifier_name1; xOptional(simple_as_clause_opt1); xToken(in1); G.E expression1])
    end;
  ]
) __n

and do_block : G.any parser = fun __n -> (
  (* do_block -> 'Do' do_header? case_statement_terminator statements_block case_statement_terminator 'Loop' (@lookahead_not('<LINE_TERMINATOR>') do_header)? *)
  let* do1 = token "DO" in
  let* do_header_opt1 = optional do_header in
  let* case_statement_terminator1 = case_statement_terminator in
  let* statements_block1 = statements_block in
  let* case_statement_terminator2 = case_statement_terminator in
  let* loop1 = token "LOOP" in
  let* lookahead_not_lt_LINE_TERMINATOR_gt_do_header_opt1 = optional 
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* do_header1 = do_header in
      pure (xGroup([do_header1]))
    end
  in
  pure (xRule "do_block" 0 [xToken(do1); xOptional(do_header_opt1); case_statement_terminator1; statements_block1; case_statement_terminator2; xToken(loop1); xOptional(lookahead_not_lt_LINE_TERMINATOR_gt_do_header_opt1)])
) __n

and do_header : G.any parser = fun __n -> (
  choice [
    begin
      (* do_header -> 'While' expression *)
      let* while1 = token "WHILE" in
      let* expression1 = expression in
      pure (xRule "do_header" 0 [xToken(while1); G.E expression1])
    end;
    begin
      (* do_header -> 'Until' expression *)
      let* until1 = token "UNTIL" in
      let* expression1 = expression in
      pure (xRule "do_header" 1 [xToken(until1); G.E expression1])
    end;
  ]
) __n

and while_block : G.any parser = fun __n -> (
  (* while_block -> 'While' expression case_statement_terminator statements_block case_statement_terminator ':'? 'End' 'While' *)
  let* while1 = token "WHILE" in
  let* expression1 = expression in
  let* case_statement_terminator1 = case_statement_terminator in
  let* statements_block1 = statements_block in
  let* case_statement_terminator2 = case_statement_terminator in
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* while2 = token "WHILE" in
  pure (xRule "while_block" 0 [xToken(while1); G.E expression1; case_statement_terminator1; statements_block1; case_statement_terminator2; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(while2)])
) __n

and soft_terminator : G.any parser = fun __n -> (
  choice [
    begin
      (* soft_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "soft_terminator" 0 [])
    end;
    begin
      (* soft_terminator -> @lookahead(':') *)
      let* _ = look_ahead ":" in
      pure (xRule "soft_terminator" 1 [])
    end;
  ]
) __n

and try_block : G.any parser = fun __n -> (
  (* try_block -> 'Try' statements_block catch_block* finally_block? ':'? 'End' 'Try' *)
  let* try1 = token "TRY" in
  let* statements_block1 = statements_block in
  let* catch_blocks1 = list_of catch_block in
  let* finally_block_opt1 = optional finally_block in
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* try2 = token "TRY" in
  pure (xRule "try_block" 0 [xToken(try1); statements_block1; xList(catch_blocks1); xOptional(finally_block_opt1); xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(try2)])
) __n

and catch_block : G.any parser = fun __n -> (
  (* catch_block -> 'Catch' identifier_name? simple_as_clause? catch_filter_clause? soft_terminator statements_block *)
  let* catch1 = token "CATCH" in
  let* identifier_name_opt1 = optional identifier_name in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* catch_filter_clause_opt1 = optional catch_filter_clause in
  let* soft_terminator1 = soft_terminator in
  let* statements_block1 = statements_block in
  pure (xRule "catch_block" 0 [xToken(catch1); xOptional(identifier_name_opt1); xOptional(simple_as_clause_opt1); xOptional(catch_filter_clause_opt1); soft_terminator1; statements_block1])
) __n

and simple_as_clause : G.any parser = fun __n -> (
  (* simple_as_clause -> 'As' attribute_list* type *)
  let* as1 = token "AS" in
  let* attribute_lists1 = list_of attribute_list in
  let* type_1 = type_ in
  pure (xRule "simple_as_clause" 0 [xToken(as1); xList(attribute_lists1); type_1])
) __n

and catch_filter_clause : G.any parser = fun __n -> (
  (* catch_filter_clause -> 'When' expression *)
  let* when1 = token "WHEN" in
  let* expression1 = expression in
  pure (xRule "catch_filter_clause" 0 [xToken(when1); G.E expression1])
) __n

and finally_block : G.any parser = fun __n -> (
  (* finally_block -> 'Finally' statements_block *)
  let* finally1 = token "FINALLY" in
  let* statements_block1 = statements_block in
  pure (xRule "finally_block" 0 [xToken(finally1); statements_block1])
) __n

and with_block : G.any parser = fun __n -> (
  (* with_block -> 'With' expression @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'With' *)
  let* with1 = token "WITH" in
  let* expression1 = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block1 = statements_block in
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* with2 = token "WITH" in
  pure (xRule "with_block" 0 [xToken(with1); G.E expression1; statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(with2)])
) __n

and sync_lock_block : G.any parser = fun __n -> (
  (* sync_lock_block -> 'SyncLock' expression @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'SyncLock' *)
  let* syncLock1 = token "SYNCLOCK" in
  let* expression1 = expression in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block1 = statements_block in
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* syncLock2 = token "SYNCLOCK" in
  pure (xRule "sync_lock_block" 0 [xToken(syncLock1); G.E expression1; statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(syncLock2)])
) __n

and using_block : G.any parser = fun __n -> (
  (* using_block -> 'Using' using_header @lookahead('<LINE_TERMINATOR>') statements_block end_using_statement *)
  let* using1 = token "USING" in
  let* using_header1 = using_header in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block1 = statements_block in
  let* end_using_statement1 = end_using_statement in
  pure (xRule "using_block" 0 [xToken(using1); using_header1; statements_block1; end_using_statement1])
) __n

and using_header : G.any parser = fun __n -> (
  (* using_header -> using_header_item (',' using_header_item)* *)
  let* using_header_item1 = using_header_item in
  let* comma_using_header_items1 = list_of 
    begin
      let* comma1 = token "," in
      let* using_header_item1 = using_header_item in
      pure (xGroup([xToken(comma1); using_header_item1]))
    end
  in
  pure (xRule "using_header" 0 [using_header_item1; xList(comma_using_header_items1)])
) __n

and using_header_item : G.any parser = fun __n -> (
  choice [
    begin
      (* using_header_item -> 'New'? variable_declarator *)
      let* new_opt1 = optional (token "NEW") in
      let* variable_declarator1 = variable_declarator in
      pure (xRule "using_header_item" 0 [xOptional(Option.map (fun x -> xToken x) new_opt1); variable_declarator1])
    end;
    begin
      (* using_header_item -> access_expression *)
      let* access_expression1 = access_expression in
      pure (xRule "using_header_item" 1 [G.E access_expression1])
    end;
  ]
) __n

and erase_statement : G.any parser = fun __n -> (
  (* erase_statement -> 'Erase' expression (',' expression)* *)
  let* erase1 = token "ERASE" in
  let* expression1 = expression in
  let* comma_expressions1 = list_of 
    begin
      let* comma1 = token "," in
      let* expression1 = expression in
      pure (xGroup([xToken(comma1); G.E expression1]))
    end
  in
  pure (xRule "erase_statement" 0 [xToken(erase1); G.E expression1; xList(comma_expressions1)])
) __n

and error_statement : G.any parser = fun __n -> (
  (* error_statement -> 'Error' expression *)
  let* error1 = token "ERROR" in
  let* expression1 = expression in
  pure (xRule "error_statement" 0 [xToken(error1); G.E expression1])
) __n

and continue_statement : G.any parser = fun __n -> (
  (* continue_statement -> 'Continue' continue_what *)
  let* continue1 = token "CONTINUE" in
  let* continue_what1 = continue_what in
  pure (xRule "continue_statement" 0 [xToken(continue1); continue_what1])
) __n

and continue_what : G.any parser = fun __n -> (
  choice [
    begin
      (* continue_what -> 'Do' *)
      let* do1 = token "DO" in
      pure (xRule "continue_what" 0 [xToken(do1)])
    end;
    begin
      (* continue_what -> 'For' *)
      let* for1 = token "FOR" in
      pure (xRule "continue_what" 1 [xToken(for1)])
    end;
    begin
      (* continue_what -> 'While' *)
      let* while1 = token "WHILE" in
      pure (xRule "continue_what" 2 [xToken(while1)])
    end;
  ]
) __n

and call_statement : G.any parser = fun __n -> (
  (* call_statement -> 'Call' expression *)
  let* call1 = token "CALL" in
  let* expression1 = expression in
  pure (xRule "call_statement" 0 [xToken(call1); G.E expression1])
) __n

and on_error_go_to_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* on_error_go_to_statement -> on_error_go_to_label_statement *)
      let* on_error_go_to_label_statement1 = on_error_go_to_label_statement in
      pure (xRule "on_error_go_to_statement" 0 [on_error_go_to_label_statement1])
    end;
    begin
      (* on_error_go_to_statement -> on_error_go_to_minus_one_statement *)
      let* on_error_go_to_minus_one_statement1 = on_error_go_to_minus_one_statement in
      pure (xRule "on_error_go_to_statement" 1 [on_error_go_to_minus_one_statement1])
    end;
    begin
      (* on_error_go_to_statement -> on_error_go_to_zero_statement *)
      let* on_error_go_to_zero_statement1 = on_error_go_to_zero_statement in
      pure (xRule "on_error_go_to_statement" 2 [on_error_go_to_zero_statement1])
    end;
  ]
) __n

and on_error_go_to_label_statement : G.any parser = fun __n -> (
  (* on_error_go_to_label_statement -> 'On' 'Error' 'GoTo' '-'? next_label *)
  let* on1 = token "ON" in
  let* error1 = token "ERROR" in
  let* goTo1 = token "GOTO" in
  let* minus_opt1 = optional (token "-") in
  let* next_label1 = next_label in
  pure (xRule "on_error_go_to_label_statement" 0 [xToken(on1); xToken(error1); xToken(goTo1); xOptional(Option.map (fun x -> xToken x) minus_opt1); next_label1])
) __n

and on_error_go_to_minus_one_statement : G.any parser = fun __n -> (
  (* on_error_go_to_minus_one_statement -> 'On' 'Error' 'GoTo' '-'? numeric_label *)
  let* on1 = token "ON" in
  let* error1 = token "ERROR" in
  let* goTo1 = token "GOTO" in
  let* minus_opt1 = optional (token "-") in
  let* numeric_label1 = numeric_label in
  pure (xRule "on_error_go_to_minus_one_statement" 0 [xToken(on1); xToken(error1); xToken(goTo1); xOptional(Option.map (fun x -> xToken x) minus_opt1); numeric_label1])
) __n

and on_error_go_to_zero_statement : G.any parser = fun __n -> (
  (* on_error_go_to_zero_statement -> 'On' 'Error' 'GoTo' '-'? identifier_label *)
  let* on1 = token "ON" in
  let* error1 = token "ERROR" in
  let* goTo1 = token "GOTO" in
  let* minus_opt1 = optional (token "-") in
  let* identifier_label1 = identifier_label in
  pure (xRule "on_error_go_to_zero_statement" 0 [xToken(on1); xToken(error1); xToken(goTo1); xOptional(Option.map (fun x -> xToken x) minus_opt1); identifier_label1])
) __n

and on_error_resume_next_statement : G.any parser = fun __n -> (
  (* on_error_resume_next_statement -> 'On' 'Error' 'Resume' 'Next' *)
  let* on1 = token "ON" in
  let* error1 = token "ERROR" in
  let* resume1 = token "RESUME" in
  let* next1 = token "NEXT" in
  pure (xRule "on_error_resume_next_statement" 0 [xToken(on1); xToken(error1); xToken(resume1); xToken(next1)])
) __n

and print_statement : G.any parser = fun __n -> (
  (* print_statement -> '?' expression *)
  let* qmark1 = token "?" in
  let* expression1 = expression in
  pure (xRule "print_statement" 0 [xToken(qmark1); G.E expression1])
) __n

and raise_event_statement : G.any parser = fun __n -> (
  (* raise_event_statement -> 'RaiseEvent' access_expression *)
  let* raiseEvent1 = token "RAISEEVENT" in
  let* access_expression1 = access_expression in
  pure (xRule "raise_event_statement" 0 [xToken(raiseEvent1); G.E access_expression1])
) __n

and resume_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* resume_statement -> 'Resume' 'Next' *)
      let* resume1 = token "RESUME" in
      let* next1 = token "NEXT" in
      pure (xRule "resume_statement" 0 [xToken(resume1); xToken(next1)])
    end;
    begin
      (* resume_statement -> 'Resume' numeric_label? *)
      let* resume1 = token "RESUME" in
      let* numeric_label_opt1 = optional numeric_label in
      pure (xRule "resume_statement" 1 [xToken(resume1); xOptional(numeric_label_opt1)])
    end;
    begin
      (* resume_statement -> 'Resume' identifier_label? *)
      let* resume1 = token "RESUME" in
      let* identifier_label_opt1 = optional identifier_label in
      pure (xRule "resume_statement" 2 [xToken(resume1); xOptional(identifier_label_opt1)])
    end;
  ]
) __n

and return_terminator : G.any parser = fun __n -> (
  choice [
    begin
      (* return_terminator -> @lookahead('<LINE_TERMINATOR>') *)
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      pure (xRule "return_terminator" 0 [])
    end;
    begin
      (* return_terminator -> @lookahead(',') *)
      let* _ = look_ahead "," in
      pure (xRule "return_terminator" 1 [])
    end;
    begin
      (* return_terminator -> @lookahead(')') *)
      let* _ = look_ahead ")" in
      pure (xRule "return_terminator" 2 [])
    end;
    begin
      (* return_terminator -> @lookahead('}') *)
      let* _ = look_ahead "}" in
      pure (xRule "return_terminator" 3 [])
    end;
  ]
) __n

and return_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* return_statement -> ':'? 'Return' return_terminator *)
      let* colon_opt1 = optional (token ":") in
      let* return1 = token "RETURN" in
      let* return_terminator1 = return_terminator in
      pure (xRule "return_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(return1); return_terminator1])
    end;
    begin
      (* return_statement -> ':'? 'Return' @lookahead_not('<LINE_TERMINATOR>') expression *)
      let* colon_opt1 = optional (token ":") in
      let* return1 = token "RETURN" in
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* expression1 = expression in
      pure (xRule "return_statement" 1 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(return1); G.E expression1])
    end;
  ]
) __n

and stop_or_end_statement : G.any parser = fun __n -> (
  (* stop_or_end_statement -> ':'? 'End' @lookahead('<LINE_TERMINATOR>') *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  pure (xRule "stop_or_end_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1)])
) __n

and yield_statement : G.any parser = fun __n -> (
  (* yield_statement -> 'Yield' expression *)
  let* yield1 = token "YIELD" in
  let* expression1 = expression in
  pure (xRule "yield_statement" 0 [xToken(yield1); G.E expression1])
) __n

and throw_statement : G.any parser = fun __n -> (
  (* throw_statement -> 'Throw' expression? *)
  let* throw1 = token "THROW" in
  let* expression_opt1 = optional expression in
  pure (xRule "throw_statement" 0 [xToken(throw1); xOptional(Option.map (fun e -> G.E e) expression_opt1)])
) __n

and toplevel_declaration : G.any parser = fun __n -> (
  (* toplevel_declaration -> attribute_list* modifier* toplevel_kw_declaration *)
  let* attribute_lists1 = list_of attribute_list in
  let* modifiers1 = list_of modifier in
  let* toplevel_kw_declaration1 = toplevel_kw_declaration in
  pure (xRule "toplevel_declaration" 0 [xList(attribute_lists1); xList(modifiers1); toplevel_kw_declaration1])
) __n

and toplevel_kw_declaration : G.any parser = fun __n -> (
  choice [
    begin
      (* toplevel_kw_declaration -> namespace_block *)
      let* namespace_block1 = namespace_block in
      pure (xRule "toplevel_kw_declaration" 0 [namespace_block1])
    end;
    begin
      (* toplevel_kw_declaration -> class_block *)
      let* class_block1 = class_block in
      pure (xRule "toplevel_kw_declaration" 1 [class_block1])
    end;
    begin
      (* toplevel_kw_declaration -> interface_block *)
      let* interface_block1 = interface_block in
      pure (xRule "toplevel_kw_declaration" 2 [interface_block1])
    end;
    begin
      (* toplevel_kw_declaration -> module_block *)
      let* module_block1 = module_block in
      pure (xRule "toplevel_kw_declaration" 3 [module_block1])
    end;
    begin
      (* toplevel_kw_declaration -> structure_block *)
      let* structure_block1 = structure_block in
      pure (xRule "toplevel_kw_declaration" 4 [structure_block1])
    end;
    begin
      (* toplevel_kw_declaration -> enum_block *)
      let* enum_block1 = enum_block in
      pure (xRule "toplevel_kw_declaration" 5 [enum_block1])
    end;
    begin
      (* toplevel_kw_declaration -> function_block *)
      let* function_block1 = function_block in
      pure (xRule "toplevel_kw_declaration" 6 [function_block1])
    end;
    begin
      (* toplevel_kw_declaration -> sub_block *)
      let* sub_block1 = sub_block in
      pure (xRule "toplevel_kw_declaration" 7 [sub_block1])
    end;
    begin
      (* toplevel_kw_declaration -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "toplevel_kw_declaration" 8 [xToken(dot_dot_dot1)])
    end;
    begin
      (* toplevel_kw_declaration -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "toplevel_kw_declaration" 9 [xToken(lt_dot_dot_dot1); G.E expression1; xToken(dot_dot_dot_gt1)])
    end;
  ]
) __n

and end_add_handler_statement : G.any parser = fun __n -> (
  (* end_add_handler_statement -> ':'? 'End' 'AddHandler' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* addHandler1 = token "ADDHANDLER" in
  pure (xRule "end_add_handler_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(addHandler1)])
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

and end_enum_statement : G.any parser = fun __n -> (
  (* end_enum_statement -> ':'? 'End' 'Enum' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* enum1 = token "ENUM" in
  pure (xRule "end_enum_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(enum1)])
) __n

and end_event_statement : G.any parser = fun __n -> (
  (* end_event_statement -> ':'? 'End' 'Event' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* event1 = token "EVENT" in
  pure (xRule "end_event_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(event1)])
) __n

and end_function_statement : G.any parser = fun __n -> (
  (* end_function_statement -> ':'? 'End' 'Function' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* function1 = token "FUNCTION" in
  pure (xRule "end_function_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(function1)])
) __n

and end_interface_statement : unit parser = fun __n -> (
  (* end_interface_statement -> ':'? 'End' 'Interface' *)
  choice [
    begin
      (* end_class_statement -> ':'? 'End' 'Class' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "INTERFACE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_module_statement : unit parser = fun __n -> (
  (* end_module_statement -> ':'? 'End' 'Module' *)
  choice [
    begin
      (* end_class_statement -> ':'? 'End' 'Class' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "MODULE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_namespace_statement : unit parser = fun __n -> (
  (* end_namespace_statement -> ':'? 'End' 'Namespace' *)
  choice [
    begin
      (* end_class_statement -> ':'? 'End' 'Class' *)
      let* _ = optional (token ":") in
      let* _ = token "END" in
      let* _ = token "NAMESPACE" in
      pure ()
    end;
    stop_on_partial
  ]
) __n |> cut

and end_operator_statement : G.any parser = fun __n -> (
  (* end_operator_statement -> ':'? 'End' 'Operator' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* operator1 = token "OPERATOR" in
  pure (xRule "end_operator_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(operator1)])
) __n

and end_property_statement : G.any parser = fun __n -> (
  (* end_property_statement -> ':'? 'End' 'Property' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* property1 = token "PROPERTY" in
  pure (xRule "end_property_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(property1)])
) __n

and end_raise_event_statement : G.any parser = fun __n -> (
  (* end_raise_event_statement -> ':'? 'End' 'RaiseEvent' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* raiseEvent1 = token "RAISEEVENT" in
  pure (xRule "end_raise_event_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(raiseEvent1)])
) __n

and end_remove_handler_statement : G.any parser = fun __n -> (
  (* end_remove_handler_statement -> ':'? 'End' 'RemoveHandler' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* removeHandler1 = token "REMOVEHANDLER" in
  pure (xRule "end_remove_handler_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(removeHandler1)])
) __n

and end_select_statement : G.any parser = fun __n -> (
  (* end_select_statement -> ':'? 'End' 'Select' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* select1 = token "SELECT" in
  pure (xRule "end_select_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(select1)])
) __n

and end_structure_statement : G.any parser = fun __n -> (
  (* end_structure_statement -> ':'? 'End' 'Structure' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* structure1 = token "STRUCTURE" in
  pure (xRule "end_structure_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(structure1)])
) __n

and end_sub_statement : G.any parser = fun __n -> (
  (* end_sub_statement -> ':'? 'End' 'Sub' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* sub1 = token "SUB" in
  pure (xRule "end_sub_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(sub1)])
) __n

and end_using_statement : G.any parser = fun __n -> (
  (* end_using_statement -> ':'? 'End' 'Using' *)
  let* colon_opt1 = optional (token ":") in
  let* end1 = token "END" in
  let* using1 = token "USING" in
  pure (xRule "end_using_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(using1)])
) __n

and enum_block : G.any parser = fun __n -> (
  (* enum_block -> enum_statement enum_block_item* end_enum_statement *)
  let* enum_statement1 = enum_statement in
  let* enum_block_items1 = list_of enum_block_item in
  let* end_enum_statement1 = end_enum_statement in
  pure (xRule "enum_block" 0 [enum_statement1; xList(enum_block_items1); end_enum_statement1])
) __n

and enum_statement : G.any parser = fun __n -> (
  (* enum_statement -> 'Enum' identifier_token as_clause? *)
  let* enum1 = token "ENUM" in
  let* identifier_token1 = identifier_token in
  let* as_clause_opt1 = optional as_clause in
  pure (xRule "enum_statement" 0 [xToken(enum1); identifier_token1; xOptional(as_clause_opt1)])
) __n

and enum_block_item : G.any parser = fun __n -> (
  (* enum_block_item -> attribute_list* single_line_statement ','? @lookahead('<LINE_TERMINATOR>') *)
  let* attribute_lists1 = list_of attribute_list in
  let* single_line_statement1 = single_line_statement in
  let* comma_opt1 = optional (token ",") in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  pure (xRule "enum_block_item" 0 [xList(attribute_lists1); single_line_statement1; xOptional(Option.map (fun x -> xToken x) comma_opt1)])
) __n

and as_clause : G.any parser = fun __n -> (
  choice [
    begin
      (* as_clause -> as_new_clause *)
      let* as_new_clause1 = as_new_clause in
      pure (xRule "as_clause" 0 [as_new_clause1])
    end;
    begin
      (* as_clause -> simple_as_clause *)
      let* simple_as_clause1 = simple_as_clause in
      pure (xRule "as_clause" 1 [simple_as_clause1])
    end;
  ]
) __n

and as_new_clause : G.any parser = fun __n -> (
  (* as_new_clause -> 'As' new_expression *)
  let* as1 = token "AS" in
  let* new_expression1 = new_expression in
  pure (xRule "as_new_clause" 0 [xToken(as1); new_expression1])
) __n

and new_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* new_expression -> anonymous_object_creation_expression *)
      let* anonymous_object_creation_expression1 = anonymous_object_creation_expression in
      pure (xRule "new_expression" 0 [anonymous_object_creation_expression1])
    end;
    begin
      (* new_expression -> array_creation_expression *)
      let* array_creation_expression1 = array_creation_expression in
      pure (xRule "new_expression" 1 [array_creation_expression1])
    end;
    begin
      (* new_expression -> object_creation_expression *)
      let* object_creation_expression1 = object_creation_expression in
      pure (xRule "new_expression" 2 [object_creation_expression1])
    end;
  ]
) __n

and anonymous_object_creation_expression : G.any parser = fun __n -> (
  (* anonymous_object_creation_expression -> 'New' attribute_list* (type argument_list?)? object_member_initializer *)
  let* new1 = token "NEW" in
  let* attribute_lists1 = list_of attribute_list in
  let* type__argument_list_opt_opt1 = optional 
    begin
      let* type_1 = type_ in
      let* argument_list_opt1 = optional argument_list in
      pure (xGroup([type_1; xOptional(argument_list_opt1)]))
    end
  in
  let* object_member_initializer1 = object_member_initializer in
  pure (xRule "anonymous_object_creation_expression" 0 [xToken(new1); xList(attribute_lists1); xOptional(type__argument_list_opt_opt1); object_member_initializer1])
) __n

and object_member_initializer : G.any parser = fun __n -> (
  (* object_member_initializer -> 'With' '{' (field_initializer (',' field_initializer)* )? '}' *)
  let* with1 = token "WITH" in
  let* lbrace1 = token "{" in
  let* field_initializer_comma_field_initializers_opt1 = optional 
    begin
      let* field_initializer1 = field_initializer in
      let* comma_field_initializers1 = list_of 
        begin
          let* comma1 = token "," in
          let* field_initializer1 = field_initializer in
          pure (xGroup([xToken(comma1); field_initializer1]))
        end
      in
      pure (xGroup([field_initializer1; xList(comma_field_initializers1)]))
    end
  in
  let* rbrace1 = token "}" in
  pure (xRule "object_member_initializer" 0 [xToken(with1); xToken(lbrace1); xOptional(field_initializer_comma_field_initializers_opt1); xToken(rbrace1)])
) __n

and field_initializer : G.any parser = fun __n -> (
  choice [
    begin
      (* field_initializer -> inferred_field_initializer *)
      let* inferred_field_initializer1 = inferred_field_initializer in
      pure (xRule "field_initializer" 0 [inferred_field_initializer1])
    end;
    begin
      (* field_initializer -> named_field_initializer *)
      let* named_field_initializer1 = named_field_initializer in
      pure (xRule "field_initializer" 1 [named_field_initializer1])
    end;
  ]
) __n

and inferred_field_initializer : G.any parser = fun __n -> (
  (* inferred_field_initializer -> 'Key'? expression *)
  let* key_opt1 = optional (token "KEY") in
  let* expression1 = expression in
  pure (xRule "inferred_field_initializer" 0 [xOptional(Option.map (fun x -> xToken x) key_opt1); G.E expression1])
) __n

and named_field_initializer : G.any parser = fun __n -> (
  (* named_field_initializer -> 'Key'? '.' identifier_name '=' expression *)
  let* key_opt1 = optional (token "KEY") in
  let* dot1 = token "." in
  let* identifier_name1 = identifier_name in
  let* eq1 = token "=" in
  let* expression1 = expression in
  pure (xRule "named_field_initializer" 0 [xOptional(Option.map (fun x -> xToken x) key_opt1); xToken(dot1); identifier_name1; xToken(eq1); G.E expression1])
) __n

and array_creation_expression : G.any parser = fun __n -> (
  (* array_creation_expression -> 'New' attribute_list* type argument_list? array_rank_specifier* collection_initializer *)
  let* new1 = token "NEW" in
  let* attribute_lists1 = list_of attribute_list in
  let* type_1 = type_ in
  let* argument_list_opt1 = optional argument_list in
  let* array_rank_specifiers1 = list_of array_rank_specifier in
  let* collection_initializer1 = collection_initializer in
  pure (xRule "array_creation_expression" 0 [xToken(new1); xList(attribute_lists1); type_1; xOptional(argument_list_opt1); xList(array_rank_specifiers1); collection_initializer1])
) __n

and array_rank_specifier : G.any parser = fun __n -> (
  (* array_rank_specifier -> '(' ','* ')' *)
  let* lparen1 = token "(" in
  let* commas1 = list_of (token ",") in
  let* rparen1 = token ")" in
  pure (xRule "array_rank_specifier" 0 [xToken(lparen1); xList(List.map (fun x -> xToken x) commas1); xToken(rparen1)])
) __n

and collection_initializer : G.any parser = fun __n -> (
  (* collection_initializer -> '{' (expression (',' expression)* )? '}' *)
  let* lbrace1 = token "{" in
  let* expression_comma_expressions_opt1 = optional 
    begin
      let* expression1 = expression in
      let* comma_expressions1 = list_of 
        begin
          let* comma1 = token "," in
          let* expression1 = expression in
          pure (xGroup([xToken(comma1); G.E expression1]))
        end
      in
      pure (xGroup([G.E expression1; xList(comma_expressions1)]))
    end
  in
  let* rbrace1 = token "}" in
  pure (xRule "collection_initializer" 0 [xToken(lbrace1); xOptional(expression_comma_expressions_opt1); xToken(rbrace1)])
) __n

and object_creation_expression : G.any parser = fun __n -> (
  (* object_creation_expression -> 'New' attribute_list* type argument_list? object_creation_initializer? *)
  let* new1 = token "NEW" in
  let* attribute_lists1 = list_of attribute_list in
  let* type_1 = type_ in
  let* argument_list_opt1 = optional argument_list in
  let* object_creation_initializer_opt1 = optional object_creation_initializer in
  pure (xRule "object_creation_expression" 0 [xToken(new1); xList(attribute_lists1); type_1; xOptional(argument_list_opt1); xOptional(object_creation_initializer_opt1)])
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

and equals_value : G.any parser = fun __n -> (
  (* equals_value -> '=' expression *)
  let* eq1 = token "=" in
  let* expression1 = expression in
  pure (xRule "equals_value" 0 [xToken(eq1); G.E expression1])
) __n

and event_block : G.any parser = fun __n -> (
  (* event_block -> event_statement (event_accessor_elem* end_event_statement)? *)
  let* event_statement1 = event_statement in
  let* event_accessor_elems_end_event_statement_opt1 = optional 
    begin
      let* event_accessor_elems1 = list_of event_accessor_elem in
      let* end_event_statement1 = end_event_statement in
      pure (xGroup([xList(event_accessor_elems1); end_event_statement1]))
    end
  in
  pure (xRule "event_block" 0 [event_statement1; xOptional(event_accessor_elems_end_event_statement_opt1)])
) __n

and event_statement : G.any parser = fun __n -> (
  (* event_statement -> 'Custom'? 'Event' identifier_token parameter_list? simple_as_clause? implements_clause? *)
  let* custom_opt1 = optional (token "CUSTOM") in
  let* event1 = token "EVENT" in
  let* identifier_token1 = identifier_token in
  let* parameter_list_opt1 = optional parameter_list in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* implements_clause_opt1 = optional implements_clause in
  pure (xRule "event_statement" 0 [xOptional(Option.map (fun x -> xToken x) custom_opt1); xToken(event1); identifier_token1; xOptional(parameter_list_opt1); xOptional(simple_as_clause_opt1); xOptional(implements_clause_opt1)])
) __n

and event_accessor_elem : G.any parser = fun __n -> (
  (* event_accessor_elem -> attribute_list* event_accessor_block *)
  let* attribute_lists1 = list_of attribute_list in
  let* event_accessor_block1 = event_accessor_block in
  pure (xRule "event_accessor_elem" 0 [xList(attribute_lists1); event_accessor_block1])
) __n

and event_accessor_block : G.any parser = fun __n -> (
  choice [
    begin
      (* event_accessor_block -> 'AddHandler' parameter_list statements_block end_add_handler_statement *)
      let* addHandler1 = token "ADDHANDLER" in
      let* parameter_list1 = parameter_list in
      let* statements_block1 = statements_block in
      let* end_add_handler_statement1 = end_add_handler_statement in
      pure (xRule "event_accessor_block" 0 [xToken(addHandler1); parameter_list1; statements_block1; end_add_handler_statement1])
    end;
    begin
      (* event_accessor_block -> 'RaiseEvent' parameter_list statements_block end_raise_event_statement *)
      let* raiseEvent1 = token "RAISEEVENT" in
      let* parameter_list1 = parameter_list in
      let* statements_block1 = statements_block in
      let* end_raise_event_statement1 = end_raise_event_statement in
      pure (xRule "event_accessor_block" 1 [xToken(raiseEvent1); parameter_list1; statements_block1; end_raise_event_statement1])
    end;
    begin
      (* event_accessor_block -> 'RemoveHandler' parameter_list statements_block end_remove_handler_statement *)
      let* removeHandler1 = token "REMOVEHANDLER" in
      let* parameter_list1 = parameter_list in
      let* statements_block1 = statements_block in
      let* end_remove_handler_statement1 = end_remove_handler_statement in
      pure (xRule "event_accessor_block" 2 [xToken(removeHandler1); parameter_list1; statements_block1; end_remove_handler_statement1])
    end;
  ]
) __n

and parameter_list : G.any parser = fun __n -> (
  (* parameter_list -> '(' (parameter (',' parameter)* )? ')' *)
  let* lparen1 = token "(" in
  let* parameter_comma_parameters_opt1 = optional 
    begin
      let* parameter1 = parameter in
      let* comma_parameters1 = list_of 
        begin
          let* comma1 = token "," in
          let* parameter1 = parameter in
          pure (xGroup([xToken(comma1); parameter1]))
        end
      in
      pure (xGroup([parameter1; xList(comma_parameters1)]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "parameter_list" 0 [xToken(lparen1); xOptional(parameter_comma_parameters_opt1); xToken(rparen1)])
) __n

and parameter : G.any parser = fun __n -> (
  choice [
    begin
      (* parameter -> attribute_list* modifier* modified_identifier simple_as_clause? equals_value? *)
      let* attribute_lists1 = list_of attribute_list in
      let* modifiers1 = list_of modifier in
      let* modified_identifier1 = modified_identifier in
      let* simple_as_clause_opt1 = optional simple_as_clause in
      let* equals_value_opt1 = optional equals_value in
      pure (xRule "parameter" 0 [xList(attribute_lists1); xList(modifiers1); modified_identifier1; xOptional(simple_as_clause_opt1); xOptional(equals_value_opt1)])
    end;
    begin
      (* parameter -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "parameter" 1 [xToken(dot_dot_dot1)])
    end;
    begin
      (* parameter -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "parameter" 2 [xToken(lt_dot_dot_dot1); G.E expression1; xToken(dot_dot_dot_gt1)])
    end;
  ]
) __n

and modified_identifier : G.any parser = fun __n -> (
  (* modified_identifier -> identifier_token '?'? argument_list? array_rank_specifier* *)
  let* identifier_token1 = identifier_token in
  let* qmark_opt1 = optional (token "?") in
  let* argument_list_opt1 = optional argument_list in
  let* array_rank_specifiers1 = list_of array_rank_specifier in
  pure (xRule "modified_identifier" 0 [identifier_token1; xOptional(Option.map (fun x -> xToken x) qmark_opt1); xOptional(argument_list_opt1); xList(array_rank_specifiers1)])
) __n

and implements_clause : G.any parser = fun __n -> (
  (* implements_clause -> ':'? 'Implements' type_list_elem (',' type_list_elem)* *)
  let* colon_opt1 = optional (token ":") in
  let* implements1 = token "IMPLEMENTS" in
  let* type_list_elem1 = type_list_elem in
  let* comma_type_list_elems1 = list_of 
    begin
      let* comma1 = token "," in
      let* type_list_elem1 = type_list_elem in
      pure (xGroup([xToken(comma1); type_list_elem1]))
    end
  in
  pure (xRule "implements_clause" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(implements1); type_list_elem1; xList(comma_type_list_elems1)])
) __n

and type_list_elem : G.any parser = fun __n -> (
  choice [
    begin
      (* type_list_elem -> type *)
      let* type_1 = type_ in
      pure (xRule "type_list_elem" 0 [type_1])
    end;
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
  ]
) __n

and qualified_name : G.any parser = fun __n -> (
  (* qualified_name -> simple_name ('.' identifier_or_keyword)* *)
  let* simple_name1 = simple_name in
  let* dot_identifier_or_keywords1 = list_of 
    begin
      let* dot1 = token "." in
      let* identifier_or_keyword1 = identifier_or_keyword in
      pure (xGroup([xToken(dot1); identifier_or_keyword1]))
    end
  in
  pure (xRule "qualified_name" 0 [simple_name1; xList(dot_identifier_or_keywords1)])
) __n

and simple_name : G.any parser = fun __n -> (
  choice [
    begin
      (* simple_name -> generic_name *)
      let* generic_name1 = generic_name in
      pure (xRule "simple_name" 0 [generic_name1])
    end;
    begin
      (* simple_name -> identifier_name *)
      let* identifier_name1 = identifier_name in
      pure (xRule "simple_name" 1 [identifier_name1])
    end;
  ]
) __n

and generic_name : G.any parser = fun __n -> (
  (* generic_name -> identifier_token type_argument_list *)
  let* identifier_token1 = identifier_token in
  let* type_argument_list1 = type_argument_list in
  pure (xRule "generic_name" 0 [identifier_token1; type_argument_list1])
) __n

and type_argument_list : G.any parser = fun __n -> (
  (* type_argument_list -> '(' 'Of' type_list_elem? (',' type_list_elem?)* ')' *)
  let* lparen1 = token "(" in
  let* of1 = token "OF" in
  let* type_list_elem_opt1 = optional type_list_elem in
  let* comma_type_list_elem_opts1 = list_of 
    begin
      let* comma1 = token "," in
      let* type_list_elem_opt1 = optional type_list_elem in
      pure (xGroup([xToken(comma1); xOptional(type_list_elem_opt1)]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "type_argument_list" 0 [xToken(lparen1); xToken(of1); xOptional(type_list_elem_opt1); xList(comma_type_list_elem_opts1); xToken(rparen1)])
) __n

and field_declaration : G.any parser = fun __n -> (
  (* field_declaration -> attribute_list* modifier+ variable_declarator (',' variable_declarator)* *)
  let* attribute_lists1 = list_of attribute_list in
  let* modifiers1 = ne_list_of modifier in
  let* variable_declarator1 = variable_declarator in
  let* comma_variable_declarators1 = list_of 
    begin
      let* comma1 = token "," in
      let* variable_declarator1 = variable_declarator in
      pure (xGroup([xToken(comma1); variable_declarator1]))
    end
  in
  pure (xRule "field_declaration" 0 [xList(attribute_lists1); xList(modifiers1); variable_declarator1; xList(comma_variable_declarators1)])
) __n

and variable_declarator : G.any parser = fun __n -> (
  (* variable_declarator -> modified_identifier (',' modified_identifier)* as_clause? equals_value? *)
  let* modified_identifier1 = modified_identifier in
  let* comma_modified_identifiers1 = list_of 
    begin
      let* comma1 = token "," in
      let* modified_identifier1 = modified_identifier in
      pure (xGroup([xToken(comma1); modified_identifier1]))
    end
  in
  let* as_clause_opt1 = optional as_clause in
  let* equals_value_opt1 = optional equals_value in
  pure (xRule "variable_declarator" 0 [modified_identifier1; xList(comma_modified_identifiers1); xOptional(as_clause_opt1); xOptional(equals_value_opt1)])
) __n

and implements_statement : G.any parser = fun __n -> (
  (* implements_statement -> ':'? 'Implements' type (',' type)* *)
  let* colon_opt1 = optional (token ":") in
  let* implements1 = token "IMPLEMENTS" in
  let* type_1 = type_ in
  let* comma_type_s1 = list_of 
    begin
      let* comma1 = token "," in
      let* type_1 = type_ in
      pure (xGroup([xToken(comma1); type_1]))
    end
  in
  pure (xRule "implements_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(implements1); type_1; xList(comma_type_s1)])
) __n

and inherits_statement : G.any parser = fun __n -> (
  (* inherits_statement -> ':'? 'Inherits' type (',' type)* *)
  let* colon_opt1 = optional (token ":") in
  let* inherits1 = token "INHERITS" in
  let* type_1 = type_ in
  let* comma_type_s1 = list_of 
    begin
      let* comma1 = token "," in
      let* type_1 = type_ in
      pure (xGroup([xToken(comma1); type_1]))
    end
  in
  pure (xRule "inherits_statement" 0 [xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(inherits1); type_1; xList(comma_type_s1)])
) __n

and text_encoding : G.any parser = fun __n -> (
  choice [
    begin
      (* text_encoding -> 'Ansi' *)
      let* ansi1 = token "ANSI" in
      pure (xRule "text_encoding" 0 [xToken(ansi1)])
    end;
    begin
      (* text_encoding -> 'Unicode' *)
      let* unicode1 = token "UNICODE" in
      pure (xRule "text_encoding" 1 [xToken(unicode1)])
    end;
    begin
      (* text_encoding -> 'Auto' *)
      let* auto1 = token "AUTO" in
      pure (xRule "text_encoding" 2 [xToken(auto1)])
    end;
  ]
) __n

and function_or_sub : G.any parser = fun __n -> (
  choice [
    begin
      (* function_or_sub -> 'Function' *)
      let* function1 = token "FUNCTION" in
      pure (xRule "function_or_sub" 0 [xToken(function1)])
    end;
    begin
      (* function_or_sub -> 'Sub' *)
      let* sub1 = token "SUB" in
      pure (xRule "function_or_sub" 1 [xToken(sub1)])
    end;
  ]
) __n

and declare_statement : G.any parser = fun __n -> (
  (* declare_statement -> 'Declare' text_encoding? function_or_sub identifier_token 'Lib' literal_expression ('Alias' literal_expression)? parameter_list simple_as_clause? *)
  let* declare1 = token "DECLARE" in
  let* text_encoding_opt1 = optional text_encoding in
  let* function_or_sub1 = function_or_sub in
  let* identifier_token1 = identifier_token in
  let* lib1 = token "LIB" in
  let* literal_expression1 = literal_expression in
  let* alias_literal_expression_opt1 = optional 
    begin
      let* alias1 = token "ALIAS" in
      let* literal_expression1 = literal_expression in
      pure (xGroup([xToken(alias1); G.E literal_expression1]))
    end
  in
  let* parameter_list1 = parameter_list in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  pure (xRule "declare_statement" 0 [xToken(declare1); xOptional(text_encoding_opt1); function_or_sub1; identifier_token1; xToken(lib1); G.E literal_expression1; xOptional(alias_literal_expression_opt1); parameter_list1; xOptional(simple_as_clause_opt1)])
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
    (* TODO
    begin
      (* literal_expression -> character_literal_token *)
      let* character_literal_token1 = character_literal_token in
      pure (xRule "literal_expression" 3 [character_literal_token1])
    end;
    begin
      (* literal_expression -> date_literal_token *)
      let* date_literal_token1 = date_literal_token in
      pure (xRule "literal_expression" 4 [date_literal_token1])
    end;
    begin
      (* literal_expression -> decimal_literal_token *)
      let* decimal_literal_token1 = decimal_literal_token in
      pure (xRule "literal_expression" 5 [decimal_literal_token1])
    end;
    begin
      (* literal_expression -> floating_literal_token *)
      let* floating_literal_token1 = floating_literal_token in
      pure (xRule "literal_expression" 6 [floating_literal_token1])
    end;
    begin
      (* literal_expression -> integer_literal_token *)
      let* integer_literal_token1 = integer_literal_token in
      pure (xRule "literal_expression" 7 [integer_literal_token1])
    end;
    begin
      (* literal_expression -> string_literal_token *)
      let* string_literal_token1 = string_literal_token in
      pure (xRule "literal_expression" 8 [string_literal_token1])
    end; *)
  ]
) __n

and type_parameter_list : G.any parser = fun __n -> (
  (* type_parameter_list -> '(' 'Of' type_parameter (',' type_parameter)* ')' *)
  let* lparen1 = token "(" in
  let* of1 = token "OF" in
  let* type_parameter1 = type_parameter in
  let* comma_type_parameters1 = list_of 
    begin
      let* comma1 = token "," in
      let* type_parameter1 = type_parameter in
      pure (xGroup([xToken(comma1); type_parameter1]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "type_parameter_list" 0 [xToken(lparen1); xToken(of1); type_parameter1; xList(comma_type_parameters1); xToken(rparen1)])
) __n

and type_parameter : G.any parser = fun __n -> (
  choice [
    begin
      (* type_parameter -> 'In'? identifier_token type_parameter_constraint_clause? *)
      let* in_opt1 = optional (token "IN") in
      let* identifier_token1 = identifier_token in
      let* type_parameter_constraint_clause_opt1 = optional type_parameter_constraint_clause in
      pure (xRule "type_parameter" 0 [xOptional(Option.map (fun x -> xToken x) in_opt1); identifier_token1; xOptional(type_parameter_constraint_clause_opt1)])
    end;
    begin
      (* type_parameter -> 'Out'? identifier_token type_parameter_constraint_clause? *)
      let* out_opt1 = optional (token "OUT") in
      let* identifier_token1 = identifier_token in
      let* type_parameter_constraint_clause_opt1 = optional type_parameter_constraint_clause in
      pure (xRule "type_parameter" 1 [xOptional(Option.map (fun x -> xToken x) out_opt1); identifier_token1; xOptional(type_parameter_constraint_clause_opt1)])
    end;
  ]
) __n

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

and class_constraint : G.any parser = fun __n -> (
  (* class_constraint -> 'Class' *)
  let* class1 = token "CLASS" in
  pure (xRule "class_constraint" 0 [xToken(class1)])
) __n

and new_constraint : G.any parser = fun __n -> (
  (* new_constraint -> 'New' *)
  let* new1 = token "NEW" in
  pure (xRule "new_constraint" 0 [xToken(new1)])
) __n

and structure_constraint : G.any parser = fun __n -> (
  (* structure_constraint -> 'Structure' *)
  let* structure1 = token "STRUCTURE" in
  pure (xRule "structure_constraint" 0 [xToken(structure1)])
) __n

and type_constraint : G.any parser = fun __n -> (
  (* type_constraint -> type *)
  let* type_1 = type_ in
  pure (xRule "type_constraint" 0 [type_1])
) __n

and type_parameter_single_constraint_clause : G.any parser = fun __n -> (
  (* type_parameter_single_constraint_clause -> 'As' constraint *)
  let* as1 = token "AS" in
  let* constraint_1 = constraint_ in
  pure (xRule "type_parameter_single_constraint_clause" 0 [xToken(as1); constraint_1])
) __n

and function_statement : G.any parser = fun __n -> (
  (* function_statement -> 'Function' identifier_token type_parameter_list? parameter_list? simple_as_clause? handles_clause? implements_clause? *)
  let* function1 = token "FUNCTION" in
  let* identifier_token1 = identifier_token in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  let* parameter_list_opt1 = optional parameter_list in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* handles_clause_opt1 = optional handles_clause in
  let* implements_clause_opt1 = optional implements_clause in
  pure (xRule "function_statement" 0 [xToken(function1); identifier_token1; xOptional(type_parameter_list_opt1); xOptional(parameter_list_opt1); xOptional(simple_as_clause_opt1); xOptional(handles_clause_opt1); xOptional(implements_clause_opt1)])
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
  pure (xRule "handles_clause_item" 0 [event_container1; xToken(dot1); identifier_name1])
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
  pure (xRule "with_events_property_event_container" 0 [with_events_event_container1; xToken(dot1); identifier_name1])
) __n

and sub_statement : G.any parser = fun __n -> (
  (* sub_statement -> 'Sub' identifier_token type_parameter_list? parameter_list? simple_as_clause? handles_clause? implements_clause? *)
  let* sub1 = token "SUB" in
  let* identifier_token1 = identifier_token in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  let* parameter_list_opt1 = optional parameter_list in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* handles_clause_opt1 = optional handles_clause in
  let* implements_clause_opt1 = optional implements_clause in
  pure (xRule "sub_statement" 0 [xToken(sub1); identifier_token1; xOptional(type_parameter_list_opt1); xOptional(parameter_list_opt1); xOptional(simple_as_clause_opt1); xOptional(handles_clause_opt1); xOptional(implements_clause_opt1)])
) __n

and operator_statement_operator : G.any parser = fun __n -> (
  choice [
    begin
      (* operator_statement_operator -> 'CType' *)
      let* cType1 = token "CTYPE" in
      pure (xRule "operator_statement_operator" 0 [xToken(cType1)])
    end;
    begin
      (* operator_statement_operator -> 'IsTrue' *)
      let* isTrue1 = token "ISTRUE" in
      pure (xRule "operator_statement_operator" 1 [xToken(isTrue1)])
    end;
    begin
      (* operator_statement_operator -> 'IsFalse' *)
      let* isFalse1 = token "ISFALSE" in
      pure (xRule "operator_statement_operator" 2 [xToken(isFalse1)])
    end;
    begin
      (* operator_statement_operator -> 'Not' *)
      let* not1 = token "NOT" in
      pure (xRule "operator_statement_operator" 3 [xToken(not1)])
    end;
    begin
      (* operator_statement_operator -> '+' *)
      let* plus1 = token "+" in
      pure (xRule "operator_statement_operator" 4 [xToken(plus1)])
    end;
    begin
      (* operator_statement_operator -> '-' *)
      let* minus1 = token "-" in
      pure (xRule "operator_statement_operator" 5 [xToken(minus1)])
    end;
    begin
      (* operator_statement_operator -> '*' *)
      let* star1 = token "*" in
      pure (xRule "operator_statement_operator" 6 [xToken(star1)])
    end;
    begin
      (* operator_statement_operator -> '/' *)
      let* slash1 = token "/" in
      pure (xRule "operator_statement_operator" 7 [xToken(slash1)])
    end;
    begin
      (* operator_statement_operator -> '^' *)
      let* caret1 = token "^" in
      pure (xRule "operator_statement_operator" 8 [xToken(caret1)])
    end;
    begin
      (* operator_statement_operator -> '\\' *)
      let* backslash1 = token "\\" in
      pure (xRule "operator_statement_operator" 9 [xToken(backslash1)])
    end;
    begin
      (* operator_statement_operator -> '&' *)
      let* amp1 = token "&" in
      pure (xRule "operator_statement_operator" 10 [xToken(amp1)])
    end;
    begin
      (* operator_statement_operator -> '<<' *)
      let* lt_lt1 = token "<<" in
      pure (xRule "operator_statement_operator" 11 [xToken(lt_lt1)])
    end;
    begin
      (* operator_statement_operator -> '>>' *)
      let* gt_gt1 = token ">>" in
      pure (xRule "operator_statement_operator" 12 [xToken(gt_gt1)])
    end;
    begin
      (* operator_statement_operator -> 'Mod' *)
      let* mod1 = token "MOD" in
      pure (xRule "operator_statement_operator" 13 [xToken(mod1)])
    end;
    begin
      (* operator_statement_operator -> 'Or' *)
      let* or1 = token "OR" in
      pure (xRule "operator_statement_operator" 14 [xToken(or1)])
    end;
    begin
      (* operator_statement_operator -> 'Xor' *)
      let* xor1 = token "XOR" in
      pure (xRule "operator_statement_operator" 15 [xToken(xor1)])
    end;
    begin
      (* operator_statement_operator -> 'And' *)
      let* and1 = token "AND" in
      pure (xRule "operator_statement_operator" 16 [xToken(and1)])
    end;
    begin
      (* operator_statement_operator -> 'Like' *)
      let* like1 = token "LIKE" in
      pure (xRule "operator_statement_operator" 17 [xToken(like1)])
    end;
    begin
      (* operator_statement_operator -> '=' *)
      let* eq1 = token "=" in
      pure (xRule "operator_statement_operator" 18 [xToken(eq1)])
    end;
    begin
      (* operator_statement_operator -> '<>' *)
      let* lt_gt1 = token "<>" in
      pure (xRule "operator_statement_operator" 19 [xToken(lt_gt1)])
    end;
    begin
      (* operator_statement_operator -> '<' *)
      let* lt1 = token "<" in
      pure (xRule "operator_statement_operator" 20 [xToken(lt1)])
    end;
    begin
      (* operator_statement_operator -> '<=' *)
      let* lt_eq1 = token "<=" in
      pure (xRule "operator_statement_operator" 21 [xToken(lt_eq1)])
    end;
    begin
      (* operator_statement_operator -> '>=' *)
      let* gt_eq1 = token ">=" in
      pure (xRule "operator_statement_operator" 22 [xToken(gt_eq1)])
    end;
    begin
      (* operator_statement_operator -> '>' *)
      let* gt1 = token ">" in
      pure (xRule "operator_statement_operator" 23 [xToken(gt1)])
    end;
  ]
) __n

and operator_statement : G.any parser = fun __n -> (
  (* operator_statement -> 'Operator' operator_statement_operator parameter_list? simple_as_clause? *)
  let* operator1 = token "OPERATOR" in
  let* operator_statement_operator1 = operator_statement_operator in
  let* parameter_list_opt1 = optional parameter_list in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  pure (xRule "operator_statement" 0 [xToken(operator1); operator_statement_operator1; xOptional(parameter_list_opt1); xOptional(simple_as_clause_opt1)])
) __n

and property_statement : G.any parser = fun __n -> (
  (* property_statement -> 'Property' identifier_token parameter_list? as_clause? equals_value? implements_clause? *)
  let* property1 = token "PROPERTY" in
  let* identifier_token1 = identifier_token in
  let* parameter_list_opt1 = optional parameter_list in
  let* as_clause_opt1 = optional as_clause in
  let* equals_value_opt1 = optional equals_value in
  let* implements_clause_opt1 = optional implements_clause in
  pure (xRule "property_statement" 0 [xToken(property1); identifier_token1; xOptional(parameter_list_opt1); xOptional(as_clause_opt1); xOptional(equals_value_opt1); xOptional(implements_clause_opt1)])
) __n

and sub_new_statement : G.any parser = fun __n -> (
  (* sub_new_statement -> 'Sub' 'New' parameter_list? *)
  let* sub1 = token "SUB" in
  let* new1 = token "NEW" in
  let* parameter_list_opt1 = optional parameter_list in
  pure (xRule "sub_new_statement" 0 [xToken(sub1); xToken(new1); xOptional(parameter_list_opt1)])
) __n

and constructor_block : G.any parser = fun __n -> (
  (* constructor_block -> sub_new_statement @lookahead('<LINE_TERMINATOR>') statements_block end_sub_statement *)
  let* sub_new_statement1 = sub_new_statement in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block1 = statements_block in
  let* end_sub_statement1 = end_sub_statement in
  pure (xRule "constructor_block" 0 [sub_new_statement1; statements_block1; end_sub_statement1])
) __n

and function_block : G.any parser = fun __n -> (
  (* function_block -> function_statement @lookahead('<LINE_TERMINATOR>') (statements_block end_function_statement)? *)
  let* function_statement1 = function_statement in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block_end_function_statement_opt1 = optional 
    begin
      let* statements_block1 = statements_block in
      let* end_function_statement1 = end_function_statement in
      pure (xGroup([statements_block1; end_function_statement1]))
    end
  in
  pure (xRule "function_block" 0 [function_statement1; xOptional(statements_block_end_function_statement_opt1)])
) __n

and sub_block : G.any parser = fun __n -> (
  (* sub_block -> sub_statement @lookahead('<LINE_TERMINATOR>') (statements_block end_sub_statement)? *)
  let* sub_statement1 = sub_statement in
  let* _ = look_ahead "<LINE_TERMINATOR>" in
  let* statements_block_end_sub_statement_opt1 = optional 
    begin
      let* statements_block1 = statements_block in
      let* end_sub_statement1 = end_sub_statement in
      pure (xGroup([statements_block1; end_sub_statement1]))
    end
  in
  pure (xRule "sub_block" 0 [sub_statement1; xOptional(statements_block_end_sub_statement_opt1)])
) __n

and operator_block : G.any parser = fun __n -> (
  (* operator_block -> operator_statement statements_block end_operator_statement *)
  let* operator_statement1 = operator_statement in
  let* statements_block1 = statements_block in
  let* end_operator_statement1 = end_operator_statement in
  pure (xRule "operator_block" 0 [operator_statement1; statements_block1; end_operator_statement1])
) __n

and namespace_block : G.any parser = fun __n -> (
  (* namespace_block -> 'Namespace' qualified_name ('.' identifier_name)* toplevel end_namespace_statement *)
  let* namespace1 = token "NAMESPACE" in
  let* qualified_name1 = qualified_name in
  let* dot_identifier_names1 = list_of 
    begin
      let* dot1 = token "." in
      let* identifier_name1 = identifier_name in
      pure (xGroup([xToken(dot1); identifier_name1]))
    end
  in
  let* toplevel1 = toplevel in
  let* _ = end_namespace_statement in
  pure (xRule "namespace_block" 0 [xToken(namespace1); qualified_name1; xList(dot_identifier_names1); toplevel1])
) __n

and property_block : G.any parser = fun __n -> (
  (* property_block -> property_statement (property_accessor_block* end_property_statement)? *)
  let* property_statement1 = property_statement in
  let* property_accessor_blocks_end_property_statement_opt1 = optional 
    begin
      let* property_accessor_blocks1 = list_of property_accessor_block in
      let* end_property_statement1 = end_property_statement in
      pure (xGroup([xList(property_accessor_blocks1); end_property_statement1]))
    end
  in
  pure (xRule "property_block" 0 [property_statement1; xOptional(property_accessor_blocks_end_property_statement_opt1)])
) __n

and property_accessor_block : G.any parser = fun __n -> (
  choice [
    begin
      (* property_accessor_block -> attribute_list* modifier* 'Get' statements_block ':'? 'End' 'Get' *)
      let* attribute_lists1 = list_of attribute_list in
      let* modifiers1 = list_of modifier in
      let* get1 = token "GET" in
      let* statements_block1 = statements_block in
      let* colon_opt1 = optional (token ":") in
      let* end1 = token "END" in
      let* get2 = token "GET" in
      pure (xRule "property_accessor_block" 0 [xList(attribute_lists1); xList(modifiers1); xToken(get1); statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(get2)])
    end;
    begin
      (* property_accessor_block -> attribute_list* modifier* 'Set' parameter_list? statements_block ':'? 'End' 'Set' *)
      let* attribute_lists1 = list_of attribute_list in
      let* modifiers1 = list_of modifier in
      let* set1 = token "SET" in
      let* parameter_list_opt1 = optional parameter_list in
      let* statements_block1 = statements_block in
      let* colon_opt1 = optional (token ":") in
      let* end1 = token "END" in
      let* set2 = token "SET" in
      pure (xRule "property_accessor_block" 1 [xList(attribute_lists1); xList(modifiers1); xToken(set1); xOptional(parameter_list_opt1); statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(set2)])
    end;
    begin
      (* property_accessor_block -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "property_accessor_block" 2 [xToken(dot_dot_dot1)])
    end;
    begin
      (* property_accessor_block -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "property_accessor_block" 3 [xToken(lt_dot_dot_dot1); G.E expression1; xToken(dot_dot_dot_gt1)])
    end;
  ]
) __n

and class_block : G.any parser = fun __n -> (
  (* class_block -> class_statement inherits_statement* implements_statement* class_block_declaration* end_class_statement *)
  let* class_statement1 = class_statement in
  let* inherits_statements1 = list_of inherits_statement in
  let* implements_statements1 = list_of implements_statement in
  let* class_block_declarations1 = list_of class_block_declaration in
  let* _ = end_class_statement in
  pure (xRule "class_block" 0 [class_statement1; xList(inherits_statements1); xList(implements_statements1); xList(class_block_declarations1)])
) __n

and class_statement : G.any parser = fun __n -> (
  (* class_statement -> 'Class' qualified_name type_parameter_list? *)
  let* class1 = token "CLASS" in
  let* qualified_name1 = qualified_name in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  pure (xRule "class_statement" 0 [xToken(class1); qualified_name1; xOptional(type_parameter_list_opt1)])
) __n

and class_block_declaration : G.any parser = fun __n -> (
  choice [
    begin
      (* class_block_declaration -> attribute_list* method_modifier* class_block_kw_declaration *)
      let* attribute_lists1 = list_of attribute_list in
      let* method_modifiers1 = list_of method_modifier in
      let* class_block_kw_declaration1 = class_block_kw_declaration in
      pure (xRule "class_block_declaration" 0 [xList(attribute_lists1); xList(method_modifiers1); class_block_kw_declaration1])
    end;
    begin
      (* class_block_declaration -> field_declaration *)
      let* field_declaration1 = field_declaration in
      pure (xRule "class_block_declaration" 1 [field_declaration1])
    end;
  ]
) __n

and class_block_kw_declaration : G.any parser = fun __n -> (
  choice [
    begin
      (* class_block_kw_declaration -> function_block *)
      let* function_block1 = function_block in
      pure (xRule "class_block_kw_declaration" 0 [function_block1])
    end;
    begin
      (* class_block_kw_declaration -> constructor_block *)
      let* constructor_block1 = constructor_block in
      pure (xRule "class_block_kw_declaration" 1 [constructor_block1])
    end;
    begin
      (* class_block_kw_declaration -> sub_block *)
      let* sub_block1 = sub_block in
      pure (xRule "class_block_kw_declaration" 2 [sub_block1])
    end;
    begin
      (* class_block_kw_declaration -> property_block *)
      let* property_block1 = property_block in
      pure (xRule "class_block_kw_declaration" 3 [property_block1])
    end;
    begin
      (* class_block_kw_declaration -> class_block *)
      let* class_block1 = class_block in
      pure (xRule "class_block_kw_declaration" 4 [class_block1])
    end;
    begin
      (* class_block_kw_declaration -> module_block *)
      let* module_block1 = module_block in
      pure (xRule "class_block_kw_declaration" 5 [module_block1])
    end;
    begin
      (* class_block_kw_declaration -> interface_block *)
      let* interface_block1 = interface_block in
      pure (xRule "class_block_kw_declaration" 6 [interface_block1])
    end;
    begin
      (* class_block_kw_declaration -> structure_block *)
      let* structure_block1 = structure_block in
      pure (xRule "class_block_kw_declaration" 7 [structure_block1])
    end;
    begin
      (* class_block_kw_declaration -> event_block *)
      let* event_block1 = event_block in
      pure (xRule "class_block_kw_declaration" 8 [event_block1])
    end;
    begin
      (* class_block_kw_declaration -> operator_block *)
      let* operator_block1 = operator_block in
      pure (xRule "class_block_kw_declaration" 9 [operator_block1])
    end;
    begin
      (* class_block_kw_declaration -> enum_block *)
      let* enum_block1 = enum_block in
      pure (xRule "class_block_kw_declaration" 10 [enum_block1])
    end;
    begin
      (* class_block_kw_declaration -> declare_statement *)
      let* declare_statement1 = declare_statement in
      pure (xRule "class_block_kw_declaration" 11 [declare_statement1])
    end;
    begin
      (* class_block_kw_declaration -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "class_block_kw_declaration" 12 [xToken(dot_dot_dot1)])
    end;
    begin
      (* class_block_kw_declaration -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "class_block_kw_declaration" 13 [xToken(lt_dot_dot_dot1); G.E expression1; xToken(dot_dot_dot_gt1)])
    end;
  ]
) __n

and interface_block : G.any parser = fun __n -> (
  (* interface_block -> interface_statement inherits_statement* implements_statement* class_block_declaration* end_interface_statement *)
  let* interface_statement1 = interface_statement in
  let* inherits_statements1 = list_of inherits_statement in
  let* implements_statements1 = list_of implements_statement in
  let* class_block_declarations1 = list_of class_block_declaration in
  let* _ = end_interface_statement in
  pure (xRule "interface_block" 0 [interface_statement1; xList(inherits_statements1); xList(implements_statements1); xList(class_block_declarations1)])
) __n

and interface_statement : G.any parser = fun __n -> (
  (* interface_statement -> 'Interface' qualified_name type_parameter_list? *)
  let* interface1 = token "INTERFACE" in
  let* qualified_name1 = qualified_name in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  pure (xRule "interface_statement" 0 [xToken(interface1); qualified_name1; xOptional(type_parameter_list_opt1)])
) __n

and module_block : G.any parser = fun __n -> (
  (* module_block -> module_statement inherits_statement* implements_statement* class_block_declaration* end_module_statement *)
  let* module_statement1 = module_statement in
  let* inherits_statements1 = list_of inherits_statement in
  let* implements_statements1 = list_of implements_statement in
  let* class_block_declarations1 = list_of class_block_declaration in
  let* _ = end_module_statement in
  pure (xRule "module_block" 0 [module_statement1; xList(inherits_statements1); xList(implements_statements1); xList(class_block_declarations1)])
) __n

and module_statement : G.any parser = fun __n -> (
  (* module_statement -> 'Module' qualified_name type_parameter_list? *)
  let* module1 = token "MODULE" in
  let* qualified_name1 = qualified_name in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  pure (xRule "module_statement" 0 [xToken(module1); qualified_name1; xOptional(type_parameter_list_opt1)])
) __n

and structure_block : G.any parser = fun __n -> (
  (* structure_block -> structure_statement inherits_statement* implements_statement* class_block_declaration* end_structure_statement *)
  let* structure_statement1 = structure_statement in
  let* inherits_statements1 = list_of inherits_statement in
  let* implements_statements1 = list_of implements_statement in
  let* class_block_declarations1 = list_of class_block_declaration in
  let* end_structure_statement1 = end_structure_statement in
  pure (xRule "structure_block" 0 [structure_statement1; xList(inherits_statements1); xList(implements_statements1); xList(class_block_declarations1); end_structure_statement1])
) __n

and structure_statement : G.any parser = fun __n -> (
  (* structure_statement -> 'Structure' qualified_name type_parameter_list? *)
  let* structure1 = token "STRUCTURE" in
  let* qualified_name1 = qualified_name in
  let* type_parameter_list_opt1 = optional type_parameter_list in
  pure (xRule "structure_statement" 0 [xToken(structure1); qualified_name1; xOptional(type_parameter_list_opt1)])
) __n

and add_remove_handler_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* add_remove_handler_statement -> add_handler_statement *)
      let* add_handler_statement1 = add_handler_statement in
      pure (xRule "add_remove_handler_statement" 0 [add_handler_statement1])
    end;
    begin
      (* add_remove_handler_statement -> remove_handler_statement *)
      let* remove_handler_statement1 = remove_handler_statement in
      pure (xRule "add_remove_handler_statement" 1 [remove_handler_statement1])
    end;
  ]
) __n

and add_handler_statement : G.any parser = fun __n -> (
  (* add_handler_statement -> 'AddHandler' expression ',' expression *)
  let* addHandler1 = token "ADDHANDLER" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* expression2 = expression in
  pure (xRule "add_handler_statement" 0 [xToken(addHandler1); G.E expression1; xToken(comma1); G.E expression2])
) __n

and remove_handler_statement : G.any parser = fun __n -> (
  (* remove_handler_statement -> 'RemoveHandler' expression ',' expression *)
  let* removeHandler1 = token "REMOVEHANDLER" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* expression2 = expression in
  pure (xRule "remove_handler_statement" 0 [xToken(removeHandler1); G.E expression1; xToken(comma1); G.E expression2])
) __n

and assignment_statement_operator : G.any parser = fun __n -> (
  choice [
    begin
      (* assignment_statement_operator -> '=' *)
      let* eq1 = token "=" in
      pure (xRule "assignment_statement_operator" 0 [xToken(eq1)])
    end;
    begin
      (* assignment_statement_operator -> '+=' *)
      let* plus_eq1 = token "+=" in
      pure (xRule "assignment_statement_operator" 1 [xToken(plus_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '-=' *)
      let* minus_eq1 = token "-=" in
      pure (xRule "assignment_statement_operator" 2 [xToken(minus_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '*=' *)
      let* star_eq1 = token "*=" in
      pure (xRule "assignment_statement_operator" 3 [xToken(star_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '/=' *)
      let* slash_eq1 = token "/=" in
      pure (xRule "assignment_statement_operator" 4 [xToken(slash_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '\\=' *)
      let* backslash_eq1 = token "\\=" in
      pure (xRule "assignment_statement_operator" 5 [xToken(backslash_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '^=' *)
      let* caret_eq1 = token "^=" in
      pure (xRule "assignment_statement_operator" 6 [xToken(caret_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '<<=' *)
      let* lt_lt_eq1 = token "<<=" in
      pure (xRule "assignment_statement_operator" 7 [xToken(lt_lt_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '>>=' *)
      let* gt_gt_eq1 = token ">>=" in
      pure (xRule "assignment_statement_operator" 8 [xToken(gt_gt_eq1)])
    end;
    begin
      (* assignment_statement_operator -> '&=' *)
      let* amp_eq1 = token "&=" in
      pure (xRule "assignment_statement_operator" 9 [xToken(amp_eq1)])
    end;
  ]
) __n

and assignment_statement : G.any parser = fun __n -> (
  (* assignment_statement -> await_expression (assignment_statement_operator expression)? *)
  let* await_expression1 = await_expression in
  let* assignment_statement_operator_expression_opt1 = optional 
    begin
      let* assignment_statement_operator1 = assignment_statement_operator in
      let* expression1 = expression in
      pure (xGroup([assignment_statement_operator1; G.E expression1]))
    end
  in
  pure (xRule "assignment_statement" 0 [G.E await_expression1; xOptional(assignment_statement_operator_expression_opt1)])
) __n

and exit_statement : G.any parser = fun __n -> (
  choice [
    begin
      (* exit_statement -> exit_do_statement *)
      let* exit_do_statement1 = exit_do_statement in
      pure (xRule "exit_statement" 0 [exit_do_statement1])
    end;
    begin
      (* exit_statement -> exit_for_statement *)
      let* exit_for_statement1 = exit_for_statement in
      pure (xRule "exit_statement" 1 [exit_for_statement1])
    end;
    begin
      (* exit_statement -> exit_function_statement *)
      let* exit_function_statement1 = exit_function_statement in
      pure (xRule "exit_statement" 2 [exit_function_statement1])
    end;
    begin
      (* exit_statement -> exit_operator_statement *)
      let* exit_operator_statement1 = exit_operator_statement in
      pure (xRule "exit_statement" 3 [exit_operator_statement1])
    end;
    begin
      (* exit_statement -> exit_property_statement *)
      let* exit_property_statement1 = exit_property_statement in
      pure (xRule "exit_statement" 4 [exit_property_statement1])
    end;
    begin
      (* exit_statement -> exit_select_statement *)
      let* exit_select_statement1 = exit_select_statement in
      pure (xRule "exit_statement" 5 [exit_select_statement1])
    end;
    begin
      (* exit_statement -> exit_sub_statement *)
      let* exit_sub_statement1 = exit_sub_statement in
      pure (xRule "exit_statement" 6 [exit_sub_statement1])
    end;
    begin
      (* exit_statement -> exit_try_statement *)
      let* exit_try_statement1 = exit_try_statement in
      pure (xRule "exit_statement" 7 [exit_try_statement1])
    end;
    begin
      (* exit_statement -> exit_while_statement *)
      let* exit_while_statement1 = exit_while_statement in
      pure (xRule "exit_statement" 8 [exit_while_statement1])
    end;
  ]
) __n

and exit_do_statement : G.any parser = fun __n -> (
  (* exit_do_statement -> 'Exit' 'Do' *)
  let* exit1 = token "EXIT" in
  let* do1 = token "DO" in
  pure (xRule "exit_do_statement" 0 [xToken(exit1); xToken(do1)])
) __n

and exit_for_statement : G.any parser = fun __n -> (
  (* exit_for_statement -> 'Exit' 'For' *)
  let* exit1 = token "EXIT" in
  let* for1 = token "FOR" in
  pure (xRule "exit_for_statement" 0 [xToken(exit1); xToken(for1)])
) __n

and exit_function_statement : G.any parser = fun __n -> (
  (* exit_function_statement -> 'Exit' 'Function' *)
  let* exit1 = token "EXIT" in
  let* function1 = token "FUNCTION" in
  pure (xRule "exit_function_statement" 0 [xToken(exit1); xToken(function1)])
) __n

and exit_operator_statement : G.any parser = fun __n -> (
  (* exit_operator_statement -> 'Exit' 'Operator' *)
  let* exit1 = token "EXIT" in
  let* operator1 = token "OPERATOR" in
  pure (xRule "exit_operator_statement" 0 [xToken(exit1); xToken(operator1)])
) __n

and exit_property_statement : G.any parser = fun __n -> (
  (* exit_property_statement -> 'Exit' 'Property' *)
  let* exit1 = token "EXIT" in
  let* property1 = token "PROPERTY" in
  pure (xRule "exit_property_statement" 0 [xToken(exit1); xToken(property1)])
) __n

and exit_select_statement : G.any parser = fun __n -> (
  (* exit_select_statement -> 'Exit' 'Select' *)
  let* exit1 = token "EXIT" in
  let* select1 = token "SELECT" in
  pure (xRule "exit_select_statement" 0 [xToken(exit1); xToken(select1)])
) __n

and exit_sub_statement : G.any parser = fun __n -> (
  (* exit_sub_statement -> 'Exit' 'Sub' *)
  let* exit1 = token "EXIT" in
  let* sub1 = token "SUB" in
  pure (xRule "exit_sub_statement" 0 [xToken(exit1); xToken(sub1)])
) __n

and exit_try_statement : G.any parser = fun __n -> (
  (* exit_try_statement -> 'Exit' 'Try' *)
  let* exit1 = token "EXIT" in
  let* try1 = token "TRY" in
  pure (xRule "exit_try_statement" 0 [xToken(exit1); xToken(try1)])
) __n

and exit_while_statement : G.any parser = fun __n -> (
  (* exit_while_statement -> 'Exit' 'While' *)
  let* exit1 = token "EXIT" in
  let* while1 = token "WHILE" in
  pure (xRule "exit_while_statement" 0 [xToken(exit1); xToken(while1)])
) __n

and go_to_statement : G.any parser = fun __n -> (
  (* go_to_statement -> 'GoTo' label *)
  let* goTo1 = token "GOTO" in
  let* label1 = label in
  pure (xRule "go_to_statement" 0 [xToken(goTo1); label1])
) __n

and label : G.any parser = fun __n -> (
  choice [
    begin
      (* label -> identifier_label *)
      let* identifier_label1 = identifier_label in
      pure (xRule "label" 0 [identifier_label1])
    end;
    begin
      (* label -> next_label *)
      let* next_label1 = next_label in
      pure (xRule "label" 1 [next_label1])
    end;
    begin
      (* label -> numeric_label *)
      let* numeric_label1 = numeric_label in
      pure (xRule "label" 2 [numeric_label1])
    end;
  ]
) __n

and identifier_label : G.any parser = fun __n -> (
  (* identifier_label -> identifier_token *)
  let* identifier_token1 = identifier_token in
  pure (xRule "identifier_label" 0 [identifier_token1])
) __n

and next_label : G.any parser = fun __n -> (
  (* next_label -> 'Next' *)
  let* next1 = token "NEXT" in
  pure (xRule "next_label" 0 [xToken(next1)])
) __n

and numeric_label : G.any parser = fun __n -> (
  (* numeric_label -> integer_literal_token *)
  let* integer_literal_token1 = integer_literal_token in
  pure (xRule "numeric_label" 0 [integer_literal_token1])
) __n

and local_declaration_statement : G.any parser = fun __n -> (
  (* local_declaration_statement -> modifier+ variable_declarator (',' variable_declarator)* *)
  let* modifiers1 = ne_list_of modifier in
  let* variable_declarator1 = variable_declarator in
  let* comma_variable_declarators1 = list_of 
    begin
      let* comma1 = token "," in
      let* variable_declarator1 = variable_declarator in
      pure (xGroup([xToken(comma1); variable_declarator1]))
    end
  in
  pure (xRule "local_declaration_statement" 0 [xList(modifiers1); variable_declarator1; xList(comma_variable_declarators1)])
) __n

and re_dim_statement : G.any parser = fun __n -> (
  (* re_dim_statement -> 'ReDim' 'Preserve'? redim_clause (',' redim_clause)* *)
  let* reDim1 = token "REDIM" in
  let* preserve_opt1 = optional (token "PRESERVE") in
  let* redim_clause1 = redim_clause in
  let* comma_redim_clauses1 = list_of 
    begin
      let* comma1 = token "," in
      let* redim_clause1 = redim_clause in
      pure (xGroup([xToken(comma1); redim_clause1]))
    end
  in
  pure (xRule "re_dim_statement" 0 [xToken(reDim1); xOptional(Option.map (fun x -> xToken x) preserve_opt1); redim_clause1; xList(comma_redim_clauses1)])
) __n

and redim_clause : G.any parser = fun __n -> (
  (* redim_clause -> access_expression *)
  let* access_expression1 = access_expression in
  pure (xRule "redim_clause" 0 [G.E access_expression1])
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

and binary_conditional_expression : G.any parser = fun __n -> (
  (* binary_conditional_expression -> 'If' '(' expression ',' expression ')' *)
  let* if1 = token "IF" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* expression2 = expression in
  let* rparen1 = token ")" in
  pure (xRule "binary_conditional_expression" 0 [xToken(if1); xToken(lparen1); G.E expression1; xToken(comma1); G.E expression2; xToken(rparen1)])
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

(* TODO *)
and access_expression : G.expr parser = fun __n -> (
  (* access_expression -> primary_expression (@lookahead_not('<LINE_TERMINATOR>') accessor)* *)
  let* e = primary_expression in
  let* _lookahead_not_lt_LINE_TERMINATOR_gt_accessors1 = list_of
    begin
      let* _ = look_ahead_not "<LINE_TERMINATOR>" in
      let* accessor1 = accessor in
      pure (xGroup([accessor1]))
    end
  in
  pure (e)
) __n

and identifier_or_keyword : G.any parser = fun __n -> (
  choice [
    begin
      (* identifier_or_keyword -> identifier_name type_argument_list? *)
      let* identifier_name1 = identifier_name in
      let* type_argument_list_opt1 = optional type_argument_list in
      pure (xRule "identifier_or_keyword" 0 [identifier_name1; xOptional(type_argument_list_opt1)])
    end;
    begin
      (* identifier_or_keyword -> '<KEYWORD>' type_argument_list? *)
      let* lt_KEYWORD_gt1 = token "<KEYWORD>" in
      let* type_argument_list_opt1 = optional type_argument_list in
      pure (xRule "identifier_or_keyword" 1 [xToken(lt_KEYWORD_gt1); xOptional(type_argument_list_opt1)])
    end;
  ]
) __n

and accessor : G.any parser = fun __n -> (
  choice [
    begin
      (* accessor -> '.' accessor_body *)
      let* dot1 = token "." in
      let* accessor_body1 = accessor_body in
      pure (xRule "accessor" 0 [xToken(dot1); accessor_body1])
    end;
    begin
      (* accessor -> '.' argument_list *)
      let* dot1 = token "." in
      let* argument_list1 = argument_list in
      pure (xRule "accessor" 1 [xToken(dot1); argument_list1])
    end;
    begin
      (* accessor -> '!' accessor_body *)
      let* bang1 = token "!" in
      let* accessor_body1 = accessor_body in
      pure (xRule "accessor" 2 [xToken(bang1); accessor_body1])
    end;
    begin
      (* accessor -> '?.' accessor_body *)
      let* qmark_dot1 = token "?." in
      let* accessor_body1 = accessor_body in
      pure (xRule "accessor" 3 [xToken(qmark_dot1); accessor_body1])
    end;
    begin
      (* accessor -> '?' '(' expression ')' *)
      let* qmark1 = token "?" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "accessor" 4 [xToken(qmark1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* accessor -> '.@' accessor_body *)
      let* dot_at1 = token ".@" in
      let* accessor_body1 = accessor_body in
      pure (xRule "accessor" 5 [xToken(dot_at1); accessor_body1])
    end;
    begin
      (* accessor -> '.' '<' accessor_body '>' *)
      let* dot1 = token "." in
      let* lt1 = token "<" in
      let* accessor_body1 = accessor_body in
      let* gt1 = token ">" in
      pure (xRule "accessor" 6 [xToken(dot1); xToken(lt1); accessor_body1; xToken(gt1)])
    end;
    begin
      (* accessor -> '...' '<' accessor_body '>' *)
      let* dot_dot_dot1 = token "..." in
      let* lt1 = token "<" in
      let* accessor_body1 = accessor_body in
      let* gt1 = token ">" in
      pure (xRule "accessor" 7 [xToken(dot_dot_dot1); xToken(lt1); accessor_body1; xToken(gt1)])
    end;
    begin
      (* accessor -> argument_list *)
      let* argument_list1 = argument_list in
      pure (xRule "accessor" 8 [argument_list1])
    end;
  ]
) __n

and accessor_body : G.any parser = fun __n -> (
  choice [
    begin
      (* accessor_body -> identifier_or_keyword *)
      let* identifier_or_keyword1 = identifier_or_keyword in
      pure (xRule "accessor_body" 0 [identifier_or_keyword1])
    end;
    begin
      (* accessor_body -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "accessor_body" 1 [xToken(dot_dot_dot1)])
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
    (* TODO
    begin
      (* primary_expression -> binary_conditional_expression *)
      let* binary_conditional_expression1 = binary_conditional_expression in
      pure (xRule "primary_expression" 2 [binary_conditional_expression1])
    end;
    begin
      (* primary_expression -> get_type_expression *)
      let* get_type_expression1 = get_type_expression in
      pure (xRule "primary_expression" 3 [get_type_expression1])
    end;
    begin
      (* primary_expression -> cast_expression *)
      let* cast_expression1 = cast_expression in
      pure (xRule "primary_expression" 4 [cast_expression1])
    end;
    begin
      (* primary_expression -> collection_initializer *)
      let* collection_initializer1 = collection_initializer in
      pure (xRule "primary_expression" 5 [collection_initializer1])
    end;
    begin
      (* primary_expression -> get_xml_namespace_expression *)
      let* get_xml_namespace_expression1 = get_xml_namespace_expression in
      pure (xRule "primary_expression" 6 [get_xml_namespace_expression1])
    end;
    begin
      (* primary_expression -> instance_expression *)
      let* instance_expression1 = instance_expression in
      pure (xRule "primary_expression" 7 [instance_expression1])
    end;
    begin
      (* primary_expression -> interpolated_string_expression *)
      let* interpolated_string_expression1 = interpolated_string_expression in
      pure (xRule "primary_expression" 8 [interpolated_string_expression1])
    end;
    begin
      (* primary_expression -> lambda_expression *)
      let* lambda_expression1 = lambda_expression in
      pure (xRule "primary_expression" 9 [lambda_expression1])
    end;
    begin
      (* primary_expression -> name_of_expression *)
      let* name_of_expression1 = name_of_expression in
      pure (xRule "primary_expression" 10 [name_of_expression1])
    end;
    begin
      (* primary_expression -> new_expression *)
      let* new_expression1 = new_expression in
      pure (xRule "primary_expression" 11 [new_expression1])
    end;
    begin
      (* primary_expression -> predefined_cast_expression *)
      let* predefined_cast_expression1 = predefined_cast_expression in
      pure (xRule "primary_expression" 12 [predefined_cast_expression1])
    end;
    begin
      (* primary_expression -> query_expression *)
      let* query_expression1 = query_expression in
      pure (xRule "primary_expression" 13 [query_expression1])
    end;
    begin
      (* primary_expression -> ternary_conditional_expression *)
      let* ternary_conditional_expression1 = ternary_conditional_expression in
      pure (xRule "primary_expression" 14 [ternary_conditional_expression1])
    end;
    begin
      (* primary_expression -> anonymous_object_creation_expression *)
      let* anonymous_object_creation_expression1 = anonymous_object_creation_expression in
      pure (xRule "primary_expression" 15 [anonymous_object_creation_expression1])
    end;
    begin
      (* primary_expression -> array_creation_expression *)
      let* array_creation_expression1 = array_creation_expression in
      pure (xRule "primary_expression" 16 [array_creation_expression1])
    end;
    begin
      (* primary_expression -> tuple_expression *)
      let* tuple_expression1 = tuple_expression in
      pure (xRule "primary_expression" 17 [tuple_expression1])
    end;
    begin
      (* primary_expression -> type_of_expression *)
      let* type_of_expression1 = type_of_expression in
      pure (xRule "primary_expression" 18 [type_of_expression1])
    end;
    begin
      (* primary_expression -> identifier_expression *)
      let* identifier_expression1 = identifier_expression in
      pure (xRule "primary_expression" 19 [identifier_expression1])
    end;
    begin
      (* primary_expression -> xml_cdata *)
      let* xml_cdata1 = xml_cdata in
      pure (xRule "primary_expression" 20 [xml_cdata1])
    end;
    begin
      (* primary_expression -> x_expression *)
      let* x_expression1 = x_expression in
      pure (xRule "primary_expression" 21 [x_expression1])
    end;
    begin
      (* primary_expression -> '...' *)
      let* dot_dot_dot1 = token "..." in
      pure (xRule "primary_expression" 22 [xToken(dot_dot_dot1)])
    end;
    begin
      (* primary_expression -> '<...' expression '...>' *)
      let* lt_dot_dot_dot1 = token "<..." in
      let* expression1 = expression in
      let* dot_dot_dot_gt1 = token "...>" in
      pure (xRule "primary_expression" 23 [xToken(lt_dot_dot_dot1); expression1; xToken(dot_dot_dot_gt1)])
    end;
    *)
  ]
) __n

and identifier_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* identifier_expression -> '.' identifier_or_keyword *)
      let* dot1 = token "." in
      let* identifier_or_keyword1 = identifier_or_keyword in
      pure (xRule "identifier_expression" 0 [xToken(dot1); identifier_or_keyword1])
    end;
    begin
      (* identifier_expression -> identifier_name type_argument_list? *)
      let* identifier_name1 = identifier_name in
      let* type_argument_list_opt1 = optional type_argument_list in
      pure (xRule "identifier_expression" 1 [identifier_name1; xOptional(type_argument_list_opt1)])
    end;
    begin
      (* identifier_expression -> 'Mid' '$' *)
      let* mid1 = token "MID" in
      let* dollar1 = token "$" in
      pure (xRule "identifier_expression" 2 [xToken(mid1); xToken(dollar1)])
    end;
  ]
) __n

and get_type_expression : G.any parser = fun __n -> (
  (* get_type_expression -> 'GetType' '(' qualified_name '?' ')' *)
  let* getType1 = token "GETTYPE" in
  let* lparen1 = token "(" in
  let* qualified_name1 = qualified_name in
  let* qmark1 = token "?" in
  let* rparen1 = token ")" in
  pure (xRule "get_type_expression" 0 [xToken(getType1); xToken(lparen1); qualified_name1; xToken(qmark1); xToken(rparen1)])
) __n

and cast_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* cast_expression -> c_type_expression *)
      let* c_type_expression1 = c_type_expression in
      pure (xRule "cast_expression" 0 [c_type_expression1])
    end;
    begin
      (* cast_expression -> direct_cast_expression *)
      let* direct_cast_expression1 = direct_cast_expression in
      pure (xRule "cast_expression" 1 [direct_cast_expression1])
    end;
    begin
      (* cast_expression -> try_cast_expression *)
      let* try_cast_expression1 = try_cast_expression in
      pure (xRule "cast_expression" 2 [try_cast_expression1])
    end;
  ]
) __n

and c_type_expression : G.any parser = fun __n -> (
  (* c_type_expression -> 'CType' '(' expression ',' type ')' *)
  let* cType1 = token "CTYPE" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* type_1 = type_ in
  let* rparen1 = token ")" in
  pure (xRule "c_type_expression" 0 [xToken(cType1); xToken(lparen1); G.E expression1; xToken(comma1); type_1; xToken(rparen1)])
) __n

and direct_cast_expression : G.any parser = fun __n -> (
  (* direct_cast_expression -> 'DirectCast' '(' expression ',' type ')' *)
  let* directCast1 = token "DIRECTCAST" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* type_1 = type_ in
  let* rparen1 = token ")" in
  pure (xRule "direct_cast_expression" 0 [xToken(directCast1); xToken(lparen1); G.E expression1; xToken(comma1); type_1; xToken(rparen1)])
) __n

and try_cast_expression : G.any parser = fun __n -> (
  (* try_cast_expression -> 'TryCast' '(' expression ',' type ')' *)
  let* tryCast1 = token "TRYCAST" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* type_1 = type_ in
  let* rparen1 = token ")" in
  pure (xRule "try_cast_expression" 0 [xToken(tryCast1); xToken(lparen1); G.E expression1; xToken(comma1); type_1; xToken(rparen1)])
) __n

and get_xml_namespace_expression : G.any parser = fun __n -> (
  (* get_xml_namespace_expression -> 'GetXmlNamespace' '(' xml_prefix_name? ')' *)
  let* getXmlNamespace1 = token "GETXMLNAMESPACE" in
  let* lparen1 = token "(" in
  let* xml_prefix_name_opt1 = optional xml_prefix_name in
  let* rparen1 = token ")" in
  pure (xRule "get_xml_namespace_expression" 0 [xToken(getXmlNamespace1); xToken(lparen1); xOptional(xml_prefix_name_opt1); xToken(rparen1)])
) __n

and xml_prefix_name : G.any parser = fun __n -> (
  (* xml_prefix_name -> '<IDENT>' *)
  let* lt_IDENT_gt1 = token "<IDENT>" in
  pure (xRule "xml_prefix_name" 0 [xToken(lt_IDENT_gt1)])
) __n

and instance_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* instance_expression -> 'Me' *)
      let* me1 = token "ME" in
      pure (xRule "instance_expression" 0 [xToken(me1)])
    end;
    begin
      (* instance_expression -> 'MyBase' *)
      let* myBase1 = token "MYBASE" in
      pure (xRule "instance_expression" 1 [xToken(myBase1)])
    end;
    begin
      (* instance_expression -> 'MyClass' *)
      let* myClass1 = token "MYCLASS" in
      pure (xRule "instance_expression" 2 [xToken(myClass1)])
    end;
  ]
) __n

and interpolated_string_expression : G.any parser = fun __n -> (
  (* interpolated_string_expression -> '$"' interpolated_string_content* '"' *)
  let* dollar_dquote1 = token "$\"" in
  let* interpolated_string_contents1 = list_of interpolated_string_content in
  let* dquote1 = token "\"" in
  pure (xRule "interpolated_string_expression" 0 [xToken(dollar_dquote1); xList(interpolated_string_contents1); xToken(dquote1)])
) __n

and interpolated_string_content : G.any parser = fun __n -> (
  choice [
    begin
      (* interpolated_string_content -> '<STRING_SEGMENT>' *)
      let* lt_STRING_SEGMENT_gt1 = token "<STRING_SEGMENT>" in
      pure (xRule "interpolated_string_content" 0 [xToken(lt_STRING_SEGMENT_gt1)])
    end;
    begin
      (* interpolated_string_content -> interpolation *)
      let* interpolation1 = interpolation in
      pure (xRule "interpolated_string_content" 1 [interpolation1])
    end;
  ]
) __n

and interpolation : G.any parser = fun __n -> (
  (* interpolation -> expression interpolation_alignment_clause? (':' interpolation_format_char* )? *)
  let* expression1 = expression in
  let* interpolation_alignment_clause_opt1 = optional interpolation_alignment_clause in
  let* colon_interpolation_format_chars_opt1 = optional 
    begin
      let* colon1 = token ":" in
      let* interpolation_format_chars1 = list_of interpolation_format_char in
      pure (xGroup([xToken(colon1); xList(interpolation_format_chars1)]))
    end
  in
  pure (xRule "interpolation" 0 [G.E expression1; xOptional(interpolation_alignment_clause_opt1); xOptional(colon_interpolation_format_chars_opt1)])
) __n

and interpolation_alignment_clause : G.any parser = fun __n -> (
  (* interpolation_alignment_clause -> ',' unary_expression *)
  let* comma1 = token "," in
  let* unary_expression1 = unary_expression in
  pure (xRule "interpolation_alignment_clause" 0 [xToken(comma1); G.E unary_expression1])
) __n

and interpolation_format_char : G.any parser = fun __n -> (
  choice [
    begin
      (* interpolation_format_char -> @lookahead_not('\\"') @lookahead_not('$\\"') '<OPERATOR>' *)
      let* _ = look_ahead_not "\"" in
      let* _ = look_ahead_not "$\"" in
      let* lt_OPERATOR_gt1 = token "<OPERATOR>" in
      pure (xRule "interpolation_format_char" 0 [xToken(lt_OPERATOR_gt1)])
    end;
    begin
      (* interpolation_format_char -> '.' *)
      let* dot1 = token "." in
      pure (xRule "interpolation_format_char" 1 [xToken(dot1)])
    end;
    begin
      (* interpolation_format_char -> '<IDENT>' *)
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xRule "interpolation_format_char" 2 [xToken(lt_IDENT_gt1)])
    end;
    begin
      (* interpolation_format_char -> '<KEYWORD>' *)
      let* lt_KEYWORD_gt1 = token "<KEYWORD>" in
      pure (xRule "interpolation_format_char" 3 [xToken(lt_KEYWORD_gt1)])
    end;
    begin
      (* interpolation_format_char -> '<INT>' *)
      let* lt_INT_gt1 = token "<INT>" in
      pure (xRule "interpolation_format_char" 4 [xToken(lt_INT_gt1)])
    end;
    begin
      (* interpolation_format_char -> '<FLOAT>' *)
      let* lt_FLOAT_gt1 = token "<FLOAT>" in
      pure (xRule "interpolation_format_char" 5 [xToken(lt_FLOAT_gt1)])
    end;
  ]
) __n

and lambda_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* lambda_expression -> multi_line_lambda_expression *)
      let* multi_line_lambda_expression1 = multi_line_lambda_expression in
      pure (xRule "lambda_expression" 0 [multi_line_lambda_expression1])
    end;
    begin
      (* lambda_expression -> single_line_lambda_expression *)
      let* single_line_lambda_expression1 = single_line_lambda_expression in
      pure (xRule "lambda_expression" 1 [single_line_lambda_expression1])
    end;
  ]
) __n

and lambda_modifier : G.any parser = fun __n -> (
  choice [
    begin
      (* lambda_modifier -> 'Async' *)
      let* async1 = token "ASYNC" in
      pure (xRule "lambda_modifier" 0 [xToken(async1)])
    end;
    begin
      (* lambda_modifier -> 'Iterator' *)
      let* iterator1 = token "ITERATOR" in
      pure (xRule "lambda_modifier" 1 [xToken(iterator1)])
    end;
  ]
) __n

and single_line_lambda_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* single_line_lambda_expression -> lambda_modifier* 'Function' parameter_list expression *)
      let* lambda_modifiers1 = list_of lambda_modifier in
      let* function1 = token "FUNCTION" in
      let* parameter_list1 = parameter_list in
      let* expression1 = expression in
      pure (xRule "single_line_lambda_expression" 0 [xList(lambda_modifiers1); xToken(function1); parameter_list1; G.E expression1])
    end;
    begin
      (* single_line_lambda_expression -> lambda_modifier* 'Sub' parameter_list single_line_statement *)
      let* lambda_modifiers1 = list_of lambda_modifier in
      let* sub1 = token "SUB" in
      let* parameter_list1 = parameter_list in
      let* single_line_statement1 = single_line_statement in
      pure (xRule "single_line_lambda_expression" 1 [xList(lambda_modifiers1); xToken(sub1); parameter_list1; single_line_statement1])
    end;
  ]
) __n

and multi_line_lambda_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* multi_line_lambda_expression -> lambda_modifier* 'Function' parameter_list simple_as_clause? @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'Function' *)
      let* lambda_modifiers1 = list_of lambda_modifier in
      let* function1 = token "FUNCTION" in
      let* parameter_list1 = parameter_list in
      let* simple_as_clause_opt1 = optional simple_as_clause in
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      let* statements_block1 = statements_block in
      let* colon_opt1 = optional (token ":") in
      let* end1 = token "END" in
      let* function2 = token "FUNCTION" in
      pure (xRule "multi_line_lambda_expression" 0 [xList(lambda_modifiers1); xToken(function1); parameter_list1; xOptional(simple_as_clause_opt1); statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(function2)])
    end;
    begin
      (* multi_line_lambda_expression -> lambda_modifier* 'Sub' parameter_list @lookahead('<LINE_TERMINATOR>') statements_block ':'? 'End' 'Sub' *)
      let* lambda_modifiers1 = list_of lambda_modifier in
      let* sub1 = token "SUB" in
      let* parameter_list1 = parameter_list in
      let* _ = look_ahead "<LINE_TERMINATOR>" in
      let* statements_block1 = statements_block in
      let* colon_opt1 = optional (token ":") in
      let* end1 = token "END" in
      let* sub2 = token "SUB" in
      pure (xRule "multi_line_lambda_expression" 1 [xList(lambda_modifiers1); xToken(sub1); parameter_list1; statements_block1; xOptional(Option.map (fun x -> xToken x) colon_opt1); xToken(end1); xToken(sub2)])
    end;
  ]
) __n

and name_of_expression : G.any parser = fun __n -> (
  (* name_of_expression -> 'NameOf' '(' expression ')' *)
  let* nameOf1 = token "NAMEOF" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* rparen1 = token ")" in
  pure (xRule "name_of_expression" 0 [xToken(nameOf1); xToken(lparen1); G.E expression1; xToken(rparen1)])
) __n

and predefined_cast_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* predefined_cast_expression -> 'CBool' '(' expression ')' *)
      let* cBool1 = token "CBOOL" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 0 [xToken(cBool1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CByte' '(' expression ')' *)
      let* cByte1 = token "CBYTE" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 1 [xToken(cByte1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CChar' '(' expression ')' *)
      let* cChar1 = token "CCHAR" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 2 [xToken(cChar1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CDate' '(' expression ')' *)
      let* cDate1 = token "CDATE" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 3 [xToken(cDate1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CDbl' '(' expression ')' *)
      let* cDbl1 = token "CDBL" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 4 [xToken(cDbl1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CDec' '(' expression ')' *)
      let* cDec1 = token "CDEC" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 5 [xToken(cDec1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CInt' '(' expression ')' *)
      let* cInt1 = token "CINT" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 6 [xToken(cInt1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CLng' '(' expression ')' *)
      let* cLng1 = token "CLNG" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 7 [xToken(cLng1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CObj' '(' expression ')' *)
      let* cObj1 = token "COBJ" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 8 [xToken(cObj1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CSByte' '(' expression ')' *)
      let* cSByte1 = token "CSBYTE" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 9 [xToken(cSByte1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CShort' '(' expression ')' *)
      let* cShort1 = token "CSHORT" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 10 [xToken(cShort1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CSng' '(' expression ')' *)
      let* cSng1 = token "CSNG" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 11 [xToken(cSng1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CStr' '(' expression ')' *)
      let* cStr1 = token "CSTR" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 12 [xToken(cStr1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CUInt' '(' expression ')' *)
      let* cUInt1 = token "CUINT" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 13 [xToken(cUInt1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CULng' '(' expression ')' *)
      let* cULng1 = token "CULNG" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 14 [xToken(cULng1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
    begin
      (* predefined_cast_expression -> 'CUShort' '(' expression ')' *)
      let* cUShort1 = token "CUSHORT" in
      let* lparen1 = token "(" in
      let* expression1 = expression in
      let* rparen1 = token ")" in
      pure (xRule "predefined_cast_expression" 15 [xToken(cUShort1); xToken(lparen1); G.E expression1; xToken(rparen1)])
    end;
  ]
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
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* in1 = token "IN" in
  let* expression1 = expression in
  pure (xRule "collection_range_variable" 0 [modified_identifier1; xOptional(simple_as_clause_opt1); xToken(in1); G.E expression1])
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
  let* simple_as_clause_opt1 = optional simple_as_clause in
  let* eq1 = token "=" in
  pure (xRule "variable_name_equals" 0 [modified_identifier1; xOptional(simple_as_clause_opt1); xToken(eq1)])
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

and ternary_conditional_expression : G.any parser = fun __n -> (
  (* ternary_conditional_expression -> 'If' '(' expression ',' expression ',' expression ')' *)
  let* if1 = token "IF" in
  let* lparen1 = token "(" in
  let* expression1 = expression in
  let* comma1 = token "," in
  let* expression2 = expression in
  let* comma2 = token "," in
  let* expression3 = expression in
  let* rparen1 = token ")" in
  pure (xRule "ternary_conditional_expression" 0 [xToken(if1); xToken(lparen1); G.E expression1; xToken(comma1); G.E expression2; xToken(comma2); G.E expression3; xToken(rparen1)])
) __n

and tuple_expression : G.any parser = fun __n -> (
  (* tuple_expression -> '(' argument (',' argument)+ ')' *)
  let* lparen1 = token "(" in
  let* argument1 = argument in
  let* comma_arguments1 = ne_list_of 
    begin
      let* comma1 = token "," in
      let* argument1 = argument in
      pure (xGroup([xToken(comma1); argument1]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "tuple_expression" 0 [xToken(lparen1); argument1; xList(comma_arguments1); xToken(rparen1)])
) __n

and is_or_is_not : G.any parser = fun __n -> (
  choice [
    begin
      (* is_or_is_not -> 'Is' *)
      let* is1 = token "IS" in
      pure (xRule "is_or_is_not" 0 [xToken(is1)])
    end;
    begin
      (* is_or_is_not -> 'IsNot' *)
      let* isNot1 = token "ISNOT" in
      pure (xRule "is_or_is_not" 1 [xToken(isNot1)])
    end;
  ]
) __n

and type_of_expression : G.any parser = fun __n -> (
  (* type_of_expression -> 'TypeOf' await_expression is_or_is_not type *)
  let* typeOf1 = token "TYPEOF" in
  let* await_expression1 = await_expression in
  let* is_or_is_not1 = is_or_is_not in
  let* type_1 = type_ in
  pure (xRule "type_of_expression" 0 [xToken(typeOf1); G.E await_expression1; is_or_is_not1; type_1])
) __n

and address_of_expression : G.expr parser = fun __n -> (
  (* address_of_expression -> 'AddressOf' unary_expression *)
  let* t = token "ADDRESSOF" in
  let* e = unary_expression in
  pure (G.Ref (t.tok, e) |> G.e)
) __n

and type_ : G.any parser = fun __n -> (
  (* type -> base_type (type_modifier)* *)
  let* base_type1 = base_type in
  let* type_modifiers1 = list_of 
    begin
      let* type_modifier1 = type_modifier in
      pure (xGroup([type_modifier1]))
    end
  in
  pure (xRule "type" 0 [base_type1; xList(type_modifiers1)])
) __n

and base_type : G.any parser = fun __n -> (
  choice [
    begin
      (* base_type -> name *)
      let* name1 = name in
      pure (xRule "base_type" 0 [name1])
    end;
    begin
      (* base_type -> predefined_type *)
      let* predefined_type1 = predefined_type in
      pure (xRule "base_type" 1 [predefined_type1])
    end;
    begin
      (* base_type -> tuple_type *)
      let* tuple_type1 = tuple_type in
      pure (xRule "base_type" 2 [tuple_type1])
    end;
  ]
) __n

and type_modifier : G.any parser = fun __n -> (
  choice [
    begin
      (* type_modifier -> array_rank_specifier *)
      let* array_rank_specifier1 = array_rank_specifier in
      pure (xRule "type_modifier" 0 [array_rank_specifier1])
    end;
    begin
      (* type_modifier -> '?' *)
      let* qmark1 = token "?" in
      pure (xRule "type_modifier" 1 [xToken(qmark1)])
    end;
  ]
) __n

and predefined_type : G.any parser = fun __n -> (
  choice [
    begin
      (* predefined_type -> 'Boolean' *)
      let* boolean1 = token "BOOLEAN" in
      pure (xRule "predefined_type" 0 [xToken(boolean1)])
    end;
    begin
      (* predefined_type -> 'Byte' *)
      let* byte1 = token "BYTE" in
      pure (xRule "predefined_type" 1 [xToken(byte1)])
    end;
    begin
      (* predefined_type -> 'Char' *)
      let* char1 = token "CHAR" in
      pure (xRule "predefined_type" 2 [xToken(char1)])
    end;
    begin
      (* predefined_type -> 'Date' *)
      let* date1 = token "DATE" in
      pure (xRule "predefined_type" 3 [xToken(date1)])
    end;
    begin
      (* predefined_type -> 'Decimal' *)
      let* decimal1 = token "DECIMAL" in
      pure (xRule "predefined_type" 4 [xToken(decimal1)])
    end;
    begin
      (* predefined_type -> 'Double' *)
      let* double1 = token "DOUBLE" in
      pure (xRule "predefined_type" 5 [xToken(double1)])
    end;
    begin
      (* predefined_type -> 'Integer' *)
      let* integer1 = token "INTEGER" in
      pure (xRule "predefined_type" 6 [xToken(integer1)])
    end;
    begin
      (* predefined_type -> 'Long' *)
      let* long1 = token "LONG" in
      pure (xRule "predefined_type" 7 [xToken(long1)])
    end;
    begin
      (* predefined_type -> 'Object' *)
      let* object1 = token "OBJECT" in
      pure (xRule "predefined_type" 8 [xToken(object1)])
    end;
    begin
      (* predefined_type -> 'SByte' *)
      let* sByte1 = token "SBYTE" in
      pure (xRule "predefined_type" 9 [xToken(sByte1)])
    end;
    begin
      (* predefined_type -> 'Short' *)
      let* short1 = token "SHORT" in
      pure (xRule "predefined_type" 10 [xToken(short1)])
    end;
    begin
      (* predefined_type -> 'Single' *)
      let* single1 = token "SINGLE" in
      pure (xRule "predefined_type" 11 [xToken(single1)])
    end;
    begin
      (* predefined_type -> 'String' *)
      let* string1 = token "STRING" in
      pure (xRule "predefined_type" 12 [xToken(string1)])
    end;
    begin
      (* predefined_type -> 'UInteger' *)
      let* uInteger1 = token "UINTEGER" in
      pure (xRule "predefined_type" 13 [xToken(uInteger1)])
    end;
    begin
      (* predefined_type -> 'ULong' *)
      let* uLong1 = token "ULONG" in
      pure (xRule "predefined_type" 14 [xToken(uLong1)])
    end;
    begin
      (* predefined_type -> 'UShort' *)
      let* uShort1 = token "USHORT" in
      pure (xRule "predefined_type" 15 [xToken(uShort1)])
    end;
  ]
) __n

and tuple_type : G.any parser = fun __n -> (
  (* tuple_type -> '(' tuple_element (',' tuple_element)+ ')' *)
  let* lparen1 = token "(" in
  let* tuple_element1 = tuple_element in
  let* comma_tuple_elements1 = ne_list_of 
    begin
      let* comma1 = token "," in
      let* tuple_element1 = tuple_element in
      pure (xGroup([xToken(comma1); tuple_element1]))
    end
  in
  let* rparen1 = token ")" in
  pure (xRule "tuple_type" 0 [xToken(lparen1); tuple_element1; xList(comma_tuple_elements1); xToken(rparen1)])
) __n

and tuple_element : G.any parser = fun __n -> (
  choice [
    begin
      (* tuple_element -> named_tuple_element *)
      let* named_tuple_element1 = named_tuple_element in
      pure (xRule "tuple_element" 0 [named_tuple_element1])
    end;
    begin
      (* tuple_element -> typed_tuple_element *)
      let* typed_tuple_element1 = typed_tuple_element in
      pure (xRule "tuple_element" 1 [typed_tuple_element1])
    end;
  ]
) __n

and named_tuple_element : G.any parser = fun __n -> (
  (* named_tuple_element -> identifier_token simple_as_clause? *)
  let* identifier_token1 = identifier_token in
  let* simple_as_clause_opt1 = optional simple_as_clause in
  pure (xRule "named_tuple_element" 0 [identifier_token1; xOptional(simple_as_clause_opt1)])
) __n

and typed_tuple_element : G.any parser = fun __n -> (
  (* typed_tuple_element -> type *)
  let* type_1 = type_ in
  pure (xRule "typed_tuple_element" 0 [type_1])
) __n

and name : G.any parser = fun __n -> (
  (* name -> base_name ('.' name_reference)* *)
  let* base_name1 = base_name in
  let* dot_name_references1 = list_of 
    begin
      let* dot1 = token "." in
      let* name_reference1 = name_reference in
      pure (xGroup([xToken(dot1); name_reference1]))
    end
  in
  pure (xRule "name" 0 [base_name1; xList(dot_name_references1)])
) __n

and base_name : G.any parser = fun __n -> (
  (* base_name -> identifier_name type_argument_list? *)
  let* identifier_name1 = identifier_name in
  let* type_argument_list_opt1 = optional type_argument_list in
  pure (xRule "base_name" 0 [identifier_name1; xOptional(type_argument_list_opt1)])
) __n

and name_reference : G.any parser = fun __n -> (
  (* name_reference -> identifier_or_keyword type_argument_list? *)
  let* identifier_or_keyword1 = identifier_or_keyword in
  let* type_argument_list_opt1 = optional type_argument_list in
  pure (xRule "name_reference" 0 [identifier_or_keyword1; xOptional(type_argument_list_opt1)])
) __n

and xml_cdata : G.any parser = fun __n -> (
  (* xml_cdata -> '<CDATA>' *)
  let* lt_CDATA_gt1 = token "<CDATA>" in
  pure (xRule "xml_cdata" 0 [xToken(lt_CDATA_gt1)])
) __n

and x_expression : G.any parser = fun __n -> (
  choice [
    begin
      (* x_expression -> x_element *)
      let* x_element1 = x_element in
      pure (xRule "x_expression" 0 [x_element1])
    end;
    begin
      (* x_expression -> x_declaration *)
      let* x_declaration1 = x_declaration in
      pure (xRule "x_expression" 1 [x_declaration1])
    end;
    begin
      (* x_expression -> x_tag_single *)
      let* x_tag_single1 = x_tag_single in
      pure (xRule "x_expression" 2 [x_tag_single1])
    end;
    begin
      (* x_expression -> x_tag_comment *)
      let* x_tag_comment1 = x_tag_comment in
      pure (xRule "x_expression" 3 [x_tag_comment1])
    end;
    begin
      (* x_expression -> x_tag_embed_expression *)
      let* x_tag_embed_expression1 = x_tag_embed_expression in
      pure (xRule "x_expression" 4 [x_tag_embed_expression1])
    end;
  ]
) __n

and x_element : G.any parser = fun __n -> (
  (* x_element -> x_tag_start x_body_element* x_tag_end *)
  let* x_tag_start1 = x_tag_start in
  let* x_body_elements1 = list_of x_body_element in
  let* x_tag_end1 = x_tag_end in
  pure (xRule "x_element" 0 [x_tag_start1; xList(x_body_elements1); x_tag_end1])
) __n

and x_declaration : G.any parser = fun __n -> (
  (* x_declaration -> '<?' x_tag_inside '?>' x_expression *)
  let* lt_qmark1 = token "<?" in
  let* x_tag_inside1 = x_tag_inside in
  let* qmark_gt1 = token "?>" in
  let* x_expression1 = x_expression in
  pure (xRule "x_declaration" 0 [xToken(lt_qmark1); x_tag_inside1; xToken(qmark_gt1); x_expression1])
) __n

and x_non_xml_operator : G.any parser = fun __n -> (
  choice [
    begin
      (* x_non_xml_operator -> '!' *)
      let* bang1 = token "!" in
      pure (xRule "x_non_xml_operator" 0 [xToken(bang1)])
    end;
    begin
      (* x_non_xml_operator -> '\\'' *)
      let* squote1 = token "'" in
      pure (xRule "x_non_xml_operator" 1 [xToken(squote1)])
    end;
    begin
      (* x_non_xml_operator -> '#' *)
      let* hash1 = token "#" in
      pure (xRule "x_non_xml_operator" 2 [xToken(hash1)])
    end;
    begin
      (* x_non_xml_operator -> '$\\'' *)
      let* dollar_squote1 = token "$'" in
      pure (xRule "x_non_xml_operator" 3 [xToken(dollar_squote1)])
    end;
    begin
      (* x_non_xml_operator -> '%>' *)
      let* percent_gt1 = token "%>" in
      pure (xRule "x_non_xml_operator" 4 [xToken(percent_gt1)])
    end;
    begin
      (* x_non_xml_operator -> '&' *)
      let* amp1 = token "&" in
      pure (xRule "x_non_xml_operator" 5 [xToken(amp1)])
    end;
    begin
      (* x_non_xml_operator -> '&=' *)
      let* amp_eq1 = token "&=" in
      pure (xRule "x_non_xml_operator" 6 [xToken(amp_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '*' *)
      let* star1 = token "*" in
      pure (xRule "x_non_xml_operator" 7 [xToken(star1)])
    end;
    begin
      (* x_non_xml_operator -> '*=' *)
      let* star_eq1 = token "*=" in
      pure (xRule "x_non_xml_operator" 8 [xToken(star_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '+' *)
      let* plus1 = token "+" in
      pure (xRule "x_non_xml_operator" 9 [xToken(plus1)])
    end;
    begin
      (* x_non_xml_operator -> '+=' *)
      let* plus_eq1 = token "+=" in
      pure (xRule "x_non_xml_operator" 10 [xToken(plus_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '-' *)
      let* minus1 = token "-" in
      pure (xRule "x_non_xml_operator" 11 [xToken(minus1)])
    end;
    begin
      (* x_non_xml_operator -> '-=' *)
      let* minus_eq1 = token "-=" in
      pure (xRule "x_non_xml_operator" 12 [xToken(minus_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '.' *)
      let* dot1 = token "." in
      pure (xRule "x_non_xml_operator" 13 [xToken(dot1)])
    end;
    begin
      (* x_non_xml_operator -> '/' *)
      let* slash1 = token "/" in
      pure (xRule "x_non_xml_operator" 14 [xToken(slash1)])
    end;
    begin
      (* x_non_xml_operator -> '/=' *)
      let* slash_eq1 = token "/=" in
      pure (xRule "x_non_xml_operator" 15 [xToken(slash_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '/>' *)
      let* slash_gt1 = token "/>" in
      pure (xRule "x_non_xml_operator" 16 [xToken(slash_gt1)])
    end;
    begin
      (* x_non_xml_operator -> ':' *)
      let* colon1 = token ":" in
      pure (xRule "x_non_xml_operator" 17 [xToken(colon1)])
    end;
    begin
      (* x_non_xml_operator -> ':=' *)
      let* colon_eq1 = token ":=" in
      pure (xRule "x_non_xml_operator" 18 [xToken(colon_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '<<' *)
      let* lt_lt1 = token "<<" in
      pure (xRule "x_non_xml_operator" 19 [xToken(lt_lt1)])
    end;
    begin
      (* x_non_xml_operator -> '<<=' *)
      let* lt_lt_eq1 = token "<<=" in
      pure (xRule "x_non_xml_operator" 20 [xToken(lt_lt_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '<=' *)
      let* lt_eq1 = token "<=" in
      pure (xRule "x_non_xml_operator" 21 [xToken(lt_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '<>' *)
      let* lt_gt1 = token "<>" in
      pure (xRule "x_non_xml_operator" 22 [xToken(lt_gt1)])
    end;
    begin
      (* x_non_xml_operator -> '=' *)
      let* eq1 = token "=" in
      pure (xRule "x_non_xml_operator" 23 [xToken(eq1)])
    end;
    begin
      (* x_non_xml_operator -> '>' *)
      let* gt1 = token ">" in
      pure (xRule "x_non_xml_operator" 24 [xToken(gt1)])
    end;
    begin
      (* x_non_xml_operator -> '>=' *)
      let* gt_eq1 = token ">=" in
      pure (xRule "x_non_xml_operator" 25 [xToken(gt_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '>>' *)
      let* gt_gt1 = token ">>" in
      pure (xRule "x_non_xml_operator" 26 [xToken(gt_gt1)])
    end;
    begin
      (* x_non_xml_operator -> '>>=' *)
      let* gt_gt_eq1 = token ">>=" in
      pure (xRule "x_non_xml_operator" 27 [xToken(gt_gt_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '?' *)
      let* qmark1 = token "?" in
      pure (xRule "x_non_xml_operator" 28 [xToken(qmark1)])
    end;
    begin
      (* x_non_xml_operator -> '?.' *)
      let* qmark_dot1 = token "?." in
      pure (xRule "x_non_xml_operator" 29 [xToken(qmark_dot1)])
    end;
    begin
      (* x_non_xml_operator -> '?>' *)
      let* qmark_gt1 = token "?>" in
      pure (xRule "x_non_xml_operator" 30 [xToken(qmark_gt1)])
    end;
    begin
      (* x_non_xml_operator -> '@' *)
      let* at1 = token "@" in
      pure (xRule "x_non_xml_operator" 31 [xToken(at1)])
    end;
    begin
      (* x_non_xml_operator -> '.@' *)
      let* dot_at1 = token ".@" in
      pure (xRule "x_non_xml_operator" 32 [xToken(dot_at1)])
    end;
    begin
      (* x_non_xml_operator -> '\\' *)
      let* backslash1 = token "\\" in
      pure (xRule "x_non_xml_operator" 33 [xToken(backslash1)])
    end;
    begin
      (* x_non_xml_operator -> '\\=' *)
      let* backslash_eq1 = token "\\=" in
      pure (xRule "x_non_xml_operator" 34 [xToken(backslash_eq1)])
    end;
    begin
      (* x_non_xml_operator -> '^' *)
      let* caret1 = token "^" in
      pure (xRule "x_non_xml_operator" 35 [xToken(caret1)])
    end;
    begin
      (* x_non_xml_operator -> '^=' *)
      let* caret_eq1 = token "^=" in
      pure (xRule "x_non_xml_operator" 36 [xToken(caret_eq1)])
    end;
    begin
      (* x_non_xml_operator -> ';' *)
      let* semi1 = token ";" in
      pure (xRule "x_non_xml_operator" 37 [xToken(semi1)])
    end;
  ]
) __n

and x_body_element : G.any parser = fun __n -> (
  choice [
    begin
      (* x_body_element -> x_expression *)
      let* x_expression1 = x_expression in
      pure (xRule "x_body_element" 0 [x_expression1])
    end;
    begin
      (* x_body_element -> '<IDENT>' *)
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xRule "x_body_element" 1 [xToken(lt_IDENT_gt1)])
    end;
    begin
      (* x_body_element -> '<KEYWORD>' *)
      let* lt_KEYWORD_gt1 = token "<KEYWORD>" in
      pure (xRule "x_body_element" 2 [xToken(lt_KEYWORD_gt1)])
    end;
    begin
      (* x_body_element -> '<PUNCTUATION>' *)
      let* lt_PUNCTUATION_gt1 = token "<PUNCTUATION>" in
      pure (xRule "x_body_element" 3 [xToken(lt_PUNCTUATION_gt1)])
    end;
    begin
      (* x_body_element -> '<INT>' *)
      let* lt_INT_gt1 = token "<INT>" in
      pure (xRule "x_body_element" 4 [xToken(lt_INT_gt1)])
    end;
    begin
      (* x_body_element -> '<FLOAT>' *)
      let* lt_FLOAT_gt1 = token "<FLOAT>" in
      pure (xRule "x_body_element" 5 [xToken(lt_FLOAT_gt1)])
    end;
    begin
      (* x_body_element -> '<CHAR>' *)
      let* lt_CHAR_gt1 = token "<CHAR>" in
      pure (xRule "x_body_element" 6 [xToken(lt_CHAR_gt1)])
    end;
    begin
      (* x_body_element -> '<STRING>' *)
      let* lt_STRING_gt1 = token "<STRING>" in
      pure (xRule "x_body_element" 7 [xToken(lt_STRING_gt1)])
    end;
    begin
      (* x_body_element -> '<STRING_SEGMENT>' *)
      let* lt_STRING_SEGMENT_gt1 = token "<STRING_SEGMENT>" in
      pure (xRule "x_body_element" 8 [xToken(lt_STRING_SEGMENT_gt1)])
    end;
    begin
      (* x_body_element -> '<DATE>' *)
      let* lt_DATE_gt1 = token "<DATE>" in
      pure (xRule "x_body_element" 9 [xToken(lt_DATE_gt1)])
    end;
    begin
      (* x_body_element -> '<CDATA>' *)
      let* lt_CDATA_gt1 = token "<CDATA>" in
      pure (xRule "x_body_element" 10 [xToken(lt_CDATA_gt1)])
    end;
    begin
      (* x_body_element -> '<OTHER>' *)
      let* lt_OTHER_gt1 = token "<OTHER>" in
      pure (xRule "x_body_element" 11 [xToken(lt_OTHER_gt1)])
    end;
    begin
      (* x_body_element -> x_non_xml_operator *)
      let* x_non_xml_operator1 = x_non_xml_operator in
      pure (xRule "x_body_element" 12 [x_non_xml_operator1])
    end;
  ]
) __n

and x_tag_start : G.any parser = fun __n -> (
  (* x_tag_start -> '<' x_tag_inside '>' *)
  let* lt1 = token "<" in
  let* x_tag_inside1 = x_tag_inside in
  let* gt1 = token ">" in
  pure (xRule "x_tag_start" 0 [xToken(lt1); x_tag_inside1; xToken(gt1)])
) __n

and x_tag_end : G.any parser = fun __n -> (
  (* x_tag_end -> '</' x_tag_inside '>' *)
  let* lt_slash1 = token "</" in
  let* x_tag_inside1 = x_tag_inside in
  let* gt1 = token ">" in
  pure (xRule "x_tag_end" 0 [xToken(lt_slash1); x_tag_inside1; xToken(gt1)])
) __n

and x_tag_single : G.any parser = fun __n -> (
  (* x_tag_single -> '<' x_tag_inside '/>' *)
  let* lt1 = token "<" in
  let* x_tag_inside1 = x_tag_inside in
  let* slash_gt1 = token "/>" in
  pure (xRule "x_tag_single" 0 [xToken(lt1); x_tag_inside1; xToken(slash_gt1)])
) __n

and x_tag_comment : G.any parser = fun __n -> (
  (* x_tag_comment -> '<!--' x_body_element* '-->' *)
  let* lt_bang_minus_minus1 = token "<!--" in
  let* x_body_elements1 = list_of x_body_element in
  let* minus_minus_gt1 = token "-->" in
  pure (xRule "x_tag_comment" 0 [xToken(lt_bang_minus_minus1); xList(x_body_elements1); xToken(minus_minus_gt1)])
) __n

and x_tag_inside : G.any parser = fun __n -> (
  (* x_tag_inside -> x_name x_param* *)
  let* x_name1 = x_name in
  let* x_params1 = list_of x_param in
  pure (xRule "x_tag_inside" 0 [x_name1; xList(x_params1)])
) __n

and x_tag_embed_expression : G.any parser = fun __n -> (
  (* x_tag_embed_expression -> '<%=' expression '%>' *)
  let* lt_percent_eq1 = token "<%=" in
  let* expression1 = expression in
  let* percent_gt1 = token "%>" in
  pure (xRule "x_tag_embed_expression" 0 [xToken(lt_percent_eq1); G.E expression1; xToken(percent_gt1)])
) __n

and x_name : G.any parser = fun __n -> (
  (* x_name -> '<IDENT>' (':' '<IDENT>')* *)
  let* lt_IDENT_gt1 = token "<IDENT>" in
  let* colon_lt_IDENT_gts1 = list_of 
    begin
      let* colon1 = token ":" in
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xGroup([xToken(colon1); xToken(lt_IDENT_gt1)]))
    end
  in
  pure (xRule "x_name" 0 [xToken(lt_IDENT_gt1); xList(colon_lt_IDENT_gts1)])
) __n

and x_param : G.any parser = fun __n -> (
  choice [
    begin
      (* x_param -> x_name ('=' x_param_value)? *)
      let* x_name1 = x_name in
      let* eq_x_param_value_opt1 = optional 
        begin
          let* eq1 = token "=" in
          let* x_param_value1 = x_param_value in
          pure (xGroup([xToken(eq1); x_param_value1]))
        end
      in
      pure (xRule "x_param" 0 [x_name1; xOptional(eq_x_param_value_opt1)])
    end;
    begin
      (* x_param -> x_tag_embed_expression *)
      let* x_tag_embed_expression1 = x_tag_embed_expression in
      pure (xRule "x_param" 1 [x_tag_embed_expression1])
    end;
  ]
) __n

and x_param_value : G.any parser = fun __n -> (
  choice [
    begin
      (* x_param_value -> '<IDENT>' *)
      let* lt_IDENT_gt1 = token "<IDENT>" in
      pure (xRule "x_param_value" 0 [xToken(lt_IDENT_gt1)])
    end;
    begin
      (* x_param_value -> '<STRING>' *)
      let* lt_STRING_gt1 = token "<STRING>" in
      pure (xRule "x_param_value" 1 [xToken(lt_STRING_gt1)])
    end;
    begin
      (* x_param_value -> '<INT>' *)
      let* lt_INT_gt1 = token "<INT>" in
      pure (xRule "x_param_value" 2 [xToken(lt_INT_gt1)])
    end;
    begin
      (* x_param_value -> x_tag_embed_expression *)
      let* x_tag_embed_expression1 = x_tag_embed_expression in
      pure (xRule "x_param_value" 3 [x_tag_embed_expression1])
    end;
  ]
) __n

and method_modifier : G.any parser = fun __n -> (
  choice [
    begin
      (* method_modifier -> 'Async' *)
      let* async1 = token "ASYNC" in
      pure (xRule "method_modifier" 0 [xToken(async1)])
    end;
    begin
      (* method_modifier -> 'Iterator' *)
      let* iterator1 = token "ITERATOR" in
      pure (xRule "method_modifier" 1 [xToken(iterator1)])
    end;
    begin
      (* method_modifier -> modifier *)
      let* modifier1 = modifier in
      pure (xRule "method_modifier" 2 [modifier1])
    end;
  ]
) __n

and modifier : G.any parser = fun __n -> (
  choice [
    begin
      (* modifier -> 'Const' *)
      let* const1 = token "CONST" in
      pure (xRule "modifier" 0 [xToken(const1)])
    end;
    begin
      (* modifier -> 'Default' *)
      let* default1 = token "DEFAULT" in
      pure (xRule "modifier" 1 [xToken(default1)])
    end;
    begin
      (* modifier -> 'Delegate' *)
      let* delegate1 = token "DELEGATE" in
      pure (xRule "modifier" 2 [xToken(delegate1)])
    end;
    begin
      (* modifier -> 'Dim' *)
      let* dim1 = token "DIM" in
      pure (xRule "modifier" 3 [xToken(dim1)])
    end;
    begin
      (* modifier -> 'Friend' *)
      let* friend1 = token "FRIEND" in
      pure (xRule "modifier" 4 [xToken(friend1)])
    end;
    begin
      (* modifier -> 'MustInherit' *)
      let* mustInherit1 = token "MUSTINHERIT" in
      pure (xRule "modifier" 5 [xToken(mustInherit1)])
    end;
    begin
      (* modifier -> 'MustOverride' *)
      let* mustOverride1 = token "MUSTOVERRIDE" in
      pure (xRule "modifier" 6 [xToken(mustOverride1)])
    end;
    begin
      (* modifier -> 'Narrowing' *)
      let* narrowing1 = token "NARROWING" in
      pure (xRule "modifier" 7 [xToken(narrowing1)])
    end;
    begin
      (* modifier -> 'NotInheritable' *)
      let* notInheritable1 = token "NOTINHERITABLE" in
      pure (xRule "modifier" 8 [xToken(notInheritable1)])
    end;
    begin
      (* modifier -> 'NotOverridable' *)
      let* notOverridable1 = token "NOTOVERRIDABLE" in
      pure (xRule "modifier" 9 [xToken(notOverridable1)])
    end;
    begin
      (* modifier -> 'Optional' *)
      let* optional1 = token "OPTIONAL" in
      pure (xRule "modifier" 10 [xToken(optional1)])
    end;
    begin
      (* modifier -> 'Overloads' *)
      let* overloads1 = token "OVERLOADS" in
      pure (xRule "modifier" 11 [xToken(overloads1)])
    end;
    begin
      (* modifier -> 'Overridable' *)
      let* overridable1 = token "OVERRIDABLE" in
      pure (xRule "modifier" 12 [xToken(overridable1)])
    end;
    begin
      (* modifier -> 'Overrides' *)
      let* overrides1 = token "OVERRIDES" in
      pure (xRule "modifier" 13 [xToken(overrides1)])
    end;
    begin
      (* modifier -> 'Partial' *)
      let* partial1 = token "PARTIAL" in
      pure (xRule "modifier" 14 [xToken(partial1)])
    end;
    begin
      (* modifier -> 'Private' *)
      let* private1 = token "PRIVATE" in
      pure (xRule "modifier" 15 [xToken(private1)])
    end;
    begin
      (* modifier -> 'Protected' *)
      let* protected1 = token "PROTECTED" in
      pure (xRule "modifier" 16 [xToken(protected1)])
    end;
    begin
      (* modifier -> 'Public' *)
      let* public1 = token "PUBLIC" in
      pure (xRule "modifier" 17 [xToken(public1)])
    end;
    begin
      (* modifier -> 'ReadOnly' *)
      let* readOnly1 = token "READONLY" in
      pure (xRule "modifier" 18 [xToken(readOnly1)])
    end;
    begin
      (* modifier -> 'Shadows' *)
      let* shadows1 = token "SHADOWS" in
      pure (xRule "modifier" 19 [xToken(shadows1)])
    end;
    begin
      (* modifier -> 'Shared' *)
      let* shared1 = token "SHARED" in
      pure (xRule "modifier" 20 [xToken(shared1)])
    end;
    begin
      (* modifier -> 'Static' *)
      let* static1 = token "STATIC" in
      pure (xRule "modifier" 21 [xToken(static1)])
    end;
    begin
      (* modifier -> 'Widening' *)
      let* widening1 = token "WIDENING" in
      pure (xRule "modifier" 22 [xToken(widening1)])
    end;
    begin
      (* modifier -> 'WithEvents' *)
      let* withEvents1 = token "WITHEVENTS" in
      pure (xRule "modifier" 23 [xToken(withEvents1)])
    end;
    begin
      (* modifier -> 'WriteOnly' *)
      let* writeOnly1 = token "WRITEONLY" in
      pure (xRule "modifier" 24 [xToken(writeOnly1)])
    end;
    begin
      (* modifier -> 'ByRef' *)
      let* byRef1 = token "BYREF" in
      pure (xRule "modifier" 25 [xToken(byRef1)])
    end;
    begin
      (* modifier -> 'ByVal' *)
      let* byVal1 = token "BYVAL" in
      pure (xRule "modifier" 26 [xToken(byVal1)])
    end;
    begin
      (* modifier -> 'ParamArray' *)
      let* paramArray1 = token "PARAMARRAY" in
      pure (xRule "modifier" 27 [xToken(paramArray1)])
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
