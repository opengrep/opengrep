(*
   Convert Bash-specific AST to generic AST.

   A Bash program is organized into the following hierarchy:

   - List: a list of pipelines. A whole program is a list (type blist).

       echo hello | tr h y; echo bye

   - Pipeline: a list of commands whose standard output and input are
     connected

       echo hello | tr h y

   - Command: everything else including calls to external programs and
     control structures that the language has to offer.

     Simple command:

       echo hello

     For loop, a compound command:

       for i in 1 2 3; do echo "$i"; done

     Variable assignment:

       answer=42

     + many others

   - Literals: space-separated elements within a simple command, which will
     be expanded into a possibly different number of elements.

       hello
       "$x"
       $items
       "$(cat foo/bar)"
       $?

       etc.

   Mapping to the generic AST:

   We use statements wherever we need to represent a control flow.
   This means all declarations and assignments must be statements.
   Members of a pipeline on the other hand execute independently and
   can be expressions.

   pipeline -> expression with '|' operator between commands
   command -> expression or pure statement
   simple command -> expression: Constructor ("sh_cmd", args)
   variable assignment -> statement: variable assignment
   function definition -> statement: function definition
   for loop -> statement: for loop

   Note that a bash pipeline may contain definitions. This is legal but
   useless and it has no effect:

     a=42 | echo hello
     echo "$a"  # variable 'a' is undefined!

   It makes more sense when a command group is used within a pipeline:

     { a=42; echo "$a"; } | cat  # prints '42'

   So, some shell commands are represented preferentially as statements
   (definitions, command groups, loops, if-then-else) and others
   are represented preferentially as expressions (simple commands, pipelines),
   and we wrap expressions in statements and vice-versa as required by
   the context.

   Shell variables and semgrep metavariables:

   - In a pattern, '$XXX' is interpreted as a semgrep metavariable,
     at matching time, represented as '$XXX'.
   - In a program, '$XXX' is interpreted as the dereferencing of a
     shell variable, at matching time, represented as 'XXX'.
   - '${XXX}' is always interpreted as the dereferencing of a shell variable
     and should be used in semgrep patterns to disambiguate from the
     metavariable '$XXX'.
*)

open! Common
open AST_bash
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fb = Tok.unsafe_fake_bracket

(*
   Convert a pair (loc, x) to a bracket, which uses a leading and trailing
   token to indicate the location.
*)
let bracket (loc : loc) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

(* We apply a different mapping whether we're parsing a pattern or target
   program. *)
type env = AST_bash.input_kind

(* Temporary representation.
   Avoids superfluous early wrapping of expressions in statements and
   vice-versa. *)
type stmt_or_expr = Stmt of loc * G.stmt | Expr of loc * G.expr

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))
let expr_of_stmt (st : G.stmt) : G.expr = G.stmt_to_expr st

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt (_loc, st) -> st
  | Expr (loc, e) -> stmt_of_expr loc e

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt (_loc, st) -> expr_of_stmt st
  | Expr (_loc, e) -> e

let stmt_or_expr_loc = function
  | Stmt (loc, _)
  | Expr (loc, _) ->
      loc

let block : stmt_or_expr list -> stmt_or_expr = function
  | [ x ] -> x
  | several ->
      let loc = Tok_range.of_list stmt_or_expr_loc several in
      let stmts = List_.map as_stmt several in
      Stmt (loc, G.s (G.Block (bracket loc stmts)))

let mk_name (str_wrap : string wrap) : G.name =
  let id_info = G.empty_id_info () in
  G.Id (str_wrap, id_info)

let get_var_name (var : variable_name) : string wrap =
  match var with
  | Simple_variable_name name
  | Special_variable_name name
  | Var_semgrep_metavar name ->
      name

let mk_var_expr (var : variable_name) : G.expr =
  let name = get_var_name var in
  G.N (mk_name name) |> G.e

module C = struct
  let mk (loc : loc) (name : string) =
    let id = AST_generic_helpers.shell_marker name in
    let id_info = G.empty_id_info ~hidden:true () in
    G.N (G.Id ((id, fst loc), id_info)) |> G.e

  (* For simple commands, e.g.
       $echo "$@"
  *)
  let cmd (loc : loc) = mk loc AST_generic_helpers.shell_command

  (* Split a string according to whitespace defined by "$IFS".
     This is done on the result of unquoted $ expansions:
       $args
       $(cat foo.txt)
       `cat foo.txt`
       ${args}
       ${args%.c}
  *)
  let _split loc = mk loc "split"

  (*
     Expand a variable X referenced as $X or ${X} in a program.
     In a pattern, $X is a metavariable that stands for anything
     (no expansion necessary), whereas ${$X} in a pattern is the expansion
     of any variable represented by the metavariable $X.
  *)
  let expand loc = mk loc "expand"

  (* Concatenate two string fragments e.g.
       foo"$bar"
  *)
  let concat loc = mk loc "concat"

  (* Concatenate two string fragments within double-quotes e.g.
       "foo $bar"
  *)
  let quoted_concat loc = mk loc "quoted_concat"

  (* Command substitution: $(...) *)
  let cmd_subst loc = mk loc "cmd_subst"

  (* Process substitution: <(...) *)
  let proc_subst loc = mk loc "proc_subst"

  (* Subshell: (...) *)
  let subshell loc = mk loc "subshell"
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call loc name exprs =
  G.Call (name loc, bracket loc (List_.map (fun e -> G.Arg e) exprs)) |> G.e

let todo_tokens ((start, end_) : loc) =
  let wrap tok = (Tok.content_of_tok tok, tok) in
  if start =*= end_ then [ G.TodoK (wrap start) ]
  else [ G.TodoK (wrap start); G.TodoK (wrap end_) ]

let _todo_stmt (loc : loc) : G.stmt =
  G.s (G.OtherStmt (G.OS_Todo, todo_tokens loc))

let todo_expr (loc : loc) : G.expr =
  let t = fst loc in
  G.e (G.OtherExpr (("BashTodo", t), todo_tokens loc))

let todo_expr2 (loc : loc) : stmt_or_expr = Expr (loc, todo_expr loc)

(*****************************************************************************)
(* Positional parameters *)
(*****************************************************************************)

(*
   A shell function declares no parameters: a call 'f a b' sets its
   positional parameters to (a b), which its body reads as $1, $2, ... and
   all at once as "$@" or "$*". 'shift' and 'set -- ...' renumber them.

   The function takes one argument, the list of the words of the call (see
   AST_generic_helpers.call_of_shell_command), destructured by a pattern
   that binds each positional parameter the body reads to its element of
   the list. A name read where the position is not known statically is
   bound to the whole list.
*)

(* The variables a command expands, and whether it runs a command that
   renumbers the positional parameters, outside the functions it defines. *)
type parameter_use = { expanded : string list; renumbers : bool }

let no_parameter_use = { expanded = []; renumbers = false }

(* A word of single-letter options of 'set', such as '-eu' or '+x'. *)
let is_set_option_cluster (word : string) : bool =
  String.length word > 1
  && (String.starts_with ~prefix:"-" word
     || String.starts_with ~prefix:"+" word)

(* 'set' replaces the positional parameters when an argument follows its
   options, when '--' ends them (with no argument, it unsets them), or when
   '-' ends them and an argument follows. Each 'o' of an option cluster
   takes the next word as an option name: 'set -euo pipefail'. *)
let rec sets_positional_parameters (words : expression list) : bool =
  match words with
  | [] -> false
  | Word ("--", _) :: _ -> true
  | Word ("-", _) :: rest -> not (List_.null rest)
  | Word (word, _) :: rest when is_set_option_cluster word ->
      let option_names =
        String.fold_left
          (fun (count : int) (c : char) ->
            if Char.equal c 'o' then count + 1 else count)
          0 word
      in
      sets_positional_parameters (List.drop option_names rest)
  | _ :: _ -> true

let renumbers_positional_parameters (arguments : expression list) : bool =
  match arguments with
  | Word ("shift", _) :: _ -> true
  | Word ("set", _) :: words -> sets_positional_parameters words
  | _ -> false

let rec use_of_blist (acc : parameter_use) (l : blist) : parameter_use =
  match l with
  | Seq (_loc, left, right) -> use_of_blist (use_of_blist acc left) right
  | Pipelines (_loc, pl) -> List.fold_left use_of_pipeline acc pl
  | Empty _loc -> acc

and use_of_pipeline (acc : parameter_use) (x : pipeline) : parameter_use =
  match x with
  | Command cmd_r -> use_of_cmd_redir acc cmd_r
  | Pipeline (_loc, pip, _bar, cmd_r) ->
      use_of_cmd_redir (use_of_pipeline acc pip) cmd_r
  | Control_operator (_loc, pip, _op) -> use_of_pipeline acc pip

and use_of_cmd_redir (acc : parameter_use) (x : cmd_redir) : parameter_use =
  List.fold_left use_of_redirect (use_of_command acc x.command) x.redirects

and use_of_redirect (acc : parameter_use) (x : redirect) : parameter_use =
  match x with
  | File_redirect (_loc, (Read (_, File e) | Write (_, _, File e))) ->
      use_of_expression acc e
  | Read_heredoc _
  | Read_herestring _
  | File_redirect _ ->
      acc

and use_of_command (acc : parameter_use) (cmd : command) : parameter_use =
  match cmd with
  | Simple_command { loc = _; assignments; arguments } ->
      let acc = List.fold_left use_of_assignment acc assignments in
      let acc = List.fold_left use_of_expression acc arguments in
      if renumbers_positional_parameters arguments then
        { acc with renumbers = true }
      else acc
  | And (_loc, left, _tok, right)
  | Or (_loc, left, _tok, right) ->
      use_of_pipeline (use_of_pipeline acc left) right
  | Subshell (_loc, (_, bl, _))
  | Command_group (_loc, (_, bl, _)) ->
      use_of_blist acc bl
  | Sh_test (_loc, (_, test, _))
  | Bash_test (_loc, (_, test, _)) ->
      use_of_test acc test
  | Arithmetic_expression _ -> acc
  | For_loop_c_style (_loc, bl) -> use_of_blist acc bl
  | For_loop (_loc, _for, _var, opt_in, _do, body, _done)
  | Select (_loc, _for, _var, opt_in, _do, body, _done) ->
      use_of_loop acc opt_in body
  | Case (_loc, _case, subject, _in, clauses, _esac) ->
      List.fold_left
        (fun (acc : parameter_use)
             ((_loc, patterns, _paren, body, _term) : case_clause) ->
          use_of_blist (List.fold_left use_of_expression acc patterns) body)
        (use_of_expression acc subject)
        clauses
  | If (_loc, _if, cond, _then, body, elifs, else_, _fi) ->
      let acc = use_of_blist (use_of_blist acc cond) body in
      let acc =
        List.fold_left
          (fun (acc : parameter_use)
               ((_loc, _elif, cond, _then, body) : elif) ->
            use_of_blist (use_of_blist acc cond) body)
          acc elifs
      in
      (match else_ with
      | Some (_loc, _else, body) -> use_of_blist acc body
      | None -> acc)
  | While_loop (_loc, _, cond, _do, body, _done)
  | Until_loop (_loc, _, cond, _do, body, _done) ->
      use_of_blist (use_of_blist acc cond) body
  | Coprocess (_loc, _, cmd)
  | Negated_command (_loc, _, cmd) ->
      use_of_command acc cmd
  | Assignment ass -> use_of_assignment acc ass
  (* The conversion drops the arguments of a declaration that are not
     assignments. *)
  | Declaration x ->
      let acc = List.fold_left use_of_assignment acc x.assignments in
      List.fold_left use_of_expression acc x.unknowns
  (* A function defined here has positional parameters of its own. *)
  | Function_definition _ -> acc

and use_of_loop (acc : parameter_use)
    (opt_in : (tok * expression list) option) (body : blist) : parameter_use =
  let acc =
    match opt_in with
    | Some (_in, values) -> List.fold_left use_of_expression acc values
    (* Without 'in', the loop iterates over "$@". *)
    | None -> { acc with expanded = "@" :: acc.expanded }
  in
  use_of_blist acc body

and use_of_assignment (acc : parameter_use) (ass : assignment) : parameter_use =
  use_of_expression acc ass.rhs

and use_of_test (acc : parameter_use) (x : test_expression) : parameter_use =
  match x with
  | T_expr (_loc, e)
  | T_unop (_loc, _, e) ->
      use_of_expression acc e
  | T_binop (_loc, left, _, right) ->
      use_of_expression (use_of_expression acc left) right
  | T_not (_loc, _, test) -> use_of_test acc test
  | T_and (_loc, left, _, right)
  | T_or (_loc, left, _, right) ->
      use_of_test (use_of_test acc left) right
  | T_todo _loc -> acc

and use_of_expression (acc : parameter_use) (e : expression) : parameter_use =
  match e with
  | Word _
  | Special_character _
  | Raw_string _
  | Ansii_c_string _
  | Expr_semgrep_ellipsis _
  | Expr_semgrep_metavar _
  | Empty_expression _
  | Equality_test (_, _, Regexp _) ->
      acc
  | String (_, frags, _) -> List.fold_left use_of_string_fragment acc frags
  | String_fragment (_loc, frag) -> use_of_string_fragment acc frag
  | Concatenation (_loc, el)
  | Array (_loc, (_, el, _)) ->
      List.fold_left use_of_expression acc el
  | Expr_semgrep_deep_ellipsis (_, (_, e, _))
  | Equality_test (_, _, Literal (_, e)) ->
      use_of_expression acc e
  | Process_substitution (_loc, (_, bl, _)) -> use_of_blist acc bl

and use_of_string_fragment (acc : parameter_use) (frag : string_fragment) :
    parameter_use =
  match frag with
  | String_content _
  | Frag_semgrep_metavar _
  | Frag_semgrep_named_ellipsis _ ->
      acc
  | Expansion (_loc, Simple_expansion (_, var)) -> use_of_variable acc var
  | Expansion (_loc, Complex_expansion (_, Variable (_, var, words), _)) ->
      List.fold_left use_of_expression (use_of_variable acc var) words
  | Expansion
      (_loc, Complex_expansion (_, Complex_expansion_TODO (_, words), _)) ->
      List.fold_left use_of_expression acc words
  | Command_substitution (_, bl, _) -> use_of_blist acc bl

and use_of_variable (acc : parameter_use) (var : variable_name) :
    parameter_use =
  match var with
  | Simple_variable_name (name, _)
  | Special_variable_name (name, _) ->
      { acc with expanded = name :: acc.expanded }
  | Var_semgrep_metavar _ -> acc

(* $1 to $9; $0 is the script, not a positional parameter. *)
let single_digit_position (name : string) : int option =
  match name with
  | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      int_of_string_opt name
  | _ -> None

(* A name that reads the positional parameters: a position, "@" or "*". A
   name that begins with a digit and has more than one character, such as
   the "10" of '${10}' or the "10" and "1x" of '$10' and '$1x', which bash
   reads as '${1}0' and '${1}x', is not a single-digit position. *)
let is_positional_name (name : string) : bool =
  match name with
  | "@"
  | "*" ->
      true
  | "0" -> false
  | _ -> String.length name > 0 && Char.Ascii.is_digit name.[0]

(* How the positional parameters the body reads relate to the arguments of
   the call: shifted by a number of positions when every 'shift' precedes
   every read at the top level of the body, or not known statically. *)
type renumbering = Shifted_by of int | Not_static

let top_level_pipelines (body : command) : pipeline list =
  let rec pipelines (l : blist) (acc : pipeline list) : pipeline list =
    match l with
    | Seq (_loc, left, right) -> pipelines left (pipelines right acc)
    | Pipelines (_loc, pl) -> pl @ acc
    | Empty _loc -> acc
  in
  match body with
  | Command_group (_loc, (_, bl, _)) -> pipelines bl []
  | _ ->
      let loc = AST_bash_loc.command_loc body in
      [ Command { loc; command = body; redirects = [] } ]

(* 'shift' or 'shift N' with N a single digit. *)
let rec literal_shift (x : pipeline) : int option =
  match x with
  | Control_operator (_loc, pip, (Foreground _, _)) -> literal_shift pip
  | Command { command = Simple_command { arguments; _ }; _ } -> (
      match arguments with
      | [ Word ("shift", _) ] -> Some 1
      | [ Word ("shift", _); Word (count, _) ] -> single_digit_position count
      | _ -> None)
  | _ -> None

let renumbering_of_body (body : command) : renumbering =
  let renumbering, _read =
    List.fold_left
      (fun ((renumbering, read) : renumbering * bool) (x : pipeline) ->
        match (renumbering, literal_shift x) with
        | Not_static, _ -> (Not_static, read)
        | Shifted_by _, Some _ when read -> (Not_static, read)
        | Shifted_by offset, Some count -> (Shifted_by (offset + count), read)
        | Shifted_by _, None ->
            let use = use_of_pipeline no_parameter_use x in
            if use.renumbers then (Not_static, read)
            else
              ( renumbering,
                read || List.exists is_positional_name use.expanded ))
      (Shifted_by 0, false) (top_level_pipelines body)
  in
  renumbering

(* The pattern the list of the arguments of a call is destructured by. Each
   single-digit position read is bound to its element; "@", "*", the other
   names that begin with a digit, and every position when the renumbering is
   not static, are bound to the whole list. After a static shift by N, when no
   single-digit position is read, the names bound to the whole list are
   bound instead to the list without its first N elements, in a rest
   pattern [...] (see Dataflow_tainting.pattern_leaves_with_offsets). *)
let positional_parameters_pattern (body : command) : G.pattern =
  let names =
    (use_of_command no_parameter_use body).expanded
    |> List.filter is_positional_name
    |> List.sort_uniq String.compare
  in
  let pat_id (name : string) : G.ident * G.id_info =
    ((name, Tok.unsafe_fake_tok name), G.empty_id_info ())
  in
  let bind_names (pat : G.pattern) (names : string list) : G.pattern =
    List.fold_left
      (fun (pat : G.pattern) (name : string) -> G.PatAs (pat, pat_id name))
      pat names
  in
  let elements, whole_list_names =
    match renumbering_of_body body with
    | Shifted_by offset -> (
        let highest =
          List.fold_left
            (fun (highest : int) (name : string) ->
              match single_digit_position name with
              | Some position -> Int.max highest position
              | None -> highest)
            0 names
        in
        let shifted =
          List.init offset (fun (_ : int) ->
              G.PatWildcard (Tok.unsafe_fake_tok "_"))
        in
        let read =
          List.init highest (fun (i : int) ->
              let id, id_info = pat_id (string_of_int (i + 1)) in
              G.PatId (id, id_info))
        in
        let list_names =
          List.filter
            (fun (name : string) -> Option.is_none (single_digit_position name))
            names
        in
        match list_names with
        | first :: others when offset > 0 && Int.equal highest 0 ->
            let id, id_info = pat_id first in
            let rest_tok = Tok.unsafe_fake_tok "..." in
            let rest =
              G.PatConstructor
                ( G.Id (("...", rest_tok), G.empty_id_info ()),
                  [ bind_names (G.PatId (id, id_info)) others ] )
            in
            (shifted @ [ rest ], [])
        | _ -> (shifted @ read, list_names))
    | Not_static -> ([], names)
  in
  bind_names (G.PatList (fb elements)) whole_list_names

(*****************************************************************************)
(* Converter from bash AST to generic AST *)
(*****************************************************************************)

let negate_expr (neg_tok : tok) (_cmd_loc : loc) (cmd : G.expr) : G.expr =
  G.opcall (G.Not, neg_tok) [ cmd ]

(*
   Redirect stderr in the last command of the pipeline.
*)
let redirect_pipeline_stderr_to_stdout pip =
  (* TODO: don't ignore redirects *)
  pip

let rec blist (env : env) (l : blist) : stmt_or_expr list =
  match l with
  | Seq (_loc, left, right) -> blist env left @ blist env right
  | Pipelines (_loc, pl) -> List_.map (fun x -> pipeline env x) pl
  | Empty _loc -> []

and pipeline (env : env) (x : pipeline) : stmt_or_expr =
  match x with
  | Command cmd_r -> cmd_redir env cmd_r
  | Pipeline (loc, pip, pipe_op, cmd_r) ->
      let pip, bar_tok =
        match pipe_op with
        | Bar, tok -> (pip, tok)
        | Bar_ampersand, tok ->
            (* Transpile:

                 a |& b

               -->

                 a 2>&1 | b
            *)
            (redirect_pipeline_stderr_to_stdout pip, tok)
      in
      let left = pipeline env pip |> as_expr in
      let right = cmd_redir env cmd_r |> as_expr in
      Expr (loc, G.opcall (G.Pipe, bar_tok) [ left; right ])
  | Control_operator (loc, pip, control_op) -> (
      match control_op with
      | Foreground _, _tok -> pipeline env pip
      | Background, amp_tok ->
          let arg = pipeline env pip |> as_expr in
          Expr (loc, G.opcall (G.Background, amp_tok) [ arg ]))

and cmd_redir (env : env) (x : cmd_redir) : stmt_or_expr =
  (* TODO: don't ignore redirects *)
  let { loc = _; command = cmd; redirects } = x in
  ignore redirects;
  command env cmd

(*
  The preference between stmt or expr depends on whether the most fitting
  construct of the generic AST is an stmt or an expr. For the todos, it
  may be little arbitrary.
*)
and command (env : env) (cmd : command) : stmt_or_expr =
  match cmd with
  | Simple_command { loc = _; assignments; arguments = [] | [ Word ("", _) ] }
    ->
      (* TODO: fix the tree-sitter grammar so it doesn't insert a "MISSING"
         node set to the empty string when there's no command following
         multiple assignments. *)
      List_.map (assignment env) assignments |> block
  | Simple_command { loc; assignments = _; arguments } -> (
      match arguments with
      | [ (Expr_semgrep_ellipsis tok as e) ] ->
          Expr ((tok, tok), expression env e)
      | [ (String_fragment (loc, Frag_semgrep_named_ellipsis _) as e) ] ->
          Expr (loc, expression env e)
      | arguments ->
          let args = List_.map (expression env) arguments in
          Expr (loc, call loc C.cmd args))
  | And (loc, left, and_tok, right) ->
      let left = pipeline env left |> as_expr in
      let right = pipeline env right |> as_expr in
      Expr (loc, G.opcall (G.And, and_tok) [ left; right ])
  | Or (loc, left, or_tok, right) ->
      let left = pipeline env left |> as_expr in
      let right = pipeline env right |> as_expr in
      Expr (loc, G.opcall (G.Or, or_tok) [ left; right ])
  | Subshell (loc, (_open_, bl, _close)) ->
      let args = [ stmt_group env loc (blist env bl) |> as_expr ] in
      Expr (loc, call loc C.subshell args)
  | Command_group (loc, (_open_, bl, _close)) ->
      stmt_group env loc (blist env bl)
  | Sh_test (loc, _) -> todo_expr2 loc
  | Bash_test (loc, _) -> todo_expr2 loc
  | Arithmetic_expression (loc, _) -> todo_expr2 loc
  | For_loop (loc, for_, loop_var, opt_in, do_, body, _done_) ->
      let header =
        let in_tk, expr =
          match opt_in with
          | Some (in_tk, vals) ->
              ( in_tk,
                G.Container
                  (List, fb (List_.map (fun x -> expression env x) vals))
                |> G.e )
          | None ->
              (*
                 Pretend there's a '"$@"', which is semantically correct
                 and avoids exception in IL_to_AST raised when the list
                 is empty.
              *)
              let fake_arg_array =
                let tok = do_ in
                let loc = (tok, tok) in
                let var_name = Special_variable_name ("@", tok) in
                let frag = Expansion (loc, Simple_expansion (loc, var_name)) in
                String (tok, [ frag ], tok)
              in
              (G.fake "", expression env fake_arg_array)
        in
        match loop_var with
        | Simple_variable_name var
        | Special_variable_name var
        | Var_semgrep_metavar var ->
            let entity = G.basic_entity var in
            let pat =
              match entity.name with
              | EN (Id (id, idinfo)) -> G.PatId (id, idinfo) |> G.p
              | EPattern pat -> pat
              | _ -> G.OtherPat (("PatEnt", G.fake "PatEnt"), [ G.En entity ])
            in
            G.ForEach (pat, in_tk, expr)
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.For (for_, header, body) |> G.s)
  | For_loop_c_style (loc, bl) ->
      (* TODO: loop *) stmt_group env loc (blist env bl)
  | Select (loc, _select, _loop_var, _opt_in, _do_, body, _done_) ->
      (* TODO *)
      stmt_group env loc (blist env body)
  | Case (loc, case, subject, _in_, clauses, _esac) ->
      let subject = expression env subject in
      let clauses =
        List_.map
          (fun (_loc, patterns, _paren, stmts, _opt_term) ->
            (* TODO: handle the different kinds of terminators. Insert breaks. *)
            let patterns =
              List_.map
                (fun e ->
                  let tok, _ = AST_bash_loc.expression_loc e in
                  let pat =
                    (* TODO: convert bash expression to generic pattern *)
                    G.OtherPat (("", tok), [ G.E (expression env e) ])
                  in
                  G.Case (tok, pat))
                patterns
            in
            G.CasesAndBody (patterns, blist env stmts |> block |> as_stmt))
          clauses
      in
      Stmt (loc, G.Switch (case, Some (Cond subject), clauses) |> G.s)
  | If (loc, if_, cond, then_, body, elifs, else_, _fi) ->
      let ifs = (loc, if_, cond, then_, body) :: elifs in
      let else_stmt =
        match else_ with
        | None -> None
        | Some (_loc, _else_tok, body) ->
            Some (blist env body |> block |> as_stmt)
      in
      let opt_stmt =
        List_.fold_right
          (fun if_ (else_stmt : G.stmt option) ->
            let _loc1, if_, cond, _then_, body = if_ in
            (* pad: TODO:  use more complex CondDecl when ready? *)
            let cond_expr = blist env cond |> block |> as_expr in
            let body_stmt = blist env body |> block |> as_stmt in
            Some (G.s (G.If (if_, G.Cond cond_expr, body_stmt, else_stmt))))
          ifs else_stmt
      in
      let stmt =
        match opt_stmt with
        | None -> (* there's at least one 'if ... then ...' *) assert false
        | Some stmt -> stmt
      in
      Stmt (loc, stmt)
  | While_loop (loc, while_, cond, _do_, body, _done_) ->
      let cond = stmt_group env loc (blist env cond) |> as_expr in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (while_, G.Cond cond, body) |> G.s)
  | Until_loop (loc, until, cond, _do_, body, _done_) ->
      let cond_loc = AST_bash_loc.blist_loc cond in
      let neg_cond =
        blist env cond |> stmt_group env loc |> as_expr
        |> negate_expr until cond_loc
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (until, G.Cond neg_cond, body) |> G.s)
  | Coprocess (_loc, _opt_name, cmd) -> (* TODO: coproc *) command env cmd
  | Assignment ass -> assignment env ass
  | Declaration x ->
      let assignments = List_.map (assignment env) x.assignments in
      (* TODO: don't ignore the "unknown" arguments that contain variables
         and such. *)
      assignments |> block
  | Negated_command (loc, excl_tok, cmd) ->
      let cmd_loc = AST_bash_loc.command_loc cmd in
      let cmd = command env cmd |> as_expr in
      let e = negate_expr excl_tok cmd_loc cmd in
      Expr (loc, e)
  | Function_definition (loc, def) ->
      let first_tok =
        match def.function_ with
        | Some function_tok -> function_tok
        | None -> fst (AST_bash_loc.variable_name_loc def.name)
      in
      (* A pattern does not constrain the positional parameters, which the
         source never declares, nor the output of the function, which
         depends on the body. *)
      let fparams, fattrs =
        match env with
        | Program ->
            ( [
                G.ParamPattern
                  ( positional_parameters_pattern def.body,
                    G.implicit_param_classic first_tok );
              ],
              [ AST_generic_helpers.command_output_attribute first_tok ] )
        | Pattern -> ([ G.ParamEllipsis (Tok.fake_tok first_tok "...") ], [])
      in
      let def_kind =
        G.FuncDef
          {
            G.fkind = (G.Function, first_tok);
            fparams = fb fparams;
            frettype = None;
            fbody = G.FBStmt (command env def.body |> as_stmt);
            fattrs;
          }
      in
      (* Function names are in another namespace than ordinary variables.
         They can't be accessed with the '$' notation. No need to prepend
         a '$' to the name like for variables. *)
      let def = (G.basic_entity (get_var_name def.name), def_kind) in
      Stmt (loc, G.DefStmt def |> G.s)

and assignment (env : env) ass =
  let var = G.N (mk_name ass.lhs) |> G.e in
  let value = expression env ass.rhs in
  let e =
    match ass.assign_op with
    | Set, tok -> G.Assign (var, tok, value)
    | Add, tok -> G.AssignOp (var, (G.Plus, tok), value)
  in
  Expr (ass.loc, G.e e)

(* This returns a Block on purpose e.g. for the body of a 'for' loop, which
   is needed for matching. We can't simplify it into a single expression
   if there's only one statement. *)
and stmt_group (_env : env) (loc : loc) (l : stmt_or_expr list) : stmt_or_expr =
  let stmts = List_.map as_stmt l in
  let start, end_ = loc in
  Stmt (loc, G.s (G.Block (start, stmts, end_)))

and expression (env : env) (e : expression) : G.expr =
  match e with
  | Word ("...", tok) when env =*= Pattern ->
      (* occurs in unquoted concatenations e.g. ...$x or $x... *)
      G.Ellipsis tok |> G.e
  | Word str -> G.L (G.String (fb str)) |> G.e
  | String (* "foo" *) (open_, frags, close) -> (
      match frags with
      | [ String_content ((str, _) as wrap) ]
        when not (env =*= Pattern && str = "...") ->
          (* normalization to enable matching of e.g. "foo" against foo *)
          G.L (G.String (fb wrap)) |> G.e
      | _ ->
          let loc = (open_, close) in
          List_.map (string_fragment env) frags |> call loc C.quoted_concat)
  | String_fragment (_loc, frag) -> string_fragment env frag
  | Raw_string (* 'foo' *) (str, tok) ->
      (* normalization to enable matching of e.g. 'foo' against foo *)
      let without_quotes =
        let len = String.length str in
        if len >= 2 && str.[0] =$= '\'' && str.[len - 1] =$= '\'' then
          String.sub str 1 (len - 2)
        else (* it's a bug but let's not fail *)
          str
      in
      G.L (G.String (fb (without_quotes, tok))) |> G.e
  | Ansii_c_string str -> G.L (G.String (fb str)) |> G.e
  | Special_character str -> G.L (G.String (fb str)) |> G.e
  | Concatenation (loc, el) ->
      List_.map (expression env) el |> call loc C.concat
  | Expr_semgrep_ellipsis tok -> G.Ellipsis tok |> G.e
  | Expr_semgrep_deep_ellipsis (_loc, (open_, e, close)) ->
      G.DeepEllipsis (open_, expression env e, close) |> G.e
  | Expr_semgrep_metavar mv -> G.N (mk_name mv) |> G.e
  | Equality_test (loc, _, _) -> (* don't know what this is *) todo_expr loc
  | Empty_expression loc -> G.L (G.String (fb ("", fst loc))) |> G.e
  | Array (_loc, (open_, elts, close)) ->
      let elts = List_.map (expression env) elts in
      G.Container (G.Array, (open_, elts, close)) |> G.e
  | Process_substitution (loc, (_open_, x, _close)) ->
      let arg = blist env x |> block |> as_expr in
      call loc C.proc_subst [ arg ]

and string_fragment (env : env) (frag : string_fragment) : G.expr =
  match frag with
  | String_content ("...", tok) when env =*= Pattern ->
      (* convert the '...' in '"${foo}...${bar}"' into an ellipsis *)
      G.Ellipsis tok |> G.e
  | String_content ((_, _tok) as wrap) -> G.e (G.L (G.String (fb wrap)))
  | Expansion (_loc, ex) -> expansion env ex
  | Command_substitution (open_, x, close) ->
      let loc = (open_, close) in
      let arg = blist env x |> block |> as_expr in
      call loc C.cmd_subst [ arg ]
  | Frag_semgrep_metavar mv -> G.N (mk_name mv) |> G.e
  | Frag_semgrep_named_ellipsis x -> G.N (mk_name x) |> G.e

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (_env : env) (x : expansion) : G.expr =
  match x with
  | Simple_expansion (loc, var_name) -> expand loc (mk_var_expr var_name)
  | Complex_expansion (_open_, x, _close) -> (
      match x with
      | Variable (loc, var, _words) -> expand loc (mk_var_expr var)
      | Complex_expansion_TODO (loc, _words) -> todo_expr loc)

and expand loc (var_expr : G.expr) : G.expr = call loc C.expand [ var_expr ]

let program_with_env (env : env) x = blist (env : env) x |> List_.map as_stmt

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
   Unwrap the pattern tree as much as possible to maximize matches.

   For example, 'echo' is parsed as a list of statements but occurs
   in the target program as a single stmt ('If' branch) or as an expr
   ('If' condition). Unwrapping into an expr allows the expr to match those
   cases.
*)
let any (x : blist) =
  let env = Pattern in
  match AST_bash_builder.blist_as_expression x with
  | Some e -> G.E (expression env e)
  (* TODO: simply | None -> G.Ss (program env x) but got regressions
   * problem with Parse_pattern.normalize_any probably
   *)
  | None -> (
      match program_with_env env x with
      | [ { G.s = G.ExprStmt (e, _semicolon); _ } ] -> G.E e
      | [ stmt ] -> G.S stmt
      | stmts -> G.Ss stmts)

let program x =
  let env = Program in
  program_with_env env x
