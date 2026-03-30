(* Brainfuck parser — produces AST_generic.program directly.

   Brainfuck has 8 instructions:
     >  Move pointer right      (represented as Call "ptr_inc")
     <  Move pointer left       (represented as Call "ptr_dec")
     +  Increment cell          (represented as Call "inc")
     -  Decrement cell          (represented as Call "dec")
     .  Output cell             (represented as Call "output")
     ,  Input to cell           (represented as Call "input")
     [  Loop start (while cell != 0)
     ]  Loop end

   All other characters are comments and ignored.
*)

module G = AST_generic

let mk_tok file offset line col str =
  let loc =
    {
      Tok.str;
      pos =
        { bytepos = offset; line; column = col; file = Fpath.v file };
    }
  in
  Tok.OriginTok loc

let mk_id s tok =
  G.N (G.Id ((s, tok), G.empty_id_info ())) |> G.e

let mk_call name tok =
  G.ExprStmt (G.Call (mk_id name tok, Tok.unsafe_fake_bracket []) |> G.e, tok)
  |> G.s

let parse_string ?(is_pattern = false) ?(file = "<pattern>") content =
  let len = String.length content in
  let stmts = ref [] in
  let stack = ref [] in
  let line = ref 1 in
  let col = ref 0 in
  let i = ref 0 in
  while !i < len do
    let c = content.[!i] in
    let tok = mk_tok file !i !line !col (String.make 1 c) in
    (match c with
    (* In pattern mode, ... is a semgrep ellipsis (match any statements) *)
    | '.' when is_pattern && !i + 2 < len
               && content.[!i + 1] = '.' && content.[!i + 2] = '.' ->
        let tok3 = mk_tok file !i !line !col "..." in
        stmts := (G.ExprStmt (G.Ellipsis tok3 |> G.e, tok3) |> G.s) :: !stmts;
        i := !i + 2; col := !col + 2
    | '>' -> stmts := mk_call ">" tok :: !stmts
    | '<' -> stmts := mk_call "<" tok :: !stmts
    | '+' -> stmts := mk_call "+" tok :: !stmts
    | '-' -> stmts := mk_call "-" tok :: !stmts
    | '.' -> stmts := mk_call "." tok :: !stmts
    | ',' -> stmts := mk_call "," tok :: !stmts
    | '[' ->
        stack := !stmts :: !stack;
        stmts := []
    | ']' -> (
        match !stack with
        | outer :: rest ->
            let body = List.rev !stmts in
            let loop =
              G.While (tok, G.Cond (G.L (G.Bool (true, tok)) |> G.e),
                       G.Block (tok, body, tok) |> G.s)
              |> G.s
            in
            stmts := loop :: outer;
            stack := rest
        | [] -> ())
    | '\n' ->
        incr line;
        col := -1
    | _ -> ());
    incr col;
    incr i
  done;
  List.rev !stmts

let program file =
  let content = UFile.read_file (Fpath.v file) in
  parse_string ~file content

let parse_pattern str =
  match parse_string ~is_pattern:true str with
  | [ { G.s = G.ExprStmt (e, _); _ } ] -> G.E e
  | [ s ] -> G.S s
  | stmts -> G.Ss stmts
