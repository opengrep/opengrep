type token_kind =
  | Identifier
  | Keyword
  | Operator
  | Punctuation
  | IntLiteral of int64
  | FloatLiteral
  | CharLiteral of string
  | StringLiteral of string
  | StringSegment
  | DateLiteral
  | CDATA of string
  | LineTerminator
  | IfDirective
  | ElseIfDirective
  | ElseDirective
  | EndIfDirective
  | Other
  | EOF

type t = {
  kind: token_kind;
  content: string;
  tok : Tok.t
}

let make ?(uppercase=false) (lexbuf : Lexing.lexbuf) (k : token_kind) : t =
  let content =
    if uppercase
      then String.uppercase_ascii (Lexing.lexeme lexbuf)
      else Lexing.lexeme lexbuf
  in
  { kind = k;
    content;
    tok = Tok.tok_of_lexbuf lexbuf
  }

let extract_string_content (t : t) : string =
  match t.kind with
  | StringLiteral s
  | CharLiteral s
  | CDATA s -> s
  | _ -> raise Common.Impossible

let extract_int_content (t : t) : int64 =
  match t.kind with
  | IntLiteral n -> n
  | _ -> raise Common.Impossible

let token_ghost (tok : t) : bool =
  match tok with
  | { kind = LineTerminator; _ } -> true
  | _ -> false

let token_line_terminator (tok : t) : bool =
  match tok with
  | { kind = LineTerminator; _ } -> true
  | _ -> false

let is_ident (tok : t) : bool =
  match tok.kind with Identifier -> true | _ -> false

let is_keyword (tok : t) : bool =
  match tok.kind with Keyword -> true | _ -> false

let is_operator (tok : t) : bool =
  match tok.kind with Operator -> true | _ -> false

let is_punctuation (tok : t) : bool =
  match tok.kind with Punctuation -> true | _ -> false

let is_int_literal (tok : t) : bool =
  match tok.kind with IntLiteral _ -> true | _ -> false

let is_float_literal (tok : t) : bool =
  match tok.kind with FloatLiteral -> true | _ -> false

let is_char_literal (tok : t) : bool =
  match tok.kind with CharLiteral _ -> true | _ -> false

let is_string_literal (tok : t) : bool =
  match tok.kind with StringLiteral _ -> true | _ -> false

let is_string_segment (tok : t) : bool =
  match tok.kind with StringSegment -> true | _ -> false

let is_date_literal (tok : t) : bool =
  match tok.kind with DateLiteral -> true | _ -> false

let is_cdata (tok : t) : bool =
  match tok.kind with CDATA _ -> true | _ -> false

let is_other (tok : t) : bool =
  match tok.kind with Other -> true | _ -> false

let is_line_terminator (tok : t) : bool =
  match tok.kind with LineTerminator -> true | _ -> false

let is_if_directive (tok : t) : bool =
  match tok.kind with IfDirective -> true | _ -> false

let is_elseif_directive (tok : t) : bool =
  match tok.kind with ElseIfDirective -> true | _ -> false

let is_else_directive (tok : t) : bool =
  match tok.kind with ElseDirective -> true | _ -> false

let is_end_if_directive (tok : t) : bool =
  match tok.kind with EndIfDirective -> true | _ -> false

let is_eof (tok : t) : bool =
  match tok.kind with EOF -> true | _ -> false

let token_match (s : string) (tok : t) : bool =
  String.equal s tok.content

let print_token (t : t) : unit =
  match t with
  | { kind = LineTerminator; _ } -> ()
  | { content = s; _ } -> print_string (s ^ " ")

let print_tokens (ts : t list) : unit =
  List.iter print_token ts

(* We don't have a good way to represent big-nums in the generic AST,
 * so in case we reach the limit, we just use a big number *)
let make_int64 (s : string) : int64 =
  match Int64.of_string_opt s with
  | Some i -> i
  | _ -> Int64.max_int
