type token_kind =
  | Identifier
  | Keyword
  | Operator
  | Punctuation
  | IntLiteral
  | FloatLiteral
  | CharLiteral
  | StringLiteral
  | StringSegment
  | DateLiteral
  | CDATA
  | LineTerminator
  | Other
  | EOF

type t = {
  kind: token_kind;
  content: string;
  tok : Tok.t
}

let make ?(uppercase=false) (lexbuf : Lexing.lexbuf) (k : token_kind) : t =
  { kind = k;
    content = if uppercase
                then String.uppercase_ascii (Lexing.lexeme lexbuf)
                else Lexing.lexeme lexbuf;
    tok = Tok.tok_of_lexbuf lexbuf
  }

let token_ghost (tok : t) : bool =
  match tok with
  | { kind = LineTerminator; _ } -> true
  | _ -> false

let token_match (s : string) (tok : t) : bool =
  match s, tok with
  | "<IDENT>", { kind = Identifier; _ }
  | "<KEYWORD>", { kind = Keyword; _ }
  | "<OPERATOR>", { kind = Operator; _ }
  | "<PUNCTUATION>", { kind = Punctuation; _ }
  | "<INT>", { kind = IntLiteral; _ }
  | "<FLOAT>", { kind = FloatLiteral; _ }
  | "<CHAR>", { kind = CharLiteral; _ }
  | "<STRING>", { kind = StringLiteral; _ }
  | "<STRING_SEGMENT>", { kind = StringSegment; _ }
  | "<DATE>", { kind = DateLiteral; _ }
  | "<CDATA>", { kind = CDATA; _ }
  | "<LINE_TERMINATOR>", { kind = LineTerminator; _ }
  | "<OTHER>", { kind = Other; _ }
  | "<EOF>", { kind = EOF; _ } -> true
  | _, _ -> String.equal s tok.content

let print_token (t : t) : unit =
  match t with
  | { kind = LineTerminator; _ } -> ()
  | { content = s; _ } -> print_string (s ^ " ")

let print_tokens (ts : t list) : unit =
  List.iter print_token ts

