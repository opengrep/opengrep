module T = Vbnet_token
open Vbnet_tokenize

let preview (t : T.t) : T.token_kind * string = t.kind, t.content

let tests =
  Testo.categorize "lexing_vbnet"
  [
    Testo.create "lexer (basic)" (fun () ->
      let s = tokenize "1 + \"hello\" } Sub" |> List.map preview in
      let r =
      T.([ IntLiteral, "1"
         ; Operator, "+"
         ; StringLiteral, "\"hello\""
         ; Punctuation, "}"
         ; Keyword, "SUB"
         ; EOF, ""])
      in
      assert (s = r));

    Testo.create "lexer (interpolated strings)" (fun () ->
      let s = tokenize "1 + $\"abc{abc xyz}xyz{\"abc\" { + }}\" {}" |> List.map preview in
      let r =
      T.([ IntLiteral, "1"
         ; Operator, "+"
         ; Operator, "$\""
         ; StringSegment, "abc"
         ; Identifier, "ABC"
         ; Identifier, "XYZ"
         ; StringSegment, "xyz"
         ; StringLiteral, "\"abc\""
         ; Punctuation, "{"
         ; Operator, "+"
         ; Punctuation, "}"
         ; Operator, "\""
         ; Punctuation, "{"
         ; Punctuation, "}"
         ; EOF, ""])
      in
      assert (s = r))
  ]
