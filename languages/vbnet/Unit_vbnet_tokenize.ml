module T = Vbnet_token
open Vbnet_tokenize

let preview (t : T.t) : T.token_kind * string = t.kind, t.content

let tests =
  Testo.categorize "lexing_vbnet"
  [
    Testo.create "lexer (basic)" (fun () ->
      let s = tokenize "12 + \"hello\" } Sub" |> List.map preview in
      let r =
      T.([ IntLiteral 12L, "12"
         ; Operator, "+"
         ; StringLiteral "hello", "\"hello\""
         ; Punctuation, "}"
         ; Keyword, "SUB"
         ; EOF, ""])
      in
      assert (s = r));

    Testo.create "lexer (interpolated strings)" (fun () ->
      let s = tokenize "&HF + $\"abc{abc xyz}xyz{\"abc\" { + }}\" {}" |> List.map preview in
      let r =
      T.([ IntLiteral 15L, "&HF"
         ; Operator, "+"
         ; Operator, "$\""
         ; StringSegment, "abc"
         ; Identifier, "ABC"
         ; Identifier, "XYZ"
         ; StringSegment, "xyz"
         ; StringLiteral "abc", "\"abc\""
         ; Punctuation, "{"
         ; Operator, "+"
         ; Punctuation, "}"
         ; Operator, "\""
         ; Punctuation, "{"
         ; Punctuation, "}"
         ; EOF, ""])
      in
      assert (s = r));

    Testo.create "lexer (nested interpolated strings)" (fun () ->
      let s = tokenize "$\"{\"xyz\" a $\"{b \"xyz\"}\"}\"" |> List.map preview in
      let r =
      T.([ Operator, "$\""
         ; StringLiteral "xyz", "\"xyz\""
         ; Identifier, "A"
         ; Operator, "$\""
         ; Identifier, "B"
         ; StringLiteral "xyz", "\"xyz\""
         ; Operator, "\""
         ; Operator, "\""
         ; EOF, ""])
      in
      assert (s = r));

    Testo.create "lexer (#If directives)" (fun () ->
      let s = tokenize "#If DEBUG\nx = 1\n#ElseIf RELEASE\ny = 2\n#Else\nz = 3\n#End If\n"
              |> List.map preview in
      T.(assert (List.mem (IfDirective, "#If DEBUG\n") s));
      T.(assert (List.mem (ElseIfDirective, "#ElseIf RELEASE\n") s));
      T.(assert (List.mem (ElseDirective, "#Else\n") s));
      T.(assert (List.mem (EndIfDirective, "#End If\n") s)));

    Testo.create "lexer (date literals)" (fun () ->
      [ "# 8/23/1970 3:45:39AM #"
      ; "# 8/23/1970 #"
      ; "# 3:45:39AM #"
      ; "# 3:45:39 #"
      ; "# 13:45:39 #"
      ; "# 1AM #"
      ; "# 13:45:39PM #"
      ] |>
      List.iter (fun d ->
        let s = tokenize d |> List.map preview in
        let r =
          T.([ DateLiteral, d
             ; EOF, ""])
        in
        assert (s = r)))
  ]
