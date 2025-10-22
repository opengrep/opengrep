module T = Vbnet_tokenize
module P = Vbnet_parser

let (>>>) (s : string) (p : 'a P.parser) : unit =
  let r = T.tokenize s |> P.run p in
  assert (List.length r = 1);
  assert ((List.hd r).P.next |> List.length = 1 (* EOF *) )

let (>/>) (s : string) (p : 'a P.parser) : unit =
  let r = T.tokenize s |> P.run p in
  assert (List.length r = 0
          || (List.hd r).P.next |> List.length > 1)

let tests =
  Testo.categorize "parsing_vbnet"
  [
    Testo.create "parser smoke test" (fun () ->
        "2 + ... + foo(1, $A, A$)"
        >>> P.expression;

        "(2 + (3 + 4)"
        >/> P.expression;

        "foo. ... (\"a\"\"b\")"
        >>> P.expression;

        "<a><b></b></a>.foo()"
        >>> P.expression;

        {| $"hello{2 + 2 & "aa" & $"aa{b}"}""" |}
        >>> P.expression;

        "If (a := 1, b := 2) Then x = not await 4 : Exit Sub Else <foo></foo>"
        >>> P.single_line_statement
  )]
