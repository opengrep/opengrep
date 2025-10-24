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

let is_ok (s : string) : bool =
  match P.tokenize_and_parse_string P.compilation_unit s with
  | Ok _ -> true
  | _ -> false

let is_partial (s : string) : bool =
  match P.tokenize_and_parse_string P.compilation_unit s with
  | Partial _ -> true
  | _ -> false

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
      >>> P.single_line_statement);

    Testo.create "partial result smoke test" (fun () ->
        assert (is_ok "Class c \n End Class");
        assert (is_ok "Class c \n Class d \n End Class \n End Class");
        assert (is_ok "Class c \n Sub foo \n End Sub \n End Class");
        assert (is_partial "Class c \n Class d \n End Class");
        assert (is_ok "Class c \n Sub foo \n End Class"); (* tricky: declaration without body is ok *)
        assert (is_ok "Class c \n Sub foo \n x = 1 \n End Sub \n End Class");
        assert (is_partial "Class c \n Sub foo \n x = 1 \n xxx yyy zzz \n End Sub \n End Class");
        assert (is_ok "Namespace n \n Class c \n End Class \n End Namespace");
        assert (is_partial "Namespace n \n Class c xx yyy zzz \n End Class \n End Namespace");
        assert (is_partial "Namespace n \n Class c \n End Class \n xxx yyy zzz \n End Namespace");
        assert (is_ok "Module c \n End Module");
        assert (is_ok "Module c \n Module d \n End Module \n End Module");
        assert (is_ok "Module c \n Sub foo \n End Sub \n End Module");
        assert (is_partial "Module c \n Module d \n End Module");
        assert (is_ok "Module c \n Sub foo \n x = 1 \n End Sub \n End Module");
        assert (is_partial "Module c \n Sub foo \n x = 1 \n xxx yyy zzz \n End Sub \n End Module");
        assert (is_ok "Namespace c \n End Namespace");
        assert (is_ok "Namespace n \n Module c \n End Module \n End Namespace");
        assert (is_partial "Namespace n \n Module c xx yyy zzz \n End Module \n End Namespace");
        assert (is_partial "Namespace n \n Module c \n End Module \n xxx yyy zzz \n End Namespace")
      )
  ]
