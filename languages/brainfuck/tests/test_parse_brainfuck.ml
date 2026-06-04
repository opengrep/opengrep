let test_hello () =
  let file = "languages/brainfuck/tests/hello.bf" in
  let ast = Parse_brainfuck.program file in
  Printf.printf "Parsed %d top-level statements\n" (List.length ast);
  (* +++++++++  = 9 inc calls
     [>++++++++<-]  = 1 while loop (11 stmts inside)
     >.         = 1 ptr_inc + 1 output
     Total top-level: 9 + 1 + 2 = 12 *)
  assert (List.length ast = 12);
  Printf.printf "PASS: hello.bf parsed correctly\n"

let test_nested_loops () =
  let file = "languages/brainfuck/tests/nested.bf" in
  let ast = Parse_brainfuck.program file in
  Printf.printf "Parsed nested.bf: %d top-level statements\n" (List.length ast);
  (* ++[>++[>+<-]<-] = 2 inc + 1 loop = 3 *)
  assert (List.length ast = 3);
  Printf.printf "PASS: nested.bf parsed correctly\n"

let test_comments_ignored () =
  let file = "languages/brainfuck/tests/comments.bf" in
  let ast = Parse_brainfuck.program file in
  Printf.printf "Parsed comments.bf: %d top-level statements\n" (List.length ast);
  (* Only + . are instructions, rest is comments *)
  assert (List.length ast = 2);
  Printf.printf "PASS: comments.bf parsed correctly\n"

let () =
  test_hello ();
  test_nested_loops ();
  test_comments_ignored ()
