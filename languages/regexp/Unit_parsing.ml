(*
   Test regexp parsing on individual files in /tests
*)

open Common

let t = Testo.create

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

(* Check that no syntax error is raised (without checking the parse tree) *)
let test_syntax pat () =
  let _ = Parse.string pat in
  ()

let test_repeat_range (pat : string) (expected : AST.repeat_range) () =
  match Parse.string pat with
  | Repeat (_, _, range, _) ->
      Alcotest.(check (pair int (option int))) pat expected range
  | _ -> Alcotest.failf "%s is not a repetition" pat

let test_back_reference (pat : string) (expected : int) () =
  match Parse.string pat with
  | Special (_, Numeric_back_reference group) ->
      Alcotest.(check int) pat expected group
  | _ -> Alcotest.failf "%s is not a back reference" pat

let test_unrecognised_char (pat : string) () =
  match Parse.string pat with
  | Char (_, Other _) -> ()
  | _ -> Alcotest.failf "%s should parse to an unrecognised character" pat

(* Check that a sequence of characters or a character class parses to these
   code points, in order. *)
let test_code_points (pat : string) (expected : int list) () =
  let rec of_char_class (char_class : AST.char_class) : int list =
    match char_class with
    | Empty -> []
    | Singleton code_point -> [ code_point ]
    | Union (a, b) -> of_char_class a @ of_char_class b
    | _ -> Alcotest.failf "%s is not a set of single code points" pat
  in
  let rec of_regexp (regexp : AST.t) : int list =
    match regexp with
    | Char (_, char_class) -> of_char_class char_class
    | Seq (_, a, b) -> of_regexp a @ of_regexp b
    | _ -> Alcotest.failf "%s is not a sequence of characters" pat
  in
  Alcotest.(check (list int)) pat expected (of_regexp (Parse.string pat))

let test_valid_files dialect rel_path () =
  let dir = Filename.concat tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files
  |> List.iter (fun file ->
         try
           let _ = Parse.parse ~conf:(Dialect.conf dialect) (Fpath.v file) in
           ()
         with
         | exn ->
             Alcotest.failf "it should correctly parse %s (exn = %s)" file
               (Common.exn_to_s exn))

let test_invalid_files dialect rel_path () =
  let dir = Filename.concat tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files
  |> List.iter (fun file ->
         try
           let _ast = Parse.file ~conf:(Dialect.conf dialect) (Fpath.v file) in
           Alcotest.failf "it should have thrown a Parse_error %s" file
         with
         | Parsing_error.Syntax_error _ -> ()
         | exn ->
             Alcotest.failf "throwing wrong exn %s on %s" (Common.exn_to_s exn)
               file)

let tests =
  Testo.categorize_suites "regexp parsing"
    [
      Testo.categorize "pcre"
        [
          (* The user mostly likely intended '[[:alpha:]]'.
             PCRE reports an error when encountering a POSIX
             character class outside of square brackets but we don't. *)
          t "not a posix character class" (test_syntax {|[:alpha:]|});
          t "not a broken posix character class" (test_syntax {|[:]|});
          (* Tolerate some malformed input.
             We don't necessarily need to tolerate malformed input but
             we want at least to avoid uninformative errors such as
             'Failure "lexing: empty token"' *)
          t "tolerate unfinished character class" (test_syntax {|[a|});
          t "tolerate unfinished posix character class"
            (test_syntax {|[[:alpha|});
          t "tolerate unfinished non-capturing group" (test_syntax {|(?|});
          t "tolerate unfinished raw sequence" (test_syntax {|\Qabc|});
          t "tolerate malformed UTF-8 in raw sequence"
            (test_code_points "\\Q\xe9\\E" [ 0xe9 ]);
          t "octal escape in a character class"
            (test_code_points {|[\101]|} [ 65 ]);
          t "8 and 9 after a backslash in a character class are literal"
            (test_code_points {|[\8\9]|} [ 56; 57 ]);
          t "octal escape in a character class reads at most three digits"
            (test_code_points {|[\0113]|} [ 9; 51 ]);
          t "octal escape outside a character class reads at most three digits"
            (test_code_points {|\0113|} [ 9; 51 ]);
          t "octal escape stops at a digit that is not octal"
            (test_code_points {|[\18]|} [ 1; 56 ]);
          t "braced octal escape" (test_code_points {|[\o{101}]|} [ 65 ]);
          t "o after a backslash without octal digits in braces is literal"
            (test_code_points {|[\o{8}]|} [ 111; 123; 56; 125 ]);
          t "o after a backslash is literal" (test_code_points {|\o|} [ 111 ]);
          t "hexadecimal escape in a character class reads at most two digits"
            (test_code_points {|[\x414]|} [ 65; 52 ]);
          t
            "hexadecimal escape outside a character class reads at most two \
             digits"
            (test_code_points {|\x414|} [ 65; 52 ]);
          t "u escape reads four hexadecimal digits"
            (test_code_points {|[\u00410]|} [ 65; 48 ]);
          t "u after a backslash without four hexadecimal digits is literal"
            (test_code_points {|\u41|} [ 117; 52; 49 ]);
          t "repetition count above max_int"
            (test_repeat_range {|a{99999999999999999999}|}
               (max_int, Some max_int));
          t "repetition bound above max_int"
            (test_repeat_range {|a{1,99999999999999999999}|} (1, Some max_int));
          t "back reference above max_int"
            (test_back_reference {|\99999999999999999999|} max_int);
          t "code point too large for an int"
            (test_unrecognised_char {|\x{ffffffffffffffffffff}|});
          t "hexadecimal code point above max_int"
            (test_unrecognised_char {|\x{7fffffffffffffff}|});
          t "octal code point above max_int"
            (test_unrecognised_char {|\o{777777777777777777777}|});
          t "code point above the Unicode range"
            (test_unrecognised_char {|\x{110000}|});
          (* Check regexps kept in files *)
          t "valid files" (test_valid_files Dialect.PCRE "regexp/pcre/parsing");
          t "invalid files"
            (test_invalid_files Dialect.PCRE "regexp/pcre/parsing_errors");
        ];
      Testo.categorize "pcre_extended"
        [
          t "valid files"
            (test_valid_files Dialect.PCRE_extended
               "regexp/pcre_extended/parsing");
          t "invalid files"
            (test_invalid_files Dialect.PCRE_extended
               "regexp/pcre_extended/parsing_errors");
        ];
      Testo.categorize "perl_xx"
        [
          t "valid files"
            (test_valid_files Dialect.Perl_xx "regexp/perl_xx/parsing");
          t "invalid files"
            (test_invalid_files Dialect.Perl_xx "regexp/perl_xx/parsing_errors");
        ];
    ]
