(*
   Unit tests for glob pattern parsing and matching.
*)

open Printf

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

let test_matching ~(match_leading_period : bool) (pattern : string)
    (path : string) (matches : bool) () =
  let pat = Parse.parse_string pattern in
  let compiled_pat =
    Match.compile ~match_leading_period
      ~source:(Match.string_loc ~source_kind:None pattern)
      pat
  in
  let res = Match.run compiled_pat path in
  printf
    "pattern: %s\n\
     path: %s\n\
     expected match: %B\n\
     actual match: %B\n\
     pattern info:\n\
     %s\n\
     %s\n"
    pattern path matches res (Pattern.show pat)
    (Match.show_compiled_pattern compiled_pat);
  Alcotest.(check bool) __LOC__ matches res

(* gitignore: '*' and '?' match a leading period *)
let test = test_matching ~match_leading_period:true

(* shell: a leading period is matched only by a literal '.' *)
let test_shell = test_matching ~match_leading_period:false

(*****************************************************************************)
(* Test data *)
(*****************************************************************************)

(*
   These tests conform with the gitignore specification, falling back to
   the official gitignore implementation in case some important behavior
   is unspecified. The tests of [test_shell] check the shell treatment of
   a leading period.
*)
let tests =
  Testo.categorize "Glob"
    [
      t "simple" (test "abc" "abc" true);
      t "anchor left" (test "abc" "xabc" false);
      t "anchor right" (test "abc" "abcd" false);
      t "absolute path" (test "/a/b" "/a/b" true);
      t "star 1" (test "*.c" "hello.c" true);
      t "star 2" (test "*.c" "hello.h" false);
      (* '*' can not match a '/' *)
      t "star 3" (test "*.c" "a/hello.c" false);
      t "question 1" (test "a.?" "a.c" true);
      t "question 2" (test "a.?" "b.c" false);
      t "relative path 1" (test "ab/*.c" "ab/hello.c" true);
      t "relative path 2" (test "ab/*.c" "hello.c" false);
      t "relative path 3" (test "ab/*.c" "ab/" false);
      (* globs are left anchored, implicit '^' *)
      t "relative path 4" (test "ab/*.c" "x/ab/hello.c" false);
      t "absolute path 1" (test "/ab/*.c" "/ab/hello.c" true);
      t "absolute path 2" (test "/ab/*.c" "ab/hello.c" false);
      (* anchored too, so no match *)
      t "absolute path 3" (test "ab/*.c" "/ab/hello.c" false);
      t "char range 1" (test "[a-bz]" "b" true);
      t "char range 2" (test "[a-bz]" "c" false);
      t "char range 3" (test "[^a-bz]" "b" false);
      t "char range 4" (test "[^a-bz]" "c" true);
      (* alternative negative range syntax *)
      t "char range 5" (test "[!a-bz]" "b" false);
      t "char range 6" (test "[!a-bz]" "c" true);
      (* remember that '?' means any character; it is not a modifier
       * to the previous construct (like in regexps), hence
       * the match below because '?' matches 'c'
       *)
      t "char range 7" (test "[a][^0-9]?" "abc" true);
      (* a path is matched byte by byte, as in git: 'é' is two bytes *)
      t "'?' matches one byte" (test "?" "\xc3\xa9" false);
      t "'??' matches a two-byte character" (test "??" "\xc3\xa9" true);
      t "a two-byte character in a bracket expression is two members"
        (test "[\xc3\xa9]" "\xc3\xa9" false);
      t "a two-byte character matched by two bracket expressions"
        (test "[\xc3\xa9][\xc3\xa9]" "\xc3\xa9" true);
      t "a non-ASCII member does not match another character"
        (test "[\xc3\xa9]" "\xc3\x83" false);
      t "a literal non-ASCII name" (test "caf\xc3\xa9.js" "caf\xc3\xa9.js" true);
      (* bracket expressions as in git *)
      t "reversed range matches its first character"
        (test "[z-a]" "z" true);
      t "reversed range matches nothing else" (test "[z-a]" "a" false);
      t "']' right after '[' is a member" (test "[]]" "]" true);
      t "']' right after the negation is a member" (test "[!]a]" "]" false);
      t "escaped ']' is a member" (test "[\\]]" "]" true);
      t "escaped '-' is a member" (test "[a\\-c]" "-" true);
      t "escaped '-' is not a range" (test "[a\\-c]" "b" false);
      t "'-' before the closing ']' is a member" (test "[a-]" "-" true);
      (* ':', '.' or '=' first and last is a set, not POSIX syntax *)
      t "':' first and last in a bracket expression" (test "x[:digit:]" "x:" true);
      t "':' first and last, a letter of the set" (test "x[:digit:]" "xd" true);
      t "':' first and last, not a digit" (test "x[:digit:]" "x1" false);
      t "'.' first and last in a bracket expression" (test "[.a.]" "a" true);
      t "'=' first and last in a bracket expression" (test "[=a=]" "=" true);
      t "POSIX class" (test "[[:digit:]]" "1" true);
      t "POSIX class 2" (test "[[:digit:]]" "a" false);
      t "complemented bracket expression does not match '/'"
        (test "x[!a]y" "x/y" false);
      t "complemented bracket expression" (test "x[!a]y" "xby" true);
      t "range that contains '/' does not match '/'" (test "x[--0]y" "x/y" false);
      t "range that contains '/'" (test "x[--0]y" "x.y" true);
      t "unclosed bracket expression is a syntax error" (fun () ->
          Alcotest.check_raises __LOC__ (Lexer.Syntax_error "missing ']'")
            (fun () -> ignore (Parse.parse_string "[]" : Pattern.t)));
      t "unknown POSIX class is a syntax error" (fun () ->
          Alcotest.check_raises __LOC__
            (Lexer.Syntax_error "unknown character class '[:foo:]'") (fun () ->
              ignore (Parse.parse_string "[[:foo:]]" : Pattern.t)));
      t "trailing slash 1" (test "a/" "a/" true);
      t "trailing slash 2" (test "a/" "a" false);
      t "trailing slash 3" (test "a" "a/" false);
      (* gitignore extensions not found in glob(3).
       * As opposed to '*', '**' can match the '/' character.
       *)
      t "ellipsis 1" (test "**" "a" true);
      t "ellipsis 2" (test "**" "a/b/c" true);
      t "ellipsis 3" (test "/**" "/a/b" true);
      t "ellipsis 4" (test "/**" "a/b" false);
      t "ellipsis 5" (test "**/" "a/b/c/" true);
      t "ellipsis 6" (test "**/" "a/b/c" false);
      (* can match any intermediate segments *)
      t "ellipsis 7" (test "a/**/z" "a/b/c/d/z" true);
      t "ellipsis 8" (test "a**" "a" true);
      t "ellipsis 9" (test "a**" "abc" true);
      (* Not matching because '**' is special only if it's alone
       * as a path segment (see the gitignore spec). a** is
       * equivalent to just a*.
       *)
      t "ellipsis 10" (test "a**" "a/b" false);
      t "ellipsis 11" (test "****" "a" true);
      t "ellipsis 12" (test "****" "a/b" false);
      t "ellipsis 13" (test "**/*b" "/a/b" true);
      t "ellipsis 14" (test "**/*b" "a/b" true);
      t "ellipsis 15" (test "**/*b" "//a/b" true);
      t "ellipsis 16" (test "a/**" "a/b/c" true);
      (* a trailing '**' matches everything inside, not the folder itself *)
      t "ellipsis 17" (test "a/**" "a/" false);
      t "ellipsis 18" (test "a/**" "a" false);
      t "double slash" (test "//a//b//" "//a//b//" true);
      t "double slash in pattern" (test "//a//b//" "/a/b/" true);
      t "double slash in path" (test "/a/b/" "//a//b//" true);
      t "empty trailing segment" (test "a/*" "a/" false);
      t "empty leading segment" (test "*/a" "/a" false);
      t "not a dot file" (test "*a" "b.a" true);
      t "match dot file with wildcard" (test "*" ".a" true);
      t "match dot file with wildcard 2" (test "*a" ".a" true);
      t "match dot file with wildcard 3" (test "*.js" ".hidden.js" true);
      t "match dot file with question mark" (test "?a" ".a" true);
      t "match dot file with wildcard sequence" (test "*?a" ".a" true);
      t "match dot file with wildcard sequence 2" (test "?*a" ".a" true);
      t "match dot file in a subfolder with wildcard"
        (test "**/*.js" "a/.hidden.js" true);
      t "shell: don't match dot file with wildcard" (test_shell "*" ".a" false);
      t "shell: don't match dot file with wildcard 2"
        (test_shell "*a" ".a" false);
      t "shell: don't match dot file with wildcard 3"
        (test_shell "*.js" ".hidden.js" false);
      t "match dot file with literal dot" (test ".?b" ".ab" true);
      t "shell: match dot file with literal dot" (test_shell ".?b" ".ab" true);
      t "shell: don't match dot file with wildcard sequence"
        (test_shell "*?a" ".a" false);
      t "shell: don't match dot file with wildcard sequence 2"
        (test_shell "?*a" ".a" false);
      t "match dot file with '**'" (test "**" ".a" true);
      t "match dot file with '**' 2" (test "a/**/c" "a/.b/c" true);
      t "don't match the parent folder segment with wildcard"
        (test "*/a" "../a" false);
      t "don't match the current folder segment with wildcard"
        (test "*/a" "./a" false);
      t "don't match the parent folder segment with question marks"
        (test "??/a" "../a" false);
      t "don't match the current folder segment with question mark"
        (test "?/a" "./a" false);
      t "match a dot file that starts with two dots with wildcard"
        (test "*" "..a" true);
      t "match the parent folder segment with literal dots"
        (test "../a" "../a" true);
      t "don't match the parent folder segment with bracket expressions"
        (test "[.][.]/a" "../a" false);
      t "don't match the current folder segment with a bracket expression"
        (test "[.]/a" "./a" false);
      t "match a dot file with a bracket expression"
        (test "[.]a" ".a" true);
      t "shell: don't match dot file with a bracket expression"
        (test_shell "[.]*" ".a" false);
      t "shell: don't match dot folder with '**'"
        (test_shell "**/*.js" ".d/c.js" false);
      t "shell: match folder with '**'" (test_shell "**/*.js" "d/c.js" true);
      t "shell: don't match dot file with trailing '**'"
        (test_shell "a/**" "a/.b" false);
      t "shell: match file with trailing '**'" (test_shell "a/**" "a/b" true);
      t "shell: don't match dot file with '**' alone" (test_shell "**" ".a" false);
      (* a long segment before a period-prefixed one *)
      t "shell: don't match a dot file below a long folder with trailing '**'"
        (test_shell "src/**" "src/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/.h/x.js"
           false);
      t "shell: match deep files with trailing '**'"
        (test_shell "src/**" "src/a/b/c.js" true);
      t "shell: match dot file with literal dot and wildcard"
        (test_shell ".*" ".a" true);
    ]
