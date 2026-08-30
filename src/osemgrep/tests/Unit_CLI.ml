(* Expectation tests for CLI.with_experimental_flag (see issue #131). *)

let t = Testo.create

let check_argv msg ~expected (actual : string array) =
  Alcotest.(check (array string)) msg expected actual

(* coupling: these two cases used to be commented-out asserts in CLI.ml *)
let test_scan_help () =
  check_argv "opengrep scan --help"
    ~expected:[| "opengrep"; "scan"; "--experimental"; "--help" |]
    (CLI.with_experimental_flag [| "opengrep"; "scan"; "--help" |])

let test_dash_c () =
  check_argv "opengrep -c rules libs"
    ~expected:[| "opengrep"; "--experimental"; "-c"; "rules"; "libs" |]
    (CLI.with_experimental_flag [| "opengrep"; "-c"; "rules"; "libs" |])

let test_bare_subcommand () =
  (* the bug fixed by #802: a bare 'opengrep ci' used to scan a target
     named "ci" instead of running the ci subcommand *)
  check_argv "opengrep ci, no other args"
    ~expected:[| "opengrep"; "ci"; "--experimental" |]
    (CLI.with_experimental_flag [| "opengrep"; "ci" |])

let test_unknown_first_arg () =
  check_argv "first arg is not a known subcommand"
    ~expected:[| "opengrep"; "--experimental"; "myproject" |]
    (CLI.with_experimental_flag [| "opengrep"; "myproject" |])

let test_multiple_flags () =
  check_argv "multiple flags: inserted after the subcommand"
    ~expected:[| "opengrep"; "scan"; "--experimental"; "--verbose"; "--json" |]
    (CLI.with_experimental_flag [| "opengrep"; "scan"; "--verbose"; "--json" |])

let test_flag_before_subcommand () =
  check_argv "flag before the subcommand"
    ~expected:[| "opengrep"; "--experimental"; "--json"; "scan" |]
    (CLI.with_experimental_flag [| "opengrep"; "--json"; "scan" |])

let test_flag_after_subcommand () =
  check_argv "flag after the subcommand"
    ~expected:[| "opengrep"; "scan"; "--experimental"; "--json" |]
    (CLI.with_experimental_flag [| "opengrep"; "scan"; "--json" |])

let test_argv0_only () =
  check_argv "only argv[0], no subcommand"
    ~expected:[| "opengrep"; "--experimental" |]
    (CLI.with_experimental_flag [| "opengrep" |])

let tests =
  [
    t "with_experimental_flag: opengrep scan --help" test_scan_help;
    t "with_experimental_flag: opengrep -c rules libs" test_dash_c;
    t "with_experimental_flag: bare subcommand" test_bare_subcommand;
    t "with_experimental_flag: unknown first arg" test_unknown_first_arg;
    t "with_experimental_flag: multiple flags" test_multiple_flags;
    t "with_experimental_flag: flag before subcommand"
      test_flag_before_subcommand;
    t "with_experimental_flag: flag after subcommand" test_flag_after_subcommand;
    t "with_experimental_flag: argv[0] only" test_argv0_only;
  ]
