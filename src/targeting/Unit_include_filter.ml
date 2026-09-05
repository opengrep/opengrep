(*
   Unit tests for the --include feature of semgrep
   (Include_filter)
*)

open Printf

let t = Testo.create ~category:[ "Include_filter" ]

let test_include patterns ppath_string expected_status =
  let name =
    sprintf "include patterns: %s, path: %s"
      (String.concat " " patterns)
      ppath_string
  in
  let test_func () =
    let filter = Include_filter.create ~project_root:(Fpath.v ".") patterns in
    let ppath = Ppath.of_string_for_tests ppath_string in
    let status, _selection_events = Include_filter.select filter ppath in
    Alcotest.(check string)
      __LOC__
      (Gitignore.show_status expected_status)
      (Gitignore.show_status status)
  in
  t name test_func

let tests =
  [
    test_include [ "a" ] "/a" Not_ignored;
    test_include [ "/a" ] "/a" Not_ignored;
    test_include [ "/a" ] "/a/b" Not_ignored;
    (* a pattern of the command line matches anywhere in the path, so a
       leading slash does not anchor it at the project root *)
    test_include [ "/b" ] "/a/b" Not_ignored;
    test_include [ "b" ] "/a/b/c" Not_ignored;
    test_include [ "c" ] "/a/b/c" Not_ignored;
    test_include [ "included.*" ]
      "/targets/exclude_include/included/included.js" Not_ignored;
    (* a pattern containing a slash matches anywhere too *)
    test_include [ "lib/b.py" ] "/src/lib/b.py" Not_ignored;
    test_include [ "lib/b.py" ] "/src/lib/c.py" Ignored;
    test_include [ "src/*" ] "/app/src/d.py" Not_ignored;
    test_include [ "src/*" ] "/app/other/d.py" Ignored;
  ]
