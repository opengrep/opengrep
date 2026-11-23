(*
   Unit tests for our semgrepignore implementation
*)

open Printf
open Testutil_files (* file/dir/symlink *)
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

(*
   In these tests, the file hierarchy must contain the
   .semgrepignore files but the target files are not
   needed. Tests that check for the detection of target files are
   in Unit_find_targets.ml.

   Similar to Unit_gitignore.test_filter, but using Semgrepignore.create
   and the includes/excludes extra parameters.
*)
let test_filter ?excludes:cli_patterns (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter =
        Semgrepignore.create ?cli_patterns ~default_semgrepignore_patterns:Empty
          ~exclusion_mechanism:
            { use_semgrepignore_files = true }
          ~project_root:root () ~builtin_patterns_root:Ppath.root
      in
      let error = ref false in
      selection
      |> List.iter (fun (path, should_be_selected) ->
             let path = Ppath.of_string_for_tests path in
             let status, selection_events =
               Gitignore_filter.select filter path
             in
             printf "Selection events for ppath %s:\n"
               (Ppath.to_string_for_tests path);
             print_string (Gitignore.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path)
               | Ignored ->
                   printf "[FAIL] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true
               | Ignored ->
                   printf "[OK] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path));
      if !error then Alcotest.fail "there were some unexpected results")

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

(*
   Test helper for custom semgrepignore filename
*)
let test_filter_custom_filename ~semgrepignore_filename (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files (custom filename: %s) ---\n" semgrepignore_filename;
      let filter =
        Semgrepignore.create ~semgrepignore_filename ~default_semgrepignore_patterns:Empty
          ~exclusion_mechanism:
            { use_semgrepignore_files = true }
          ~project_root:root () ~builtin_patterns_root:Ppath.root
      in
      let error = ref false in
      selection
      |> List.iter (fun (path, should_be_selected) ->
             let path = Ppath.of_string_for_tests path in
             let status, selection_events =
               Gitignore_filter.select filter path
             in
             printf "Selection events for ppath %s:\n"
               (Ppath.to_string_for_tests path);
             print_string (Gitignore.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path)
               | Ignored ->
                   printf "[FAIL] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true
               | Ignored ->
                   printf "[OK] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path));
      if !error then Alcotest.fail "there were some unexpected results")

let tests =
  Testo.categorize "Semgrepignore"
    [
      t "gitignore is not read"
        (test_filter
           [ File (".gitignore", "*.c"); file "hello.c" ]
           [ ("/hello.c", true); ("/generated.c", true) ]);
      t "semgrepignore negation re-includes a file"
        (test_filter
           [
             File (".semgrepignore", "*.c\n!hello.*");
             file "hello.c";
             file "bye.c";
           ]
           [ ("/hello.c", true); ("/bye.c", false) ]);
      t "semgrepignore escaped trailing space"
        (test_filter
           [ File (".semgrepignore", "a.js\\ \n"); file "a.js "; file "a.js" ]
           [ ("/a.js ", false); ("/a.js", true) ]);
      t "semgrepignore trailing spaces of the last line"
        (test_filter
           [ File (".semgrepignore", "a.js  "); file "a.js"; file "b.js" ]
           [ ("/a.js", false); ("/b.js", true) ]);
      t "semgrepignore line that is not valid UTF-8"
        (test_filter
           [ File (".semgrepignore", "caf\xE9.js\nb.js\n"); file "c.js" ]
           [ ("/caf\xE9.js", false); ("/b.js", false); ("/c.js", true) ]);
      t "semgrepignore with a UTF-8 byte order mark"
        (test_filter
           [ File (".semgrepignore", "\xEF\xBB\xBFa.js\nb.js\n"); file "c.js" ]
           [ ("/a.js", false); ("/b.js", false); ("/c.js", true) ]);
      t "semgrepignore with Windows line endings"
        (test_filter
           [ File (".semgrepignore", "a.js\r\nb.js \r\nc.js\r"); file "d.js" ]
           [
             ("/a.js", false); ("/b.js", false); ("/c.js", false); ("/d.js", true);
           ]);
      t "semgrepignore slash in a bracket expression anchors the pattern"
        (test_filter
           [ File (".semgrepignore", "[a/]b.js\n") ]
           [ ("/ab.js", false); ("/x/ab.js", true) ]);
      t "semgrepignore escaped slash anchors the pattern"
        (test_filter
           [ File (".semgrepignore", "d\\/b.js\n") ]
           [ ("/d/b.js", false); ("/x/d/b.js", true) ]);
      t "semgrepignore trailing '**/' ignores the folders inside"
        (test_filter
           [ File (".semgrepignore", "x/**/\n") ]
           [ ("/x/a.js", true); ("/x/d/b.js", false) ]);
      t "semgrepignore pattern that is empty without its trailing slash"
        (test_filter
           [ File (".semgrepignore", "/\n") ]
           [ ("/a.js", true); ("/x/a.js", true) ]);
      t "semgrepignore pattern that is not a valid glob matches nothing"
        (test_filter
           [ File (".semgrepignore", "[abc\nc.js\n"); file "[abc"; file "c.js" ]
           [ ("/[abc", true); ("/c.js", false) ]);
      t "semgrepignore pattern with a trailing backslash matches nothing"
        (test_filter
           [ File (".semgrepignore", "a.js\\\n"); file "a.js" ]
           [ ("/a.js", true) ]);
      t "semgrepignore alone"
        (test_filter
           [ File (".semgrepignore", "hello.*"); file "hello.c"; file "bye.c" ]
           [ ("/hello.c", false); ("/bye.c", true) ]);
      t "legacy semgrepignore with :include"
        (test_filter
           [
             File
               ( ".semgrepignore",
                 "a\n  :include   subdir/extra-semgrepignore \n" );
             file "a";
             file "b";
             file "c";
             dir "subdir"
               [
                 (* exclude only the 'b' file at the root, not the one in
                    this folder *)
                 File ("extra-semgrepignore", "/b\n");
                 file "a";
                 file "b";
               ];
           ]
           [
             ("/a", false);
             ("/b", false);
             ("/c", true);
             ("/subdir/a", false);
             ("/subdir/b", true);
           ]);
      (* A path that is not valid UTF-8 cannot be created on every file
         system, so the warning of the directive shows that the line is
         read as one. *)
      t "legacy semgrepignore with :include of a path that is not valid UTF-8"
        (fun () ->
          let (), (output : string) =
            Testo.with_capture stderr
              (test_filter
                 [ File (".semgrepignore", ":include caf\xE9\n"); file "a" ]
                 [ ("/a", true) ])
          in
          Alcotest.(check bool)
            "the line is an include directive" true
            (String_.contains ~term:"Deprecated include directive" output));
      (* a warning gives the file and the line of its pattern, in the
         included file and after the ':include' line *)
      t "legacy semgrepignore with :include, the lines of the warnings"
        (fun () ->
          let (), (output : string) =
            Testo.with_capture stderr
              (test_filter
                 [
                   File
                     (".semgrepignore", "# c\n:include extra.ignore\nb.js\n[abc\n");
                   File ("extra.ignore", "x.js\n[bad\n");
                 ]
                 [ ("/x.js", false); ("/b.js", false); ("/a.js", true) ])
          in
          Alcotest.(check bool)
            "the line of the included file" true
            (String_.contains ~term:"extra.ignore, line 2: '[bad'" output);
          Alcotest.(check bool)
            "the line after the ':include' line" true
            (String_.contains ~term:".semgrepignore, line 4: '[abc'" output));
      (* the line 'x.js' in UTF-16, with its byte order mark *)
      t "legacy semgrepignore with :include of a file in UTF-16" (fun () ->
          let (), (output : string) =
            Testo.with_capture stderr
              (test_filter
                 [
                   File (".semgrepignore", ":include extra.ignore\n");
                   File ("extra.ignore", "\xFF\xFEx\000.\000j\000s\000\n\000");
                 ]
                 [ ("/x.js", true) ])
          in
          Alcotest.(check bool)
            "the included file is left out with a warning" true
            (String_.contains ~term:"encoded in UTF-16; save it as UTF-8" output));
      t "nested semgrepignore, gitignore is not read"
        (test_filter
           [
             File (".gitignore", "a");
             dir "dir"
               [ File (".semgrepignore", "b"); file "a"; file "b"; file "c" ];
             file "a";
             file "b";
           ]
           [
             ("/a", true);
             ("/b", true);
             ("/dir/a", true);
             ("/dir/b", false);
             ("/dir/c", true);
           ]);
      t "excludes"
        (test_filter ~excludes:[ "*.ml" ] []
           [
             ("/a.ml", false);
             ("/a.c", true);
             ("/b/a.ml", false);
             ("/b/a.c", true);
           ]);
      t "excludes with a slash anchor at the project root"
        (test_filter ~excludes:[ "/vendor"; "lib/gen"; "**/deep/gen" ] []
           [
             ("/vendor/a.c", false);
             ("/b/vendor/a.c", true);
             ("/lib/gen/a.c", false);
             ("/b/lib/gen/a.c", true);
             ("/deep/gen/a.c", false);
             ("/b/deep/gen/a.c", false);
           ]);
      (* Tests for custom semgrepignore filename feature *)
      t "custom semgrepignore filename"
        (test_filter_custom_filename ~semgrepignore_filename:"custom.ignore"
           [
             File ("custom.ignore", "*.temp");
             file "test.temp";
             file "test.c";
           ]
           [ ("/test.temp", false); ("/test.c", true) ]);
      t "custom semgrepignore filename - different name"
        (test_filter_custom_filename ~semgrepignore_filename:"myignore"
           [
             File ("myignore", "build/*");
             dir "build" [ file "output.txt" ];
             file "source.c";
           ]
           [ ("/build/output.txt", false); ("/source.c", true) ]);
      t "custom semgrepignore filename - file not found uses defaults"
        (test_filter_custom_filename ~semgrepignore_filename:"nonexistent.ignore"
           [
             file "test.c";
             dir "build" [ file "temp.txt" ];
           ]
           [ ("/test.c", true); ("/build/temp.txt", true) ]);
      t "custom semgrepignore filename rejects paths with slashes"
        (fun () ->
           F.with_tempdir ~chdir:true (fun root ->
             try
               let _filter =
                 Semgrepignore.create ~semgrepignore_filename:"config/ignore.txt"
                   ~default_semgrepignore_patterns:Empty
                   ~exclusion_mechanism:
                     { use_semgrepignore_files = true }
                   ~project_root:root ()
               in
               Alcotest.fail "Expected Invalid_argument exception for path with slash"
             with
             | Invalid_argument msg when String.contains msg '/' ->
                 printf "[OK] Correctly rejected filename with slash: %s\n" msg
             | exn ->
                 Alcotest.fail (Printf.sprintf "Unexpected exception: %s" (Printexc.to_string exn))));
    ]
