type make_fixed_lines_case = {
  test_name : string;
  contents : string;
  (* start, end, replacement *)
  fixes : (int * int * string) list;
  expected : string list option list;
}

let make_fixed_lines_cases =
  [
    {
      test_name = "Ordinary case with one overlapping fix";
      contents = "foo\nbar\n";
      fixes = [ (1, 5, "1\n2"); (1, 2, "1") ];
      expected = [ Some [ "f1"; "2ar" ]; None ];
    };
    {
      test_name = "Ordinary case with no overlapping fixes";
      contents = "foo\nbar\n";
      fixes = [ (1, 2, "1"); (5, 6, "2") ];
      expected = [ Some [ "f1o" ]; Some [ "b2r" ] ];
    };
    {
      test_name = "Two lines entirely deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (0, 7, "") ];
      expected = [ None ];
    };
    {
      test_name = "One line entirely deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (4, 7, "") ];
      expected = [ None ];
    };
    {
      test_name = "One line partially deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (5, 7, "") ];
      expected = [ Some [ "b" ] ];
    };
    {
      test_name = "Two lines partially deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (1, 7, "") ];
      expected = [ Some [ "f" ] ];
    };
    {
      test_name = "Second fix contained entirely in first";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (2, 3, "b") ];
      expected = [ Some [ "0a456789" ]; None ];
    };
    {
      test_name = "First fix contained entirely in second";
      contents = "0123456789";
      fixes = [ (2, 3, "a"); (1, 4, "b") ];
      expected = [ Some [ "01a3456789" ]; None ];
    };
    {
      test_name = "End of first fix overlaps with start of second";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (3, 6, "b") ];
      expected = [ Some [ "0a456789" ]; None ];
    };
    {
      test_name = "End of second fix overlaps with start of first";
      contents = "0123456789";
      fixes = [ (3, 6, "b"); (1, 4, "a") ];
      expected = [ Some [ "012b6789" ]; None ];
    };
    {
      test_name = "Start of second fix is adjacent to end of first";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (4, 6, "b") ];
      expected = [ Some [ "0a456789" ]; Some [ "0123b6789" ] ];
    };
    {
      test_name = "Start of first fix is adjacent to end of second";
      contents = "0123456789";
      fixes = [ (4, 6, "b"); (1, 4, "a") ];
      expected = [ Some [ "0123b6789" ]; Some [ "0a456789" ] ];
    };
  ]

let fakepath = Fpath.v (Filename.get_temp_dir_name () ^ "/fake.txt")

let create_make_fixed_lines_test { test_name; contents; fixes; expected } =
  Testo.create test_name (fun () ->
      let edits =
        List_.map
          (fun (start, end_, replacement_text) ->
            Textedit.{ start; end_; replacement_text; path = fakepath })
          fixes
      in
      let env = Fixed_lines.mk_env () in
      let all_fixed_lines : string list option list =
        List_.map (Fixed_lines.make_fixed_lines_of_string env contents) edits
      in
      Alcotest.(check (string |> list |> option |> list))
        "fixed_lines" expected all_fixed_lines)

(* The edits of a symbolic link and of a file with another hard link are not
 * written. *)
let test_apply_edits_skips_links () =
  Testutil_files.with_tempfiles ~chdir:true
    [
      Testutil_files.File ("plain.txt", "foo\n");
      Testutil_files.File ("linked.txt", "foo\n");
      Testutil_files.Hardlink ("other.txt", "linked.txt");
      Testutil_files.File ("pointed_at.txt", "foo\n");
      Testutil_files.Symlink ("sym.txt", "pointed_at.txt");
    ]
    (fun _cwd ->
      let edit (path : string) : Textedit.t =
        { start = 0; end_ = 3; replacement_text = "bar"; path = Fpath.v path }
      in
      let { Textedit.modified_files; skipped_files; conflicting_edits } =
        Textedit.apply_edits ~dryrun:false
          [ edit "plain.txt"; edit "linked.txt"; edit "sym.txt" ]
      in
      Alcotest.(check (list string))
        "modified files" [ "plain.txt" ]
        (List_.map Fpath.to_string modified_files);
      Alcotest.(check (list string))
        "skipped files" [ "linked.txt"; "sym.txt" ]
        (List.sort String.compare (List_.map Fpath.to_string skipped_files));
      Alcotest.(check int) "conflicting edits" 0 (List.length conflicting_edits);
      Alcotest.(check string)
        "plain file" "bar\n"
        (UFile.read_file (Fpath.v "plain.txt"));
      Alcotest.(check string)
        "linked file" "foo\n"
        (UFile.read_file (Fpath.v "linked.txt"));
      Alcotest.(check string)
        "pointed at file" "foo\n"
        (UFile.read_file (Fpath.v "pointed_at.txt")))

let tests =
  Testo.categorize "autofix"
    (Testo.categorize "make_fixed_lines"
       (List_.map create_make_fixed_lines_test make_fixed_lines_cases)
    @ [ Testo.create "apply_edits, aliased files" test_apply_edits_skips_links ])
