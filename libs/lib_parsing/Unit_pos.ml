(*
   Unit tests for the line and column converter of a file
*)

let t = Testo.create

let with_converters (contents : string)
    (f : Pos.bytepos_linecol_converters -> unit) : unit =
  UTmp.with_temp_file ~contents (fun (file : Fpath.t) ->
      f (Pos.full_converters_large file))

let linecol : (int * int) Alcotest.testable = Alcotest.(pair int int)

let check_both_ways (converters : Pos.bytepos_linecol_converters)
    (bytepos : int) ((line, col) : int * int) : unit =
  Alcotest.check linecol
    (Printf.sprintf "byte %d" bytepos)
    (line, col)
    (converters.bytepos_to_linecol_fun bytepos);
  Alcotest.(check int)
    (Printf.sprintf "line %d, column %d" line col)
    bytepos
    (converters.linecol_to_bytepos_fun (line, col))

let test_no_final_newline () : unit =
  with_converters "a\nhi" (fun (converters : Pos.bytepos_linecol_converters) ->
      check_both_ways converters 2 (2, 0);
      check_both_ways converters 3 (2, 1);
      check_both_ways converters 4 (2, 2))

let test_after_last_line () : unit =
  with_converters "a\nhi\n"
    (fun (converters : Pos.bytepos_linecol_converters) ->
      check_both_ways converters 4 (2, 2);
      Alcotest.check linecol "byte 5" (2, 0)
        (converters.bytepos_to_linecol_fun 5);
      Alcotest.check linecol "byte 100" (2, 0)
        (converters.bytepos_to_linecol_fun 100))

let test_crlf () : unit =
  with_converters "a\r\nb\r\n"
    (fun (converters : Pos.bytepos_linecol_converters) ->
      check_both_ways converters 1 (1, 1);
      check_both_ways converters 2 (1, 2);
      check_both_ways converters 3 (2, 0);
      check_both_ways converters 5 (2, 2))

let test_past_end_of_line () : unit =
  with_converters "a\nhi\n"
    (fun (converters : Pos.bytepos_linecol_converters) ->
      Alcotest.check_raises "line 1, column 5" Not_found (fun () ->
          ignore (converters.linecol_to_bytepos_fun (1, 5)));
      Alcotest.check_raises "line 3, column 0" Not_found (fun () ->
          ignore (converters.linecol_to_bytepos_fun (3, 0))))

let test_same_as_string () : unit =
  let contents = "let x = 1\n\n  foo (x)\r\nbar\n" in
  let of_string = Pos.full_converters_str contents in
  with_converters contents (fun (of_file : Pos.bytepos_linecol_converters) ->
      List.iter
        (fun (bytepos : int) ->
          Alcotest.check linecol
            (Printf.sprintf "byte %d" bytepos)
            (of_string.bytepos_to_linecol_fun bytepos)
            (of_file.bytepos_to_linecol_fun bytepos))
        (List.init (String.length contents) Fun.id))

let tests =
  Testo.categorize "Pos"
    [
      t "a file without a final newline" test_no_final_newline;
      t "a position after the last line" test_after_last_line;
      t "a carriage return belongs to its line" test_crlf;
      t "a column past the end of its line" test_past_end_of_line;
      t "a file and a string of the same contents" test_same_as_string;
    ]
