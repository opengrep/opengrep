(*
   Entrypoint to run the unit tests from the command line.
*)

(* The matching debug output is enabled for the duration of each test only,
   so that it does not leak into the other tests of the same process. *)
let with_match_debug (test : Testo.t) : Testo.t =
  Testo.update test ~func:(fun () ->
      let saved = !Spacegrep.Match.debug in
      Spacegrep.Match.debug := true;
      Fun.protect ~finally:(fun () -> Spacegrep.Match.debug := saved) test.func)

let tests () : Testo.t list =
  Testo.categorize_suites "spacegrep"
    [ File_type.test; Parser.test; Matcher.test; Src_file.test; Comment.test ]
  |> List.map with_match_debug
