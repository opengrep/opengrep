(*
   Unit tests for Regex
*)

let t = Testo.create

let test_match_limit_ok () =
  let rex = Pcre2_.regexp "(a+)+$" in
  match Pcre2_.pmatch ~rex "aaaaaaaaaaaaaaaaa!" with
  | Ok _ -> ()
  | Error Pcre2.MatchLimit ->
      Alcotest.fail "should not have failed with error MatchLimit"
  | Error _ -> Alcotest.fail "unexpected error"

(* PCRE2 charges fewer match-limit units than PCRE1 for the same pattern:
   19 a's is the smallest input where "(a+)+$" exceeds the 1_000_000 limit
   set in Pcre2_.regexp (18 suffice under PCRE1). *)
let test_match_limit_fail () =
  let rex = Pcre2_.regexp "(a+)+$" in
  match Pcre2_.pmatch ~rex "aaaaaaaaaaaaaaaaaaa!" with
  | Ok _ -> Alcotest.fail "should have failed with error MatchLimit"
  | Error Pcre2.MatchLimit -> ()
  | Error _ -> Alcotest.fail "unexpected error"

let test_register_exception_printer () =
  (* This is a little dirty since we can't undo it. *)
  Pcre2_.register_exception_printer ();

  let msg =
    try
      ignore (Pcre2_.regexp "???");
      Alcotest.fail "should have failed to compile the regexp"
    with
    | e ->
      let err =  Printexc.to_string e
      in
      String.sub err 0 11
  in
  Alcotest.(check string)
    "equal"
    "Pcre2.Error"
    msg

let tests =
  Testo.categorize "pcre2 settings"
    [
      t "match limit ok" test_match_limit_ok;
      t "match limit fail" test_match_limit_fail;
      t "exception printer" test_register_exception_printer;
    ]
