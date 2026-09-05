let t = Testo.create

(* Every maximal invalid UTF-8 subpart becomes one U+FFFD, as Python does
   when a file is read with errors="replace". *)
let test_sanitize () =
  let check (expected : string) (str : string) : unit =
    Alcotest.(check string) __LOC__ expected (Utf8.sanitize str)
  in
  let rep : string = "\xef\xbf\xbd" in
  check "" "";
  check "ascii" "ascii";
  (* already valid UTF-8 is returned unchanged, whatever its width *)
  check "caf\xc3\xa9" "caf\xc3\xa9";
  check "\xe2\x82\xac" "\xe2\x82\xac";
  check "\xf0\x9f\x92\xa9" "\xf0\x9f\x92\xa9";
  (* bytes that cannot start a sequence *)
  check rep "\xff";
  check (rep ^ rep) "\xff\xfe";
  check ("caf" ^ rep) "caf\xe9";
  (* a sequence cut short *)
  check rep "\xe2\x82";
  check (rep ^ "a") "\xe2\x82a"

let test_length_and_offsets () =
  Alcotest.(check int) "characters" 4 (Utf8.length "caf\xc3\xa9");
  Alcotest.(check (array int))
    "offsets" [| 0; 1; 2; 3; 5 |]
    (Utf8.code_point_offsets "caf\xc3\xa9");
  (* a malformed subpart is one character *)
  Alcotest.(check int) "malformed" 2 (Utf8.length "\xe2\x82a")

let tests =
  Testo.categorize "Utf8"
    [ t "sanitize" test_sanitize; t "length and offsets" test_length_and_offsets ]
