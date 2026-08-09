(*
   Tests for our Lazy_with_restart module.
*)

let t = Testo.create

exception Boom

(* The whole point of the module: forcing raises the first time, then the next
   force restarts the computation and succeeds; [is_val] stays false as long as
   no force has succeeded. *)
let test_restart_on_exception () =
  let calls = ref 0 in
  let lz =
    Lazy_with_restart.from_fun (fun () ->
        incr calls;
        if !calls = 1 then raise Boom;
        !calls)
  in
  Alcotest.(check bool) "not forced initially" false (Lazy_with_restart.is_val lz);
  (try
     ignore (Lazy_with_restart.force lz);
     Alcotest.fail "expected the first force to raise"
   with Boom -> ());
  Alcotest.(check bool) "not forced after a failed force" false
    (Lazy_with_restart.is_val lz);
  Alcotest.(check int) "force restarts and succeeds" 2 (Lazy_with_restart.force lz);
  Alcotest.(check bool) "forced after success" true (Lazy_with_restart.is_val lz)

(* A successful result is memoized: the thunk runs exactly once. *)
let test_memoize_on_success () =
  let calls = ref 0 in
  let lz =
    Lazy_with_restart.from_fun (fun () ->
        incr calls;
        42)
  in
  Alcotest.(check int) "first force" 42 (Lazy_with_restart.force lz);
  Alcotest.(check int) "second force (cached)" 42 (Lazy_with_restart.force lz);
  Alcotest.(check int) "thunk ran once" 1 !calls

(* [from_val] is an already-forced suspension. *)
let test_from_val () =
  let lz = Lazy_with_restart.from_val "x" in
  Alcotest.(check bool) "from_val implies is_val" true (Lazy_with_restart.is_val lz);
  Alcotest.(check string) "from_val value" "x" (Lazy_with_restart.force lz)

let tests =
  Testo.categorize "Lazy_with_restart"
    [
      t "restart on exception" test_restart_on_exception;
      t "memoize on success" test_memoize_on_success;
      t "from_val" test_from_val;
    ]
