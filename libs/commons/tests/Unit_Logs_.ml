(*
   Unit tests for the lock of our Logs_ module and for diverting its stderr
   reporter.
*)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The reporter of Logs_ at the Info level, its messages diverted to [sink]
   if one is given, and the logging state of the test runner put back
   afterwards. *)
let with_logging ?(at = Logs.Info) ?(sink : (string -> unit) option)
    (f : unit -> 'a) : 'a =
  let level = Logs.level () in
  let src_levels =
    Logs.Src.list ()
    |> List_.map (fun (src : Logs.src) -> (src, Logs.Src.level src))
  in
  let reporter = Logs.reporter () in
  Common.protect
    ~finally:(fun () ->
      Logs_.undivert_stderr (fun () -> ());
      Logs.set_level ~all:false level;
      src_levels
      |> List.iter (fun ((src : Logs.src), (level : Logs.level option)) ->
             Logs.Src.set_level src level);
      Logs.set_reporter reporter)
    (fun () ->
      Logs_.setup_basic ~level:(Some at) ();
      Option.iter Logs_.divert_stderr sink;
      f ())

(* the lock is taken by nobody, this thread included *)
let lock_is_free () : bool =
  if Mutex.try_lock Logs_.logs_mutex then (
    Mutex.unlock Logs_.logs_mutex;
    true)
  else false

(* A sink that keeps what it is handed, and what it holds, oldest first. *)
let collecting_sink () : (string -> unit) * (unit -> string list) =
  let got : string list ref = ref [] in
  ((fun (text : string) -> got := text :: !got), fun () -> List.rev !got)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* The lock is released once however the message ends: a second release
   would fail, and its error would take the place of this one. *)
let test_failing_printer_keeps_its_exception () =
  Alcotest.check_raises "the printer's own exception" (Failure "printer")
    (fun () ->
      with_logging (fun () ->
          Logs.app (fun m -> m "%a" (fun _ () -> failwith "printer") ())));
  Alcotest.(check bool) "the lock is free" true (lock_is_free ())

(* A diverted message is the text stderr would have got, and stderr gets
   nothing. *)
let test_diverted_message_is_the_text () =
  let message () = Logs.app (fun m -> m "a %s message" "diverted") in
  let (), written = Testo.with_capture stderr (fun () -> with_logging message) in
  let sink, got = collecting_sink () in
  let (), on_stderr =
    Testo.with_capture stderr (fun () -> with_logging ~sink message)
  in
  Alcotest.(check (list string)) "the sink has the text" [ written ] (got ());
  Alcotest.(check string) "stderr has nothing" "" on_stderr

(* A debug message the tag filter drops produces no text, and so hands
   nothing over. *)
let test_dropped_debug_message_diverts_nothing () =
  let sink, got = collecting_sink () in
  with_logging ~at:Logs.Debug ~sink (fun () ->
      (* setup_basic selects no tag, so this one is dropped *)
      Logs.debug (fun m -> m "dropped");
      Logs.info (fun m -> m "written"));
  match got () with
  | [ text ] ->
      Alcotest.(check bool)
        "the written message" true
        (String_.contains ~term:"written" text)
  | texts -> Alcotest.failf "one message expected, got %d" (List.length texts)

(* What the flush of undivert_stderr writes comes before any message logged
   after it. *)
let test_undivert_flushes_first () =
  let sink, got = collecting_sink () in
  let (), on_stderr =
    Testo.with_capture stderr (fun () ->
        with_logging ~sink (fun () ->
            Logs.app (fun m -> m "first");
            Logs_.undivert_stderr (fun () ->
                got () |> List.iter prerr_string;
                flush stderr);
            Logs.app (fun m -> m "second")))
  in
  Alcotest.(check string) "in the order logged" "first\nsecond\n" on_stderr

(* A message whose printer fails hands nothing over, not half a message. *)
let test_failing_printer_diverts_nothing () =
  let sink, got = collecting_sink () in
  Alcotest.check_raises "the printer's own exception" (Failure "printer")
    (fun () ->
      with_logging ~sink (fun () ->
          Logs.app (fun m ->
              m "half a message%a" (fun _ () -> failwith "printer") ())));
  Alcotest.(check (list string)) "nothing handed over" [] (got ());
  Alcotest.(check bool) "the lock is free" true (lock_is_free ())

let tests =
  Testo.categorize "Logs_"
    [
      t "a failing printer keeps its exception"
        test_failing_printer_keeps_its_exception;
      t "a diverted message is the text stderr would have got"
        test_diverted_message_is_the_text;
      t "a debug message the tag filter drops diverts nothing"
        test_dropped_debug_message_diverts_nothing;
      t "undivert writes its flush before a later message"
        test_undivert_flushes_first;
      t "a message whose printer fails diverts nothing"
        test_failing_printer_diverts_nothing;
    ]
