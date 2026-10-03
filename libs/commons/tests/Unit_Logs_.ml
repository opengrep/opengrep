let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Runs [f] under the reporter of Logs_ at level [at], its messages
   redirected to [sink] if one is given, and restores the logging state of
   the test runner afterwards. *)
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
      Logs_.restore_stderr (fun () -> ());
      Logs.set_level ~all:false level;
      src_levels
      |> List.iter (fun ((src : Logs.src), (level : Logs.level option)) ->
             Logs.Src.set_level src level);
      Logs.set_reporter reporter)
    (fun () ->
      Logs_.setup_basic ~level:(Some at) ();
      Option.iter Logs_.redirect_stderr sink;
      f ())

let lock_is_free () : bool =
  if Mutex.try_lock Logs_.logs_mutex then (
    Mutex.unlock Logs_.logs_mutex;
    true)
  else false

(* A sink that stores the texts it receives, and a function that returns
   them, oldest first. *)
let collecting_sink () : (string -> unit) * (unit -> string list) =
  let got : string list ref = ref [] in
  ((fun (text : string) -> got := text :: !got), fun () -> List.rev !got)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* The lock is released once however the message ends: a second release
   would fail, and its error would replace this one. *)
let test_failing_printer_raises_its_exception () =
  Alcotest.check_raises "the printer's own exception" (Failure "printer")
    (fun () ->
      with_logging (fun () ->
          Logs.app (fun m -> m "%a" (fun _ () -> failwith "printer") ())));
  Alcotest.(check bool) "the lock is free" true (lock_is_free ())

(* A redirected message is the text stderr would have received, and stderr
   receives nothing. *)
let test_redirected_message_is_the_text () =
  let message () = Logs.app (fun m -> m "a %s message" "redirected") in
  let (), written = Testo.with_capture stderr (fun () -> with_logging message) in
  let sink, got = collecting_sink () in
  let (), on_stderr =
    Testo.with_capture stderr (fun () -> with_logging ~sink message)
  in
  Alcotest.(check (list string)) "the sink has the text" [ written ] (got ());
  Alcotest.(check string) "stderr has nothing" "" on_stderr

(* A debug message that the tag filter drops produces no text and reaches no
   sink. *)
let test_dropped_debug_message_redirects_nothing () =
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

(* What the flush of restore_stderr writes comes before any message logged
   after it. *)
let test_restore_flushes_first () =
  let sink, got = collecting_sink () in
  let (), on_stderr =
    Testo.with_capture stderr (fun () ->
        with_logging ~sink (fun () ->
            Logs.app (fun m -> m "first");
            Logs_.restore_stderr (fun () ->
                got () |> List.iter prerr_string;
                flush stderr);
            Logs.app (fun m -> m "second")))
  in
  Alcotest.(check string) "in the order logged" "first\nsecond\n" on_stderr

(* A message whose printer fails reaches no sink, not even in part. *)
let test_failing_printer_redirects_nothing () =
  let sink, got = collecting_sink () in
  Alcotest.check_raises "the printer's own exception" (Failure "printer")
    (fun () ->
      with_logging ~sink (fun () ->
          Logs.app (fun m ->
              m "half a message%a" (fun _ () -> failwith "printer") ())));
  Alcotest.(check (list string)) "nothing redirected" [] (got ());
  Alcotest.(check bool) "the lock is free" true (lock_is_free ())

let tests =
  Testo.categorize "Logs_"
    [
      t "a failing printer raises its own exception"
        test_failing_printer_raises_its_exception;
      t "a redirected message is the text stderr would have received"
        test_redirected_message_is_the_text;
      t "a debug message that the tag filter drops redirects nothing"
        test_dropped_debug_message_redirects_nothing;
      t "restore_stderr writes its flush before a later message"
        test_restore_flushes_first;
      t "a message whose printer fails redirects nothing"
        test_failing_printer_redirects_nothing;
    ]
