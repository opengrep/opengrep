(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A line at the bottom of the terminal saying what the scan is doing, with a
 * spinner and, once the scan reaches its targets, a progress bar.
 *
 * It is drawn by a thread of its own, which is what lets it animate while
 * the scan works, and while the bar is up that thread is the only writer to
 * the terminal:
 *
 *  - a log message is not written by whoever logs it. Logs_ hands it over
 *    as text (Logs_.divert_stderr), it is queued, and the thread writes it
 *    between two frames: the bar erased, the messages, the bar again. A
 *    message is never cut in half by a redraw, and is at most a frame late
 *    while the terminal takes what is written; one that has stopped
 *    reading holds the messages up until it reads again (see [draw]).
 *  - the thread writes to a copy of stderr taken when the bar starts, so a
 *    capture of stderr (UCmd, around a git command) sees neither a frame
 *    nor one of our log lines.
 *  - the findings must not be printed over it. The caller stops the bar
 *    before the report; under --incremental-output, where findings reach
 *    stdout while the scan runs, there is no bar at all.
 *
 * Output that does not go through Logs -- the OCaml runtime's, a C
 * library's -- is not covered, and lands on the bar's line.
 *)

type phase =
  (* fetching a large ruleset over the network takes long enough that
     without this the app looks wedged *)
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  (* the second engine run of a --baseline-commit scan, which re-scans the
     changed paths at the baseline commit only to work out which findings
     are new. It reports no count of its own: it is a different pass over
     the same files, and a counter restarting from zero would read as the
     scan having gone backwards. *)
  | Comparing_with_baseline
  (* Targets and interfile rules are counted as one, though a rule is by far
     the longer unit: they run together in one pool from the start, so any
     split leaves whichever half is not on show looking stalled. One count
     advances unevenly, but the unevenness is there between one interfile
     rule and the next anyway. *)
  | Scanning of { total : int; completed : int Atomic.t }

type t = {
  (* replaced whole by [set_phase], so that the domains finishing work read
     either the old phase or the new one *)
  phase : phase Atomic.t;
  stop : bool Atomic.t;
  (* a Ctrl-Z, which the loop serves between two frames *)
  suspend_requested : bool Atomic.t;
  (* Whether the frames are drawn: false once the bar is put away for good,
     by a Ctrl-Z or a signal ending the scan. Messages are still written
     after a Ctrl-Z, with no bar under them. *)
  drawing : bool Atomic.t;
  (* the log messages waiting for the next frame, each whole lines *)
  messages : string Saturn.Single_consumer_queue.t;
  (* $COLUMNS, if set: see [columns_from_env] *)
  columns_from_env : int option;
  (* held around every write to [terminal], so that a signal handler can let
     one already under way finish *)
  write_mutex : Mutex.t;
  (* A copy of stderr as it was when the bar started, which reaches the
     terminal even while stderr is redirected into a capture. Never closed:
     a signal handler already running on another thread could otherwise
     write to it after [finish], by then perhaps another file's descriptor.
     It is one descriptor per run, and not inherited by commands. *)
  terminal : Unix.file_descr;
  mutable thread : Thread.t;
  (* the behaviours of the signals the bar handles while it is up, from
     before it started; put back by [finish] *)
  mutable previous_signals : (int * Sys.signal_behavior) list;
}

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
let erase_line_str = "\r\027[2K"
let hide_cursor_str = "\027[?25l"
let show_cursor_str = "\027[?25h"
let bold_str = "\027[1m"
let faint_str = "\027[2m"

(* "normal intensity": closes bold and faint alike *)
let normal_str = "\027[22m"

(* A bar only means something once there are enough targets for it to move;
 * below that it would jump from empty to full and read as a flicker. *)
let min_targets_for_bar = 200

(* Dots read as a row of beads rather than as a filled rule, so the track
 * shows where it ends without brackets around it, and carries its meaning
 * with fewer cells than a solid bar needs. *)
let bar_width = 30
let filled_dot = "●"
let empty_dot = "·"

(* The spinner and the space after it, which render_frame puts before
   everything below. *)
let spinner_cols = 2

(* Narrower than this and nothing the bar can say is worth saying, so a
   width below it is taken for a terminal that does not know its own size
   rather than for a very small one. A pty whose size was never set reports
   zero, and COLUMNS=0 turns up in CI and under script(1). *)
let min_sensible_columns = 10

(* How long [finish] gives a terminal behind on its reading to take the
   sequence that puts the cursor back. Long enough for a pane that is
   catching up, short enough not to read as a hang. *)
let cursor_restore_wait = 2.0

(* The width of the terminal, asked of whichever descriptor will answer.
   The bar draws on stderr, but nothing we depend on will report the size
   of a descriptor we name: Terminal_size asks about stdout, ANSITerminal
   about stdin. With stdout redirected -- > out, | less -- the first has
   nothing to say, and the bar would then draw at its full width and wrap
   a narrower window, which the one-row erase cannot clean up. Stdin is
   still the terminal in that case, and is the same terminal as stderr in
   any arrangement worth serving, so it answers for it.

   The real width, not Findings_layout.text_width, which floors at 40 and so
   would claim room a narrow terminal does not have. Read for every frame,
   so that resizing a window is picked up without watching for SIGWINCH;
   $COLUMNS, [from_env], wins when it is set, and is read once, by
   [columns_from_env]. [None] means "no idea", which costs the dotted bar
   but not the counts; see [counter]. *)
let terminal_columns ~(from_env : int option) : int option =
  (* raises, rather than returning an option, when stdin is not a terminal
     and on a platform its stub cannot serve *)
  let from_stdin () : int option =
    match ANSITerminal.size () with
    | width, _height -> Some width
    | exception _ -> None
  in
  let columns =
    match from_env with
    | Some w when w > 0 -> Some w
    | _ -> (
        match Terminal_size.get_columns () with
        | Some _ as w -> w
        | None -> from_stdin ())
  in
  match columns with
  | Some w when w >= min_sensible_columns -> Some w
  | _ -> None

(* $COLUMNS, read when the bar starts rather than for every frame: the
   environment does not change during the run, and the lookup goes through
   Str (Opengrep_env's alias of SEMGREP_ names), which the loop must not
   use. Str keeps the last match in state its domain shares with every
   thread on it, and the main thread's own "search, then read the groups"
   -- Common.(=~) then Common.matched1 -- would find it overwritten. *)
let columns_from_env () : int option =
  Opengrep_env.getenv_opt "COLUMNS"
  |> Option.map String.trim
  |> Fun.flip Option.bind int_of_string_opt

(* $NO_COLOR and --force-color are resolved once into the console's
   highlight setting; the bar follows it as the rest of the report does.
   Only the styling goes: the erase and the spinner are not colour, and a
   reader who turned colour off still wants to see that work is happening. *)
let styling_on () : bool =
  match Console.get_highlight () with
  | Console.On -> true
  | Console.Off -> false

let progress_bar ~(width : int) ~(filled : int) ~(total : int) : string =
  let ratio =
    if total > 0 then float_of_int filled /. float_of_int total else 0.0
  in
  let filled_len =
    min (Float.to_int (Float.min ratio 1.0 *. float_of_int width)) width
  in
  let buf = Buffer.create ((width * 3) + 8) in
  for _ = 1 to filled_len do
    Buffer.add_string buf filled_dot
  done;
  if filled_len < width then begin
    let styled = styling_on () in
    if styled then Buffer.add_string buf faint_str;
    for _ = 1 to width - filled_len do
      Buffer.add_string buf empty_dot
    done;
    if styled then Buffer.add_string buf normal_str
  end;
  Buffer.contents buf

let titled (s : string) : string =
  if styling_on () then bold_str ^ s ^ normal_str else s

(* A line wider than the terminal wraps, and the erase before each frame
   clears one row, so the rows above it would be left behind as the bar
   redrew. Hence three widths: the bar, then the counts alone, then the
   counts clipped -- the numbers are what carry the meaning. *)
let counter ~(title : string) ~(with_bar : bool) ~(done_ : int) ~(total : int)
    ~(columns : int option) : string =
  let pct = if total > 0 then done_ * 100 / total else 0 in
  let numbers = Printf.sprintf "%d/%d (%d%%)" done_ total pct in
  let room_for (cols : int) : bool =
    match columns with
    | None -> true (* no terminal to ask: behave as it always did *)
    | Some available -> cols <= available
  in
  (* The bar is the widest of the three and the one worth giving up when
     nothing will say how wide the terminal is. The counts are short enough
     to risk; a wrapped bar is not, since the row it spills onto outlives
     the one-row erase and stays on the screen. Every ordinary run answers
     through one descriptor or another, so this is the redirected-stdout,
     redirected-stdin case and no other. *)
  let known_room_for (cols : int) : bool =
    Option.is_some columns && room_for cols
  in
  let with_title = spinner_cols + String.length title + 1 in
  if
    with_bar
    && known_room_for (with_title + bar_width + 1 + String.length numbers)
  then
    Printf.sprintf "%s %s %s" (titled title)
      (progress_bar ~width:bar_width ~filled:done_ ~total)
      numbers
  else if room_for (with_title + String.length numbers) then
    Printf.sprintf "%s %s" (titled title) numbers
  else
    match columns with
    | Some available ->
        String_.safe_sub numbers 0
          (min (String.length numbers) (max 0 (available - spinner_cols)))
    | None -> numbers

(* Clipped before the escapes go on, so that a narrow terminal never cuts
   one in half. *)
let label ~(columns : int option) (s : string) : string =
  let s =
    match columns with
    | Some available when String.length s > available - spinner_cols ->
        String_.safe_sub s 0 (max 0 (available - spinner_cols))
    | _ -> s
  in
  titled s

let phase_to_string ~(columns : int option) (phase : phase) : string =
  match phase with
  | Loading_rules -> label ~columns "Loading rules..."
  | Analyzing_targets -> label ~columns "Analyzing targets..."
  | Building_interfile_graph -> label ~columns "Building call graph..."
  | Comparing_with_baseline -> label ~columns "Comparing with baseline..."
  | Scanning { total; completed } ->
      let done_ = Atomic.get completed in
      (* Every unit of work is done, but the engine is still gathering what
         they found, which with many findings takes a while: a bar held at
         100% would read as a hang. *)
      if total > 0 && done_ >= total then label ~columns "Processing results..."
      else if total > 0 then
        counter ~title:"Scanning:"
          ~with_bar:(total >= min_targets_for_bar)
          ~done_ ~total ~columns
      else
        Printf.sprintf "%s %s"
          (label ~columns "Scanning:")
          (String_.unit_str done_ "target")

let render_frame ~(frame_index : int) ~(columns : int option) (phase : phase) :
    string =
  let glyph = spinner.(frame_index mod Array.length spinner) in
  Printf.sprintf "%s%s %s" erase_line_str glyph
    (phase_to_string ~columns phase)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

(* All of [s], or as much as the terminal took before failing. It never
   raises: a terminal that has gone away -- a closed window, a dropped ssh
   session -- makes the write fail, and losing the bar, or a message that
   had nowhere to go, costs nothing next to a scan stopped by it. A signal
   interrupting the write does not cut it short. *)
let write_all (terminal : Unix.file_descr) (s : string) : unit =
  let rec write_from (from : int) : unit =
    if from < String.length s then
      match
        Unix.single_write_substring terminal s from (String.length s - from)
      with
      | written -> write_from (from + written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_from from
  in
  try write_from 0 with
  | Unix.Unix_error _ -> ()

(* [s] if the terminal can take it within [wait] seconds, else nothing.

   A frame is dropped when the terminal cannot take it. A pty whose reader
   has stopped -- Ctrl-S, a paused tmux pane, a stalled ssh link -- fills
   its buffer, and a write then blocks; [finish] would join a thread that
   never wakes, and a decorative line would have stopped the scan. A
   dropped frame costs nothing: the next one redraws the whole line.

   The cursor is put back through here too, with a wait, so that a terminal
   briefly behind on its reading still gets it, while one that has stopped
   reading for good costs a bounded pause and not a scan that never ends. *)
let write_if_ready ?(wait : float = 0.) (terminal : Unix.file_descr)
    (s : string) : unit =
  match Unix.select [] [ terminal ] [] wait with
  | _, _ :: _, _ -> write_all terminal s
  | _, [], _ -> ()
  | exception Unix.Unix_error _ -> ()

(* The messages queued since the last frame, in the order they were
   logged. The loop is the queue's only consumer, then [finish], once the
   loop has stopped. *)
let rec take_messages (bar : t) (taken : string list) : string list =
  match Saturn.Single_consumer_queue.pop_opt bar.messages with
  | Some message -> take_messages bar (message :: taken)
  | None -> List.rev taken

(* The bar off the screen for good: erased, [messages] written in its place,
   and the cursor shown again. Called with [write_mutex] held. *)
let put_away (bar : t) ~(wait : float) (messages : string list) : unit =
  let was_drawing = Atomic.exchange bar.drawing false in
  write_all bar.terminal
    ((if was_drawing then erase_line_str else "") ^ String.concat "" messages);
  if was_drawing then write_if_ready ~wait bar.terminal show_cursor_str

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

(* Ctrl-Z. The signal handler only asks, and the loop answers between two
   frames, when no frame can be half written: it puts the bar away with the
   messages queued until then, and stops the process as the signal would
   have. The scan goes on after fg or bg, its messages still written but
   with no bar under them: in the background the bar would draw over the
   shell, and nothing here can tell the two apart. *)
let serve_suspension (bar : t) : unit =
  if Atomic.exchange bar.suspend_requested false then begin
    Mutex.protect bar.write_mutex (fun () ->
        put_away bar ~wait:0.5 (take_messages bar []));
    Sys.set_signal Sys.sigtstp Sys.Signal_default;
    ignore (Thread.sigmask Unix.SIG_UNBLOCK [ Sys.sigtstp ] : int list);
    Unix.kill (Unix.getpid ()) Sys.sigtstp
  end

(* One frame: the messages queued since the last, then the bar. The
   messages are never dropped: the loop waits for the terminal to take
   them. A terminal that has stopped reading (Ctrl-S, a paused pane) holds
   up the bar, the messages queued behind these, and [finish], which joins
   the loop -- but not the scan, as whoever logs only queues. Writing the
   messages directly would block the scan instead. *)
let draw (bar : t) ~(frame_index : int) : unit =
  let messages = take_messages bar [] in
  let frame =
    if Atomic.get bar.drawing then
      Some
        (render_frame ~frame_index
           ~columns:(terminal_columns ~from_env:bar.columns_from_env)
           (Atomic.get bar.phase))
    else None
  in
  Mutex.protect bar.write_mutex (fun () ->
      (* read again: a signal may have put the bar away meanwhile *)
      let frame = if Atomic.get bar.drawing then frame else None in
      match messages with
      | [] -> Option.iter (write_if_ready bar.terminal) frame
      | _ :: _ ->
          write_all bar.terminal
            (match frame with
            | Some frame ->
                erase_line_str ^ String.concat "" messages ^ frame
            | None -> String.concat "" messages))

let render_loop (bar : t) : unit =
  Mutex.protect bar.write_mutex (fun () ->
      if Atomic.get bar.drawing then write_if_ready bar.terminal hide_cursor_str);
  let rec loop (frame_index : int) : unit =
    if not (Atomic.get bar.stop) then begin
      serve_suspension bar;
      draw bar ~frame_index;
      Thread.delay 0.05;
      loop (frame_index + 1)
    end
  in
  loop 0

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* A terminal that says it cannot move the cursor, or says nothing: every
   terminal emulator on Unix sets $TERM, so a pty without it is not one.
   Emacs's shell and compilation buffers, and some CI runners, run the scan
   on such a pty, which would show every frame as a line of its own,
   escapes and all.
   python: rich's Console.is_dumb_terminal, which is the first case *)
let is_dumb_terminal () : bool =
  match Opengrep_env.getenv_opt "TERM" with
  | Some term -> List.mem (String.lowercase_ascii term) [ "dumb"; "unknown" ]
  | None -> true

(* $CI, which CI services set, and some of them on a pty: the convention
   for "no one is watching, do not animate". "false" and "0" are how it is
   turned off by hand. *)
let is_ci () : bool =
  match Opengrep_env.getenv_opt "CI" with
  | Some value -> not (List.mem (String.lowercase_ascii value) [ "false"; "0" ])
  | None -> false

(* The bar is drawn on stderr, so it needs a terminal there, one that can
 * redraw a line, in a run someone is watching. Not on Windows, where the
 * classic console prints the escapes as text unless the program turns
 * their processing on, which nothing here does. *)
let should_enable () : bool =
  Sys.unix
  && !ANSITerminal.isatty Unix.stderr
  && (not (is_dumb_terminal ()))
  && not (is_ci ())

(* Ctrl-C, SIGTERM, a closed terminal (SIGHUP) and Ctrl-\ (SIGQUIT) kill
   the process where it stands, and with it the [finish] that would erase
   the bar and show the cursor again: the shell's prompt would come back
   with no cursor, after the last frame. While the bar is up they do that
   much themselves, and then kill the process as the signal would have, so
   that a shell still sees an interrupted scan. The messages still queued
   are dropped: the scan is being interrupted. That is a frame's worth
   while the terminal reads, and everything held up by one that has
   stopped, which could not have been written either.

   The loop keeps drawing on another thread, so the bar is put away first,
   and a write already under way is let finish by waiting for
   [write_mutex], which is then kept so that nothing is written after the
   sequence. Both waits are bounded, the one for the mutex and the one for
   the terminal to take the sequence: the thread the signal interrupted may
   be the one holding the mutex, and a terminal that has stopped reading
   (Ctrl-S, a paused pane) must not keep a signal from ending the scan.

   Each signal comes with the status a shell reports for a process it
   killed, 128 plus its number. *)
let signals_ending_the_scan =
  [ (Sys.sighup, 129); (Sys.sigint, 130); (Sys.sigquit, 131); (Sys.sigterm, 143) ]

let restore_terminal_on (bar : t) ((signal : int), (killed_status : int)) :
    Sys.signal_behavior =
  Sys.Signal_handle
    (fun (_ : int) ->
      let was_drawing = Atomic.exchange bar.drawing false in
      let rec take_the_mutex (tries : int) : unit =
        if (not (Mutex.try_lock bar.write_mutex)) && tries > 0 then begin
          Thread.delay 0.01;
          take_the_mutex (tries - 1)
        end
      in
      take_the_mutex 20;
      if was_drawing then
        write_if_ready ~wait:0.5 bar.terminal (erase_line_str ^ show_cursor_str);
      Sys.set_signal signal Sys.Signal_default;
      (* the signal is blocked on the thread that handles it, which may be
         the only one that could take it *)
      ignore (Thread.sigmask Unix.SIG_UNBLOCK [ signal ] : int list);
      Unix.kill (Unix.getpid ()) signal;
      (* The signal ends the process, but not always before [kill] returns
         to this thread, which must not carry on meanwhile: it may hold the
         mutex that its own loop is about to take. A process still here a
         second later ends with the status the signal would have given.
         Not exit: from a signal handler it must not run the at_exit
         handlers, [finish] among them. *)
      Unix.sleepf 1.0;
      (* nosemgrep: forbid-exit *)
      Unix._exit killed_status)

(* A signal the process was started with ignored stays ignored: a scan run
   in the background by a shell without job control, or under nohup, is
   meant to survive it. [Sys.signal] reports a behaviour only by replacing
   it, so the ignore goes in first. A signal arriving in the moment before
   the handler replaces the ignore is lost rather than handled, which is
   the safe side to err on. The behaviour replaced is returned, for [finish]
   to put back. *)
let install_unless_ignored ((signal : int), (behavior : Sys.signal_behavior))
    : (int * Sys.signal_behavior) option =
  match Sys.signal signal Sys.Signal_ignore with
  | Sys.Signal_ignore -> None
  | previous ->
      Sys.set_signal signal behavior;
      Some (signal, previous)

(* Stopping joins the thread, so the work happens once however often this
 * is called: the caller stops the bar before printing its report, and an
 * enclosing handler stops it again on the way out. *)
let finish (bar : t) : unit =
  if not (Atomic.exchange bar.stop true) then begin
    Thread.join bar.thread;
    (* Diverting stops, and what is left is written, under the log mutex:
       a message from another thread comes either before, queued and
       written here, or after, straight to stderr on a line the bar has
       left. *)
    Logs_.undivert_stderr (fun () ->
        Mutex.protect bar.write_mutex (fun () ->
            put_away bar ~wait:cursor_restore_wait (take_messages bar [])));
    (* last, so that a signal arriving until now still finds the terminal
       put back *)
    bar.previous_signals
    |> List.iter (fun ((signal : int), (behavior : Sys.signal_behavior)) ->
           Sys.set_signal signal behavior);
    (* a Ctrl-Z the loop stopped before serving: there is no bar left to
       put away, so the process only has to stop *)
    if Atomic.exchange bar.suspend_requested false then begin
      ignore (Thread.sigmask Unix.SIG_UNBLOCK [ Sys.sigtstp ] : int list);
      Unix.kill (Unix.getpid ()) Sys.sigtstp
    end
  end

let create (initial_phase : phase) : t option =
  if not (should_enable ()) then None
  else
    match Unix.dup ~cloexec:true Unix.stderr with
    | exception Unix.Unix_error _ -> None
    | terminal -> (
        let bar =
          {
            phase = Atomic.make initial_phase;
            stop = Atomic.make false;
            suspend_requested = Atomic.make false;
            drawing = Atomic.make true;
            messages = Saturn.Single_consumer_queue.create ();
            columns_from_env = columns_from_env ();
            write_mutex = Mutex.create ();
            terminal;
            (* replaced below; Thread.t has no other neutral value *)
            thread = Thread.self ();
            previous_signals = [];
          }
        in
        (* before the loop starts, whose first frame hides the cursor *)
        bar.previous_signals <-
          ( Sys.sigtstp,
            Sys.Signal_handle
              (fun (_ : int) -> Atomic.set bar.suspend_requested true) )
          :: (signals_ending_the_scan
             |> List_.map (fun ((signal : int), (status : int)) ->
                    (signal, restore_terminal_on bar (signal, status))))
          |> List_.filter_map install_unless_ignored;
        Logs_.divert_stderr (Saturn.Single_consumer_queue.push bar.messages);
        match Thread.create render_loop bar with
        | thread ->
            bar.thread <- thread;
            (* For a run that ends by [exit], without the callers' finally.
               [finish] takes the log mutex, so an exit while logging -- in
               a printer, or under Logs_.logs_mutex -- would fail here;
               nothing exits there. *)
            Stdlib.at_exit (fun () -> finish bar);
            Some bar
        (* A process at its limit of threads costs the bar, not the scan.
           Thread.create raises Out_of_memory rather than Sys_error when the
           cause is ENOMEM. *)
        | exception ((Sys_error _ | Out_of_memory) as exn) ->
            (* what was logged meanwhile is written as it would have been
               without a bar *)
            Logs_.undivert_stderr (fun () ->
                write_all terminal (String.concat "" (take_messages bar [])));
            bar.previous_signals
            |> List.iter
                 (fun ((signal : int), (behavior : Sys.signal_behavior)) ->
                   Sys.set_signal signal behavior);
            Logs.debug (fun m ->
                m "no status bar, its thread did not start: %s"
                  (Printexc.to_string exn));
            None)

let set_phase (bar : t) (new_phase : phase) : unit =
  Atomic.set bar.phase new_phase

(* Called from whichever domain finished the unit of work. A tick landing
   on a phase about to be replaced is one frame's worth of undercount. *)
let notify_work_item_done (bar : t) : unit =
  match Atomic.get bar.phase with
  | Scanning { completed; _ } -> Atomic.incr completed
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  | Comparing_with_baseline ->
      ()
