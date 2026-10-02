(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A thread of its own draws the status line, so that it animates while the
 * scan runs, and is the only writer to the terminal while the status line
 * is drawn:
 *
 *  - a log message is not written by the thread that logs it. Logs_ passes
 *    it as text to the sink of [Logs_.redirect_stderr], which queues it, and
 *    the drawing thread writes it between two redraws: the status line
 *    erased, the messages, the status line again. A redraw never cuts a
 *    message in half, and a message is at most one redraw interval late
 *    while the terminal reads; a terminal that has stopped reading holds the
 *    messages back until it reads again (see [draw]).
 *  - the drawing thread writes to a duplicate of the stderr descriptor taken
 *    at [create], so a capture of stderr (UCmd, around a git command)
 *    receives neither a redraw nor a log line.
 *
 * Output that does not go through Logs, such as the OCaml runtime's or a C
 * library's, is not redirected and lands on the status line.
 *)

type phase =
  (* fetching a large ruleset over the network can take long enough to look
     like a hung scan *)
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  (* No count: the baseline scan is a second pass over the same files, and a
     count restarting from zero would look like the scan going backwards. *)
  | Comparing_with_baseline
  (* Targets and interfile rules are counted together, although one
     interfile rule takes far longer than one target: they run in one pool
     from the start, so a count that is not shown would look stalled. The
     single count advances unevenly, as a count of interfile rules alone
     would. *)
  | Scanning of { total : int; completed : int Atomic.t }

type t = {
  phase : phase Atomic.t;
  stop : bool Atomic.t;
  (* set by the SIGTSTP handler; [handle_suspension] acts on it between two
     redraws *)
  suspend_requested : bool Atomic.t;
  (* False once [stop_drawing] has run, after a Ctrl-Z or a signal that ends
     the scan. Messages are still written after a Ctrl-Z, without a status
     line under them. *)
  drawing : bool Atomic.t;
  (* log messages waiting for the next redraw, each one or more complete
     lines *)
  messages : string Saturn.Single_consumer_queue.t;
  columns_from_env : int option;
  (* held around every write to [terminal], so that a signal handler can wait
     for a write in progress to finish *)
  write_mutex : Mutex.t;
  (* A duplicate of the stderr descriptor taken at [create], which reaches
     the terminal even while stderr is redirected into a capture. Never
     closed: a signal handler running on another thread could otherwise write
     to it after [finish], when the number may refer to another file. One
     descriptor per run, not inherited by child processes. *)
  terminal : Unix.file_descr;
  mutable thread : Thread.t;
  (* the signal behaviours that [create] replaced, restored by [finish] *)
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

(* normal intensity: ends both bold and faint *)
let normal_str = "\027[22m"

(* Below this total the progress bar would jump from empty to full, so only
 * the counts are shown. *)
let min_targets_for_bar = 200

let bar_width = 30
let filled_dot = "●"
let empty_dot = "·"

(* the spinner and the space after it, which [render_frame] puts before the
   text *)
let spinner_cols = 2

(* A width below this is taken as unknown rather than as a very narrow
   terminal: a pty whose size was never set reports zero, and COLUMNS=0
   occurs in CI and under script(1). *)
let min_sensible_columns = 10

(* Seconds [finish] waits for a terminal that is behind on reading to accept
   the sequence that shows the cursor again. *)
let cursor_restore_wait = 2.0

(* The width of the terminal, from whichever descriptor reports it. The
   status line is drawn on stderr, but no library in use reports the size of
   a given descriptor: Terminal_size queries stdout, ANSITerminal stdin. With
   stdout redirected (> out, | less) the first returns nothing, and the
   status line would be drawn at full width and wrap in a narrower window,
   which the one-row erase does not clear. Stdin is still the terminal in
   that case, and the same terminal as stderr when a person runs the scan.

   The actual width, not Findings_layout.text_width, which has a floor of
   40. Read on every redraw, so that a resized window takes effect without a
   SIGWINCH handler; [from_env], from $COLUMNS, takes precedence when set.
   [None] means unknown, which drops the dotted progress bar but not the
   counts; see [counter]. *)
let terminal_columns ~(from_env : int option) : int option =
  (* raises when stdin is not a terminal and on an unsupported platform *)
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

(* $COLUMNS, read once at [create] rather than on every redraw: the lookup
   goes through Str (the SEMGREP_ alias in Opengrep_env), which the drawing
   thread must not use. Str keeps the last match in state shared by every
   thread of a domain, so a match on the drawing thread would overwrite the
   groups that the main thread reads between Common.(=~) and
   Common.matched1. *)
let columns_from_env () : int option =
  Opengrep_env.getenv_opt "COLUMNS"
  |> Option.map String.trim
  |> Fun.flip Option.bind int_of_string_opt

(* The console's highlight setting, which resolves $NO_COLOR and
   --force-color. With highlighting off only the styling is dropped: the
   erase and the spinner remain. *)
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

(* A line wider than the terminal wraps, and the erase before each redraw
   clears only one row, so the rows above would remain on screen. Hence
   three widths: the progress bar with the counts, the counts alone, and the
   counts clipped. *)
let counter ~(title : string) ~(with_bar : bool) ~(done_ : int) ~(total : int)
    ~(columns : int option) : string =
  let pct = if total > 0 then done_ * 100 / total else 0 in
  let numbers = Printf.sprintf "%d/%d (%d%%)" done_ total pct in
  let room_for (cols : int) : bool =
    match columns with
    | None -> true (* unknown width: the counts are not clipped *)
    | Some available -> cols <= available
  in
  (* The progress bar is drawn only at a known width: a wrapped progress bar
     leaves a row that the one-row erase does not clear, while the counts
     alone are short enough to risk. The width is unknown when both stdout
     and stdin are redirected. *)
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

(* Clipped before the escapes are added, so that clipping never cuts an
   escape sequence. *)
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
      (* Every work item is done, but the engine still collects the results,
         which takes time with many findings; a progress bar held at 100%
         would look like a hang. *)
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

(* Writes all of [s], or as much as the terminal accepts before an error,
   and never raises: a terminal that has gone away (a closed window, a
   dropped ssh session) makes the write fail, and the scan must not stop for
   it. A write interrupted by a signal is resumed. *)
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

(* Writes [s] if the terminal accepts it within [wait] seconds, else
   nothing.

   A redraw is dropped when the terminal is not ready. A pty whose reader
   has stopped (Ctrl-S, a paused tmux pane, a stalled ssh link) fills its
   buffer, and a write then blocks; [finish] would join a thread that never
   returns, and the status line would stop the scan. A dropped redraw is
   harmless: the next one redraws the whole line.

   The sequence that shows the cursor again also goes through here, with a
   wait, so that a terminal briefly behind on reading still receives it,
   while one that has stopped reading costs a bounded pause. *)
let write_if_ready ?(wait : float = 0.) (terminal : Unix.file_descr)
    (s : string) : unit =
  match Unix.select [] [ terminal ] [] wait with
  | _, _ :: _, _ -> write_all terminal s
  | _, [], _ -> ()
  | exception Unix.Unix_error _ -> ()

(* The messages queued since the last redraw, in the order they were
   logged. The drawing thread is the queue's only consumer until it stops,
   and [finish] after. *)
let rec take_messages (bar : t) (taken : string list) : string list =
  match Saturn.Single_consumer_queue.pop_opt bar.messages with
  | Some message -> take_messages bar (message :: taken)
  | None -> List.rev taken

(* Erases the status line for good, writes [messages] in its place and shows
   the cursor again. Called with [write_mutex] held. *)
let stop_drawing (bar : t) ~(wait : float) (messages : string list) : unit =
  let was_drawing = Atomic.exchange bar.drawing false in
  write_all bar.terminal
    ((if was_drawing then erase_line_str else "") ^ String.concat "" messages);
  if was_drawing then write_if_ready ~wait bar.terminal show_cursor_str

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

(* Ctrl-Z. The SIGTSTP handler only sets [suspend_requested]; the drawing
   thread acts on it between two redraws, when no redraw is half written:
   it stops drawing, writes the messages queued until then, and stops the
   process as the signal would have. After fg or bg the scan continues and
   its messages are written without a status line: in the background the
   status line would be drawn over the shell, and the process cannot tell
   the two cases apart. *)
let handle_suspension (bar : t) : unit =
  if Atomic.exchange bar.suspend_requested false then begin
    Mutex.protect bar.write_mutex (fun () ->
        stop_drawing bar ~wait:0.5 (take_messages bar []));
    Sys.set_signal Sys.sigtstp Sys.Signal_default;
    ignore (Thread.sigmask Unix.SIG_UNBLOCK [ Sys.sigtstp ] : int list);
    Unix.kill (Unix.getpid ()) Sys.sigtstp
  end

(* One redraw: the messages queued since the last one, then the status line.
   Messages are never dropped: the drawing thread waits for the terminal to
   accept them. A terminal that has stopped reading (Ctrl-S, a paused pane)
   holds back the status line, the messages queued after these, and
   [finish], which joins the drawing thread, but not the scan, since a
   thread that logs only queues. Writing the messages directly would block
   the scan instead. *)
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
      (* read again: a signal handler may have stopped the drawing meanwhile *)
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
      handle_suspension bar;
      draw bar ~frame_index;
      Thread.delay 0.05;
      loop (frame_index + 1)
    end
  in
  loop 0

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* $TERM unset, "dumb" or "unknown". Every terminal emulator on Unix sets
   $TERM, so a pty without it is not a terminal emulator. Emacs's shell and
   compilation buffers, and some CI runners, run the scan on such a pty,
   which would show every redraw as a separate line, escapes included.
   python: rich's Console.is_dumb_terminal, which is the first case *)
let is_dumb_terminal () : bool =
  match Opengrep_env.getenv_opt "TERM" with
  | Some term -> List.mem (String.lowercase_ascii term) [ "dumb"; "unknown" ]
  | None -> true

(* $CI, which CI services set, some of them on a pty. The values "false" and
   "0" disable it. *)
let is_ci () : bool =
  match Opengrep_env.getenv_opt "CI" with
  | Some value -> not (List.mem (String.lowercase_ascii value) [ "false"; "0" ])
  | None -> false

(* stderr must be a terminal that can redraw a line, outside CI. Not on
 * Windows, where the classic console prints the escapes as text unless the
 * program enables their processing, which opengrep does not. *)
let should_enable () : bool =
  Sys.unix
  && !ANSITerminal.isatty Unix.stderr
  && (not (is_dumb_terminal ()))
  && not (is_ci ())

(* Ctrl-C, SIGTERM, a closed terminal (SIGHUP) and Ctrl-\ (SIGQUIT) end the
   process at once, without the [finish] that erases the status line and
   shows the cursor: the shell prompt would return without a cursor, after
   the last redraw. While the status line is drawn, the handler does both,
   then kills the process with the same signal, so that the shell still sees
   an interrupted scan. The messages still queued are dropped: at most one
   redraw interval of messages while the terminal reads, and everything held
   back by one that has stopped, which could not be written either.

   The drawing thread runs on, so the handler first stops the drawing, then
   waits for a write in progress by taking [write_mutex], which it keeps so
   that nothing is written after the sequence. Both waits are bounded, for
   the mutex and for the terminal to accept the sequence: the interrupted
   thread may hold the mutex, and a terminal that has stopped reading
   (Ctrl-S, a paused pane) must not keep a signal from ending the scan.

   Each signal comes with the exit status a shell reports for a process it
   killed, 128 plus the signal number. *)
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
         the only thread that can receive it *)
      ignore (Thread.sigmask Unix.SIG_UNBLOCK [ signal ] : int list);
      Unix.kill (Unix.getpid ()) signal;
      (* The signal does not always end the process before [kill] returns,
         and this thread must not continue meanwhile: it may hold the mutex
         that its own loop is about to take. A process still alive a second
         later exits with the status the signal would have given. Not
         [exit]: a signal handler must not run the at_exit handlers, [finish]
         among them. *)
      Unix.sleepf 1.0;
      (* nosemgrep: forbid-exit *)
      Unix._exit killed_status)

(* [Sys.signal] returns the previous behaviour only by replacing it, so
   Signal_ignore goes in first; a signal that arrives before the handler
   replaces it is ignored. A signal ignored at startup stays ignored, as a
   scan run under nohup or by a shell without job control expects. Returns
   the replaced behaviour, which [finish] restores. *)
let install_unless_ignored ((signal : int), (behavior : Sys.signal_behavior))
    : (int * Sys.signal_behavior) option =
  match Sys.signal signal Sys.Signal_ignore with
  | Sys.Signal_ignore -> None
  | previous ->
      Sys.set_signal signal behavior;
      Some (signal, previous)

let finish (bar : t) : unit =
  if not (Atomic.exchange bar.stop true) then begin
    Thread.join bar.thread;
    (* The redirection ends and the remaining messages are written under the
       log mutex: a message from another thread comes either before, queued
       and written here, or after, directly to stderr on a line that the
       status line no longer occupies. *)
    Logs_.restore_stderr (fun () ->
        Mutex.protect bar.write_mutex (fun () ->
            stop_drawing bar ~wait:cursor_restore_wait (take_messages bar [])));
    (* last, so that a signal that arrives before this point still restores
       the terminal *)
    bar.previous_signals
    |> List.iter (fun ((signal : int), (behavior : Sys.signal_behavior)) ->
           Sys.set_signal signal behavior);
    (* a Ctrl-Z that the drawing thread did not handle before it stopped:
       there is no status line left to erase, so the process only stops *)
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
            (* replaced below; Thread.t has no placeholder value *)
            thread = Thread.self ();
            previous_signals = [];
          }
        in
        (* installed before the drawing thread starts and hides the cursor *)
        bar.previous_signals <-
          ( Sys.sigtstp,
            Sys.Signal_handle
              (fun (_ : int) -> Atomic.set bar.suspend_requested true) )
          :: (signals_ending_the_scan
             |> List_.map (fun ((signal : int), (status : int)) ->
                    (signal, restore_terminal_on bar (signal, status))))
          |> List_.filter_map install_unless_ignored;
        Logs_.redirect_stderr (Saturn.Single_consumer_queue.push bar.messages);
        match Thread.create render_loop bar with
        | thread ->
            bar.thread <- thread;
            (* For a run that ends by [exit], which skips the callers'
               finally. [finish] takes the log mutex, so an exit while
               logging (in a printer, or under Logs_.logs_mutex) would fail
               here; no code exits there. *)
            Stdlib.at_exit (fun () -> finish bar);
            Some bar
        (* A process at its thread limit runs without a status line.
           Thread.create raises Out_of_memory rather than Sys_error on
           ENOMEM. *)
        | exception ((Sys_error _ | Out_of_memory) as exn) ->
            (* the messages logged meanwhile are written as without a status
               line *)
            Logs_.restore_stderr (fun () ->
                write_all terminal (String.concat "" (take_messages bar [])));
            bar.previous_signals
            |> List.iter
                 (fun ((signal : int), (behavior : Sys.signal_behavior)) ->
                   Sys.set_signal signal behavior);
            Logs.debug (fun m ->
                m "no status line, its thread did not start: %s"
                  (Printexc.to_string exn));
            None)

let set_phase (bar : t) (new_phase : phase) : unit =
  Atomic.set bar.phase new_phase

(* A count that reaches a phase about to be replaced is lost, which shows
   for at most one redraw interval. *)
let notify_work_item_done (bar : t) : unit =
  match Atomic.get bar.phase with
  | Scanning { completed; _ } -> Atomic.incr completed
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  | Comparing_with_baseline ->
      ()

let progress_hook (bar : t option) : Core_scan_config.progress -> unit =
  match bar with
  | None -> fun (_ : Core_scan_config.progress) -> ()
  | Some (bar : t) -> (
      function
      | Core_scan_config.Target_done
      | Core_scan_config.Interfile_rule_done ->
          notify_work_item_done bar
      | Core_scan_config.Analyzing_targets -> set_phase bar Analyzing_targets
      | Core_scan_config.Building_interfile_graph ->
          set_phase bar Building_interfile_graph
      | Core_scan_config.Scanning_started { targets; interfile_rules } ->
          set_phase bar
            (Scanning
               { total = targets + interfile_rules; completed = Atomic.make 0 }))
