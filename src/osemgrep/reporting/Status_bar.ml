(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A line at the bottom of the terminal saying what the scan is doing, with a
 * spinner and, once the scan reaches its targets, a progress bar.
 *
 * It is drawn by a thread of its own, which is what lets it animate while
 * the scan works. That thread and the rest of the program share the
 * terminal, so three things have to be arranged:
 *
 *  - a log message must not be cut in half by a redraw. Logs_ calls
 *    [before_log_hook] and [after_log_hook] inside its reporter mutex, and
 *    the render loop takes the same mutex, so the bar is erased before a
 *    message and redrawn after it.
 *  - a redraw must not land in a captured stderr. UCmd calls its pause
 *    hooks around the capture, and the loop stops drawing in between.
 *  - the findings must not be printed over it. The caller stops the bar
 *    before the report; under --incremental-output, where findings reach
 *    stdout while the scan runs, there is no bar at all.
 *
 * Everything is written through Format.err_formatter, the formatter the
 * Logs reporter uses, so the two share one buffer and cannot interleave
 * half-written lines.
 *)

type phase =
  | Analyzing_targets
  | Building_interfile_graph
  | Scanning of {
      targets : int;
      targets_done : int Atomic.t;
      interfile_rules : int;
      interfile_rules_done : int Atomic.t;
    }

type t = {
  mutable phase : phase;
  phase_mutex : Mutex.t;
  stop : bool Atomic.t;
  paused : bool Atomic.t;
  mutable thread : Thread.t;
  (* what the loop drew last, so a log message can put it back *)
  last_rendered : string ref;
}

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
let erase_line_str = "\r\027[2K"
let hide_cursor_str = "\027[?25l"
let show_cursor_str = "\027[?25h"
let bold_str = "\027[1m"
let unbold_str = "\027[22m"

(* A bar only means something once there are enough targets for it to move;
 * below that it would jump from empty to full and read as a flicker. *)
let min_targets_for_bar = 200
let bar_width = 46

let progress_bar ~(width : int) ~(filled : int) ~(total : int) : string =
  let ratio =
    if total > 0 then float_of_int filled /. float_of_int total else 0.0
  in
  let filled_len =
    min (Float.to_int (Float.min ratio 1.0 *. float_of_int width)) width
  in
  let buf = Buffer.create (width * 4) in
  Buffer.add_char buf '[';
  for _ = 1 to filled_len do
    Buffer.add_string buf "━"
  done;
  for _ = 1 to width - filled_len do
    Buffer.add_char buf ' '
  done;
  Buffer.add_char buf ']';
  Buffer.contents buf

let titled (s : string) : string = bold_str ^ s ^ unbold_str

let counter ~(title : string) ~(with_bar : bool) ~(done_ : int) ~(total : int) :
    string =
  let pct = if total > 0 then done_ * 100 / total else 0 in
  if with_bar then
    Printf.sprintf "%s %s %d/%d (%d%%)" (titled title)
      (progress_bar ~width:bar_width ~filled:done_ ~total)
      done_ total pct
  else Printf.sprintf "%s %d/%d (%d%%)" (titled title) done_ total pct

let phase_to_string (phase : phase) : string =
  match phase with
  | Analyzing_targets -> titled "Analyzing targets..."
  | Building_interfile_graph -> titled "Building call graph..."
  | Scanning { targets; targets_done; interfile_rules; interfile_rules_done }
    ->
      let targets_done = Atomic.get targets_done in
      let rules_done = Atomic.get interfile_rules_done in
      (* Interfile rules share the pool with the targets but can outlast all
         of them, so once the targets are through they are what is left to
         report; showing a full target bar instead would read as a hang. *)
      if targets_done >= targets && rules_done < interfile_rules then
        counter ~title:"Cross-file analysis:" ~with_bar:true ~done_:rules_done
          ~total:interfile_rules
      else if targets >= min_targets_for_bar then
        counter ~title:"Scanning:" ~with_bar:true ~done_:targets_done
          ~total:targets
      else if targets > 0 then
        counter ~title:"Scanning:" ~with_bar:false ~done_:targets_done
          ~total:targets
      else
        Printf.sprintf "%s %s" (titled "Scanning:")
          (String_.unit_str targets_done "target")

(* Everything goes through the formatter the Logs reporter writes to, so the
 * two cannot interleave. pp_print_as with a width of 0 keeps the escapes
 * from counting towards the formatter's line length. *)
let fmt_eprintf (s : string) : unit =
  Format.pp_print_as Format.err_formatter 0 s;
  Format.pp_print_flush Format.err_formatter ()

let render_frame ~(frame_index : int) (phase : phase) : string =
  let glyph = spinner.(frame_index mod Array.length spinner) in
  Printf.sprintf "%s%s %s" erase_line_str glyph (phase_to_string phase)

let erase_status_bar () : unit = fmt_eprintf erase_line_str

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

let render_loop (bar : t) : unit =
  let frame_index = ref 0 in
  fmt_eprintf hide_cursor_str;
  while not (Atomic.get bar.stop) do
    if not (Atomic.get bar.paused) then (
      let phase = Mutex.protect bar.phase_mutex (fun () -> bar.phase) in
      let line = render_frame ~frame_index:!frame_index phase in
      (* the same mutex the log reporter takes, so a redraw and a message
         cannot be written at once *)
      Mutex.protect Logs_.logs_mutex (fun () ->
          bar.last_rendered := line;
          fmt_eprintf line);
      incr frame_index);
    Thread.delay 0.05
  done

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* The bar is drawn on stderr, so it needs a terminal there; it also needs
 * threads, which the JSOO build does not have. *)
let should_enable () : bool = Sys.unix && !ANSITerminal.isatty Unix.stderr

let create (initial_phase : phase) : t option =
  if not (should_enable ()) then None
  else begin
    let bar =
      {
        phase = initial_phase;
        phase_mutex = Mutex.create ();
        stop = Atomic.make false;
        paused = Atomic.make false;
        (* replaced below; Thread.t has no other neutral value *)
        thread = Thread.self ();
        last_rendered = ref "";
      }
    in
    Logs_.before_log_hook := erase_status_bar;
    Logs_.after_log_hook :=
      (fun () ->
        if not (Atomic.get bar.paused) then fmt_eprintf !(bar.last_rendered));
    UCmd.pause_stderr_hook := (fun () -> Atomic.set bar.paused true);
    UCmd.unpause_stderr_hook := (fun () -> Atomic.set bar.paused false);
    bar.thread <- Thread.create render_loop bar;
    Some bar
  end

let set_phase (bar : t) (new_phase : phase) : unit =
  Mutex.protect bar.phase_mutex (fun () -> bar.phase <- new_phase)

(* Called from whichever domain finished the unit of work, so these read the
   phase without the mutex; [phase] is a single word and the counters it
   holds are atomic, and a tick landing on a phase about to be replaced is
   one frame's worth of undercount. *)
let notify_target_done (bar : t) : unit =
  match bar.phase with
  | Scanning { targets_done; _ } -> Atomic.incr targets_done
  | Analyzing_targets
  | Building_interfile_graph ->
      ()

let notify_interfile_rule_done (bar : t) : unit =
  match bar.phase with
  | Scanning { interfile_rules_done; _ } -> Atomic.incr interfile_rules_done
  | Analyzing_targets
  | Building_interfile_graph ->
      ()

(* Stopping joins the thread, so this must run once: a second call would
 * join a thread that has already been joined. *)
let finish (bar : t) : unit =
  Atomic.set bar.stop true;
  Thread.join bar.thread;
  Logs_.before_log_hook := (fun () -> ());
  Logs_.after_log_hook := (fun () -> ());
  UCmd.pause_stderr_hook := (fun () -> ());
  UCmd.unpause_stderr_hook := (fun () -> ());
  fmt_eprintf (erase_line_str ^ show_cursor_str)
