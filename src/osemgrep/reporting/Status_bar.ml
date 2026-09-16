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
  (* Targets and interfile rules are counted as one, though a rule is by far
     the longer unit: they run together in one pool from the start, so any
     split leaves whichever half is not on show looking stalled. One count
     advances unevenly, but the unevenness is there between one interfile
     rule and the next anyway. *)
  | Scanning of { total : int; completed : int Atomic.t }

type t = {
  mutable phase : phase;
  phase_mutex : Mutex.t;
  stop : bool Atomic.t;
  (* a depth, not a flag: were two stderr captures ever to overlap, the
     inner one ending must not let the bar draw into the outer one's
     still-redirected stderr *)
  paused : int Atomic.t;
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

(* The real width, not Findings_layout.text_width, which floors at 40 and so
   would claim room a narrow terminal does not have. Read for every frame,
   so that resizing a window is picked up without watching for SIGWINCH. *)
let terminal_columns () : int option =
  match
    Opengrep_env.getenv_opt "COLUMNS"
    |> Option.map String.trim
    |> Fun.flip Option.bind int_of_string_opt
  with
  | Some w when w > 0 -> Some w
  | _ -> Terminal_size.get_columns ()

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
    Buffer.add_string buf faint_str;
    for _ = 1 to width - filled_len do
      Buffer.add_string buf empty_dot
    done;
    Buffer.add_string buf normal_str
  end;
  Buffer.contents buf

let titled (s : string) : string = bold_str ^ s ^ normal_str

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
  let with_title = spinner_cols + String.length title + 1 in
  if with_bar && room_for (with_title + bar_width + 1 + String.length numbers)
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
  | Analyzing_targets -> label ~columns "Analyzing targets..."
  | Building_interfile_graph -> label ~columns "Building call graph..."
  | Scanning { total; completed } ->
      let done_ = Atomic.get completed in
      if total > 0 then
        counter ~title:"Scanning:"
          ~with_bar:(total >= min_targets_for_bar)
          ~done_ ~total ~columns
      else
        Printf.sprintf "%s %s"
          (label ~columns "Scanning:")
          (String_.unit_str done_ "target")

(* Everything goes through the formatter the Logs reporter writes to, so the
 * two cannot interleave. pp_print_as with a width of 0 keeps the escapes
 * from counting towards the formatter's line length.
 *
 * It never raises. A terminal that has gone away -- a closed window, a
 * dropped ssh session -- makes the write fail, and these writes happen in
 * the render thread and, through the hooks below, inside the log
 * reporter's lock. An exception from there would surface out of some
 * unrelated log call and leave that lock held, hanging every message after
 * it. Losing the bar costs nothing by comparison. *)
let fmt_eprintf (s : string) : unit =
  try
    Format.pp_print_as Format.err_formatter 0 s;
    Format.pp_print_flush Format.err_formatter ()
  with
  | Sys_error _
  | Unix.Unix_error _ ->
      ()

let render_frame ~(frame_index : int) ~(columns : int option) (phase : phase) :
    string =
  let glyph = spinner.(frame_index mod Array.length spinner) in
  Printf.sprintf "%s%s %s" erase_line_str glyph
    (phase_to_string ~columns phase)

let erase_status_bar () : unit = fmt_eprintf erase_line_str

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

(* Reads the depth without the mutex, and so must never take it: the loop
   calls this from inside the very section that holds it. *)
let is_paused (bar : t) : bool = Atomic.get bar.paused > 0

(* Entered and left under the log mutex, the one the loop holds while it
   writes. Moving the depth outside it would settle nothing: the loop could
   read zero, be descheduled for as long as the scheduler likes, and write
   its frame after stderr had been redirected, dropping escapes into a
   subprocess's captured output. Taking the mutex lets a frame already in
   flight finish, and the loop reads the depth again inside the same
   section, so no frame can follow the pause. *)
let pause (bar : t) : unit =
  Mutex.protect Logs_.logs_mutex (fun () -> Atomic.incr bar.paused)

let unpause (bar : t) : unit =
  Mutex.protect Logs_.logs_mutex (fun () ->
      (* never below zero, or an unmatched unpause would leave the next
         pause counting up to zero and reading as "not paused" *)
      if Atomic.get bar.paused > 0 then Atomic.decr bar.paused)

let render_loop (bar : t) : unit =
  let frame_index = ref 0 in
  fmt_eprintf hide_cursor_str;
  while not (Atomic.get bar.stop) do
    (* a cheap early-out, so that a paused loop does not build frames it is
       going to throw away; the flag is read again below to decide *)
    if not (is_paused bar) then (
      let phase = Mutex.protect bar.phase_mutex (fun () -> bar.phase) in
      let line =
        render_frame ~frame_index:!frame_index ~columns:(terminal_columns ())
          phase
      in
      (* the same mutex the log reporter takes, so a redraw and a message
         cannot be written at once *)
      Mutex.protect Logs_.logs_mutex (fun () ->
          if not (is_paused bar) then begin
            bar.last_rendered := line;
            fmt_eprintf line
          end);
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
        paused = Atomic.make 0;
        (* replaced below; Thread.t has no other neutral value *)
        thread = Thread.self ();
        last_rendered = ref "";
      }
    in
    (* These two run with Logs_.logs_mutex held: see the contract on
       Logs_.before_log_hook. Neither body may log or take a lock, which is
       why is_paused reads the count without one and why fmt_eprintf
       swallows a failed write instead of raising. *)
    Logs_.before_log_hook := erase_status_bar;
    Logs_.after_log_hook :=
      (fun () ->
        if not (is_paused bar) then fmt_eprintf !(bar.last_rendered));
    UCmd.pause_stderr_hook := (fun () -> pause bar);
    UCmd.unpause_stderr_hook := (fun () -> unpause bar);
    bar.thread <- Thread.create render_loop bar;
    Some bar
  end

let set_phase (bar : t) (new_phase : phase) : unit =
  Mutex.protect bar.phase_mutex (fun () -> bar.phase <- new_phase)

(* Called from whichever domain finished the unit of work, so this reads the
   phase without the mutex; [phase] is a single word and the counter it
   holds is atomic, and a tick landing on a phase about to be replaced is
   one frame's worth of undercount. *)
let notify_work_item_done (bar : t) : unit =
  match bar.phase with
  | Scanning { completed; _ } -> Atomic.incr completed
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
