type phase =
  | Loading_rules
  | Analyzing_targets
  | Scanning of { total : int Atomic.t; completed : int Atomic.t }

type t = {
  mutable phase : phase;
  phase_mutex : Mutex.t;
  stop : bool Atomic.t;
  paused : bool Atomic.t;
  mutable thread : Thread.t;
  last_rendered : string ref;
}

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

(* Rainbow hue cycle: red → yellow → green → cyan → blue → magenta *)
let rainbow_colors =
  [| "\027[31m"; "\027[33m"; "\027[32m"; "\027[36m"; "\027[34m"; "\027[35m" |]

let should_enable (maturity : Maturity.t) : bool =
  match maturity with
  | Experimental | Develop ->
      Sys.unix && !ANSITerminal.isatty Unix.stderr
  | Default | Legacy -> false

let reset_str = "\027[0m"
let red_bold = "\027[1;31m"

let progress_bar ~width ~filled ~total =
  let ratio = if total > 0 then float_of_int filled /. float_of_int total else 0.0 in
  let filled_len = min (Float.to_int (Float.min ratio 1.0 *. float_of_int width)) width in
  let buf = Buffer.create (width * 4) in
  Buffer.add_char buf '[';
  Buffer.add_string buf red_bold;
  for _ = 1 to filled_len do Buffer.add_string buf "❱" done;
  Buffer.add_string buf reset_str;
  for _ = 1 to width - filled_len do Buffer.add_char buf ' ' done;
  Buffer.add_char buf ']';
  Buffer.contents buf

let phase_to_string = function
  | Loading_rules -> "\027[1mLoading rules...\027[22m"
  | Analyzing_targets -> "\027[1mAnalyzing targets...\027[22m"
  | Scanning { total; completed } ->
      let total = Atomic.get total in
      let done_ = Atomic.get completed in
      let pct = if total > 0 then done_ * 100 / total else 0 in
      (* To avoid flichering, show progress bar only if there are more than 200 targets *)
      if total >= 200 then
        let bar = progress_bar ~width:30 ~filled:done_ ~total in
        Printf.sprintf "\027[1mScanning:\027[22m %s %d/%d (%d%%)" bar done_ total pct
      else if total > 0 then
        Printf.sprintf "\027[1mScanning:\027[22m %d/%d (%d%%)" done_ total pct
      else
        Printf.sprintf "\027[1mScanning:\027[22m %d targets" done_

let erase_line_str = "\r\027[2K"
let hide_cursor_str = "\027[?25l"
let show_cursor_str = "\027[?25h"

(* All output goes through Format.err_formatter so it shares the same
   buffer as the Logs reporter. This avoids ghost lines caused by
   Printf.eprintf and Format.err_formatter having separate buffers. *)
let fmt_eprintf s =
  Format.pp_print_as Format.err_formatter 0 s;
  Format.pp_print_flush Format.err_formatter ()

let render_frame ~frame_index phase =
  let s = spinner.(frame_index mod Array.length spinner) in
  let color = rainbow_colors.(frame_index mod Array.length rainbow_colors) in
  let msg = phase_to_string phase in
  Printf.sprintf "%s%s%s%s %s" erase_line_str color s reset_str msg

let erase_status_bar () =
  fmt_eprintf erase_line_str

let render_loop (bar : t) : unit =
  let frame_index = ref 0 in
  fmt_eprintf hide_cursor_str;
  while not (Atomic.get bar.stop) do
    if not (Atomic.get bar.paused) then begin
      let phase =
        Mutex.protect bar.phase_mutex (fun () -> bar.phase)
      in
      let line = render_frame ~frame_index:!frame_index phase in
      Mutex.protect Logs_.logs_mutex (fun () ->
        bar.last_rendered := line;
        fmt_eprintf line
      );
      incr frame_index
    end;
    Thread.delay 0.05
  done

let create (maturity : Maturity.t) (initial_phase : phase) : t option =
  if not (should_enable maturity) then None
  else begin
    let last_rendered = ref "" in
    let bar =
      {
        phase = initial_phase;
        phase_mutex = Mutex.create ();
        stop = Atomic.make false;
        paused = Atomic.make false;
        thread = Thread.self (); (* placeholder, replaced below *)
        last_rendered;
      }
    in
    (* Erase status bar before log output, redraw it after.
       Both hooks run inside the Logs mutex, and use the same
       Format.err_formatter as the Logs reporter — no buffer ordering issues. *)
    Logs_.before_log_hook := erase_status_bar;
    Logs_.after_log_hook := (fun () ->
      if not (Atomic.get bar.paused) then
        fmt_eprintf !(bar.last_rendered)
    );
    (* Hook for UCmd: pause the render loop while stderr is captured *)
    UCmd.pause_stderr_hook := (fun () ->
      Atomic.set bar.paused true;
      (* TODO: Just in case erase the status bar before stderr is redirected? *)
      (* Mutex.protect Logs_.logs_mutex (fun () -> erase_status_bar ()) *)
    );
    UCmd.unpause_stderr_hook := (fun () ->
      Atomic.set bar.paused false
    );
    bar.thread <- Thread.create render_loop bar;
    Some bar
  end

let set_phase (bar : t) (new_phase : phase) : unit =
  Mutex.protect bar.phase_mutex (fun () ->
    bar.phase <- new_phase
  )

let notify_file_done (bar : t) : unit =
  match bar.phase with
  | Scanning { completed; _ } -> Atomic.incr completed
  | Loading_rules | Analyzing_targets -> ()

let finish (bar : t) : unit =
  Atomic.set bar.stop true;
  Thread.join bar.thread;
  Logs_.before_log_hook := (fun () -> ());
  Logs_.after_log_hook := (fun () -> ());
  UCmd.pause_stderr_hook := (fun () -> ());
  UCmd.unpause_stderr_hook := (fun () -> ());
  fmt_eprintf (erase_line_str ^ show_cursor_str)
