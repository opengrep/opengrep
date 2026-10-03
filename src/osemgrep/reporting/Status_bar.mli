(* A status line at the bottom of the terminal: the phase of the scan, a
   spinner, and a progress bar once the scan reaches its targets. While it
   is drawn, the log messages written to stderr appear above it. The caller
   stops it before printing the report. *)

type phase =
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  (* the baseline scan of a --baseline-commit scan *)
  | Comparing_with_baseline
  (* [total] counts targets and interfile rules together *)
  | Scanning of { total : int; completed : int Atomic.t }

type t

(* None when stderr is not a terminal that can redraw a line, in CI, and
   when the drawing thread cannot be started. Until [finish], the messages
   of the Logs reporter that writes to stderr go to the status line's queue
   (see Logs_.redirect_stderr).
   While the status line is drawn, SIGINT, SIGTERM, SIGHUP and SIGQUIT erase
   it and show the cursor before they end the process as they would
   otherwise, and Ctrl-Z (SIGTSTP) stops the drawing for the rest of the scan
   before the process stops. A signal that was ignored when the process
   started stays ignored. *)
val create : phase -> t option

val set_phase : t -> phase -> unit

(* Counts one completed work item, a target or an interfile rule; ignored
   outside the scanning phase, and safe to call from any domain. *)
val notify_work_item_done : t -> unit

val progress_hook : t option -> Core_scan_config.progress -> unit

(* Stops the drawing thread, writes the messages still queued, clears the
   line and restores the signal handlers that [create] replaced; later log
   messages go to stderr again. Only the first call has an effect; a run
   that ends by [exit] calls it from at_exit. *)
val finish : t -> unit
