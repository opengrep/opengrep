(* A line at the bottom of the terminal saying what the scan is doing, with
   a spinner and a progress bar once the scan reaches its targets.

   It is drawn by a thread of its own, the only writer to the terminal while
   the bar is up: log messages are queued and written between two frames,
   and the thread writes to a copy of stderr, so that a capture of stderr
   never sees a frame. The caller stops it before printing the report. *)

type phase =
  | Loading_rules
  | Analyzing_targets
  | Building_interfile_graph
  (* the baseline replay of a differential scan; see Status_bar.ml *)
  | Comparing_with_baseline
  (* targets and interfile rules counted as one; see Status_bar.ml *)
  | Scanning of { total : int; completed : int Atomic.t }

type t

(* None off a terminal, where there is nothing to animate and a redrawn line
   would only pile up, and when the thread that draws it cannot be started.
   Until [finish], the messages of the Logs reporter that writes to stderr
   go to the bar's queue (see Logs_.divert_stderr).
   While the bar is up, SIGINT, SIGTERM, SIGHUP and SIGQUIT erase it and
   show the cursor before they kill the process as they would have, and
   Ctrl-Z (SIGTSTP) puts it away for the rest of the scan before the
   process stops. A signal the process was started with ignored stays
   ignored. *)
val create : phase -> t option

val set_phase : t -> phase -> unit

(* One more unit of work finished, target or interfile rule alike; ignored
   outside the scanning phase, and safe to call from any domain. *)
val notify_work_item_done : t -> unit

(* Stops the thread, writes the messages still queued, clears the line and
   puts back the signal handlers that [create] replaced; from then on log
   messages go to stderr again. Calling it more than once is harmless; only
   the first call does the work, and a run that ends by [exit] calls it
   then. *)
val finish : t -> unit
