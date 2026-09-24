(* A line at the bottom of the terminal saying what the scan is doing, with
   a spinner and a progress bar once the scan reaches its targets.

   It is drawn by a thread of its own and coordinates with the log reporter
   and with stderr capture, so a message is never cut in half and a redraw
   never lands in captured output. The caller stops it before printing the
   report. *)

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
   would only pile up. *)
val create : phase -> t option

val set_phase : t -> phase -> unit

(* One more unit of work finished, target or interfile rule alike; ignored
   outside the scanning phase, and safe to call from any domain. *)
val notify_work_item_done : t -> unit

(* Stops the thread, unhooks, and clears the line. Calling it more than
   once is harmless; only the first call does the work. *)
val finish : t -> unit
