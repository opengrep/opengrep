(* A line at the bottom of the terminal saying what the scan is doing, with
   a spinner and a progress bar once the scan reaches its targets.

   It is drawn by a thread of its own and coordinates with the log reporter
   and with stderr capture, so a message is never cut in half and a redraw
   never lands in captured output. The caller stops it before printing the
   report. *)

type phase =
  | Analyzing_targets
  | Building_interfile_graph
  | Scanning of {
      targets : int;
      targets_done : int Atomic.t;
      interfile_rules : int;
      interfile_rules_done : int Atomic.t;
    }

type t

(* None off a terminal, where there is nothing to animate and a redrawn line
   would only pile up. *)
val create : phase -> t option

val set_phase : t -> phase -> unit

(* One more unit of work finished; ignored outside the scanning phase, and
   safe to call from any domain. *)
val notify_target_done : t -> unit
val notify_interfile_rule_done : t -> unit

(* Stops the thread, unhooks, and clears the line. Call it once: it joins
   the render thread. *)
val finish : t -> unit
