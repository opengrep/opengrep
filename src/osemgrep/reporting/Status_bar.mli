type phase =
  | Loading_rules
  | Analyzing_targets
  | Scanning of { total : int Atomic.t; completed : int Atomic.t }

type t

val should_enable : Maturity.t -> bool
(** [should_enable maturity] returns [true] when [maturity] is
    [Experimental] or [Develop], stderr is a TTY, and [Sys.unix] is true. *)

val create : Maturity.t -> phase -> t option
(** [create maturity phase] returns [Some t] if [should_enable maturity],
    starting a background render thread. Returns [None] otherwise. *)

val set_phase : t -> phase -> unit
(** Thread-safe phase update. *)

val notify_file_done : t -> unit
(** Lock-free [Atomic.incr] on the scanning counter.
    Safe to call from parallel domains. *)

val finish : t -> unit
(** Stops the render thread, erases the status line, and restores the cursor. *)
