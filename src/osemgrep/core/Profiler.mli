(** A really basic profiler.

    This module implements a very basic tool for recording the time consumed by
    a user-specified key. Its use corresponds to:

    {[
      let run profiler =
        Profiler.start profiler ~name:"my computation";
        my_computation ();
        Profiler.stop_ign profiler ~name:"my computation"

      let () =
        let profiler = Profiler.make () in
        run profiler;
        Profiler.elapsed profiler ~name:"my computation"
        |> Option.iter (fun time ->
               Format.printf "%S took %fs\n%!" "my computation" time)
    ]}

    We can only record sequential tasks - the use of lwt {b is not} recommended.
    {!module:Profiling} also provides a poor's man profiler but here we want to
    restrict profiling to broad categories that are compatible with [pysemgrep]
    and needed by {!module:Metrics_}.
*)

type t = (string, value) Hashtbl.t
and value = Start of float | Recorded of float
(** The type of profilers. *)

val make : unit -> t
(** [make ()] creates a new profiler. *)

val start : t -> name:string -> unit
(** [start profiler ~name] starts to record [name]. *)

val stop_ign : t -> name:string -> unit
(** [stop_ign profiler ~name] stops to profile [name] and record the time spent
    internally. If [name] does not exist or is already recorded, it does
    nothing. *)

val record : t -> name:string -> (unit -> 'a) -> 'a
(** [record t ~name fn] records the time spent by the given [fn] and save it
    into the profiler with the name [name]. *)

val elapsed : t -> name:string -> float option
(** [elapsed profiler ~name] is the time recorded for [name], or the time
    spent so far when [name] is still being recorded. *)
