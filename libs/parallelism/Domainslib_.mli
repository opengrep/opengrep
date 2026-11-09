(* From [Parmap_] interface. *)
(* internal helper useful outside if you want to reproduce with
 * List.map the semantic of Parmap_.parmap()
 *)
val wrap_result :
  ('a -> 'b) ->
  exception_handler:('a -> Exception.t -> 'err) ->
  'a ->
  ('b, 'err) result

val parmap :
  < Cap.fork > ->
  ?chunksize:int ->
  num_domains:int ->
  exception_handler:('b -> Exception.t -> 'c) ->
  ('b -> 'd) ->
  'b list ->
  ('d, 'c) result list
(** [parmap caps ?init ?finalize ~ncores ~chunksize ~exception_handler f xs] is
    like [Parmap.parmap], but will return a result, containing [Ok(f x)] if it
    succeeds, or if an exception is raised while [f x] is being computed and is
    not caught, the result will contain [Error (exception_handler x e)] where
    [e] is the caught exception.
*)

val get_cpu_count : unit -> int
(** Return the number of domains, kept original name for compatibility. *)

val default_num_domains : unit -> int
(** The number of domains a scan runs on by default: at most 16, and at most
    [get_cpu_count ()]. *)

val num_domains_of_jobs : int -> int
(** The number of domains for [-j jobs], as the v1.33.1 command line selected
    it: at most [get_cpu_count ()], and [default_num_domains ()] for 0 or
    less. *)
