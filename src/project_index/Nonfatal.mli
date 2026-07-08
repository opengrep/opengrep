(* Run [f ()]; on a non-fatal exception return [default]. [Out_of_memory],
   [Stack_overflow], [Time_limit.Timeout] are re-raised (same fatal set as
   [run_visit]). *)
val catch : default:'a -> (unit -> 'a) -> 'a
