(* Run [f ()]; on a non-fatal exception log a warning and return [default].
   [Out_of_memory], [Stack_overflow] are re-raised.
   [?on] is the file being processed, printed in the warning. *)
val catch : ?on:Fpath.t -> default:'a -> (unit -> 'a) -> 'a

(* [catch], logging at info level, which --verbose and --debug show. *)
val catch_info : ?on:Fpath.t -> default:'a -> (unit -> 'a) -> 'a
