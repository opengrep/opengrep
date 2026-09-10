(* Run [f ()]; on a non-fatal exception log a warning and return [default].
   [Out_of_memory], [Stack_overflow] are re-raised.
   [?on] names the file being processed in the warning. *)
val catch : ?on:Fpath.t -> default:'a -> (unit -> 'a) -> 'a
