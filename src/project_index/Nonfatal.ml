let catch ~default f =
  try f ()
  with
  | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as e -> raise e
  | e ->
    (* Default-visible ([Logs], not [Log_projidx]): an error here degrades
       call-graph coverage, which must never be silent. *)
    Logs.warn (fun m ->
      m "projidx: failed, item skipped: %s" (Printexc.to_string e));
    default
