let catch ~default f =
  try f ()
  with
  | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as e -> raise e
  | e ->
    Log_projidx.Log.debug (fun m ->
      m "nonfatal: swallowed %s" (Printexc.to_string e));
    default
