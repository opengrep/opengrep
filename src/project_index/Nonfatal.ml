let catch ~default thunk =
  try thunk ()
  with
  | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as exn -> raise exn
  | exn ->
    (* Default-visible ([Logs], not [Log_projidx]): an error here degrades
       call-graph coverage, which must never be silent. *)
    Logs.warn (fun m ->
      m "projidx: failed, item skipped: %s" (Printexc.to_string exn));
    default
