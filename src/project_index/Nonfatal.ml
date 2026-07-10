let catch ?(on : Fpath.t option) ~default thunk =
  try thunk ()
  with
  | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as exn -> raise exn
  | exn ->
    (* Default-visible ([Logs], not [Log_projidx]): an error here degrades
       call-graph coverage, which must never be silent. *)
    let where = match on with
      | None -> ""
      | Some file -> Fpath.to_string file ^ ": "
    in
    Logs.warn (fun m ->
      m "projidx: %sfailed, item skipped: %s" where (Printexc.to_string exn));
    default
