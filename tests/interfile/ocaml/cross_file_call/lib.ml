let leak v =
  (* todoruleid: cross-file-call-ocaml (OCaml taint reports no flow, even within a function) *)
  sink v
