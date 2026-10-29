let catch_at (level : Logs.level) ?(on : Fpath.t option) ~(default : 'a)
    (thunk : unit -> 'a) : 'a =
  try thunk ()
  with
  | Out_of_memory | Stack_overflow as exn -> raise exn
  | exn ->
    (* [Logs], not [Log_projidx]: the default source is shown without
       OPENGREP_LOG_SRCS. *)
    let where = match on with
      | None -> ""
      | Some file -> Fpath.to_string file ^ ": "
    in
    Logs.msg level (fun m ->
      m "projidx: %sfailed, item skipped: %s" where (Printexc.to_string exn));
    default

(* Default-visible: an error here degrades call-graph coverage, which must
   never be silent. *)
let catch ?(on : Fpath.t option) ~(default : 'a) (thunk : unit -> 'a) : 'a =
  catch_at Logs.Warning ?on ~default thunk

let catch_info ?(on : Fpath.t option) ~(default : 'a) (thunk : unit -> 'a)
    : 'a =
  catch_at Logs.Info ?on ~default thunk
