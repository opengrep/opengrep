val run_core_search :
  Match_env.xconfig ->
  Rule.search_rule ->
  Fpath.t ->
  Core_result.processed_match list option
(** [run_core_search] runs a search intended for the /semgrep/search IDE
    search command, by hooking lower into the Match_search_mode matching
    process, bypassing the CLI.
  *)

val run_semgrep :
  ?targets:Fpath.t list ->
  ?rules:Rule.t list ->
  ?git_ref:string ->
  Session.t ->
  Semgrep_output_v1_t.cli_match list * Fpath.t list
(** [run_semgrep session] runs a scan with the session's cached rules (or
    [rules] if provided) on the session's targets (or [targets] if provided)
    and returns the processed matches and the list of scanned files. This is
    the scan primitive shared with 'opengrep server'. *)

val run_semgrep_detached :
  ?targets:Fpath.t list ->
  ?rules:Rule.t list ->
  ?git_ref:string ->
  Session.t ->
  (Semgrep_output_v1_t.cli_match list * Fpath.t list) Lwt.t
(** [run_semgrep_detached session] is [run_semgrep] on a detached preemptive
    thread so it does not block the Lwt event loop. *)

val scan_workspace : Session.t -> Lsp_.Reply.t
(** [scan_workspace server] scans the workspace of the given session. *)

val scan_open_documents : Session.t -> Lsp_.Reply.t
(** [scan_open_documents server] scans the open documents of the given session. *)

val scan_file : Session.t -> Lsp__Uri0.t -> Lsp_.Reply.t
(** [scan_file server] scans the given file. If [content] is provided, it will
  * be used as the content of the file. Otherwise, the content will be read
  * from the file system.
  *)

val refresh_rules : Session.t -> Lsp_.Reply.t
(** [refresh_rules server] refreshes the rules of the given session. *)
