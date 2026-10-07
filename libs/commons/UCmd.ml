open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Bos.OS.Cmd
 *
 * A few functions contain a 'nosemgrep: forbid-exec' because anyway
 * those functions will/are also blacklisted in forbid-exec.jsonnet.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Log every external command.

   Let's not log environment variables because they may contain sensitive
   secrets.
   Note that we're using Logs.info below on purpose; this is probably
   something the user wants to know.
*)
let log_command cmd =
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m ->
      m "Running external command: %s" (Redact.apply (Cmd.to_string cmd)))

(* Capture error output and log it at the same level as 'log_command' above.

   This uses a utility function from Testo which is weird since it's
   not a general-purpose library. Bos doesn't seem to provide a simple
   equivalent (?)
*)
let capture_and_log_stderr func =
  let res, err = Testo.with_capture UStdlib.stderr func in
  if err <> "" then
    (* nosemgrep: no-logs-in-library *)
    Logs.info (fun m -> m "error output: %s" (Redact.apply err));
  res

let resolve_program (cmd : Cmd.t) : (Bos.Cmd.t, [> Rresult.R.msg ]) result =
  let/ dirs =
    Bos.OS.Cmd.search_path_dirs
      (Option.value ~default:"" (USys.getenv_opt "PATH"))
  in
  let search = List.filter Fpath.is_abs dirs in
  Cmd.bos_apply (Bos.OS.Cmd.resolve ~search) cmd

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let string_of_run ~trim cmd =
  log_command cmd;
  capture_and_log_stderr (fun () ->
      (* nosemgrep: forbid-exec *)
      let out = Result.map Bos.OS.Cmd.run_out (resolve_program cmd) in
      (* nosemgrep: forbid-exec *)
      Result.bind out (Bos.OS.Cmd.out_string ~trim))

(* The method of using Testo.with_capture here is odd, but is copied from
 * capture_and_log_stderr as defined above--see that function for the
 * reasoning for doing it this way. *)
(* TODO: this is potentially a source of high memory usage if the captured program
 * outputs a lot of log spew. We should add a limit on the data read. *)
let string_of_run_with_stderr ~trim cmd =
  log_command cmd;
  let res, err =
    Testo.with_capture UStdlib.stderr (fun () ->
        (* nosemgrep: forbid-exec *)
        let out = Result.map Bos.OS.Cmd.run_out (resolve_program cmd) in
        (* nosemgrep: forbid-exec *)
        Result.bind out (Bos.OS.Cmd.out_string ~trim))
  in
  (res, err)

let lines_of_run ~trim cmd =
  log_command cmd;
  capture_and_log_stderr (fun () ->
      (* nosemgrep: forbid-exec *)
      let out = Result.map Bos.OS.Cmd.run_out (resolve_program cmd) in
      (* nosemgrep: forbid-exec *)
      Result.bind out (Bos.OS.Cmd.out_lines ~trim))

(* nosemgrep: forbid-exec *)
let status_of_run ?quiet cmd =
  log_command cmd;
  capture_and_log_stderr (fun () ->
      (* nosemgrep: forbid-exec *)
      Result.bind (resolve_program cmd) (Bos.OS.Cmd.run_status ?quiet))
