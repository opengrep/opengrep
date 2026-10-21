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

let path_sep : char = if Sys.win32 then ';' else ':'

let absolute_path_entries () : string list =
  Option.value ~default:"" (USys.getenv_opt "PATH")
  |> String.split_on_char path_sep
  |> List.filter (fun (dir : string) ->
         match Fpath.of_string dir with
         | Ok path -> Fpath.is_abs path
         | Error _ -> false)

let restrict_program_search () : unit =
  if Option.is_some (USys.getenv_opt "PATH") then
    UUnix.putenv "PATH"
      (String.concat (String.make 1 path_sep) (absolute_path_entries ()));
  if Sys.win32 then UUnix.putenv "NoDefaultCurrentDirectoryInExePath" "1"

let resolve_program (cmd : Cmd.t) : (Bos.Cmd.t, [> Rresult.R.msg ]) result =
  let Cmd.Name name, _ = cmd in
  let file_name =
    if Sys.win32 && not (String.ends_with ~suffix:".exe" name) then
      name ^ ".exe"
    else name
  in
  (* Bos.OS.Cmd.resolve accepts any entry that passes an execute access
     check, a directory included *)
  let search =
    absolute_path_entries ()
    |> List.filter_map (fun (dir : string) ->
           if
             UFile.is_reg ~follow_symlinks:true
               (Fpath.v (Filename.concat dir file_name))
           then Some (Fpath.v dir)
           else None)
  in
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
