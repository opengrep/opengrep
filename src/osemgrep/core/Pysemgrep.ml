open Common

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Temporary module while migrating code to osemgrep to fallback to
 * pysemgrep when osemgrep does not handle yet certain options.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)
exception Fallback

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

let exec (caps : < Cap.exec >) prog argv =
  let prog =
    match UCmd.resolve_program (Cmd.Name prog, []) with
    | Ok cmd -> Bos.Cmd.get_line_tool cmd
    | Error (`Msg _) -> raise (Unix.Unix_error (Unix.ENOENT, "exec", prog))
  in
  if Sys.os_type = "Win32" then
    let pid = CapUnix.create_process caps#exec prog argv Unix.stdin Unix.stdout Unix.stderr in
    let _pid, process_status = CapUnix.waitpid caps#exec [Unix.WUNTRACED] pid in
    match process_status with
    | Unix.WEXITED exit_code -> exit exit_code
    | _ -> assert false (* On Windows, only WEXITED is used. *)
  else
    CapUnix.execvp caps#exec prog argv

(* dispatch back to pysemgrep! *)
let pysemgrep (caps : < Cap.exec >) argv =
  Logs.debug (fun m ->
      m "execute pyopengrep: %s"
        (argv |> Array.to_list
        |> List_.map (fun arg -> spf "%S" arg)
        |> String.concat " "));
  (* pysemgrep should be in the PATH, thx to the code in
     ../../../cli/bin/semgrep *)
  match Sys.getenv_opt "_OPENGREP_BINARY" with
  | Some opengrep_bin ->
    exec
      caps
      opengrep_bin
      (Array.concat [[|argv.(0)|];
                     [| "--legacy" |]; (* forces `pyopengrep` *)
                     Array.sub argv 1 (Array.length argv - 1)])
  | None ->
    exec caps "pyopengrep" argv
