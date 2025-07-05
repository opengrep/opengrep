open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Typed access to Semgrep environment variables (e.g., SEMGREP_IN_DOCKER).

   Translated from env.py.

   There are other Semgrep environment variables which are not mentioned
   in this file because their value is accessed by Cmdliner
   in Scan_CLI.ml (SEMGREP_BASELINE_COMMIT, SEMGREP_SEND_METRICS,
   SEMGREP_TIMEOUT, and SEMGREP_RULES).

   TODO: Maybe we should make Env.v a lazy value. If we get an
   exn for any reason during the init, it will be raised even before
   main() is called, which leaves no room for error handling and
   better error messaging.

   TODO: switch to cmdliner, like we did in Git_metadata.mli so
   those variables can be combined in xxx_CLI.ml and be part
   of the man pages of those commands!
*)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this causes parse failure in codemap/efuns so commented for now *)
(*
let%test_unit "Semgrep_envvars.(/)" =
  [%test_eq: Base.string] ("a/b/" / "c/d" / "foo.c") "a/b/c/d/foo.c"
*)

(*
   Treat environment variables with an empty value as if they were unset.

   Since OCaml doesn't provide an 'unsetenv' function (which exists in libc),
   tests that that set environment variable temporarily can't unset them,
   leaving them with the empty value instead.
*)
let env_opt var =
  match Sys.getenv_opt var with
  | Some "" -> None
  | x -> x

(*****************************************************************************)
(* Don't use Sys.getenv* or Unix.getenv* starting from here!  *)
(*****************************************************************************)
(*
   TODO: ensure that the whole application uses our 'env_opt' function.
*)

let env_or conv var default =
  match env_opt var with
  | None -> default
  | Some x -> conv x

let in_env var = env_opt var <> None

let env_truthy var =
  env_opt var |> Option.value ~default:"" |> String.lowercase_ascii |> function
  | "true"
  | "1"
  | "yes"
  | "y" ->
      true
  | _ -> false

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* TODO: could we use deriving to automate the generation of
 * env below? [@default = ...] or use ATD?
 *)
type t = {
  semgrep_url : Uri.t;
  version_check_url : Uri.t;
  version_check_timeout : int;
  version_check_cache_path : Fpath.t;
  git_command_timeout : int;
  src_directory : Fpath.t;
  user_home_dir : Fpath.t;
  user_dot_semgrep_dir : Fpath.t;
  user_log_file : Fpath.t;
  no_color : bool;
  is_ci : bool;
  in_docker : bool;
  in_gh_action : bool;
  (* deprecated *)
  min_fetch_depth : int;
  (* TODO(reynir): is this deprecated?! *)
  mock_using_registry : bool;
  in_test: bool;
}

(* What about temp? Well we use ocaml stdlib definition of a temp directory.
   This is fine EXCEPT on windows. stdlib on windows expects TEMP (not TMP)
   to be set, and if it's not (which it isn't by default), it uses "." :(
*)

(* less: make it Lazy? so at least not run in ocaml init time before main() *)
let of_current_sys_env () : t =
  let user_home_dir =
    let home_env_var =
      (* In windows USERPROFILE=C:\Users\<user> *)
      if Sys.win32 then "USERPROFILE" else "XDG_CONFIG_HOME"
    in
    match env_opt home_env_var with
    | Some x when Sys.is_directory x -> Fpath.v x
    | Some _
    | None ->
        Fpath.v (env_or (fun x -> x) "HOME" "/")
  in
  let user_dot_semgrep_dir = user_home_dir / ".semgrep" in (* TODO: This should be `.opengrep` now. *)
  {
    (* semgrep_url is set by env vars $SEMGREP_URL | $SEMGREP_APP_URL, or default *)
    semgrep_url =
      env_opt "SEMGREP_URL"
      |> Option.value
           ~default:
             (env_opt "SEMGREP_APP_URL"
             |> Option.value ~default:"https://semgrep.dev")
      |> Uri.of_string;
    (* fail_open_url =
         env_or Uri.of_string "SEMGREP_FAIL_OPEN_URL"
           (Uri.of_string "https://fail-open.prod.semgrep.dev/failure"); *)
    (* integration_name can take a label like "funkyintegration" for custom partner integrations *)
    version_check_url =
      env_or Uri.of_string "OPENGREP_VERSION_CHECK_URL"
        (Uri.of_string "https://opengrep.dev/api/check-version");
    version_check_timeout =
      env_or int_of_string "OPENGREP_VERSION_CHECK_TIMEOUT" 2;
    version_check_cache_path =
      env_or Fpath.v "OPENGREP_VERSION_CACHE_PATH"
        (Fpath.v (Sys.getcwd ()) / ".cache" / "opengrep_version");
    git_command_timeout = env_or int_of_string "SEMGREP_GIT_COMMAND_TIMEOUT" 300;
    src_directory = env_or Fpath.v "SEMGREP_SRC_DIRECTORY" (Fpath.v "/src");
    user_home_dir;
    user_dot_semgrep_dir;
    user_log_file =
      env_or Fpath.v "OPENGREEP_LOG_FILE" (user_dot_semgrep_dir / "semgrep.log");
    no_color = env_truthy "NO_COLOR" || env_truthy "SEMGREP_COLOR_NO_COLOR";
    is_ci = in_env "CI";
    in_docker = in_env "SEMGREP_IN_DOCKER";
    in_gh_action = in_env "GITHUB_WORKSPACE";
    min_fetch_depth = env_or int_of_string "SEMGREP_GHA_MIN_FETCH_DEPTH" 0;
    mock_using_registry = in_env "MOCK_USING_REGISTRY";
    in_test = in_env "OPENGREP_IN_TEST";
  }

(* less: make it Lazy? so at least not run in ocaml init time before main() *)
let v : t ref = ref (of_current_sys_env ())

(*****************************************************************************)
(* For testing *)
(*****************************************************************************)

let with_envvars newv f = Common.save_excursion_unsafe v newv f

let with_envvar envvar str f =
  Testutil_mock.with_setenv envvar str (fun () ->
      with_envvars (of_current_sys_env ()) f)
