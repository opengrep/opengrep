(*
   Typed access to Semgrep environment variables (e.g., SEMGREP_IN_DOCKER).
 *)

(* Most (typed) fields below correspond directly to an uppercase environment
 * metavariable with the SEMGREP_ prefix.
 * For example, [git_command_timeout] will contain the content of
 * SEMGREP_GIT_COMMAND_TIMEOUT in the environment (or a default value if
 * it's not in the environment).
 *)
type t = {
  (* $SEMGREP_URL | $SEMGREP_APP_URL *)
  semgrep_url : Uri.t;
  (* $SEMGREP_VERSION_CHECK_URL *)
  version_check_url : Uri.t;
  version_check_timeout : int;
  (* .cache/semgrep_version *)
  version_check_cache_path : Fpath.t;
  git_command_timeout : int;
  (* "/src" *)
  src_directory : Fpath.t;
  (* XDG_CONFIG_HOME on *nix, USERPROFILE on windows, or default to HOME or / *)
  user_home_dir : Fpath.t;
  (* user_home_dir/.semgrep *)
  user_dot_semgrep_dir : Fpath.t;
  (* $SEMGREP_LOG_FILE or ~/.semgrep/semgrep.log  *)
  user_log_file : Fpath.t;
  (* TODO: Reconcile $SEMGREP_FORCE_COLOR via o_force_color *)
  (* ($NO_COLOR | $SEMGREP_COLOR_NO_COLOR) *)
  no_color : bool;
  is_ci : bool;
  in_docker : bool;
  (* $GITHUB_WORKSPACE *)
  in_gh_action : bool;
  (* $SEMGREP_xxx *)
  min_fetch_depth : int;
  (* $MOCK_USING_REGISTRY *)
  mock_using_registry : bool;
  in_test : bool;
}

val v : t ref
(** [v] is a reference to environment variables and user settings gathered
  * during initialization. This is a reference to allow these settings to
  * be modified by tests.
  *)

(* useful to work in cunjonction with with_envvars in testing context *)
val of_current_sys_env : unit -> t

val with_envvars : t -> (unit -> 'a) -> 'a
(** [with_envvars env f] temporarily modifies [v] above with a new [env]
 * and run [f] in this new context. This is useful in tests.
 *)

val with_envvar : string -> string -> (unit -> 'a) -> 'a
(** [with_envvar envvar value f] temporarily modifies [v] above
 * with a new envvar and run [f] in this new context. This is useful
 * in tests.
 *)
