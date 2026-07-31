(** Compatibility between the legacy Semgrep environment variable names and the
    Opengrep ones.

    Opengrep is a fork of Semgrep, so historically many environment variables
    are named SEMGREP_*. We now also support OPENGREP_* variants, giving them
    precedence over the legacy SEMGREP_* names when both are set. *)

val opengrep_alias : string -> string option
(** [opengrep_alias var] is the OPENGREP_* name corresponding to a legacy
    SEMGREP_* [var], obtained by replacing the "SEMGREP" substring with
    "OPENGREP" (e.g., "SEMGREP_URL" -> "OPENGREP_URL",
    "PYTEST_SEMGREP_LOG_LEVEL" -> "PYTEST_OPENGREP_LOG_LEVEL"), or [None] when
    [var] contains no "SEMGREP" substring. *)

val getenv_opt : string -> string option
(** [getenv_opt var] looks up [var] in the environment, preferring its
    OPENGREP_* alias (see {!opengrep_alias}) when that alias is set, so that
    OPENGREP_* variables take precedence over the legacy SEMGREP_* ones. Empty
    values are treated as unset. Its signature matches the [~env] lookup
    expected by {!Cmdliner.Cmd.eval_value}. *)
