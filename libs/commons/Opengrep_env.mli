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

val getenv_nonempty : string -> string option
(** [getenv_nonempty var] looks up exactly [var], with no alias, treating an
    empty value as unset. Use it to tell whether a specific name is set; use
    {!getenv_opt} to read a variable's value. *)

val getenv_with_name_opt : string -> (string * string) option
(** [getenv_with_name_opt var] is {!getenv_opt} paired with the name of the
    variable the value was read from, so that a diagnostic can name the
    OPENGREP_* alias or the legacy SEMGREP_* name the user actually set. *)

val getenv_opt : string -> string option
(** [getenv_opt var] looks up [var] in the environment, preferring its
    OPENGREP_* alias (see {!opengrep_alias}) when that alias is set, so that
    OPENGREP_* variables take precedence over the legacy SEMGREP_* ones. Empty
    values are treated as unset. Its signature matches the [~env] lookup
    expected by {!Cmdliner.Cmd.eval_value}. *)
