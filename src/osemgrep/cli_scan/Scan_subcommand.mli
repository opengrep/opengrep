(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main caps [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.

   Note that this subcommand can also calls the 'test', 'validate', and 'show'
   subcommands when using legacy flags (e.g., with 'semgrep scan --test').
*)

type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t
val run_scan_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t

(* internal: also used in CI *)
val rules_from_rules_source :
  ?skip_invalid_configs:bool ->
  rewrite_rule_ids:bool ->
  strict:bool ->
  < Cap.network ; Cap.tmp > ->
  Rule_fetching.source ->
  Rule_fetching.rules_and_origin list * Rule_error.t list

(* internal: also used in CI *)
val core_errors_of_fatal_rule_errors : Rule_error.t list -> Core_error.t list

(* internal: also used in CI. text_message is what text mode reports (it
 * raises Semgrep_error with it); the other formats output the errors.
 * The exit code is the code of the first error. *)
val output_and_exit_from_fatal_core_errors_exn :
  text_message:string ->
  < Cap.stdout > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Core_error.t list ->
  Exit_code.t

(* internal: also used in CI. The targets of the scanning roots, or the exit
 * code after reporting the roots that do not exist. *)
val get_targets_or_exit :
  < Cap.stdout > ->
  Scan_CLI.conf ->
  Profiler.t ->
  (Fpath.t Find_targets.targets, Exit_code.t) result

(* internal: also used in CI.
 * print_summary is python's output(print_summary=...): 'opengrep ci' passes
 * false and prints its own completion lines instead of "Ran N rules ...".
 *)
val check_targets_with_rules :
  ?print_summary:bool ->
  (* caps - network *)
  < Cap.stdout
  ; Cap.chdir
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit
  ; .. > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fpath.t Find_targets.targets ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
