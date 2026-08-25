module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term
module H = Cmdliner_
module SC = Scan_CLI

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 'opengrep ci' command-line parsing.
 *
 * The flags are the scan_options subset that the ci command of ci.py
 * accepts, plus the ci-only flags, minus flags with no effect here:
 * --oss-only, --supply-chain, --code, --autofix and the --x-* internals.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = {
  (* exit 0 for the events named here even with blocking findings *)
  audit_on : string list;
  (* errors other than findings exit 0 (python: ErrorHandler.suppress) *)
  suppress_errors : bool;
  (* scan only this subdirectory of the current directory *)
  subdir : string option;
  (* the raw --baseline-commit rev, handed to the metadata layer as the
   * cli_baseline_ref (the scan's baseline is set from the metadata) *)
  baseline_commit : string option;
  (* accepted like in ci.py but ignored by ci, warned about at runtime;
   * their Scan_CLI.conf counterparts are scrubbed to the defaults *)
  opengrep_ignore_pattern : string option;
  inline_metavariables : bool;
  (* 'opengrep ci' shares most of its flags with 'opengrep scan' *)
  scan_conf : Scan_CLI.conf;
}
[@@deriving show]

(*************************************************************************)
(* 'ci' only command-line flags *)
(*************************************************************************)

let o_audit_on : string list Term.t =
  let info =
    Arg.info [ "audit-on" ]
      ~env:(Cmd.Env.info "SEMGREP_AUDIT_ON")
      ~doc:
        {|Exit with code 0 even with blocking findings when the CI event
matches one of the given names (e.g., --audit-on push).|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_suppress_errors : bool Term.t =
  H.negatable_flag_with_env [ "suppress-errors" ]
    ~neg_options:[ "no-suppress-errors" ]
    ~env:"SEMGREP_SUPPRESS_ERRORS"
    ~default:true
    ~doc:
      {|Configures how the CI command reacts when an error occurs.
If true, encountered errors are suppressed and the exit code is zero (success).
If false, encountered errors are not suppressed and the exit code is non-zero
(failure).|}

(* for monorepos *)
let o_subdir : string option Term.t =
  let info =
    Arg.info [ "subdir" ]
      ~doc:
        {|Scan only a subdirectory of the current directory. Expects a
relative path.|}
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

(*************************************************************************)
(* Turn argv into conf *)
(*************************************************************************)

(* coupling: Scan_CLI.cmdline_term *)
let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
     of the corresponding '$ o_xx $' further below! *)
  let combine allow_local_builds allow_rule_timeout_control
      apply_includes_excludes_to_files audit_on baseline_commit common config
      dataflow_traces dynamic_timeout
      dynamic_timeout_max_multiplier dynamic_timeout_unit_kb emacs
      emacs_outputs exclude_ exclude_rule_ids files_with_matches force_color
      gitlab_sast gitlab_sast_outputs gitlab_secrets gitlab_secrets_outputs
      include_ inline_metavariables json json_outputs junit_xml
      junit_xml_outputs matching_explanations max_chars_per_line
      max_lines_per_finding max_log_list_entries max_match_per_file
      max_memory_mb max_target_bytes nosem num_jobs opengrep_ignore_pattern
      optimizations output rewrite_rule_ids sarif sarif_outputs
      scan_unknown_extensions subdir suppress_errors taint_intrafile text
      text_outputs time_flag timeout _timeout_interfileTODO timeout_threshold
      use_git version_check vim vim_outputs =
    let output_format : Output_format.t =
      SC.output_format_conf ~text ~files_with_matches ~json ~emacs ~vim ~sarif
        ~gitlab_sast ~gitlab_secrets ~junit_xml
    in
    let outputs =
      SC.outputs_conf ~text_outputs ~json_outputs ~emacs_outputs ~vim_outputs
        ~sarif_outputs ~gitlab_sast_outputs ~gitlab_secrets_outputs
        ~junit_xml_outputs
    in
    let output_conf : Output.conf =
      {
        output_format;
        output;
        outputs;
        max_chars_per_line;
        max_lines_per_finding;
        force_color;
        show_dataflow_traces = dataflow_traces;
        strict = false;
        (* python: ci always runs the scan with dryrun=True *)
        fixed_lines = true;
        skipped_files =
          (match common.CLI_common.logging_level with
          | Some (Info | Debug) -> true
          | _else_ -> false);
        max_log_list_entries;
        is_ci_invocation = true;
      }
    in
    let rules_source : Rules_source.t =
      (* python: config = config or (AUTO_CONFIG_KEY,) *)
      match config with
      | [] -> Rules_source.Configs [ "auto" ]
      | configs -> Rules_source.Configs configs
    in
    let engine_config : Engine_config.t =
      {
        (* --opengrep-ignore-pattern is accepted but ignored by ci *)
        Engine_config.custom_ignore_pattern = None;
        taint_intrafile = Some taint_intrafile;
      }
    in
    let core_runner_conf : Core_runner.conf =
      {
        Core_runner.num_jobs;
        optimizations;
        timeout;
        dynamic_timeout;
        dynamic_timeout_max_multiplier;
        dynamic_timeout_unit_kb;
        allow_rule_timeout_control;
        timeout_threshold;
        max_memory_mb;
        max_match_per_file;
        dataflow_traces;
        (* --enable-nosem; the engine still annotates the matches, and the
           ignored ones are dropped in check_targets_with_rules *)
        nosem;
        strict = false;
        time_flag;
        (* --inline-metavariables is accepted but ignored by ci *)
        inline_metavariables = false;
        matching_explanations;
        taint_intrafile;
        effect_guards = false;
        engine_config;
      }
    in
    let include_ =
      match include_ with
      | [] -> None
      | nonempty -> Some nonempty
    in
    let targeting_conf : Find_targets.conf =
      {
        force_project_root = None;
        force_novcs_project = not use_git;
        exclude = exclude_;
        include_;
        apply_includes_excludes_to_file_targets =
          apply_includes_excludes_to_files;
        (* set from the CI metadata in Ci_subcommand.run_conf *)
        baseline_commit = None;
        max_target_bytes;
        always_select_explicit_targets = scan_unknown_extensions;
        explicit_targets = Find_targets.Explicit_targets.empty;
        respect_gitignore = use_git;
        respect_semgrepignore_files = true;
        semgrepignore_filename = None;
        exclude_minified_files = false;
      }
    in
    let rule_filtering_conf : Rule_filtering.conf =
      {
        Rule_filtering.exclude_rule_ids =
          List_.map Rule_ID.of_string_exn exclude_rule_ids;
        severity = [];
        exclude_products = [];
      }
    in
    let matching_conf : Match_patterns.matching_conf =
      { Match_patterns.track_enclosing_context = false }
    in
    if include_ <> None && exclude_ <> [] then
      Logs.warn (fun m ->
          m
            "Paths that match both --include and --exclude will be skipped by \
             Opengrep.");
    let scan_conf : Scan_CLI.conf =
      {
        rules_source;
        (* the target is the current directory (or --subdir), set in
           Ci_subcommand.run_conf *)
        target_roots = [];
        rule_filtering_conf;
        targeting_conf;
        core_runner_conf;
        error_on_findings = false;
        (* like in ci.py: ci reports and gates, it never modifies the
           checkout *)
        autofix = false;
        (* accepted like in ci.py, where the version check is inert *)
        version_check;
        output_conf;
        incremental_output = false;
        incremental_output_postprocess = false;
        rewrite_rule_ids;
        skip_invalid_configs = SC.default.skip_invalid_configs;
        matching_conf;
        common;
        version = false;
        show = None;
        validate = None;
        test = None;
        allow_local_builds;
        ls = false;
        ls_format = Ls_subcommand.default_format;
      }
    in
    {
      audit_on;
      suppress_errors;
      subdir;
      baseline_commit;
      opengrep_ignore_pattern;
      inline_metavariables;
      scan_conf;
    }
  in
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ SC.o_allow_local_builds $ SC.o_allow_rule_timeout_control
    $ SC.o_apply_includes_excludes_to_files $ o_audit_on
    $ SC.o_baseline_commit $ CLI_common.o_common $ SC.o_config
    $ SC.o_dataflow_traces $ SC.o_dynamic_timeout
    $ SC.o_dynamic_timeout_max_multiplier $ SC.o_dynamic_timeout_unit_kb
    $ SC.o_emacs $ SC.o_emacs_outputs $ SC.o_exclude $ SC.o_exclude_rule_ids
    $ SC.o_files_with_matches $ SC.o_force_color $ SC.o_gitlab_sast
    $ SC.o_gitlab_sast_outputs $ SC.o_gitlab_secrets
    $ SC.o_gitlab_secrets_outputs $ SC.o_include $ SC.o_inline_metavariables
    $ SC.o_json $ SC.o_json_outputs $ SC.o_junit_xml $ SC.o_junit_xml_outputs
    $ SC.o_matching_explanations $ SC.o_max_chars_per_line
    $ SC.o_max_lines_per_finding $ SC.o_max_log_list_entries
    $ SC.o_max_match_per_file $ SC.o_max_memory_mb $ SC.o_max_target_bytes
    $ SC.o_nosem $ SC.o_num_jobs $ SC.o_opengrep_ignore_pattern
    $ SC.o_optimizations $ SC.o_output $ SC.o_rewrite_rule_ids $ SC.o_sarif
    $ SC.o_sarif_outputs $ SC.o_scan_unknown_extensions $ o_subdir
    $ o_suppress_errors $ SC.o_taint_intrafile $ SC.o_text $ SC.o_text_outputs
    $ SC.o_time $ SC.o_timeout $ SC.o_timeout_interfile
    $ SC.o_timeout_threshold $ SC.o_use_git $ SC.o_version_check $ SC.o_vim
    $ SC.o_vim_outputs)

let doc = "the recommended way to run opengrep in CI"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "In pull_request/merge_request (PR/MR) contexts, `opengrep ci` will \
       only report findings that were introduced by the PR/MR.";
    `S Cmdliner.Manpage.s_environment;
    `P
      "For each SEMGREP_* variable, its OPENGREP_* alias is also honoured \
       and wins when both are set.";
    `P "$(b,SEMGREP_REPO_NAME): override the detected repository name.";
    `P
      "$(b,SEMGREP_REPO_DISPLAY_NAME): the name the repository is displayed \
       as; setting it per directory keeps the scans of one monorepo apart.";
    `P "$(b,SEMGREP_REPO_URL): override the detected repository URL.";
    `P
      "$(b,SEMGREP_COMMIT): override the detected commit (a full commit id).";
    `P "$(b,SEMGREP_BRANCH): override the detected branch.";
    `P
      "$(b,SEMGREP_PR_ID), $(b,SEMGREP_PR_TITLE): override the detected \
       PR/MR id and title.";
    `P "$(b,SEMGREP_JOB_URL): override the detected CI job URL.";
    `P
      "$(b,GH_TOKEN): on GitHub Actions pull requests, ask the GitHub API \
       for the merge base instead of fetching history until it can be \
       computed locally; set it from $(b,github.token) in the workflow.";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "opengrep ci" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
