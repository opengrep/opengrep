import atexit
import json
import os
import sys
import time
from collections import defaultdict
from pathlib import Path
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import click
from rich.padding import Padding
from rich.table import Table

import semgrep.rpc_call
import semgrep.run_scan
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
# from semgrep import tracing
from semgrep.config_resolver import AUTO_CONFIG_KEY
# from semgrep.commands.install import run_install_semgrep_pro
from semgrep.commands.scan import collect_additional_outputs
from semgrep.commands.scan import scan_options
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import DEFAULT_MAX_MATCH_PER_FILE
from semgrep.constants import DEFAULT_ALLOW_RULE_TIMEOUT_CONTROL
from semgrep.constants import DEFAULT_DYNAMIC_TIMEOUT
from semgrep.constants import DEFAULT_DYNAMIC_TIMEOUT_MAX_MULTIPLIER
from semgrep.constants import DEFAULT_DYNAMIC_TIMEOUT_UNIT_KB
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import OutputFormat
from semgrep.engine import EngineType
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.git import git_check_output
from semgrep.git import is_git_repo_root_approx
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.meta import generate_meta_from_environment
from semgrep.meta import GithubMeta
from semgrep.meta import GitMeta
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.target_manager import ALL_PRODUCTS
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# These patterns are excluded via --exclude regardless of other ignore configuration
ALWAYS_EXCLUDE_PATTERNS = [".semgrep/", ".semgrep_logs/"]

# These patterns are excluded via --exclude unless the user provides their own .semgrepignore
DEFAULT_EXCLUDE_PATTERNS = ["test/", "tests/", "*_test.go"]

def is_valid_pattern(pattern: str) -> bool:
    """
    Parses patterns from semgrep.dev and returns the lines that
    are non-empty and do not start with #
    """
    pattern = pattern.strip()
    return pattern != "" and not pattern.startswith("#")


def get_exclude_paths(
    requested_patterns: Optional[Mapping[out.Product, Sequence[str]]],
) -> Mapping[out.Product, Sequence[str]]:
    patterns = {
        product: (
            [
                pattern.strip()
                for pattern in requested_patterns.get(product, [])
                if is_valid_pattern(pattern)
            ]
            if requested_patterns
            else []
        )
        for product in ALL_PRODUCTS
    }

    for product in ALL_PRODUCTS:
        patterns[product].extend(ALWAYS_EXCLUDE_PATTERNS)
        # This logic isn't clear to me, since I don't see why adding these
        # default patterns is done here or why it would depend on
        # .semgrepignore. But, we've had this for a while, so leaving it not to
        # potentially break things.
        if Path(IGNORE_FILE_NAME).is_file() and not requested_patterns:
            patterns[product].extend(DEFAULT_EXCLUDE_PATTERNS)

    return patterns


def fix_head_if_github_action(metadata: GitMeta) -> None:
    """
    GHA can checkout the incorrect commit for a PR (it will create a fake merge commit),
    so we need to reset the head to the actual PR branch head before continuing.

    Assumes cwd is a valid git project and that if we are in github-actions pull_request,
    metadata.head_branch_hash point to head commit of current branch
    """
    if not (isinstance(metadata, GithubMeta) and metadata.is_pull_request_event):
        return

    assert metadata.head_branch_hash is not None  # Not none when github action PR
    assert metadata.base_branch_hash is not None

    logger.info("Fixing git state for github action pull request")

    logger.debug("Calling git rev-parse HEAD")
    stashed_rev = git_check_output(["git", "rev-parse", "HEAD"])
    logger.debug(f"stashed_rev: {stashed_rev}")

    logger.info(f"Not on head ref: {metadata.head_branch_hash}; checking that out now.")
    git_check_output(["git", "checkout", metadata.head_branch_hash])

    atexit.register(git_check_output, ["git", "checkout", stashed_rev], os.getcwd())


@click.command()
@click.pass_context
@scan_options
@click.option(
    "--audit-on",
    envvar="SEMGREP_AUDIT_ON",
    multiple=True,
    type=str,
    hidden=True,
)
@click.option(
    "--config",
    "-c",
    "-f",
    multiple=True,
    envvar="SEMGREP_RULES",
)
@click.option(
    "--suppress-errors/--no-suppress-errors",
    "suppress_errors",
    default=True,
    envvar="SEMGREP_SUPPRESS_ERRORS",
)
@click.option(
    "--subdir",
    type=click.Path(allow_dash=True, path_type=Path),
)
@click.option(
    "--internal-ci-scan-results",
    "internal_ci_scan_results",
    is_flag=True,
    hidden=True,
)
@click.option(
    "--x-dump-rule-partitions",
    "dump_n_rule_partitions",
    type=int,
    default=0,
    hidden=True,
)
@click.option(
    "--x-dump-rule-partitions-dir",
    "dump_rule_partitions_dir",
    type=click.Path(allow_dash=True, path_type=Path),
    hidden=True,
)
@handle_command_errors
def ci(
    ctx: click.Context,
    *,
    audit_on: Sequence[str],
    baseline_commit: Optional[str],
    internal_ci_scan_results: bool,
    config: Optional[Tuple[str, ...]],
    debug: bool,
    diff_depth: int,
    dump_command_for_core: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    exclude: Optional[Tuple[str, ...]],
    exclude_rule: Optional[Tuple[str, ...]],
    suppress_errors: bool,
    force_color: bool,
    include: Optional[Tuple[str, ...]],
    jobs: int,
    matching_explanations: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_log_list_entries: int,
    max_memory: Optional[int],
    max_target_bytes: int,
    optimizations: str,
    dataflow_traces: Optional[bool],
    output: Optional[str],
    output_format: OutputFormat,
    outputs_text: List[str],
    outputs_emacs: List[str],
    outputs_json: List[str],
    outputs_vim: List[str],
    outputs_gitlab_sast: List[str],
    outputs_gitlab_secrets: List[str],
    outputs_junit_xml: List[str],
    outputs_sarif: List[str],
    quiet: bool,
    rewrite_rule_ids: bool,
    scan_unknown_extensions: bool,
    subdir: Optional[Path],
    time_flag: bool,
    timeout_threshold: int,
    timeout: int,
    interfile_timeout: Optional[int],
    # trace: bool,
    # trace_endpoint: str,
    use_git_ignore: bool,
    verbose: bool,
    allow_local_builds: bool,
    dump_n_rule_partitions: Optional[int],
    dump_rule_partitions_dir: Optional[Path],
    opengrep_ignore_pattern: Optional[str],
    bypass_includes_excludes_for_files: bool = True,
    inline_metavariables: bool = False,
    max_match_per_file: Optional[int] = DEFAULT_MAX_MATCH_PER_FILE,
    allow_rule_timeout_control: bool = DEFAULT_ALLOW_RULE_TIMEOUT_CONTROL,
    dynamic_timeout: bool = DEFAULT_DYNAMIC_TIMEOUT,
    dynamic_timeout_unit_kb: int = DEFAULT_DYNAMIC_TIMEOUT_UNIT_KB,
    dynamic_timeout_max_multiplier: int = DEFAULT_DYNAMIC_TIMEOUT_MAX_MULTIPLIER,
    taint_intrafile: bool = False,
) -> None:
    state = get_state()

    # state.traces.configure(trace, trace_endpoint)
    # with tracing.TRACER.start_as_current_span("semgrep.commands.ci"):
    state.terminal.configure(
        verbose=verbose,
        debug=debug,
        quiet=quiet,
        force_color=force_color,
        output_format=output_format,
    )

    # NOTE: In fact --opengrep-ignore-pattern is not a valid parameter, but we
    # need to have it on the signature for some reason, so ok...
    if opengrep_ignore_pattern:
        logger.info(
            "WARNING: --opengrep-ignore-pattern is set but will be ignored: "
            "all results are returned by the ci command"
        )

    # Maybe move this and the above to the scan-only params, since they are not
    # needed here.
    if not bypass_includes_excludes_for_files:
        logger.info(
            "WARNING: --force-exclude is set but will be ignored: "
            "no explicit targets are passed to the ci command"
        )

    if inline_metavariables:
        logger.info(
            "WARNING: --inline-metavariables is set but will be ignored."
        )

    state.error_handler.configure(suppress_errors)
    capture_core_stderr = not debug

    if subdir:
        subdir = subdir.resolve()  # normalize path & resolve symlinks
        # subdir.is_relative_to(Path.cwd()) is only available from Python 3.9
        try:
            subdir = subdir.relative_to(Path.cwd())
        except ValueError:
            logger.info(
                "`opengrep ci --subdir` must be given a directory that is actually a subdirectory of the current directory"
            )
            sys.exit(FATAL_EXIT_CODE)

    if not is_git_repo_root_approx():
        logger.info(
            "WARNING: `opengrep ci` is meant to be run from the root of a git repo.\nWhen `opengrep ci` is not run from a git repo, it will not be able to perform all operations.\nWhen `opengrep ci` is run from a git repo, but not the root, links in the uploaded findings may be broken.\n\nTo run `opengrep ci` on only a subdirectory of a git repo, see `--subdir`."
        )

    if (dump_n_rule_partitions and not dump_rule_partitions_dir) or (
        not dump_n_rule_partitions and dump_rule_partitions_dir
    ):
        logger.info(
            "Both or none of --x-dump-rule-partitions and --x-dump-rule-partitions-dir must be specified."
        )
        sys.exit(FATAL_EXIT_CODE)

    if not config:
        config = (AUTO_CONFIG_KEY,)

    metadata = generate_meta_from_environment(baseline_commit, subdir)

    console.print(Title("Debugging Info"))
    debugging_table = Table.grid(padding=(0, 1))
    debugging_table.add_row(
        "versions",
        "-",
        f"opengrep [bold]{semgrep.__VERSION__}[/bold] on python [bold]{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}[/bold]",
    )
    debugging_table.add_row(
        "environment",
        "-",
        f"running in environment [bold]{metadata.environment}[/bold], triggering event is [bold]{metadata.event_name}[/bold]",
    )

    console.print(Title("Scan Environment", order=2))
    console.print(debugging_table, markup=True)

    fix_head_if_github_action(metadata)

    engine_type = EngineType.decide_engine_type()

    # set default settings for selected engine type
    if dataflow_traces is None:
        dataflow_traces = engine_type.has_dataflow_traces

    if max_memory is None:
        max_memory = engine_type.default_max_memory

    if interfile_timeout is None:
        interfile_timeout = engine_type.default_interfile_timeout

    # if engine_type.is_pro:
    #     console.print(Padding(Title("Engine", order=2), (1, 0, 0, 0)))
    #     if run_secrets:
    #         console.print("Semgrep Secrets requires Semgrep Pro Engine")
    #     if engine_type.check_if_installed():
    #         console.print(
    #             f"Using Semgrep Pro Version: [bold]{engine_type.get_pro_version()}[/bold]",
    #             markup=True,
    #         )
    #         console.print(
    #             f"Installed at [bold]{engine_type.get_binary_path()}[/bold]",
    #             markup=True,
    #             soft_wrap=True,
    #         )
    #     else:
    #         run_install_semgrep_pro()

    outputs = collect_additional_outputs(
        outputs_text=outputs_text,
        outputs_emacs=outputs_emacs,
        outputs_json=outputs_json,
        outputs_vim=outputs_vim,
        outputs_gitlab_sast=outputs_gitlab_sast,
        outputs_gitlab_secrets=outputs_gitlab_secrets,
        outputs_junit_xml=outputs_junit_xml,
        outputs_sarif=outputs_sarif,
    )
    output_settings = OutputSettings(
        outputs=outputs,
        output_format=output_format,
        output_destination=output,
        verbose_errors=verbose,
        timeout_threshold=timeout_threshold,
        output_time=time_flag,
        output_per_finding_max_lines_limit=max_lines_per_finding,
        output_per_line_max_chars_limit=max_chars_per_line,
        dataflow_traces=dataflow_traces,
        max_log_list_entries=max_log_list_entries,
    )
    output_handler = OutputHandler(output_settings)
    output_handler.check_destinations()

    per_product_excludes = {
        product: [*exclude] if exclude else [] for product in ALL_PRODUCTS
    }
    additional_exclude_paths = get_exclude_paths(None)
    for product in ALL_PRODUCTS:
        per_product_excludes[product].extend(additional_exclude_paths[product])

    target = os.curdir
    if subdir:
        target += f"/{subdir}"

    # Base arguments for actually running the scan. This is done here so we can
    # re-use this in the event we need to perform a second scan. Currently the
    # only case for this is a separate "historical" scan, where we scan the git
    # history for secrets. This must be split since the targeting logic for the
    # historical scans is entirely in pro, but otherwise here is still
    # performed by the python. Once osemgrep is complete we need only combine
    # the two target lists and perform one scan.
    run_scan_args = {
        "engine_type": engine_type,
        "output_handler": output_handler,
        "taint_intrafile": taint_intrafile,
        "target": [target],
        "pattern": None,
        "lang": None,
        "configs": config,
        "no_rewrite_rule_ids": (not rewrite_rule_ids),
        "dump_command_for_core": dump_command_for_core,
        "jobs": jobs,
        "include": include,
        "exclude": per_product_excludes,
        "exclude_rule": exclude_rule,
        "max_target_bytes": max_target_bytes,
        "autofix": False,
        "dryrun": True,
        # Always true, as we want to always report all findings, even
        # ignored ones
        "disable_nosem": True,
        "no_git_ignore": (not use_git_ignore),
        "timeout": timeout,
        "allow_rule_timeout_control": allow_rule_timeout_control,
        "dynamic_timeout": dynamic_timeout,
        "dynamic_timeout_unit_kb": dynamic_timeout_unit_kb,
        "dynamic_timeout_max_multiplier": dynamic_timeout_max_multiplier,
        "max_memory": max_memory,
        "max_match_per_file": max_match_per_file,
        "interfile_timeout": interfile_timeout,
        # "trace": trace,
        # "trace_endpoint": trace_endpoint,
        "timeout_threshold": timeout_threshold,
        "skip_unknown_extensions": (not scan_unknown_extensions),
        "optimizations": optimizations,
        "baseline_commit": metadata.merge_base_ref,
        "baseline_commit_is_mergebase": True,
        "diff_depth": diff_depth,
        "capture_core_stderr": capture_core_stderr,
        "allow_local_builds": allow_local_builds,
        "dump_n_rule_partitions": dump_n_rule_partitions,
        "dump_rule_partitions_dir": dump_rule_partitions_dir,
        "prioritize_dependency_graph_generation": False,
    }

    try:
        start = time.time()

        # TODO? we're not passing time_flag below (or matching_explanations),
        # is it indended?
        (
            filtered_matches_by_rule,
            semgrep_errors,
            renamed_targets,
            ignore_log,
            filtered_rules,
            profiler,
            output_extra,
            shown_severities,
            dependencies,
            dependency_parser_errors,
            _executed_rule_count,
            _missed_rule_count,
            all_subprojects,
        ) = semgrep.run_scan.run_scan(**run_scan_args)
    except SemgrepError as e:
        if isinstance(e, SemgrepError):
            exit_code = e.code
        else:
            exit_code = FATAL_EXIT_CODE

        output_handler.handle_semgrep_errors([e])
        output_handler.output({}, all_targets=set(), filtered_rules=[])
        logger.info(f"Encountered error when running rules: {e}")

        sys.exit(exit_code)

    total_time = time.time() - start

    # Split up rules into respective categories:
    blocking_rules: List[Rule] = []
    nonblocking_rules: List[Rule] = []
    for rule in filtered_rules:
        if "r2c-internal-cai" in rule.id:
            pass
        elif rule.from_transient_scan:
            pass
        elif rule.is_blocking:
            blocking_rules.append(rule)
        else:
            nonblocking_rules.append(rule)

    # Split up matches into respective categories
    non_cai_matches_by_rule: RuleMatchMap = defaultdict(list)
    blocking_matches: List[RuleMatch] = []
    nonblocking_matches: List[RuleMatch] = []
    cai_matches: List[RuleMatch] = []

    # Remove the prev scan matches by the rules that are in the current scan
    # Done before the next loop to avoid interfering with ignore logic
    removed_prev_scan_matches = {
        rule: [match for match in matches]
        for rule, matches in filtered_matches_by_rule.items()
        if (not rule.from_transient_scan)
    }

    # Since we keep nosemgrep disabled for the actual scan, we have to
    # apply that flag here.
    # If there are multiple outputs and any request to keep_ignores
    # then all outputs keep the ignores. The only output format that
    # keep ignored matches currently is sarif.
    keep_ignored = not enable_nosem or output_handler.keep_ignores()
    for rule, matches in removed_prev_scan_matches.items():
        for match in matches:
            if match.is_ignored and not keep_ignored:
                continue

            # Keep plain branches here: Nuitka miscompiles the nested
            # conditional expression that used to select the list, losing
            # the append in compiled builds.
            if "r2c-internal-cai" in rule.id:
                applicable_result_list = cai_matches
            elif match.is_blocking:
                applicable_result_list = blocking_matches
            else:
                applicable_result_list = nonblocking_matches
            applicable_result_list.append(match)
            if "r2c-internal-cai" not in rule.id:
                non_cai_matches_by_rule[rule].append(match)

    num_nonblocking_findings = len(nonblocking_matches)
    num_blocking_findings = len(blocking_matches)
    filtered_rules = [*blocking_rules, *nonblocking_rules]

    # After computing the number of blocking/non-blocking findings, here
    # is were the cli comes up with the suggested exit code that we send
    # to the semgrep app
    #
    # NOTE: this is not the exit code the cli will use to exit with, as
    # the cli depends on the apps response to compute its own exit code!
    cli_suggested_exit_code = 1 if num_blocking_findings > 0 else 0

    if not internal_ci_scan_results:
        output_handler.output(
            non_cai_matches_by_rule,
            all_targets=output_extra.all_targets,
            engine_type=engine_type,
            ignore_log=ignore_log,
            profiler=profiler,
            filtered_rules=filtered_rules,
            extra=output_extra,
            severities=shown_severities,
            is_ci_invocation=True,
            print_summary=False,
        )

    logger.info("CI scan completed successfully.")
    logger.info(
        f"  Found {unit_str(num_blocking_findings + num_nonblocking_findings, 'finding')} ({num_blocking_findings} blocking) from {unit_str(len(filtered_rules), 'rule')}."
    )

    audit_mode = metadata.event_name in audit_on
    if cli_suggested_exit_code == 1:
        if audit_mode:
            logger.info(
                f"  Audit mode is on for {metadata.event_name}, so exiting with code 0 even if matches found",
            )
            exit_code = 0
        else:
            logger.info("  Has findings for blocking rules so exiting with code 1")
            exit_code = 1
    else:
        logger.info("  No blocking findings so exiting with code 0")
        exit_code = 0

    sys.exit(exit_code)
