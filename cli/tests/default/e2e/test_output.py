import json
import re
import shutil
import subprocess
from pathlib import Path

import pytest
from tests.conftest import RULES_PATH
from tests.conftest import TARGETS_PATH
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

# coupling: also in test_ci.py
REPO_DIR_NAME = "project_name"


@pytest.mark.kinda_slow
def test_json_output_with_dataflow_traces(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint_trace.yaml",
            target_name="taint/taint_trace.cpp",
            output_format=OutputFormat.JSON,
            options=["--dataflow-traces"],
        ).stdout,
        "results.json",
    )


IGNORE_LOG_REPORT_FIRST_LINE = "Some files were skipped or only partially analyzed."
IGNORE_LOG_REPORT_LAST_LINE = (
    "  For a full list of skipped files, run opengrep with the --verbose flag.\n"
)


# pysemgrep/osemgrep status: osemgrep reports 2 more files that are being
# excluded. They're excluded in both implementations.
@pytest.mark.kinda_slow
@pytest.mark.pysemfail
def test_semgrepignore_ignore_log_report(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot
):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TARGETS_PATH / "ignores" / ".semgrepignore").resolve()
    )
    # See remarks in test_ignores.py:
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".gitignore"), tmp_path / ".gitignore"
    )

    _, stderr = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.TEXT,
        force_color=True,
        target_name="ignores",
    )

    report = re.search(
        f"^{IGNORE_LOG_REPORT_FIRST_LINE}$.*?^{IGNORE_LOG_REPORT_LAST_LINE}$",
        stderr,
        flags=re.MULTILINE | re.DOTALL,
    )
    assert (
        report is not None
    ), "can't find ignore log report based on expected start and end lines"
    snapshot.assert_match(report.group(), "report.txt")


# Tolerate a different snapshot with pysemgrep than osemgrep.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_ignore_log_report_pysemgrep(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot
):
    test_semgrepignore_ignore_log_report(run_semgrep_in_tmp, tmp_path, snapshot)


# pysemgrep/osemgrep status: osemgrep reports 2 more files that are being
# excluded. They're excluded in both implementations.
@pytest.mark.kinda_slow
@pytest.mark.pysemfail
def test_semgrepignore_ignore_log_json_report(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot
):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TARGETS_PATH / "ignores" / ".semgrepignore").resolve()
    )
    # See remarks in test_ignores.py:
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".gitignore"), tmp_path / ".gitignore"
    )

    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.JSON,
        target_name="ignores",
    )
    parsed_output = json.loads(stdout)
    assert "paths" in parsed_output

    snapshot.assert_match(
        json.dumps(parsed_output["paths"], indent=2, sort_keys=True), "report.json"
    )


# Tolerate a different snapshot with pysemgrep than osemgrep.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_ignore_log_json_report_pysemgrep(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot
):
    test_semgrepignore_ignore_log_json_report(run_semgrep_in_tmp, tmp_path, snapshot)


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "git_repo",
    [True, False],
)
@pytest.mark.osemfail
def test_git_repo_output(
    run_semgrep: RunSemgrep, git_repo, tmp_path, monkeypatch, snapshot
):
    """
    Initialize a git repo at a temp directory
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir(parents=True)

    monkeypatch.chdir(repo_base)

    if git_repo:
        # Initialize State
        subprocess.run(["git", "init"], check=True, capture_output=True)
        # Symlink the gitignore to the temp directory
        (repo_base / ".gitignore").symlink_to(
            Path(TARGETS_PATH / "ignores" / ".gitignore").resolve()
        )

    # Symlink rules
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())

    monkeypatch.chdir(tmp_path)
    snapshot.assert_match(
        run_semgrep(
            "rules/eqeq-basic.yaml",
            output_format=OutputFormat.TEXT,
            assume_targets_dir=False,
            target_name=repo_base,
        ).stderr,
        "results.txt",
    )


# This is currently not passing because the loc field in the explanation
# differs between pysemgrep and osemgrep because it's a location in the rule
# (not in the target), and pysemgrep passes a preprocessed rule file to
# semgrep-core hence the mistmatch.
@pytest.mark.slow
@pytest.mark.osemfail
def test_output_matching_explanations(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        target_name="basic/stupid.js",
        options=["--matching-explanations"],
        output_format=OutputFormat.JSON,  # Not the real output format; just disables JSON parsing
    )
    snapshot.assert_match(stdout, "report.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "target_dir",
    ["multilangproj", "language-filtering", "exclude_include"],
)
@pytest.mark.osemfail
def test_file_count_multifile(run_semgrep_in_tmp: RunSemgrep, snapshot, target_dir):
    _, stderr = run_semgrep_in_tmp(
        "rules/filecount.yaml",
        output_format=OutputFormat.TEXT,
        target_name=target_dir,
        options=[],
    )
    snapshot.assert_match(stderr, "result.out")
