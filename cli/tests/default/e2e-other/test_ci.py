##############################################################################
# Prelude
##############################################################################
# Testing 'opengrep ci' end-to-end.
#
# opengrep ci scans the current repository and decides an exit code from the
# findings. There is no backend to register a scan with or upload findings to,
# so everything here runs against local rules.
import os
import subprocess
import tempfile
from pathlib import Path
from textwrap import dedent
from typing import Optional

import pytest
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

pytestmark = pytest.mark.kinda_slow

REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.opengrep.dev"
AUTHOR_NAME = "Environment Test"
MAIN_BRANCH_NAME = "main"

BLOCKING_RULE = dedent(
    """\
    rules:
      - id: eqeq-bad
        pattern: $X == $X
        message: useless comparison
        languages: [python]
        severity: ERROR
    """
)

BAD_RULE = dedent(
    """\
    rules:
      - id: no-pattern-here
        message: this rule has no pattern
        languages: [python]
        severity: ERROR
    """
)


@pytest.fixture
def git_tmp_path_with_commit(monkeypatch, tmp_path):
    """
    Initialize a git repo at a temp directory with one dummy commit.
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir()

    monkeypatch.chdir(repo_base)

    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "checkout", "-B", MAIN_BRANCH_NAME],
        check=True,
        capture_output=True,
    )

    (repo_base / "foo.py").write_text("x = 1\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "initial commit"],
        check=True,
        capture_output=True,
    )

    yield repo_base


def write_rule(repo_base: Path, contents: str) -> Path:
    rule_file = repo_base / "rules.yaml"
    rule_file.write_text(contents)
    return rule_file


def run_ci(config: Optional[Path] = None, extra_args: Optional[list] = None):
    """
    Run 'opengrep ci' in the current directory, without touching the user's
    settings file.
    """
    settings_file = tempfile.NamedTemporaryFile().name
    Path(settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
    )

    cmd = SEMGREP_BASE_SCAN_COMMAND + ["ci", "--disable-version-check"]
    if config is not None:
        cmd.extend(["--config", str(config)])
    if extra_args:
        cmd.extend(extra_args)

    return subprocess.run(
        cmd,
        capture_output=True,
        encoding="utf-8",
        env={
            "PATH": os.environ.get("PATH", ""),
            "SEMGREP_SETTINGS_FILE": settings_file,
        },
    )


def test_ci_exits_zero_without_findings(git_tmp_path_with_commit):
    """
    A scan that finds nothing exits 0.
    """
    rule_file = write_rule(git_tmp_path_with_commit, BLOCKING_RULE)

    output = run_ci(config=rule_file)

    assert output.returncode == 0
    assert "CI scan completed successfully." in output.stderr
    assert "No blocking findings so exiting with code 0" in output.stderr


def test_ci_exits_one_on_blocking_findings(git_tmp_path_with_commit):
    """
    A blocking finding exits 1.
    """
    (git_tmp_path_with_commit / "foo.py").write_text("x = 1\nif a == a:\n    pass\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add a finding"], check=True, capture_output=True
    )
    rule_file = write_rule(git_tmp_path_with_commit, BLOCKING_RULE)

    output = run_ci(config=rule_file)

    assert output.returncode == 1
    assert "Has findings for blocking rules so exiting with code 1" in output.stderr


def test_ci_audit_mode_exits_zero_with_findings(git_tmp_path_with_commit):
    """
    Audit mode reports findings but still exits 0.
    """
    (git_tmp_path_with_commit / "foo.py").write_text("x = 1\nif a == a:\n    pass\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add a finding"], check=True, capture_output=True
    )
    rule_file = write_rule(git_tmp_path_with_commit, BLOCKING_RULE)

    output = run_ci(config=rule_file, extra_args=["--audit-on", "unknown"])

    assert output.returncode == 0
    assert "Audit mode is on for unknown" in output.stderr


def test_ci_reports_an_invalid_rule(git_tmp_path_with_commit):
    """
    An unparseable rule file is reported rather than silently ignored.
    """
    rule_file = write_rule(git_tmp_path_with_commit, BAD_RULE)

    output = run_ci(config=rule_file, extra_args=["--no-suppress-errors"])

    assert output.returncode != 0
    assert "Invalid rule schema" in output.stderr
