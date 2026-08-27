import subprocess

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.error import SemgrepError

RULE = """
rules:
  - id: no-match
    pattern: does_not_appear()
    message: no match
    languages: [python]
    severity: ERROR
"""


@pytest.fixture
def gitlab_mr_repo(tmp_path, monkeypatch):
    """A git repo in a GitLab merge-request environment whose merge-base
    fetch fails with a SemgrepError."""
    repo = tmp_path / "repo"
    repo.mkdir()
    monkeypatch.chdir(repo)
    subprocess.run(["git", "init", "-q"], check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.email=test@example.com",
            "-c",
            "user.name=test",
            "commit",
            "-q",
            "--allow-empty",
            "-m",
            "init",
        ],
        check=True,
    )
    (repo / "rule.yaml").write_text(RULE)
    monkeypatch.setenv("SEMGREP_SETTINGS_FILE", str(tmp_path / "settings.yaml"))
    monkeypatch.setenv("GITLAB_CI", "true")
    monkeypatch.setenv("CI_MERGE_REQUEST_TARGET_BRANCH_NAME", "main")

    def fail_fetch(branch_name, head_sha):
        raise SemgrepError("gitlab merge-base fetch failed")

    monkeypatch.setattr(
        "semgrep.meta.GitlabMeta._fetch_branch_get_merge_base",
        staticmethod(fail_fetch),
    )
    return repo


@pytest.mark.quick
def test_gitlab_fetch_failure_is_reported_and_suppressed(gitlab_mr_repo):
    result = CliRunner().invoke(cli, ["ci", "--config", "rule.yaml"])
    assert "gitlab merge-base fetch failed" in result.stderr
    assert result.exit_code == 0


@pytest.mark.quick
def test_gitlab_fetch_failure_is_fatal_without_suppression(gitlab_mr_repo):
    result = CliRunner().invoke(
        cli, ["ci", "--no-suppress-errors", "--config", "rule.yaml"]
    )
    assert "gitlab merge-base fetch failed" in result.stderr
    assert result.exit_code == 2
