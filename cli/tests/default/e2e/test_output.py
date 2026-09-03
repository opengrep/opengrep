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
