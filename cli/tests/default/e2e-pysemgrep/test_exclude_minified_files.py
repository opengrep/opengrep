import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "options",
    [[], ["--exclude-minified-files"]],
    ids=["default", "exclude_minified_files"],
)
def test_exclude_minified_files(run_semgrep_in_tmp: RunSemgrep, snapshot, options):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="minified",
        options=["--verbose", *options],
    )
    snapshot.assert_match(stdout, "results.json")
