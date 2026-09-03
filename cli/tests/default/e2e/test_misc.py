import pytest
from tests.fixtures import RunSemgrep


GITHUB_TEST_GIST_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)


@pytest.mark.slow
def test_url_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(GITHUB_TEST_GIST_URL).stdout, "results.json"
    )


@pytest.mark.slow
def test_multiple_configs_different_origins(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", GITHUB_TEST_GIST_URL]).stdout,
        "results.json",
    )
