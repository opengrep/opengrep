import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule",
    [
        "rules/aspnet_handler_csharp.yaml",
        "rules/aspnet_web_service_vb.yaml",
    ],
)
def test_aspnet_handlers(run_semgrep_in_tmp: RunSemgrep, snapshot, rule):
    stdout, _ = run_semgrep_in_tmp(rule, target_name="aspnet")
    snapshot.assert_match(stdout, "results.json")


@pytest.mark.kinda_slow
def test_aspnet_handler_explicit_target(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/aspnet_handler_csharp.yaml",
        target_name="aspnet/aspnet_handler_csharp.ashx",
    )
    snapshot.assert_match(stdout, "results.json")
