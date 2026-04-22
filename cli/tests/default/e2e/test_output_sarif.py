import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

# NOTE: the osemgrep-compatible SARIF tests have been ported to the OCaml
# test suite (see src/osemgrep/cli_scan/Test_scan_subcommand_sarif.ml). Only
# the cases that osemgrep cannot yet reproduce remain here, kept as a ledger
# of known gaps until the corresponding features land in osemgrep.


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule_and_target",
    [
        # TODO: osemgrep does not take into account labels
        # and the rule.py formula_string() is not fully ported
        ("rules/taint_trace.yaml", "taint/taint_trace.cpp"),
    ],
)
@pytest.mark.parametrize("dataflow_traces", [True, False])
@pytest.mark.osemfail
def test_sarif_output_osemfail(
    run_semgrep_in_tmp: RunSemgrep, snapshot, rule_and_target, dataflow_traces
):
    rule, target = rule_and_target
    if dataflow_traces:
        options = ["--verbose", "--dataflow-traces"]
    else:
        options = ["--verbose"]

    res = run_semgrep_in_tmp(
        rule,
        target_name=target,
        options=options,
        output_format=OutputFormat.SARIF,
        assert_exit_code=0,
        is_logged_in_weak=True,
    )
    snapshot.assert_match(res.stdout, "results.sarif")


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_nosemgrep_and_error(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="nosemgrep/eqeq-nosemgrep.py",
            output_format=OutputFormat.SARIF,
            options=["--error"],
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_when_errors(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="basic/inexistent.py",
            output_format=OutputFormat.SARIF,
            assert_exit_code=2,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )
