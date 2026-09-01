import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

# NOTE: the osemgrep-compatible SARIF tests have been ported to the OCaml
# test suite (see src/osemgrep/cli_scan/Test_scan_subcommand_sarif.ml). Only
# the cases that osemgrep cannot yet reproduce remain here, kept as a ledger
# of known gaps until the corresponding features land in osemgrep.


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
