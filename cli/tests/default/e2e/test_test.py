# Tests for 'semgrep --test' (accessible also now via 'osemgrep test').
# See https://semgrep.dev/docs/writing-rules/testing-rules/ for more info.
#
# See also test_fixtest.py for the autofix "fixtest" tests.
#
# The other tests of this file are now Testo tests, in
# src/osemgrep/cli_test/Unit_test_subcommand.ml.
#
# TODO:
#  - test to detect wrong or missing ruleid: in a target/test file
#    (e.g., missed an annotation), with passed=false in the JSON
#  - test to detect invalid ruleid: annotation (wrong ruleid syntax)
#    like ruleid: without anything after, or a wrong character in rule id
#  - test do detect correctly annotations in different languages, using
#    different style of comments
#  - test to take a single directory and iterate over. This is actually
#    the main use case for --test and what we use in semgrep-rules/. However,
#    is a bit harder to test here given how run_semgrep_in_tmp() was
#    designed with always a 'config' and a 'target_name' parameter
#    (and the fact that the e2e rules and targets are in different dirs)
#    (maybe we could write this test in Testo instead and for osemgrep-only
#    once we removed test.py)
import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# It should output an "error" field with the right error message (timeout)
# in the JSON output.
# --timeout must be passed explicitly: --test otherwise runs with no timeout at
# all, and this rule is written to never finish.
@pytest.mark.slow
@pytest.mark.osemfail
def test_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/rule_that_timeout.yaml",
        options=["--test", "--timeout", "1"],
        target_name="test_test/long.py",
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )
