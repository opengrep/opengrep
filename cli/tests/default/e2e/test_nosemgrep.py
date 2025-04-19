import pytest
import tempfile
from pathlib import Path
from tests.conftest import _clean_stdout
from tests.fixtures import RunSemgrep

TEST_CONTENT = """
function test() {
    // This should be ignored by default patterns
    if (x == y) { // nosem
        console.log("Equal");
    }

    if (a == b) { // nosemgrep
        console.log("Also equal");
    }
}
"""

TEST_CONTENT_WITH_CUSTOM = """
function test() {
    // This should NOT be ignored by default patterns
    if (x == y) { // nosem
        console.log("Equal");
    }

    // This should be ignored by the custom pattern
    if (a == b) { // mycustom
        console.log("Also equal");
    }
}
"""

EQEQ_RULE = """
rules:
  - id: eqeq-basic
    pattern: $X == $Y
    message: "useless comparison"
    languages: [javascript]
    severity: ERROR
"""

@pytest.mark.kinda_slow
def test_regex_rule__nosemgrep(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-nosemgrep.yaml", target_name="basic/regex-nosemgrep.txt"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_nosem_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/nosem.yaml").stdout, "results.json")


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_nosem_rule__invalid_id(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/nosem.yaml", target_name="nosem_invalid_id", assert_exit_code=2
    )

    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")


@pytest.mark.kinda_slow
def test_nosem_with_multiple_ids(run_semgrep_in_tmp: RunSemgrep):
    run_semgrep_in_tmp(
        "rules/two_matches.yaml",
        target_name="nosemgrep/multiple-nosemgrep.py",
        assert_exit_code=0,
    )


@pytest.mark.kinda_slow
def test_nosem_rule__with_disable_nosem(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nosem.yaml", options=["--disable-nosem"]).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_custom_ignore_pattern(run_semgrep_in_tmp: RunSemgrep, tmp_path: Path, snapshot):
    # Create temporary files for the test
    test_file = tmp_path / "test.js"
    test_file.write_text(TEST_CONTENT)
    
    test_file_custom = tmp_path / "test_custom.js"
    test_file_custom.write_text(TEST_CONTENT_WITH_CUSTOM)
    
    rule_file = tmp_path / "rule.yaml"
    rule_file.write_text(EQEQ_RULE)

    # First run with default patterns (nosem/nosemgrep) - should find nothing due to nosem comments
    stdout1 = run_semgrep_in_tmp(
        str(rule_file),
        target_name=str(test_file)
    ).stdout

    # Then run with custom pattern that replaces default patterns - should find something 
    # because it no longer recognizes nosem/nosemgrep
    stdout2 = run_semgrep_in_tmp(
        str(rule_file),
        target_name=str(test_file),
        options=["--opengrep-ignore-pattern=mycustom"]
    ).stdout

    # Finally run with code that uses the custom pattern - should find nothing for the mycustom line
    stdout3 = run_semgrep_in_tmp(
        str(rule_file),
        target_name=str(test_file_custom),
        options=["--opengrep-ignore-pattern=mycustom"]
    ).stdout

    snapshot.assert_match(stdout1, "results_default.json")
    snapshot.assert_match(stdout2, "results_custom_pattern.json")
    snapshot.assert_match(stdout3, "results_with_custom_pattern.json")
