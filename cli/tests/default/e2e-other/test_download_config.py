from textwrap import dedent

import pytest
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader


# What is this test really for? The old output was an error due the field
# 'pattern-new-feature' being unknown. The new output is another error
# about a missing 'pattern' or similar field.
# NOTE (dimitris): The output is produced as expected, but the test does not capture
# it properly now that we use a thread-pool to load rules. So for now let's disable
# that test.
@pytest.mark.quick
@pytest.mark.osemfail
@pytest.mark.pysemfail
def test_new_feature_registry_config(monkeypatch, snapshot, mocker, tmp_path):
    config_file = ConfigFile(
        None,
        dedent(
            """
            rules:
            - id: eqeq-bad
              pattern-new-feature: $X == $X
              message: "useless comparison"
              languages: [python]
              severity: ERROR
            """
        ).lstrip(),
        "https://semgrep.dev/p/ci",
    )
    mocker.patch.object(
        ConfigLoader, "_download_config_from_url", return_value=config_file
    )

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
        },
        use_click_runner=True,
    )
    result = runner.invoke(cli, subcommand="scan", args=["--config", "p/ci"])
    snapshot.assert_match(result.output, "output.txt")
