import pytest

from semgrep.engine import EngineType as ET
from semgrep.error import SemgrepError
from semgrep.meta import GitMeta


@pytest.mark.quick
@pytest.mark.parametrize("is_supply_chain_only", [True, False])
@pytest.mark.parametrize("is_secrets_scan", [True, False])
@pytest.mark.parametrize(
    "engine_flag", [None, ET.OSS, ET.PRO_LANG, ET.PRO_INTRAFILE, ET.PRO_INTERFILE]
)
@pytest.mark.parametrize(
    ("is_git_full_scan", "interfile_diff_scan_enabled"),
    [
        # `is_git_full_scan` is None for scans without git metadata.
        # Without an engine requested via a CLI argument the engine is always
        # OSS; running Secrets or Supply Chain may still change it.
        (None, False),
        (None, True),
        (False, False),
        (False, True),
        (True, False),
        (True, True),
    ],
)
def test_decide_engine_type(
    mocker,
    is_git_full_scan,
    interfile_diff_scan_enabled,
    is_supply_chain_only,
    is_secrets_scan,
    engine_flag,
):
    git_meta = None

    if is_git_full_scan is not None:  # None means there was no metadata
        git_meta = mocker.Mock(spec=GitMeta)
        git_meta.is_full_scan = is_git_full_scan

    args = [
        engine_flag,
        is_secrets_scan,
        interfile_diff_scan_enabled,
        git_meta,
        is_supply_chain_only,
    ]
    if is_secrets_scan and engine_flag is ET.OSS:
        pytest.raises(SemgrepError, ET.decide_engine_type, *args)
    else:
        diff_scan_override = not (
            (is_git_full_scan is None or is_git_full_scan)
            or interfile_diff_scan_enabled
        )
        assert ET.decide_engine_type(*args) == expected_engine_type(
            is_supply_chain_only,
            is_secrets_scan,
            diff_scan_override,
            engine_flag,
        )


def expected_engine_type(
    is_supply_chain_only,
    is_secrets_scan,
    diff_scan_override,
    engine_flag,
):
    if engine_flag is None:
        expected = ET.OSS
    else:
        expected = engine_flag

    # Overrides
    if is_secrets_scan:
        expected = ET.PRO_INTRAFILE if expected is ET.OSS else expected

    if is_supply_chain_only:
        expected = ET.PRO_INTRAFILE if expected is ET.PRO_INTERFILE else expected

    if diff_scan_override and expected is ET.PRO_INTERFILE:
        expected = ET.PRO_INTRAFILE

    return expected
