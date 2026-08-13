from uuid import uuid4

import pytest
import requests
from requests.exceptions import ConnectionError

from semgrep.commands.wrapper import handle_command_errors
from semgrep.error_handler import ErrorHandler


FAKE_USER_AGENT = "user-agent"


@pytest.fixture
def error_handler_enabled() -> bool:
    return True


@pytest.fixture
def error_handler(error_handler_enabled) -> ErrorHandler:
    error_handler = ErrorHandler()
    error_handler.configure(suppress_errors=error_handler_enabled)
    return error_handler


@pytest.fixture(autouse=True)
def mocked_state(mocker, error_handler):
    mocked = mocker.MagicMock()
    mocked.app_session.user_agent = FAKE_USER_AGENT
    mocked.local_scan_id = uuid4()
    mocked.error_handler = error_handler
    mocker.patch("semgrep.state.get_state", return_value=mocked)
    yield mocked


@pytest.fixture(autouse=True)
def mock_broken_request(requests_mock):
    return requests_mock.get(
        "https://semgrep.dev/api/agent/deployments/current", exc=ConnectionError
    )


@handle_command_errors
def fake_command():
    requests.get("https://semgrep.dev/api/agent/deployments/current")


@pytest.mark.quick
def test_errors_suppressed() -> None:
    """
    Check that a failing command exits zero when errors are suppressed
    """
    with pytest.raises(SystemExit) as exit_exc:
        fake_command()

    assert exit_exc.type == SystemExit
    assert exit_exc.value.code == 0


@pytest.mark.quick
@pytest.mark.parametrize("error_handler_enabled", [False])
def test_errors_not_suppressed() -> None:
    """
    Check that a failing command keeps its fatal exit code otherwise
    """
    with pytest.raises(SystemExit) as exit_exc:
        fake_command()

    assert exit_exc.type == SystemExit
    assert exit_exc.value.code == 2
