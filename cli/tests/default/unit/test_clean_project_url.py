from unittest.mock import patch

import pytest

from semgrep.git import clean_project_url
from semgrep.git import get_project_url
from semgrep.git import redact_url_userinfo


@pytest.mark.quick
def test_redact_url_userinfo():
    assert (
        redact_url_userinfo(
            "git fetch https://gitlab-ci-token:fake-64_wFuiRFQk9t841JHKQnAT@gitlab.company.world/app/test-case.git main"
        )
        == "git fetch https://***@gitlab.company.world/app/test-case.git main"
    )
    assert (
        redact_url_userinfo("git merge-base --all HEAD FETCH_HEAD")
        == "git merge-base --all HEAD FETCH_HEAD"
    )


@pytest.mark.quick
def test_git_url_clean():
    assert (
        clean_project_url(
            "https://gitlab-ci-token:fake-64_wFuiRFQk9t841JHKQnAT@gitlab.company.world/app/test-case.git"
        )
        == "https://gitlab.company.world/app/test-case.git"
    )


@pytest.mark.quick
@patch("semgrep.git.clean_project_url")
def test_get_project_url(patched_clean_project_url):
    get_project_url()
    patched_clean_project_url.assert_called_once()
