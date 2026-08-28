import pytest

from semgrep.external.git_url_parser import Parser
from semgrep.meta import get_url_from_sstp_url


@pytest.mark.quick
def test_git_url_parser():
    # Note these tests do not indicate correct behavior as much as
    # just document the current behavior of a hacky piece of code.
    tests = [
        (
            "ssh://user@host.xz:20/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": "20",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz:20/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "port": "20",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:path/to/repo.git",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "rsync://host.xz/path/to/repo.git/",
            {
                "protocol": "rsync",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        # Git transport Protocol
        (
            "git://host.xz/path/to/repo.git/",
            {
                "protocol": "git",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "git://host.xz/~user/path/to/repo.git/",
            {
                "protocol": "git",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        # HTTP
        (
            "http://host.xz/path/to/repo.git/",
            {
                "protocol": "http",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "https://host.xz/path/to/repo.git/",
            {
                "protocol": "https",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        # Local file system paths
        (
            "/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "",
                "port": None,
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        # Note URLs with the file scheme are completely busted.
        (
            "file:///path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "file://~/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "https://gitlab.net/foo.bar/a-b/a-b-c-d",
            {
                "protocol": "https",
                "user": None,
                "resource": "gitlab.net",
                "port": None,
                "owner": "foo.bar/a-b",
                "name": "a-b-c-d",
            },
        ),
    ]
    for url, expected in tests:
        actual = {}
        # TODO: mypy complains about .items not defined on object
        for key, _value in expected.items():  # type: ignore[attr-defined]
            actual[key] = getattr(Parser(url).parse(), key)
        assert (url, actual) == (url, expected)


@pytest.mark.quick
def test_get_url_from_sstp_url():
    tests = [
        # This used to cause the URL parser to crash.
        (
            "https://test@dev.azure.com/test/TestName/_git/Core.Thing",
            "https://dev.azure.com/test/TestName/_git/Core.Thing",
        ),
        (
            "https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification",
            "https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification",
        ),
        # This one has a "subgroup" structure, which we should be able to parse.
        (
            "https://gitlab.com/example/group2/group3/test-case.git",
            "https://gitlab.com/example/group2/group3/test-case",
        ),
        (
            "https://gitlab.com/example/test-case.git",
            "https://gitlab.com/example/test-case",
        ),
        (
            "git@code1.somecompany.internal:somecompany-eval/owasp-juice-shop",
            "https://code1.somecompany.internal/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@code2.somecompany.internal:somecompany-eval/owasp-juice-shop.git",
            "https://code2.somecompany.internal/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@github.com:somecompany-eval/owasp-juice-shop",
            "https://github.com/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@code3.somecompany.internal:eval/owasp-juice-shop.git",
            "https://code3.somecompany.internal/eval/owasp-juice-shop",
        ),
    ]

    for url, expected in tests:
        assert get_url_from_sstp_url(url) == expected


@pytest.mark.quick
def test_gitlab_fetch_token_via_config_env(monkeypatch):
    """
    On current git the merge-base fetch carries the plain project url; the
    token goes to the child process as an Authorization header.
    """
    import base64

    from semgrep.meta import GitlabMeta

    calls = []
    monkeypatch.setenv(
        "CI_MERGE_REQUEST_PROJECT_URL", "https://gitlab.example/org/repo"
    )
    monkeypatch.setenv("CI_JOB_TOKEN", "fake-token")
    monkeypatch.setattr("semgrep.meta.git_supports_config_env", lambda: True)
    monkeypatch.setattr(
        "semgrep.meta.git_check_output_with_config",
        lambda cmd, config: calls.append((cmd, config)) or "",
    )
    monkeypatch.setattr("semgrep.meta.git_check_output", lambda cmd: "")

    GitlabMeta._fetch_branch_get_merge_base("main", "headsha")

    ((cmd, config),) = calls
    assert cmd == ["git", "fetch", "https://gitlab.example/org/repo", "main"]
    expected_header = "Authorization: Basic " + base64.b64encode(
        b"gitlab-ci-token:fake-token"
    ).decode()
    assert config == {
        "http.https://gitlab.example/org/repo.extraHeader": expected_header
    }


@pytest.mark.quick
def test_gitlab_fetch_helper_on_old_git(monkeypatch):
    """
    Older git ignores the GIT_CONFIG_* variables: an inline credential
    helper supplies the token, and the command line never carries it.
    """
    from semgrep.meta import GitlabMeta

    calls = []
    monkeypatch.setenv(
        "CI_MERGE_REQUEST_PROJECT_URL", "https://gitlab.example/org/repo"
    )
    monkeypatch.setenv("CI_JOB_TOKEN", "fake-token")
    monkeypatch.setattr("semgrep.meta.git_supports_config_env", lambda: False)
    monkeypatch.setattr(
        "semgrep.meta.git_check_output", lambda cmd: calls.append(cmd) or ""
    )

    GitlabMeta._fetch_branch_get_merge_base("main", "headsha")

    assert calls[0] == [
        "git",
        "-c",
        "credential.helper=!f() { echo username=gitlab-ci-token; "
        "echo password=$CI_JOB_TOKEN; }; f",
        "fetch",
        "https://gitlab.example/org/repo",
        "main",
    ]
    assert "fake-token" not in " ".join(calls[0])


@pytest.mark.quick
def test_gitlab_baseline_rev_resolves_base_sha(tmp_path, monkeypatch):
    """
    A baseline rev reaches the project metadata's base_sha as the commit
    it names; an unresolvable rev leaves it out.
    """
    import subprocess

    import semgrep.semgrep_interfaces.semgrep_output_v1 as out
    from semgrep.meta import GitlabMeta

    repo = tmp_path / "repo"
    repo.mkdir()
    monkeypatch.chdir(repo)
    subprocess.run(["git", "init", "-q"], check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.email=test@example.com",
            "-c",
            "user.name=test",
            "commit",
            "-q",
            "--allow-empty",
            "-m",
            "init",
        ],
        check=True,
    )
    subprocess.run(["git", "branch", "base"], check=True)
    head = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        capture_output=True,
        encoding="utf-8",
        check=True,
    ).stdout.strip()

    assert GitlabMeta("base").to_project_metadata().base_sha == out.Sha1(head)
    assert GitlabMeta("no-such-branch").to_project_metadata().base_sha is None


def _branchoff_meta(monkeypatch, merge_base_returncode, commits_local):
    """
    A GithubMeta in PR context whose git merge-base always fails with the
    given exit code; fetches are recorded, git cat-file -e reports the
    commits as present or missing.
    """
    import subprocess

    from semgrep.error import SemgrepError
    from semgrep.meta import GithubMeta

    fetch_depths = []

    def fake_git_check_output(cmd):
        if cmd[1] == "fetch":
            fetch_depths.append(cmd[cmd.index("--depth") + 1])
        elif cmd[1] == "cat-file" and not commits_local:
            raise SemgrepError("missing")
        return ""

    def failing_merge_base(cmd, **kwargs):
        raise subprocess.CalledProcessError(
            merge_base_returncode, cmd, stderr="some git error"
        )

    monkeypatch.setattr("semgrep.meta.git_check_output", fake_git_check_output)
    monkeypatch.setattr("semgrep.meta.subprocess.run", failing_merge_base)
    monkeypatch.setattr(GithubMeta, "MAX_FETCH_ATTEMPT_COUNT", 1)
    monkeypatch.setattr(GithubMeta, "_base_branch_ref", property(lambda _: "main"))
    monkeypatch.setattr(GithubMeta, "_head_branch_ref", property(lambda _: "feat"))
    monkeypatch.delenv("GH_TOKEN", raising=False)
    meta = GithubMeta(None)
    meta.__dict__["head_branch_hash"] = "a" * 40
    meta.__dict__["base_branch_hash"] = "b" * 40
    return meta, fetch_depths


@pytest.mark.quick
def test_github_branchoff_last_attempt_fetches_everything(monkeypatch):
    """
    No common ancestor (exit 1) fetches deeper, and the last attempt
    fetches all commits before giving up.
    """
    meta, fetch_depths = _branchoff_meta(monkeypatch, 1, commits_local=True)
    with pytest.raises(Exception, match="Could not find branch-off point"):
        meta._find_branchoff_point()
    assert fetch_depths == [str(2**31 - 1)] * 2


@pytest.mark.quick
def test_github_branchoff_missing_commit_fetches_deeper(monkeypatch):
    """
    Any other failure with a commit missing locally also fetches deeper.
    """
    meta, fetch_depths = _branchoff_meta(monkeypatch, 128, commits_local=False)
    with pytest.raises(Exception, match="Could not find branch-off point"):
        meta._find_branchoff_point()
    assert len(fetch_depths) == 2


@pytest.mark.quick
def test_github_branchoff_unexpected_failure_is_final(monkeypatch):
    """
    A failure with both commits present raises at once, without fetching.
    """
    meta, fetch_depths = _branchoff_meta(monkeypatch, 128, commits_local=True)
    with pytest.raises(Exception, match="Unexpected git merge-base error"):
        meta._find_branchoff_point()
    assert fetch_depths == []


@pytest.mark.quick
def test_provider_subdir_qualifies_display_name(monkeypatch):
    """
    --subdir keeps its display-name qualifier under a CI provider.
    """
    from pathlib import Path

    from semgrep.meta import generate_meta_from_environment

    monkeypatch.setenv("GITLAB_CI", "true")
    monkeypatch.setenv("CI_PROJECT_PATH", "group/project")
    meta = generate_meta_from_environment(None, Path("services/api"))
    assert meta.repo_display_name == "group/project/services/api"
