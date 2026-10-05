<!-- reference
id: cmd-install-ci
kind: command
name: opengrep install-ci
summary: Add a GitHub Actions workflow that runs opengrep ci on pull requests.
related: [cmd-ci, flag-env, flag-repo, flag-update, flag-dryrun]
-->
# `opengrep install-ci`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep ci`](ci.md), [`--env`](../flags/env.md), [`--repo`](../flags/repo.md), [`--update`](../flags/update.md), [`--dryrun`](../flags/dryrun.md)
<!-- END GENERATED: facts -->

```
opengrep install-ci [FLAGS] [REPO_PATH]
```

Adds a workflow file, `.github/workflows/opengrep.yml`, that runs
[`opengrep ci`](ci.md) on pull requests against the default branch. The
repository is the current directory unless [`--repo`](../flags/repo.md) or the
positional argument names another one, as a path or as `owner/repo`.

Only GitHub Actions is supported; see [`--env`](../flags/env.md).

The command drives the [GitHub CLI](https://cli.github.com/), `gh`, which must
be installed and authenticated. It asks `gh` whether the workflow already
exists, writes the file, commits it, and opens a pull request.
An existing workflow is left alone unless [`--update`](../flags/update.md) is
given, and [`--dryrun`](../flags/dryrun.md) reports what would happen without
running any of it.

## Exit status

0 on success, 2 on a fatal error, 141 when the reader of the output closed the
pipe.

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | Listed in [Internal and debugging interfaces](../internal.md#flags). |
| [`--dryrun`](../flags/dryrun.md) | Show what would be changed without changing it; the two commands mean different things by it. |
| [`--env`](../flags/env.md) | The CI system to install a workflow for; only GitHub Actions is supported. |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| `--profile` | Listed in [Internal and debugging interfaces](../internal.md#flags). |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--repo`](../flags/repo.md) | The repository to add the workflow to, as a path or as owner/repo. |
| [`--update`](../flags/update.md) | Replace a workflow that is already there instead of leaving it alone. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
<!-- END GENERATED: flags -->

## Examples

### Adding the workflow

<!-- not run: needs an authenticated GitHub CLI and a repository on GitHub -->
**Command:**
```console no-check
$ opengrep install-ci
 SUCCESS  Installed opengrep workflow for this repository
$ cat .github/workflows/opengrep.yml
```
