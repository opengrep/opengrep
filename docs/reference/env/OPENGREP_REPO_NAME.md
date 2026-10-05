<!-- reference
id: env-OPENGREP_REPO_NAME
kind: env
name: OPENGREP_REPO_NAME
aliases: [SEMGREP_REPO_NAME]
summary: On GitHub Actions, the repository opengrep ci asks GitHub about for a pull request's merge base.
value: a repository name
related: [cmd-ci]
-->
# `OPENGREP_REPO_NAME`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_REPO_NAME`
- **Value:** a repository name
- **See also:** [`opengrep ci`](../commands/ci.md)
<!-- END GENERATED: facts -->

On GitHub Actions, when [`opengrep ci`](../commands/ci.md) scans a pull request
and `GH_TOKEN` is set, it asks the GitHub API for the pull request's merge base.
This variable names the repository to ask about, as `owner/repo`; when it is
unset, `GITHUB_REPOSITORY`, which GitHub Actions sets, names it instead. A
wrong name makes that lookup fail, and opengrep then fetches history and
computes the merge base locally, which is slower.

## Examples

### Naming the repository on GitHub Actions

<!-- not run: needs GitHub Actions, GH_TOKEN and a pull request on GitHub -->
**Command:**
```console no-check
$ OPENGREP_REPO_NAME=my-org/my-repo opengrep ci --config rule.yaml
```
