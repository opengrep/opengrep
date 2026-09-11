<!-- reference
id: flag-repo
kind: flag
name: --repo
aliases: [-r]
summary: The repository to add the workflow to, as a path or as owner/repo.
commands: [install-ci]
value: `REPO_PATH`
default: .
related: [cmd-install-ci, flag-env, flag-update]
-->
# `--repo`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep install-ci`](../commands/install-ci.md)
- **Also spelled:** `-r`
- **Value:** `REPO_PATH`
- **Default:** `.`
- **See also:** [`opengrep install-ci`](../commands/install-ci.md), [`--env`](env.md), [`--update`](update.md)
<!-- END GENERATED: facts -->

Tells [`opengrep install-ci`](../commands/install-ci.md) which repository to
work on. The value is either a path to a local git repository, or a name of
the form `owner/repo`, which the GitHub CLI resolves. The default is the
current directory.

The same value can be given as the positional argument,
`opengrep install-ci owner/repo`. When both are given, `--repo` wins.

## Examples

### Naming a repository

The command needs the GitHub CLI, logged in to an account, and a repository on
GitHub. See [`opengrep install-ci`](../commands/install-ci.md) for what it
reports when those are missing.

<!-- not run: needs an authenticated GitHub CLI and a repository on GitHub -->
**Command:**
```console no-check
$ opengrep install-ci --repo owner/repo
 SUCCESS  Installed opengrep workflow for this repository
```
