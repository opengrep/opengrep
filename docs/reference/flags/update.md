<!-- reference
id: flag-update
kind: flag
name: --update
aliases: [-u]
summary: Replace a workflow that is already there instead of leaving it alone.
commands: [install-ci]
default: false
related: [cmd-install-ci, flag-repo]
-->
# `--update`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep install-ci`](../commands/install-ci.md)
- **Also spelled:** `-u`
- **Default:** `false`
- **See also:** [`opengrep install-ci`](../commands/install-ci.md), [`--repo`](repo.md)
<!-- END GENERATED: facts -->

[`opengrep install-ci`](../commands/install-ci.md) asks the GitHub CLI whether
the repository already has an `opengrep.yml` workflow, and leaves it alone if
it does. `--update` writes the workflow anyway, which is what you want after
an attempt that only half succeeded.

## Examples

### Replacing an existing workflow

The command needs the GitHub CLI, logged in to an account, and a repository on
GitHub.

<!-- not run: needs an authenticated GitHub CLI and a repository on GitHub -->
**Command:**
```console no-check
$ opengrep install-ci --update
 SUCCESS  Installed opengrep workflow for this repository
```
