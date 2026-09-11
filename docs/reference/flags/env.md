<!-- reference
id: flag-env
kind: flag
name: --env
summary: The CI system to install a workflow for; only GitHub Actions is supported.
commands: [install-ci]
value: `CI_ENV`
default: Github
related: [cmd-install-ci, flag-repo]
-->
# `--env`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep install-ci`](../commands/install-ci.md)
- **Value:** `CI_ENV`
- **Default:** `Github`
- **See also:** [`opengrep install-ci`](../commands/install-ci.md), [`--repo`](repo.md)
<!-- END GENERATED: facts -->

Names the CI system that [`opengrep install-ci`](../commands/install-ci.md)
writes a workflow for. `Github` is the only value it accepts, and the default.
Any other value is a fatal error, reported with the name in lower case.

## Examples

### The only value

**Command and result:**
```console
$ opengrep install-ci --env Gitlab 2>&1 | tail -1
[00.04][ERROR]: CI_ENV 'gitlab' not supported!
$ opengrep install-ci --env Gitlab > /dev/null 2>&1; echo "exit status: $?"
exit status: 2
```
