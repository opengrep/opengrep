<!-- reference
id: flag-version
kind: flag
name: --version
summary: Print the opengrep version and exit.
commands: [scan]
related: [cmd-show, flag-show-supported-languages]
-->
# `--version`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`opengrep show`](../commands/show.md), [`--show-supported-languages`](show-supported-languages.md), [`--enable-version-check`](enable-version-check.md), [`--experimental`](experimental.md)
<!-- END GENERATED: facts -->

Prints the version on standard output and exits 0, scanning nothing. Since
`scan` is the default command, `opengrep --version` works as well, and
[`opengrep show version`](../commands/show.md) prints the same string.

The version is what a rule's `min-version` and `max-version` keys are compared
against.

## Examples

### Asking for the version

**Command and result:**
```console
$ opengrep --version
X.Y.Z
$ opengrep scan --version
X.Y.Z
```
