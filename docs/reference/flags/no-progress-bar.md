<!-- reference
id: flag-no-progress-bar
kind: flag
name: --no-progress-bar
summary: Do not draw the line that shows the progress of a scan on the terminal.
commands: [scan]
related: [flag-skin, flag-incremental-output, env-CI, env-TERM]
-->
# `--no-progress-bar`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`--skin`](skin.md), [`--incremental-output`](incremental-output.md), [`CI`](../env/CI.md), [`TERM`](../env/TERM.md)
<!-- END GENERATED: facts -->

While a scan runs, opengrep draws a status line on the terminal that says what
it is doing, such as loading the rules or analysing the targets. The line is
erased when the scan ends and never appears in the report. This flag turns it
off.

The line is drawn on standard error, and only when all of these hold, so the
flag is rarely needed:

- standard error is a terminal, and [`TERM`](../env/TERM.md) is set to
  something other than `dumb` or `unknown`;
- [`CI`](../env/CI.md) is not set, or is `false` or `0`;
- opengrep is not running on Windows;
- the [skin](skin.md) is `simple` or `vivid`;
- [`--incremental-output`](incremental-output.md) is not given;
- neither `--quiet`, `--verbose` nor `--debug` is given.

The output format does not matter: a `--json` scan draws it too, since the
JSON goes to standard output. `opengrep ci` never draws it.

## Examples

### A scan without the status line

<!-- not run: the status line is drawn only on a terminal -->
**Command:**
```console no-check
$ opengrep scan --config rule.yaml --no-progress-bar .
```
