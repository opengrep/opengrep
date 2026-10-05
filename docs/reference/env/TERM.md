<!-- reference
id: env-TERM
kind: env
name: TERM
summary: When unset, dumb or unknown, a scan draws no status line on the terminal.
value: the terminal type, such as `xterm-256color`
-->
# `TERM`

<!-- BEGIN GENERATED: facts -->
- **Value:** the terminal type, such as `xterm-256color`
- **See also:** [`--no-progress-bar`](../flags/no-progress-bar.md)
<!-- END GENERATED: facts -->

Terminal emulators set `TERM` to the type of terminal they provide. When it is
unset, `dumb` or `unknown`, `opengrep scan` does not draw the status line that
shows the progress of the scan: a terminal of that kind, such as an Emacs shell
buffer, would show each redraw as a line of its own.

It has no other effect. Colour does not depend on it. The name is read as it
is, with no `OPENGREP_` form.

## Examples

### A scan on a terminal that cannot redraw a line

<!-- not run: the status line is drawn only on a terminal -->
**Command:**
```console no-check
$ TERM=dumb opengrep scan --config rule.yaml .
```
