<!-- reference
id: env-CI
kind: env
name: CI
summary: Set by CI services; any value other than false or 0 turns off the status line of a scan.
value: any value; `false` and `0` count as unset
-->
# `CI`

<!-- BEGIN GENERATED: facts -->
- **Value:** any value; `false` and `0` count as unset
- **See also:** [`--no-progress-bar`](../flags/no-progress-bar.md)
<!-- END GENERATED: facts -->

CI services set `CI` in the environment of the jobs they run, some of them on
a terminal. When it is set, `opengrep scan` does not draw the status line that
shows the progress of the scan, which a CI log would otherwise record as one
line per redraw. The values `false` and `0`, in any case, count as unset.

It has no other effect: [`opengrep ci`](../commands/ci.md) finds out which CI
service it runs on from that service's own variables. The name is read as it
is, with no `OPENGREP_` form.

## Examples

### A scan on a terminal, as a CI job

<!-- not run: the status line is drawn only on a terminal -->
**Command:**
```console no-check
$ CI=true opengrep scan --config rule.yaml .
```
