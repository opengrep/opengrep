<!-- reference
id: flag-force-color
kind: flag
name: --force-color
aliases: [--no-force-color]
summary: Style the output even when it is not going to a terminal.
commands: [scan, ci, test, validate]
env: [env-OPENGREP_FORCE_COLOR]
related: [env-NO_COLOR, flag-skin]
-->
# `--force-color`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep validate`](../commands/validate.md)
- **Also spelled:** `--no-force-color`
- **Environment:** [`OPENGREP_FORCE_COLOR`](../env/OPENGREP_FORCE_COLOR.md)
- **See also:** [`NO_COLOR`](../env/NO_COLOR.md), [`--skin`](skin.md)
<!-- END GENERATED: facts -->

Opengrep styles its output only when it is writing to a terminal, so a report
sent through a pipe or into a file arrives as plain text. `--force-color`
keeps the colour anyway, which is what a pager or a CI log viewer that
understands ANSI codes wants. `--no-force-color` turns it back off.

It wins over [`NO_COLOR`](../env/NO_COLOR.md) and
`OPENGREP_FORCE_NO_COLOR`. The same thing can be asked for with
`OPENGREP_FORCE_COLOR`.

## Examples

### Colour through a pipe

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py | grep -q $'\e\[' && echo styled || echo plain
plain
$ opengrep scan --config rule.yaml --force-color app.py | grep -q $'\e\[' && echo styled || echo plain
styled
```
