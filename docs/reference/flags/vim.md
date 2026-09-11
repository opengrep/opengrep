<!-- reference
id: flag-vim
kind: flag
name: --vim
summary: Print one line per finding, in the form Vim quickfix lists parse.
commands: [scan, ci]
related: [flag-emacs, flag-json, flag-output, flag-files-with-matches]
-->
# `--vim`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--emacs`](emacs.md), [`--json`](json.md), [`--output`](output.md), [`--files-with-matches`](files-with-matches.md), [`--vim-output`](vim-output.md)
<!-- END GENERATED: facts -->

Prints one line per finding:

```
PATH:LINE:COL:SEVERITY:RULE_ID:MESSAGE
```

The severity is a single letter, `W` for a warning. The line is what Vim's
`errorformat` reads, so `:cfile` turns a scan into a quickfix list.

[`--vim-output`](vim-output.md) writes the same lines to a file, alongside
whatever the run prints, which leaves a quickfix file behind. Only one format
goes to standard output at a time, so `--vim` cannot be combined with
`--json`, `--emacs`, `--sarif` and the others.

## Examples

### One line per finding

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
$ opengrep scan --config rule.yaml --vim app.py
app.py:1:1:W:find-eval:found eval
```
