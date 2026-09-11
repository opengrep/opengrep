<!-- reference
id: flag-vim-output
kind: flag
name: --vim-output
summary: Also write the findings in Vim quickfix format to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-vim, flag-output]
-->
# `--vim-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--vim`](vim.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the lines that [`--vim`](vim.md) prints, one per finding, to FILE, in
addition to what the run prints on standard output. In Vim, `:cfile FILE` turns
the file into a quickfix list.

Give the flag several times to write several copies. When the scan finds
nothing, the file is written and empty. [`--output`](output.md) describes the
rules that all output files follow.

## Examples

### A quickfix file

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
$ opengrep scan --config rule.yaml --vim-output quickfix.txt app.py > /dev/null 2>&1
$ cat quickfix.txt
app.py:1:1:W:find-eval:found eval
```
