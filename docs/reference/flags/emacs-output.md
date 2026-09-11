<!-- reference
id: flag-emacs-output
kind: flag
name: --emacs-output
summary: Also write the findings in Emacs single-line format to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-emacs, flag-output]
-->
# `--emacs-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--emacs`](emacs.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the lines that [`--emacs`](emacs.md) prints, one per finding, to FILE,
in addition to what the run prints on standard output. Emacs
`compilation-mode` can then read the file while the terminal shows the usual
report.

Give the flag several times to write several copies. When the scan finds
nothing, the file is written and empty. [`--output`](output.md) describes the
rules that all output files follow.

## Examples

### A file for Emacs

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
$ opengrep scan --config rule.yaml --emacs-output findings.txt app.py > /dev/null 2>&1
$ cat findings.txt
app.py:1:1:warning(find-eval):eval(1):found eval
```
