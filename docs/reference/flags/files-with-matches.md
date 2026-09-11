<!-- reference
id: flag-files-with-matches
kind: flag
name: --files-with-matches
summary: Print only the paths of the files that have findings.
commands: [scan, ci]
related: [flag-emacs, flag-vim, flag-json, flag-output]
-->
# `--files-with-matches`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--emacs`](emacs.md), [`--vim`](vim.md), [`--json`](json.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Replaces the report with the list of files that have at least one finding, one
path per line, like `grep -l`. Nothing is said about which rule matched or
where, which makes it easy to feed the list to another command.

## Examples

### The files, not the findings

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

**`clean.py`**
```python title="clean.py"
print(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --files-with-matches .
app.py
```
