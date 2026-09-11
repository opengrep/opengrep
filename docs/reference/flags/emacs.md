<!-- reference
id: flag-emacs
kind: flag
name: --emacs
summary: Print one line per finding, in the form Emacs compilation buffers parse.
commands: [scan, ci]
related: [flag-vim, flag-json, flag-output, flag-files-with-matches]
-->
# `--emacs`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--vim`](vim.md), [`--json`](json.md), [`--output`](output.md), [`--files-with-matches`](files-with-matches.md), [`--emacs-output`](emacs-output.md)
<!-- END GENERATED: facts -->

Prints one line per finding:

```
PATH:LINE:COL:SEVERITY(RULE_ID):MATCHED_LINES:MESSAGE
```

The severity is the rule's, in lower case. This is the shape Emacs
`compilation-mode` and similar tools expect, so a finding can be jumped to
from the buffer.

[`--emacs-output`](emacs-output.md) writes the same lines to a file, alongside
whatever the run prints. Only one format may be printed to standard output at
a time, so `--emacs` cannot be combined with `--json`, `--vim`, `--sarif` and
the others.

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
$ opengrep scan --config rule.yaml --emacs app.py
app.py:1:1:warning(find-eval):eval(1):found eval
$ opengrep scan --config rule.yaml --emacs-output findings.txt app.py > /dev/null 2>&1
$ cat findings.txt
app.py:1:1:warning(find-eval):eval(1):found eval
```
