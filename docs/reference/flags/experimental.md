<!-- reference
id: flag-experimental
kind: flag
name: --experimental
summary: Accepted for compatibility; opengrep has only the one implementation.
commands: [scan, ci, test, validate, show, lsp, install-ci]
related: [flag-version]
-->
# `--experimental`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep validate`](../commands/validate.md), [`opengrep show`](../commands/show.md), [`opengrep lsp`](../commands/lsp.md), [`opengrep install-ci`](../commands/install-ci.md)
- **See also:** [`--version`](version.md)
<!-- END GENERATED: facts -->

Semgrep shipped two implementations of its command line, a Python one and an
OCaml one, and this flag chose the OCaml one. Opengrep has only the OCaml
implementation, so the flag changes nothing. It is accepted so that scripts
and CI configurations written for semgrep keep working.

It is one of the three flags that may come before the command, as in
`opengrep --experimental scan …`, the others being `--debug` and `--profile`.

Its sibling `--develop` is for opengrep's own development, and the two are
mutually exclusive. See
[Internal and debugging interfaces](../internal.md).

## Examples

### With and without, the same scan

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
$ opengrep --experimental scan --config rule.yaml app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
