<!-- reference
id: flag-enable-version-check
kind: flag
name: --enable-version-check
aliases: [--disable-version-check]
summary: Accepted for compatibility; opengrep never checks for a newer version.
commands: [scan, ci]
default: true
related: [flag-version]
-->
# `--enable-version-check`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--disable-version-check`
- **Default:** `true`
- **See also:** [`--version`](version.md)
<!-- END GENERATED: facts -->

Semgrep asks a server whether a newer release exists, and these flags turned
that on and off. Opengrep never makes the request, so neither spelling changes
anything: they are accepted so that scripts and CI configurations written for
semgrep keep working.

Nothing here reaches the network. The registry is the only thing opengrep
fetches, and only when [`--config`](config.md) asks for it.

## Examples

### Neither spelling changes the scan

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
$ opengrep scan --config rule.yaml --disable-version-check app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
