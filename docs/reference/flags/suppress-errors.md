<!-- reference
id: flag-suppress-errors
kind: flag
name: --suppress-errors
aliases: [--no-suppress-errors]
summary: Whether an error fails opengrep ci; on by default, so errors exit 0.
commands: [ci]
default: true
env: [env-OPENGREP_SUPPRESS_ERRORS]
related: [cmd-ci, flag-audit-on]
-->
# `--suppress-errors`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--no-suppress-errors`
- **Default:** `true`
- **Environment:** [`OPENGREP_SUPPRESS_ERRORS`](../env/OPENGREP_SUPPRESS_ERRORS.md)
- **See also:** [`opengrep ci`](../commands/ci.md), [`--audit-on`](audit-on.md)
<!-- END GENERATED: facts -->

On by default. Any exit status of [`opengrep ci`](../commands/ci.md) other
than 0 and 1 becomes 0, so a run that breaks does not fail the build. The
findings that were produced before the error are still reported, and the
reason appears on standard error:

```
There were errors during analysis but the scan will succeed because there
were no blocking findings, use --no-suppress-errors if you want it to fail
when there are errors.
```

`--no-suppress-errors` keeps the real status, which is the one
[`opengrep scan`](../commands/scan.md) would have returned: 7 for a rule file
that does not load, 2 for a fatal error, and so on. Blocking findings are not
errors, so exit status 1 is unaffected either way. `OPENGREP_SUPPRESS_ERRORS`
sets the same thing.

## Examples

### A rule file that does not load

**`broken.yaml`**
```yaml title="broken.yaml"
rules:
  - id: broken
    patterns:
      - pattern-not: eval("...")
    message: a rule that cannot work
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ git init -q && git add . && git commit -qm init
$ opengrep ci --config broken.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep ci --config broken.yaml --no-suppress-errors > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
$ opengrep scan --config broken.yaml . > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
