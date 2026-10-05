<!-- reference
id: env-OPENGREP_SUPPRESS_ERRORS
kind: env
name: OPENGREP_SUPPRESS_ERRORS
aliases: [SEMGREP_SUPPRESS_ERRORS]
summary: Whether errors fail opengrep ci, when the flag is not given.
value: `true`, `yes`, `1` or `false`, `no`, `0`
related: []
-->
# `OPENGREP_SUPPRESS_ERRORS`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_SUPPRESS_ERRORS`
- **Value:** `true`, `yes`, `1` or `false`, `no`, `0`
- **Equivalent to:** [`--suppress-errors`](../flags/suppress-errors.md)
<!-- END GENERATED: facts -->

When neither [`--suppress-errors`](../flags/suppress-errors.md) nor
`--no-suppress-errors` is given, `opengrep ci` reads this variable. A true value
keeps the default, in which an error exits 0; a false value keeps the real exit
status, so a broken rule file fails the job.

The accepted spellings are `true`, `TRUE`, `yes` and `1` for true, and
`false`, `FALSE`, `no` and `0` for false. Anything else, including `on` and
`off`, is rejected like a bad flag value, and `ci` exits 2.

## Examples

### Letting a broken rule file fail the job

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
$ OPENGREP_SUPPRESS_ERRORS=false opengrep ci --config broken.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
$ OPENGREP_SUPPRESS_ERRORS=off opengrep ci --config broken.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 2
```
