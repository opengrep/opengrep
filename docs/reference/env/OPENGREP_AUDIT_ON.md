<!-- reference
id: env-OPENGREP_AUDIT_ON
kind: env
name: OPENGREP_AUDIT_ON
aliases: [SEMGREP_AUDIT_ON]
summary: Event names for --audit-on when the flag is not given, separated by whitespace.
value: whitespace-separated event names
related: [env-OPENGREP_PR_ID]
-->
# `OPENGREP_AUDIT_ON`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_AUDIT_ON`
- **Value:** whitespace-separated event names
- **Equivalent to:** [`--audit-on`](../flags/audit-on.md)
- **See also:** [`OPENGREP_PR_ID`](OPENGREP_PR_ID.md)
<!-- END GENERATED: facts -->

When [`--audit-on`](../flags/audit-on.md) is not given, `opengrep ci` reads the
event names from this variable. If the event that triggered the run is among
them, `ci` still reports its blocking findings but exits 0.

The value is split at whitespace, so `"push schedule"` names two events. Any
`--audit-on` on the command line replaces the whole list, with a warning.

## Examples

### Audit mode set in the environment

Outside a CI provider the event is `unknown`.

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
$ git init -q && git add . && git commit -qm init
$ opengrep ci --config rule.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
$ OPENGREP_AUDIT_ON="push unknown" opengrep ci --config rule.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ OPENGREP_AUDIT_ON=unknown opengrep ci --config rule.yaml --audit-on push 2>&1 >/dev/null | grep ignoring
[00.04][WARNING]: --audit-on is given; ignoring $OPENGREP_AUDIT_ON
```
