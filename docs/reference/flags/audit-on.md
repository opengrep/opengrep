<!-- reference
id: flag-audit-on
kind: flag
name: --audit-on
summary: Report blocking findings but exit 0 when the CI event has one of these names.
commands: [ci]
value: `EVENT`, repeatable
env: [env-OPENGREP_AUDIT_ON]
related: [cmd-ci, flag-suppress-errors]
-->
# `--audit-on`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep ci`](../commands/ci.md)
- **Value:** `EVENT`, repeatable
- **Environment:** [`OPENGREP_AUDIT_ON`](../env/OPENGREP_AUDIT_ON.md)
- **See also:** [`opengrep ci`](../commands/ci.md), [`--suppress-errors`](suppress-errors.md)
<!-- END GENERATED: facts -->

[`opengrep ci`](../commands/ci.md) exits 1 when a scan reports a blocking
finding. With `--audit-on EVENT`, it still reports the findings but exits 0,
as long as the event that triggered the run is named EVENT. This runs a rule
set over a repository without failing the job while the findings are being
triaged.

The event name is the one `ci` prints in its `SCAN ENVIRONMENT` block, such as
`push` or `pull_request` on GitHub Actions. Outside a CI provider it is
`unknown`. Repeat the flag to name several events, or set
`OPENGREP_AUDIT_ON` to a whitespace-separated list.

## Examples

### Findings without a failing job

Outside a CI provider the event is `unknown`, so `--audit-on unknown` shows
what the flag does.

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
$ opengrep ci --config rule.yaml --audit-on unknown 2>&1 >/dev/null | tail -3
CI scan completed successfully.
  Found 1 finding (1 blocking) from 1 rule.
  Audit mode is on for unknown, so exiting with code 0 even if matches found
$ opengrep ci --config rule.yaml --audit-on unknown > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep ci --config rule.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
```
