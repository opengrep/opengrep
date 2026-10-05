<!-- reference
id: env-OPENGREP_BASELINE_COMMIT
kind: env
name: OPENGREP_BASELINE_COMMIT
aliases: [SEMGREP_BASELINE_COMMIT]
summary: The baseline for scan and ci when --baseline-commit is not given.
value: a git revision
related: [env-OPENGREP_BASELINE_REF]
-->
# `OPENGREP_BASELINE_COMMIT`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_BASELINE_COMMIT`
- **Value:** a git revision
- **Equivalent to:** [`--baseline-commit`](../flags/baseline-commit.md)
- **See also:** [`OPENGREP_BASELINE_REF`](OPENGREP_BASELINE_REF.md)
<!-- END GENERATED: facts -->

When [`--baseline-commit`](../flags/baseline-commit.md) is not given,
`opengrep scan` and `opengrep ci` take the baseline from this variable, and
report only the findings that are new relative to it. The value is any
revision git understands.

Four variables can set the baseline, and the first one set wins, in this
order: `OPENGREP_BASELINE_COMMIT`, `SEMGREP_BASELINE_COMMIT`,
[`OPENGREP_BASELINE_REF`](OPENGREP_BASELINE_REF.md),
`SEMGREP_BASELINE_REF`. A variable set to the empty string does not count, so a
CI template whose base-branch variable is empty scans everything instead of
failing.

## Examples

### A baseline from the environment

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: no-print
    pattern: print(...)
    message: use logging instead of print
    languages: [python]
    severity: INFO
```

**`a.py`**
```python title="a.py"
print("old")
```

**Command and result:**
```console
$ git init -q && git add . && git commit -qm one
$ echo 'print("new")' > b.py && git add b.py && git commit -qm two
$ OPENGREP_BASELINE_COMMIT=HEAD~1 opengrep scan --config rule.yaml --files-with-matches .
b.py
$ OPENGREP_BASELINE_COMMIT=HEAD~1 opengrep scan --config rule.yaml --baseline-commit HEAD~1 . 2>&1 >/dev/null | grep ignoring
[00.04][WARNING]: --baseline-commit is given; ignoring $OPENGREP_BASELINE_COMMIT
```
