<!-- reference
id: env-OPENGREP_BASELINE_REF
kind: env
name: OPENGREP_BASELINE_REF
aliases: [SEMGREP_BASELINE_REF]
summary: Another name for the baseline, used when OPENGREP_BASELINE_COMMIT is not set.
value: a git revision
related: []
-->
# `OPENGREP_BASELINE_REF`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_BASELINE_REF`
- **Value:** a git revision
- **Equivalent to:** [`--baseline-commit`](../flags/baseline-commit.md)
- **See also:** [`OPENGREP_BASELINE_COMMIT`](OPENGREP_BASELINE_COMMIT.md)
<!-- END GENERATED: facts -->

Does what [`OPENGREP_BASELINE_COMMIT`](OPENGREP_BASELINE_COMMIT.md) does, under
a second name that some CI templates use. It is consulted only when neither
`OPENGREP_BASELINE_COMMIT` nor `SEMGREP_BASELINE_COMMIT` is set, and like them
it gives way to [`--baseline-commit`](../flags/baseline-commit.md) on the
command line.

## Examples

### Which name wins

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
$ OPENGREP_BASELINE_REF=HEAD~1 opengrep scan --config rule.yaml --files-with-matches .
b.py
$ OPENGREP_BASELINE_COMMIT=HEAD OPENGREP_BASELINE_REF=HEAD~1 opengrep scan --config rule.yaml --files-with-matches .
```

The last command prints nothing. `OPENGREP_BASELINE_COMMIT` wins, and relative
to `HEAD` there is nothing new.
