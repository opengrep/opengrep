<!-- reference
id: flag-matching-diagnosis
kind: flag
name: --matching-diagnosis
summary: Explain why a failing rule test did not match.
commands: [test]
related: [cmd-test, flag-matching-explanations]
-->
# `--matching-diagnosis`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep test`](../commands/test.md)
- **See also:** [`opengrep test`](../commands/test.md), [`--matching-explanations`](matching-explanations.md)
<!-- END GENERATED: facts -->

After a failing test, [`opengrep test`](../commands/test.md) says which lines
were missed, but not why. With `--matching-diagnosis` it adds a section that
works through the rule against the annotated line and says what went wrong:
which part of the pattern never matched, or which condition removed the match.

This is the test-time companion to
[`--matching-explanations`](matching-explanations.md), which puts the same
kind of information into the JSON output of a scan.

## Examples

### Why a line was not matched

**`find-eval.yaml`**
```yaml title="find-eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`find-eval.py`**
```python title="find-eval.py"
# ruleid: find-eval
evaluate(1)
```

**Command and result:**
```console
$ opengrep test --matching-diagnosis . 2>&1 | grep -A2 'Matching diagnosis:'
Matching diagnosis:
find-eval.py: Unexpected lack of match at line 2
This line was never matched by any base pattern, nor introduced by any `focus-metavariable`.
```

The annotation asks for a match on line 2, which `evaluate(1)` does not give,
so the test fails and the diagnosis says the line matched no pattern at all.
