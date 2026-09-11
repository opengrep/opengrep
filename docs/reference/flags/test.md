<!-- reference
id: flag-test
kind: flag
name: --test
summary: Run the rule tests and scan nothing; the older spelling of the test command.
commands: [scan]
related: [cmd-test, flag-validate, flag-test-ignore-todo]
-->
# `--test`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`opengrep test`](../commands/test.md), [`--validate`](validate.md), [`--test-ignore-todo`](test-ignore-todo.md)
<!-- END GENERATED: facts -->

Runs the rule tests instead of scanning, exactly as
[`opengrep test`](../commands/test.md) does: each rule file is paired with the
file named after it, and the `# ruleid:` and `# ok:` annotations are checked.

It exists because older versions had no `test` command. Prefer the command,
which is where the annotations and the pairing rules are documented.

## Examples

### Running the tests of a directory

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
eval(1)
# ok: find-eval
evaluate(2)
```

**Command and result:**
```console
$ opengrep scan --test .
1/1: ✓ All tests passed
No tests for fixes found.
```
