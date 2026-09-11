<!-- reference
id: flag-test-ignore-todo
kind: flag
name: --test-ignore-todo
aliases: [--no-test-ignore-todo]
summary: Documented as ignoring todoruleid annotations; in 1.30.0 it changes nothing.
commands: [scan, test]
default: false
related: [cmd-test, flag-test]
-->
# `--test-ignore-todo`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep test`](../commands/test.md)
- **Also spelled:** `--no-test-ignore-todo`
- **Default:** `false`
- **See also:** [`opengrep test`](../commands/test.md), [`--test`](test.md)
<!-- END GENERATED: facts -->

In a test file, `# todoruleid: <id>` marks a line the rule ought to match but
does not yet, and `# todook: <id>` the reverse. Neither fails a test: they
record work still to do while keeping the suite green. See
[`opengrep test`](../commands/test.md) for the annotations.

The flag is documented as making [`opengrep test`](../commands/test.md) ignore
the `todoruleid:` lines. In 1.30.0 it makes no difference to the outcome. Both
kinds of annotation were tried, on a line the rule matches and on a line it
does not, and the result and exit status were the same with the flag and
without it. A missing `ruleid:` still fails, so the tests themselves work as
documented.

## Examples

### The same result either way

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
# todoruleid: find-eval
evaluate(1)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
$ opengrep test --test-ignore-todo .
1/1: ✓ All tests passed
No tests for fixes found.
```
