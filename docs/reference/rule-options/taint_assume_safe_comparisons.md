<!-- reference
id: opt-taint_assume_safe_comparisons
kind: option
name: taint_assume_safe_comparisons
summary: Assume that the result of a comparison is clean, even when an operand is tainted.
value: `true` or `false`
default: false
related: []
-->
# `taint_assume_safe_comparisons`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
<!-- END GENERATED: facts -->

By default, an operation on tainted data gives a tainted result, and a
comparison is no exception: `answer == "yes"` is tainted when `answer` is.

With `taint_assume_safe_comparisons: true`, the result of a comparison is
clean. In Python that covers `==`, `!=`, `<`, `>=`, `in` and `is`, among the
others. Operators that are not comparisons, such as `+` and `not`, still
spread taint.

## Examples

### Comparisons on user input

The rule `logged` turns the option on.

**`logged.yaml`**
```yaml title="logged.yaml"
rules:
  - id: logged-default
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
  - id: logged
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
    options:
      taint_assume_safe_comparisons: true
```

**`logged.py`**
```python title="logged.py"
answer = input()
# ruleid: logged-default, logged
log(answer + "!")
# ruleid: logged-default, logged
log(not answer)
# ruleid: logged-default
log(answer == "yes")
# ruleid: logged-default
log(answer != "no")
# ruleid: logged-default
log(len(answer) >= 3)
# ruleid: logged-default
log(answer in choices)
# ruleid: logged-default
log(answer is None)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
