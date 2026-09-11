<!-- reference
id: opt-taint_assume_safe_booleans
kind: option
name: taint_assume_safe_booleans
summary: Assume that a boolean value is clean, even when it was computed from tainted data.
value: `true` or `false`
default: false
related: [opt-taint_assume_safe_numbers]
-->
# `taint_assume_safe_booleans`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`taint_assume_safe_numbers`](taint_assume_safe_numbers.md)
<!-- END GENERATED: facts -->

With `taint_assume_safe_booleans: true`, a value of boolean type is clean,
even when it was computed from tainted data, as `query.isEmpty()` is from
`query`.

This depends on opengrep knowing the type. In Java it does, both from the
declared type of a variable and from the return type of a method, including
library methods such as `String.isEmpty()`. In Go and TypeScript, the declared
type of a variable counts, but the return type of a function does not. In
Python the option has no effect, even with type annotations.

## Examples

### Booleans in Java

The rule `query-logged` turns the option on.

**`Search.yaml`**
```yaml title="Search.yaml"
rules:
  - id: query-logged-default
    mode: taint
    pattern-sources:
      - pattern: request.getParameter(...)
    pattern-sinks:
      - pattern: log.info(...)
    message: request parameter is logged
    languages: [java]
    severity: WARNING
  - id: query-logged
    mode: taint
    pattern-sources:
      - pattern: request.getParameter(...)
    pattern-sinks:
      - pattern: log.info(...)
    message: request parameter is logged
    languages: [java]
    severity: WARNING
    options:
      taint_assume_safe_booleans: true
```

**`Search.java`**
```java title="Search.java"
class Search {
  void handle() {
    String query = request.getParameter("q");
    // ruleid: query-logged-default, query-logged
    log.info(query.trim());
    // ruleid: query-logged-default
    log.info(query.isEmpty());
    boolean valid = isValid(query);
    // ruleid: query-logged-default
    log.info(valid);
  }

  boolean isValid(String q) { return true; }
}
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### No effect in Python

**`flags.yaml`**
```yaml title="flags.yaml"
rules:
  - id: flag-logged
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
    options:
      taint_assume_safe_booleans: true
```

**`flags.py`**
```python title="flags.py"
def is_short(s: str) -> bool:
    return True

text = input()
digits: bool = text.isdigit()
# ruleid: flag-logged
log(digits)
short = is_short(text)
# ruleid: flag-logged
log(short)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
