<!-- reference
id: opt-taint_assume_safe_numbers
kind: option
name: taint_assume_safe_numbers
summary: Assume that a number is clean, even when it was computed from tainted data.
value: `true` or `false`
default: false
related: []
-->
# `taint_assume_safe_numbers`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`taint_assume_safe_booleans`](taint_assume_safe_booleans.md)
<!-- END GENERATED: facts -->

With `taint_assume_safe_numbers: true`, a value of integer or floating-point
type is clean, even when it was computed from tainted data, as
`Integer.parseInt(page)` is from `page`.

Like [`taint_assume_safe_booleans`](taint_assume_safe_booleans.md), this
depends on opengrep knowing the type. In Java it does, from declared variable
types and method return types. In Go and TypeScript, only the declared type of
a variable counts. In Python the option has no effect.

## Examples

### Numbers in Java

The rule `page-logged` turns the option on.

**`Paging.yaml`**
```yaml title="Paging.yaml"
rules:
  - id: page-logged-default
    mode: taint
    pattern-sources:
      - pattern: request.getParameter(...)
    pattern-sinks:
      - pattern: log.info(...)
    message: request parameter is logged
    languages: [java]
    severity: WARNING
  - id: page-logged
    mode: taint
    pattern-sources:
      - pattern: request.getParameter(...)
    pattern-sinks:
      - pattern: log.info(...)
    message: request parameter is logged
    languages: [java]
    severity: WARNING
    options:
      taint_assume_safe_numbers: true
```

**`Paging.java`**
```java title="Paging.java"
class Paging {
  void handle() {
    String page = request.getParameter("page");
    // ruleid: page-logged-default, page-logged
    log.info(page.trim());
    int number = Integer.parseInt(page);
    // ruleid: page-logged-default
    log.info(number);
    // ruleid: page-logged-default
    log.info(page.length());
  }
}
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### No effect in Python

**`sizes.yaml`**
```yaml title="sizes.yaml"
rules:
  - id: size-logged
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
    options:
      taint_assume_safe_numbers: true
```

**`sizes.py`**
```python title="sizes.py"
text = input()
number = int(text)
# ruleid: size-logged
log(number)
size = len(text)
# ruleid: size-logged
log(size)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
