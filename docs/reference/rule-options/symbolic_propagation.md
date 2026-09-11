<!-- reference
id: opt-symbolic_propagation
kind: option
name: symbolic_propagation
summary: Let patterns see through variables that hold the result of an expression.
value: `true` or `false`
default: false
related: [opt-constant_propagation]
-->
# `symbolic_propagation`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** `constant_propagation`
<!-- END GENERATED: facts -->

With `symbolic_propagation: true`, a variable that is assigned an expression
also matches that expression. After `cur = db.cursor()`, the pattern
`$DB.cursor().execute(...)` matches `cur.execute(sql)`.

This works only for a variable that is assigned once. A variable that is
assigned again is matched as itself.

Constant values need no option. [`constant_propagation`](../rule-options.md),
on by default, already lets `"..."` match a variable holding a string literal.
`symbolic_propagation` has no effect when `constant_propagation` is `false`.

## Examples

### A cursor held in a variable

```yaml title="unsafe-query.yaml"
rules:
  - id: unsafe-query
    pattern: $DB.cursor().execute(...)
    message: raw SQL executed on a cursor
    languages: [python]
    severity: WARNING
    options:
      symbolic_propagation: true
```

```python title="unsafe-query.py"
def query(db, sql):
    cur = db.cursor()
    # ruleid: unsafe-query
    cur.execute(sql)

def query_other(db, other, sql):
    cur = db.cursor()
    cur = other
    # ok: unsafe-query
    cur.execute(sql)
```

```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### Without the option

The same rule without `symbolic_propagation` misses line 4.

```yaml title="unsafe-query.yaml"
rules:
  - id: unsafe-query
    pattern: $DB.cursor().execute(...)
    message: raw SQL executed on a cursor
    languages: [python]
    severity: WARNING
```

```python title="unsafe-query.py"
def query(db, sql):
    cur = db.cursor()
    # ruleid: unsafe-query
    cur.execute(sql)
```

```console
$ opengrep test .
0/1: 1 unit tests did not pass:
--------------------------------------------------------------------------------
	✖ unsafe-query
	missed lines: [4], incorrect lines: []
	test file path: <tmp>/unsafe-query.py


No tests for fixes found.
```
