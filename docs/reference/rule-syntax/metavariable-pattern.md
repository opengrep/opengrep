<!-- reference
id: key-metavariable-pattern
kind: rule-key
name: metavariable-pattern
summary: Keep the matches where the code a metavariable matched also matches a further pattern.
related: [key-patterns, key-metavariable-regex]
-->
# `metavariable-pattern`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md), [`metavariable-regex`](metavariable-regex.md)
<!-- END GENERATED: facts -->

`metavariable-pattern` is an item of [`patterns`](patterns.md). It keeps the
matches where the code bound to a metavariable matches a further pattern. It
takes a mapping with:

- `metavariable`: the metavariable, bound by another item;
- one of [`pattern`](pattern.md), [`pattern-either`](pattern-either.md),
  [`patterns`](patterns.md) or [`pattern-regex`](pattern-regex.md), matched
  against that code;
- `language`, optionally: the language to read the code in, when it is not
  the rule's own. With `generic`, for example, the text of a string can be
  matched word by word.

## Examples

### A query built from strings

**`sql-concat.yaml`**
```yaml title="sql-concat.yaml"
rules:
  - id: sql-concat
    patterns:
      - pattern: $CURSOR.execute($QUERY, ...)
      - metavariable-pattern:
          metavariable: $QUERY
          pattern-either:
            - pattern: '"..." + $X'
            - pattern: f"..."
    message: a SQL query built from strings
    languages: [python]
    severity: ERROR
```

**`sql-concat.py`**
```python title="sql-concat.py"
def find_user(cursor, name):
    # ruleid: sql-concat
    cursor.execute("SELECT * FROM users WHERE name = '" + name + "'")
    # ruleid: sql-concat
    cursor.execute(f"SELECT * FROM users WHERE name = '{name}'")
    # ok: sql-concat
    cursor.execute("SELECT * FROM users WHERE name = %s", (name,))
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### Words inside a string

**`drop-table.yaml`**
```yaml title="drop-table.yaml"
rules:
  - id: drop-table
    patterns:
      - pattern: $CURSOR.execute("$SQL", ...)
      - metavariable-pattern:
          metavariable: $SQL
          language: generic
          pattern: DROP TABLE $T
    message: a query that drops a table
    languages: [python]
    severity: WARNING
```

**`drop-table.py`**
```python title="drop-table.py"
# ruleid: drop-table
cursor.execute("DROP TABLE users")
# ok: drop-table
cursor.execute("SELECT * FROM users")
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
