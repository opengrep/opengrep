<!-- reference
id: key-metavariable-type
kind: rule-key
name: metavariable-type
summary: Keep the matches where a metavariable's expression has a given type.
covers: [key-type, key-types]
related: [key-patterns]
-->
# `metavariable-type`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`metavariable-type` is an item of [`patterns`](patterns.md). It keeps the
matches where the expression bound to a metavariable has a given type, so a
rule can tell apart two calls that look the same. It takes a mapping:

- `metavariable`: the metavariable, bound by another item;
- `type`: the type, written as in the rule's language. A short name such as
  `Statement` and a full name such as `java.sql.Statement` both work;
- `types`, instead of `type`: a list of types, any one of which will do.

The type of a variable comes from its declaration, such as the type of a
parameter.

## Examples

### `execute` on a JDBC statement

**`statement-execute.yaml`**
```yaml title="statement-execute.yaml"
rules:
  - id: statement-execute
    patterns:
      - pattern: $X.execute($Q)
      - metavariable-type:
          metavariable: $X
          type: java.sql.Statement
    message: raw SQL executed on a java.sql.Statement
    languages: [java]
    severity: WARNING
```

**`statement-execute.java`**
```java title="statement-execute.java"
import java.sql.Statement;
import java.util.concurrent.Executor;

class Jobs {
    void run(Statement stmt, Executor pool, String query, Runnable task) {
        // ruleid: statement-execute
        stmt.execute(query);
        // ok: statement-execute
        pool.execute(task);
    }
}
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
