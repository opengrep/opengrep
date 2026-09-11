<!-- reference
id: opt-generic_caseless
kind: option
name: generic_caseless
summary: With aliengrep, match text regardless of letter case.
value: `true` or `false`
default: false
related: []
-->
# `generic_caseless`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
<!-- END GENERATED: facts -->

With [`generic_engine: aliengrep`](generic_engine.md) and
`generic_caseless: true`, the pattern matches text in any letter case:
`SELECT ... FROM users` also matches `select * from users` and
`Select * From Users`.

The option has no effect with the default engine, spacegrep, which always
matches case exactly.

## Examples

### SQL keywords in any case

The rule `select-users` turns the option on.

**`queries.yaml`**
```yaml title="queries.yaml"
rules:
  - id: select-users-default
    pattern: SELECT ... FROM users
    message: query on users
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
  - id: select-users
    pattern: SELECT ... FROM users
    message: query on users
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_caseless: true
```

**`queries.sql`**
```sql title="queries.sql"
SELECT * FROM users
select * from users
```

**Command and result:**
```console
$ opengrep scan --config queries.yaml queries.sql
queries.sql

  info  select-users
  query on users

    1 │ SELECT * FROM users

  info  select-users-default
  query on users

    1 │ SELECT * FROM users

  info  select-users
  query on users

    2 │ select * from users
```
