<!-- reference
id: opt-generic_engine
kind: option
name: generic_engine
summary: Choose the engine behind languages: [generic], spacegrep or aliengrep.
value: `spacegrep` or `aliengrep`
default: spacegrep
related: []
-->
# `generic_engine`

<!-- BEGIN GENERATED: facts -->
- **Value:** `spacegrep` or `aliengrep`
- **Default:** `spacegrep`
- **See also:** [`generic_multiline`](generic_multiline.md)
<!-- END GENERATED: facts -->

A rule with `languages: [generic]` matches plain text, with one of two
engines. `spacegrep`, the default, splits text into words and brackets, and an
ellipsis `...` spans at most 10 line breaks. `aliengrep` lets `...` span any
number of lines, and can be configured further.

Each engine reads its own options and ignores the others':

| Engine | Options |
|---|---|
| `spacegrep` | [`generic_ellipsis_max_span`](generic_ellipsis_max_span.md), [`generic_comment_style`](generic_comment_style.md) |
| `aliengrep` | [`generic_multiline`](generic_multiline.md), [`generic_caseless`](generic_caseless.md), [`generic_braces`](generic_braces.md), [`generic_extra_braces`](generic_extra_braces.md), [`generic_extra_word_characters`](generic_extra_word_characters.md) |

The option has no effect on rules for other languages.

## Examples

### A block longer than 10 lines

**`spacegrep.yaml`**
```yaml title="spacegrep.yaml"
rules:
  - id: block
    pattern: begin ... end
    message: a block
    languages: [generic]
    severity: INFO
```

**`aliengrep.yaml`**
```yaml title="aliengrep.yaml"
rules:
  - id: block
    pattern: begin ... end
    message: a block
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
```

**Command and result:**
```console
$ python3 -c "open('long.txt','w').write('begin\n' + 'x\n' * 11 + 'end\n')"
$ opengrep scan --config spacegrep.yaml --json long.txt 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [],
  "errors": [],
  "paths": {
    "scanned": [
      "long.txt"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
$ opengrep scan --config aliengrep.yaml --json long.txt 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "block",
      "path": "long.txt",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 13,
        "col": 4,
        "offset": 31
      },
      "extra": {
        "metavars": {},
        "message": "a block",
        "metadata": {},
        "severity": "INFO",
        "fingerprint": "469c2010195f6b6494ec9b2c2fe48d1d88a70a644a35e637ede0e89e945d466f50cdb9d50a7436f43a10ff7ca084557e577b2d05be707e41f08512c8cb43626c_0",
        "lines": "begin\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nend",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "long.txt"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
```

The block has 12 line breaks between `begin` and `end`: too many for
spacegrep, which finds nothing, but not for aliengrep, whose finding runs from
line 1 to line 13.

### An aliengrep option under spacegrep

**`caseless.yaml`**
```yaml title="caseless.yaml"
rules:
  - id: select-users
    pattern: SELECT ... FROM users
    message: query on users
    languages: [generic]
    severity: INFO
    options:
      generic_caseless: true
```

**`query.sql`**
```sql title="query.sql"
select * from users
```

**Command and result:**
```console
$ opengrep scan --config caseless.yaml query.sql
```

The rule uses spacegrep, which ignores `generic_caseless`, so the lower-case
query is not found and the command prints nothing.
