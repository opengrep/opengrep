<!-- reference
id: flag-max-match-per-file
kind: flag
name: --max-match-per-file
summary: How many findings one file may have, across all rules, before they are all dropped.
commands: [scan, ci]
value: `INT`
default: 10000
related: [opt-max_match_per_file, flag-timeout, flag-max-memory]
-->
# `--max-match-per-file`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `10000`
- **See also:** [`max_match_per_file`](../rule-options/max_match_per_file.md), [`--timeout`](timeout.md), [`--max-memory`](max-memory.md)
<!-- END GENERATED: facts -->

A rule that matches thousands of times in one file is almost always a mistake
in the rule. The limit counts the matches of all rules in a file together.
When a file has more, opengrep drops every finding in that file, of every
rule, records a `Too many matches` error, and warns with the rule that matched
most:

```
most offending rule: id = find-eval, matches = 3, pattern = eval(...)
```

The file counts as partially analysed, and the scan carries on with the other
files.

This is a guard against a runaway rule, not a way to shorten a report: passing
the limit loses the findings rather than trimming them. To keep only the first
findings of one rule, give that rule the
[`max_match_per_file`](../rule-options/max_match_per_file.md) option.

## Examples

### A rule that matches too often

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**Command and result:**
```console
$ printf 'eval(1)\neval(2)\neval(3)\n' > many.py
$ opengrep scan --config rule.yaml --json many.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "find-eval",
      "path": "many.py",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 1,
        "col": 8,
        "offset": 7
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "516e2365a1eeb419dad03622c4e7dd819f330ed97d9d0bfe1f405be794df9ee17b7566f637c5df8b1b4ad21d704b5b2e531dfe9cad3790f17c8f2abed7a23587_0",
        "lines": "eval(1)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    },
    {
      "check_id": "find-eval",
      "path": "many.py",
      "start": {
        "line": 2,
        "col": 1,
        "offset": 8
      },
      "end": {
        "line": 2,
        "col": 8,
        "offset": 15
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "516e2365a1eeb419dad03622c4e7dd819f330ed97d9d0bfe1f405be794df9ee17b7566f637c5df8b1b4ad21d704b5b2e531dfe9cad3790f17c8f2abed7a23587_1",
        "lines": "eval(2)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    },
    {
      "check_id": "find-eval",
      "path": "many.py",
      "start": {
        "line": 3,
        "col": 1,
        "offset": 16
      },
      "end": {
        "line": 3,
        "col": 8,
        "offset": 23
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "516e2365a1eeb419dad03622c4e7dd819f330ed97d9d0bfe1f405be794df9ee17b7566f637c5df8b1b4ad21d704b5b2e531dfe9cad3790f17c8f2abed7a23587_2",
        "lines": "eval(3)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "many.py"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
$ opengrep scan --config rule.yaml --json --max-match-per-file 2 many.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [],
  "errors": [
    {
      "code": 2,
      "level": "warn",
      "type": "Too many matches",
      "rule_id": "find-eval",
      "message": "Too many matches when running find-eval on many.py:\n An error occurred while invoking the Opengrep engine. Please help us fix this by creating an issue at https://github.com/opengrep/opengrep\n\n1 rules result in too many matches, most offending rule has 3: eval(...)",
      "path": "many.py"
    }
  ],
  "paths": {
    "scanned": [
      "many.py"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
```

### Two rules share the limit

**`two.yaml`**
```yaml title="two.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
  - id: find-exec
    pattern: exec(...)
    message: found exec
    languages: [python]
    severity: WARNING
```

**`two.py`**
```python title="two.py"
eval(1)
exec(1)
```

**Command and result:**
```console
$ opengrep scan --config two.yaml --json --max-match-per-file 1 two.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [],
  "errors": [
    {
      "code": 2,
      "level": "warn",
      "type": "Too many matches",
      "rule_id": "find-eval",
      "message": "Too many matches when running find-eval on two.py:\n An error occurred while invoking the Opengrep engine. Please help us fix this by creating an issue at https://github.com/opengrep/opengrep\n\n2 rules result in too many matches, most offending rule has 1: eval(...)",
      "path": "two.py"
    }
  ],
  "paths": {
    "scanned": [
      "two.py"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
$ opengrep scan --config two.yaml --json --max-match-per-file 2 two.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "find-eval",
      "path": "two.py",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 1,
        "col": 8,
        "offset": 7
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "fe58b530500822f323ac19d98001497d7b30bdd3f20e12468582ffdb71e028856c5375826cffee4912c0352a79e51f4c12b34804050a451f63903605ed54fb6e_0",
        "lines": "eval(1)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    },
    {
      "check_id": "find-exec",
      "path": "two.py",
      "start": {
        "line": 2,
        "col": 1,
        "offset": 8
      },
      "end": {
        "line": 2,
        "col": 8,
        "offset": 15
      },
      "extra": {
        "metavars": {},
        "message": "found exec",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "628e39a4f93bc421a2802c802608f9fe43e4f6b93e15acebb3be020e0152756ca906608e5b9a391f98b424d2f5cb9d4dab5bef47f597f3438da956d1593d3d83_0",
        "lines": "exec(1)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "two.py"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
```

Each rule matches once, but together they match twice: a limit of 1 drops
both findings, and a limit of 2 keeps them.
