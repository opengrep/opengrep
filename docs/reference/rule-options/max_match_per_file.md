<!-- reference
id: opt-max_match_per_file
kind: option
name: max_match_per_file
summary: Keep only the first findings of this rule in each file.
value: an integer
related: []
-->
# `max_match_per_file`

<!-- BEGIN GENERATED: facts -->
- **Value:** an integer
- **See also:** [`--max-match-per-file`](../flags/max-match-per-file.md)
<!-- END GENERATED: facts -->

With `max_match_per_file: N`, opengrep keeps the first N findings of this rule
in each file, in order of position, and drops the rest without an error. `0`
or a negative number keeps none. Unlike the timeout options, it needs no flag.

The scan's [`--max-match-per-file`](../flags/max-match-per-file.md) is checked
first, on the matches of all rules in the file together. When a file has more
than that, every finding in it is dropped with a `Too many matches` error,
whatever this option says.

## Examples

### The first two findings

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
    options:
      max_match_per_file: 2
```

**`many.py`**
```python title="many.py"
eval(1)
eval(2)
eval(3)
eval(4)
```

**Command and result:**
```console
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
$ opengrep scan --config rule.yaml --json --max-match-per-file 3 many.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [],
  "errors": [
    {
      "code": 2,
      "level": "warn",
      "type": "Too many matches",
      "rule_id": "find-eval",
      "message": "Too many matches when running find-eval on many.py:\n An error occurred while invoking the Opengrep engine. Please help us fix this by creating an issue at https://github.com/opengrep/opengrep\n\n1 rules result in too many matches, most offending rule has 4: eval(...)",
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

The first scan reports the findings on lines 1 and 2 only. The second scan
allows 3 matches per file. The rule matches 4 times before its own option
trims it to 2, so the file passes the scan's limit and loses all its findings.
