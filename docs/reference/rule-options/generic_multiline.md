<!-- reference
id: opt-generic_multiline
kind: option
name: generic_multiline
summary: With aliengrep, whether an ellipsis may span lines; false keeps it within one.
value: `true` or `false`
default: true
related: [opt-generic_engine]
-->
# `generic_multiline`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`generic_engine`](generic_engine.md)
<!-- END GENERATED: facts -->

With [`generic_engine: aliengrep`](generic_engine.md), an ellipsis `...` or a
metavariable ellipsis `$...X` may span lines. With `generic_multiline: false`,
they stay within one line.

In that line mode, the long ellipsis `....`, and `$....X`, may still cross line
breaks, though in this version not after text on a line: `begin....end`
matches `begin` followed by `x end` on the next line, but not `begin`, `x` and
`end` on three lines.

The option has no effect with the default engine, spacegrep.

## Examples

### Three ellipses on two blocks

**`blocks.yaml`**
```yaml title="blocks.yaml"
rules:
  - id: dots-default
    pattern: begin ... end
    message: block
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
  - id: dots
    pattern: begin ... end
    message: block
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_multiline: false
  - id: long-dots
    pattern: begin....end
    message: block
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_multiline: false
```

**`blocks.txt`**
```text title="blocks.txt"
begin
x end
begin
x
end
```

**Command and result:**
```console
$ opengrep scan --config blocks.yaml --json blocks.txt 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "dots-default",
      "path": "blocks.txt",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 2,
        "col": 6,
        "offset": 11
      },
      "extra": {
        "metavars": {},
        "message": "block",
        "metadata": {},
        "severity": "INFO",
        "fingerprint": "0da745f4f1aba903ca3655fb40bac322a3dcc6d8f50c64e0e8ec04ec35dbe7a3b5388ad4344cce39a65c8a509294d32e497d0bdfde9f0359219a4ec80f2d0992_0",
        "lines": "begin\nx end",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    },
    {
      "check_id": "long-dots",
      "path": "blocks.txt",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 2,
        "col": 6,
        "offset": 11
      },
      "extra": {
        "metavars": {},
        "message": "block",
        "metadata": {},
        "severity": "INFO",
        "fingerprint": "6c956abaf56277bb20ed677d411a86395b530598d0722c4888a812eee5d20041719483cb30a7be16637c54ad066a0f22f21f5d4720a3d32a7ec86a6ed7c1bece_0",
        "lines": "begin\nx end",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    },
    {
      "check_id": "dots-default",
      "path": "blocks.txt",
      "start": {
        "line": 3,
        "col": 1,
        "offset": 12
      },
      "end": {
        "line": 5,
        "col": 4,
        "offset": 23
      },
      "extra": {
        "metavars": {},
        "message": "block",
        "metadata": {},
        "severity": "INFO",
        "fingerprint": "0da745f4f1aba903ca3655fb40bac322a3dcc6d8f50c64e0e8ec04ec35dbe7a3b5388ad4344cce39a65c8a509294d32e497d0bdfde9f0359219a4ec80f2d0992_1",
        "lines": "begin\nx\nend",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "blocks.txt"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
```

`dots-default` finds both blocks. In line mode, `dots` finds neither, and
`long-dots` finds the two-line block but not the three-line one.
