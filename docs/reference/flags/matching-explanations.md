<!-- reference
id: flag-matching-explanations
kind: flag
name: --matching-explanations
summary: Add to the JSON output a trace of how each part of a rule matched.
commands: [scan, ci]
related: [flag-json, flag-dataflow-traces, key-patterns]
-->
# `--matching-explanations`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--json`](json.md), [`--dataflow-traces`](dataflow-traces.md), [`patterns`](../rule-syntax/patterns.md), [`--matching-diagnosis`](matching-diagnosis.md), [`--optimizations`](optimizations.md)
<!-- END GENERATED: facts -->

Adds an `explanations` array to the [JSON output](json.md), recording how the
parts of a rule matched: each operator of the rule, what it matched, and what
its children contributed. This is for working out why a rule fires where it
does, or why it does not fire at all, and it is the information behind the
rule playground on semgrep.dev.

It changes the JSON output only. The flag costs time and makes the document
considerably larger, so it is meant for debugging a rule rather than for a
normal scan.

## Examples

### The explanations of a match

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: eval-in-function
    patterns:
      - pattern-inside: |
          def $F(...):
              ...
      - pattern: eval($X)
    message: eval inside a function
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
def handler(user):
    eval(user)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --json --matching-explanations app.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "eval-in-function",
      "path": "app.py",
      "start": {
        "line": 2,
        "col": 5,
        "offset": 23
      },
      "end": {
        "line": 2,
        "col": 15,
        "offset": 33
      },
      "extra": {
        "metavars": {
          "$F": {
            "start": {
              "line": 1,
              "col": 5,
              "offset": 4
            },
            "end": {
              "line": 1,
              "col": 12,
              "offset": 11
            },
            "abstract_content": "handler"
          },
          "$X": {
            "start": {
              "line": 2,
              "col": 10,
              "offset": 28
            },
            "end": {
              "line": 2,
              "col": 14,
              "offset": 32
            },
            "abstract_content": "user"
          }
        },
        "message": "eval inside a function",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "e4f28acf86ffd5b4749bce46013ccfc565ce0f6621b7dfb300e2017c0410e5f408166fa621cc42ce50ccddc298a099f69185267f66820c593738ae8b89296bda_0",
        "lines": "    eval(user)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "app.py"
    ]
  },
  "explanations": [
    {
      "op": "And",
      "children": [
        {
          "op": [
            "XPat",
            "eval($X)"
          ],
          "children": [],
          "matches": [
            {
              "check_id": "eval-in-function",
              "path": "app.py",
              "start": {
                "line": 2,
                "col": 5,
                "offset": 23
              },
              "end": {
                "line": 2,
                "col": 15,
                "offset": 33
              },
              "extra": {
                "metavars": {
                  "$X": {
                    "start": {
                      "line": 2,
                      "col": 10,
                      "offset": 28
                    },
                    "end": {
                      "line": 2,
                      "col": 14,
                      "offset": 32
                    },
                    "abstract_content": "user"
                  }
                },
                "engine_kind": "OSS",
                "is_ignored": false,
                "message": "eval inside a function",
                "validation_state": "NO_VALIDATOR"
              }
            }
          ],
          "loc": {
            "path": "rule.yaml",
            "start": {
              "line": 7,
              "col": 18,
              "offset": 130
            },
            "end": {
              "line": 7,
              "col": 26,
              "offset": 138
            }
          }
        },
        {
          "op": "Inside",
          "children": [
            {
              "op": [
                "XPat",
                "def $F(...):\n    ...\n"
              ],
              "children": [],
              "matches": [
                {
                  "check_id": "eval-in-function",
                  "path": "app.py",
                  "start": {
                    "line": 1,
                    "col": 1,
                    "offset": 0
                  },
                  "end": {
                    "line": 2,
                    "col": 15,
                    "offset": 33
                  },
                  "extra": {
                    "metavars": {
                      "$F": {
                        "start": {
                          "line": 1,
                          "col": 5,
                          "offset": 4
                        },
                        "end": {
                          "line": 1,
                          "col": 12,
                          "offset": 11
                        },
                        "abstract_content": "handler"
                      }
                    },
                    "engine_kind": "OSS",
                    "is_ignored": false,
                    "message": "eval inside a function",
                    "validation_state": "NO_VALIDATOR"
                  }
                }
              ],
              "loc": {
                "path": "rule.yaml",
                "start": {
                  "line": 4,
                  "col": 26,
                  "offset": 71
                },
                "end": {
                  "line": 6,
                  "col": 19,
                  "offset": 113
                }
              }
            }
          ],
          "matches": [
            {
              "check_id": "eval-in-function",
              "path": "app.py",
              "start": {
                "line": 1,
                "col": 1,
                "offset": 0
              },
              "end": {
                "line": 2,
                "col": 15,
                "offset": 33
              },
              "extra": {
                "metavars": {
                  "$F": {
                    "start": {
                      "line": 1,
                      "col": 5,
                      "offset": 4
                    },
                    "end": {
                      "line": 1,
                      "col": 12,
                      "offset": 11
                    },
                    "abstract_content": "handler"
                  }
                },
                "engine_kind": "OSS",
                "is_ignored": false,
                "message": "eval inside a function",
                "validation_state": "NO_VALIDATOR"
              }
            }
          ],
          "loc": {
            "path": "rule.yaml",
            "start": {
              "line": 4,
              "col": 9,
              "offset": 54
            },
            "end": {
              "line": 4,
              "col": 23,
              "offset": 68
            }
          }
        }
      ],
      "matches": [
        {
          "check_id": "eval-in-function",
          "path": "app.py",
          "start": {
            "line": 2,
            "col": 5,
            "offset": 23
          },
          "end": {
            "line": 2,
            "col": 15,
            "offset": 33
          },
          "extra": {
            "metavars": {
              "$F": {
                "start": {
                  "line": 1,
                  "col": 5,
                  "offset": 4
                },
                "end": {
                  "line": 1,
                  "col": 12,
                  "offset": 11
                },
                "abstract_content": "handler"
              },
              "$X": {
                "start": {
                  "line": 2,
                  "col": 10,
                  "offset": 28
                },
                "end": {
                  "line": 2,
                  "col": 14,
                  "offset": 32
                },
                "abstract_content": "user"
              }
            },
            "engine_kind": "OSS",
            "is_ignored": false,
            "message": "eval inside a function",
            "validation_state": "NO_VALIDATOR"
          }
        }
      ],
      "loc": {
        "path": "rule.yaml",
        "start": {
          "line": 3,
          "col": 5,
          "offset": 36
        },
        "end": {
          "line": 3,
          "col": 13,
          "offset": 44
        }
      },
      "extra": {
        "before_negation_matches": [
          "Some",
          [
            {
              "check_id": "eval-in-function",
              "path": "app.py",
              "start": {
                "line": 2,
                "col": 5,
                "offset": 23
              },
              "end": {
                "line": 2,
                "col": 15,
                "offset": 33
              },
              "extra": {
                "metavars": {
                  "$F": {
                    "start": {
                      "line": 1,
                      "col": 5,
                      "offset": 4
                    },
                    "end": {
                      "line": 1,
                      "col": 12,
                      "offset": 11
                    },
                    "abstract_content": "handler"
                  },
                  "$X": {
                    "start": {
                      "line": 2,
                      "col": 10,
                      "offset": 28
                    },
                    "end": {
                      "line": 2,
                      "col": 14,
                      "offset": 32
                    },
                    "abstract_content": "user"
                  }
                },
                "engine_kind": "OSS",
                "is_ignored": false,
                "message": "eval inside a function",
                "validation_state": "NO_VALIDATOR"
              }
            }
          ]
        ],
        "before_filter_matches": "None"
      }
    }
  ],
  "interfile_languages_used": [],
  "skipped_rules": []
}
```
