<!-- reference
id: flag-config
kind: flag
name: --config
aliases: [-c, -f]
summary: Load rules from a file, a directory, a URL, a git repository or the Semgrep registry.
commands: [scan, ci, test]
value: `SOURCE`, repeatable
env: [env-OPENGREP_RULES]
related: [flag-pattern, flag-skip-invalid-configs, flag-rewrite-rule-ids, cmd-test]
-->
# `--config`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), `ci`, [`opengrep test`](../commands/test.md)
- **Also spelled:** `-c`, `-f`
- **Value:** `SOURCE`, repeatable
- **Environment:** [`OPENGREP_RULES`](../env/OPENGREP_RULES.md)
- **See also:** `--pattern`, `--skip-invalid-configs`, `--rewrite-rule-ids`, [`opengrep test`](../commands/test.md), [`opengrep scan`](../commands/scan.md)
<!-- END GENERATED: facts -->

Each `--config` names one source of rules. Repeat the flag to combine sources:
the rules of all sources run together.

| SOURCE | Rules loaded |
|---|---|
| `path/rules.yaml` | The rules in the file. |
| `path/dir/` | Every `.yaml` and `.yml` file under the directory, at any depth, in path order. Test targets such as `x.test.yaml` are skipped. |
| `https://…` | The rule file at the URL. |
| `git+URL`, `git+URL#REF` | Every rule file in the git repository, cloned at its default branch or at the branch or tag `REF`. Git uses its own credentials (ssh agent, credential helpers) and never prompts. |
| `p/NAME`, `r/NAME`, `s/NAME`, `auto`, `r2c` | Rules from the Semgrep registry, at [`OPENGREP_URL`](../environment.md) (default `https://semgrep.dev`). `auto` sends the project URL to the registry, which selects the rules. |

A rule file that is not valid stops the scan (exit status 7). When the file
comes from a directory or a git repository,
[`--skip-invalid-configs`](../flags.md) turns this into a warning and skips the
file.

## Rule ids

When the rules come from a file in a subdirectory, each rule id gets the
directory path as a prefix, with dots for slashes. For example, with
`--config rules/` or `--config rules/python/eval.yaml`, the rule `find-eval`
in `rules/python/eval.yaml` is reported as `rules.python.find-eval`.
`--no-rewrite-rule-ids` keeps the ids as written.

## Without `--config`

When no `--config` is given, `scan`, `ci` and `test` read the sources from
[`OPENGREP_RULES`](../env/OPENGREP_RULES.md). If that variable is not set
either, `scan` runs a single pattern given with `-e`/`--pattern`, and failing
that it uses `--config auto`.

For [`opengrep test`](../commands/test.md), `--config` also changes what is
tested. The given rules run on the given targets, instead of each rule file
being paired with the files named after it.

## Examples

### Two rule files

```yaml title="eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```yaml title="exec.yaml"
rules:
  - id: find-exec
    pattern: exec(...)
    message: found exec
    languages: [python]
    severity: WARNING
```

```python title="app.py"
eval(code)
exec(code)
```

```console
$ opengrep scan --config eval.yaml --config exec.yaml app.py


┌─────────────────┐
│ 2 Code Findings │
└─────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(code)

    ❯❱ find-exec
          found exec

            2┆ exec(code)
```

### A directory of rules

```yaml title="rules/python/eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="app.py"
eval(code)
```

```console
$ opengrep scan --config rules/ app.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ rules.python.find-eval
          found eval

            1┆ eval(code)

$ opengrep scan --config rules/ --no-rewrite-rule-ids app.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(code)
```
