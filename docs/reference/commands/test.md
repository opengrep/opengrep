<!-- reference
id: cmd-test
kind: command
name: opengrep test
summary: Check rules against example files annotated with the lines they must and must not match.
related: [cmd-scan, flag-config]
-->
# `opengrep test`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep scan`](scan.md), [`--config`](../flags/config.md)
<!-- END GENERATED: facts -->

```
opengrep test [FLAGS] [PATHS...]
opengrep test --config RULES [FLAGS] TARGETS...
```

Runs rules on example files and compares the findings with annotations in
those files. The default PATH is the current directory.

## Pairing rules with targets

Without `--config`, every rule file (`.yaml` or `.yml`) found under PATHS is
tested against the files in the same directory that have the same name up to
the extensions. For example, `find-eval.yaml` is tested against `find-eval.py`
and `find-eval.test.yaml`. A directory named like the rule file, such as
`find-eval/`, provides all the files under it.

With [`--config`](../flags/config.md), the given rules run on the given
targets, whatever their names.

## Annotations

An annotation is a comment on the line before the code it is about, or on the
same line. The comment is written in the target language, for example `#`,
`//`, `/* */`, `<!-- -->` or `(* *)`.

| Annotation | Meaning |
|---|---|
| `ruleid: <id>` | The rule must report this line. |
| `ok: <id>` | The rule must not report this line. |
| `todoruleid: <id>` | The rule should report this line but does not yet. The test passes either way. |
| `todook: <id>` | The rule reports this line but should not. The test passes either way. |

Several rule ids can be given, separated by commas: `# ruleid: rule-a, rule-b`.
The test fails when a rule misses a `ruleid:` line ("missed lines") or reports
a line that no `ruleid:` annotation names ("incorrect lines"). With
`--test-ignore-todo`, `todoruleid:` and `todook:` lines count like `ok:` and
`ruleid:` lines. The `deepok:`, `deepruleid:`, `prook:` and `proruleid:`
prefixes are accepted for compatibility and are ignored.

## Autofix tests

If a target `x.py` has a file `x.fixed.py` next to it, `opengrep test` also
applies the rules' `fix` to `x.py` and checks that the result equals
`x.fixed.py`. The line `No tests for fixes found.` means there was no such
file.

## Exit status

0 when all tests pass, 1 when a test fails, and 2 on a fatal error.

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--config`](../flags/config.md) | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| `--debug` | *not yet documented* |
| `--develop` | *not yet documented* |
| `--experimental` | *not yet documented* |
| `--force-color` | *not yet documented* |
| `--json` | *not yet documented* |
| `--matching-diagnosis` | *not yet documented* |
| `--max-memory` | *not yet documented* |
| `--opengrep-ignore-pattern` | *not yet documented* |
| `--profile` | *not yet documented* |
| `--quiet` | *not yet documented* |
| `--strict` | *not yet documented* |
| `--taint-intrafile` | *not yet documented* |
| `--test-ignore-todo` | *not yet documented* |
| [`--timeout`](../flags/timeout.md) | Maximum time in seconds for one rule on one file; 0 means no limit. |
| `--timeout-threshold` | *not yet documented* |
| `--verbose` | *not yet documented* |
<!-- END GENERATED: flags -->

## Examples

### A rule next to its test file

```yaml title="find-eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="find-eval.py"
# ruleid: find-eval
eval(user_input)
# ok: find-eval
evaluate(user_input)
```

```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A failing test

The second `eval` call is wrongly annotated `ok:`.

```yaml title="find-eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="find-eval.py"
# ruleid: find-eval
eval(a)
# ok: find-eval
eval(b)
```

```console
$ opengrep test .; echo "exit status: $?"
0/1: 1 unit tests did not pass:
--------------------------------------------------------------------------------
	✖ find-eval
	missed lines: [], incorrect lines: [4]
	test file path: <tmp>/find-eval.py


No tests for fixes found.
exit status: 1
```
