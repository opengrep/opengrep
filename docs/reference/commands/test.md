<!-- reference
id: cmd-test
kind: command
name: opengrep test
summary: Check rules against example files annotated with the lines they must and must not match.
related: [cmd-scan, flag-config]
-->
# `opengrep test`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep scan`](scan.md), [`--config`](../flags/config.md), [`opengrep validate`](validate.md), [`--autofix`](../flags/autofix.md), [`--matching-diagnosis`](../flags/matching-diagnosis.md), [`--test-ignore-todo`](../flags/test-ignore-todo.md), [`--test`](../flags/test.md)
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
a line that no `ruleid:` annotation names ("incorrect lines"). A `todoruleid:`
or `todook:` line never fails a test, whichever way the rule behaves on it;
see [`--test-ignore-todo`](../flags/test-ignore-todo.md). The `deepok:`,
`deepruleid:`, `prook:` and `proruleid:` prefixes are accepted for
compatibility and are ignored.

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
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| [`--force-color`](../flags/force-color.md) | Style the output even when it is not going to a terminal. |
| [`--json`](../flags/json.md) | Print the findings as a JSON document instead of the text report. |
| [`--matching-diagnosis`](../flags/matching-diagnosis.md) | Explain why a failing rule test did not match. |
| [`--max-memory`](../flags/max-memory.md) | Memory a single file's analysis may use before it is abandoned. |
| [`--opengrep-ignore-pattern`](../flags/opengrep-ignore-pattern.md) | Recognise one more comment prefix that silences findings on a line. |
| `--profile` | *not yet documented* |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--strict`](../flags/strict.md) | Fail the run when a file could not be parsed or another warning-level error occurred. |
| [`--taint-intrafile`](../flags/taint-intrafile.md) | Follow taint through calls to functions defined in the same file, for every taint rule. |
| [`--test-ignore-todo`](../flags/test-ignore-todo.md) | Documented as ignoring todoruleid annotations; in 1.30.0 it changes nothing. |
| [`--timeout`](../flags/timeout.md) | Maximum time in seconds for one rule on one file; 0 means no limit. |
| [`--timeout-threshold`](../flags/timeout-threshold.md) | How many rules may time out on a file before opengrep gives up on that file. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
<!-- END GENERATED: flags -->

## Examples

### A rule next to its test file

**`find-eval.yaml`**
```yaml title="find-eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`find-eval.py`**
```python title="find-eval.py"
# ruleid: find-eval
eval(user_input)
# ok: find-eval
evaluate(user_input)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A failing test

The second `eval` call is wrongly annotated `ok:`.

**`find-eval.yaml`**
```yaml title="find-eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`find-eval.py`**
```python title="find-eval.py"
# ruleid: find-eval
eval(a)
# ok: find-eval
eval(b)
```

**Command and result:**
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
