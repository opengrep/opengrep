<!-- reference
id: flag-optimizations
kind: flag
name: --optimizations
summary: Turn the engine's optimizations, chiefly the prefilter, on or off.
commands: [scan, ci]
value: `all` or `none`
default: all
related: [flag-jobs, flag-timeout, flag-matching-explanations]
-->
# `--optimizations`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `all` or `none`
- **Default:** `all`
- **See also:** [`--jobs`](jobs.md), [`--timeout`](timeout.md), [`--matching-explanations`](matching-explanations.md)
<!-- END GENERATED: facts -->

`all`, the default, lets the engine use its optimizations. The important one is
the prefilter: before running a rule on a file, opengrep decides from the
rule's literals whether the file could match at all, and skips it when it
could not. On a large tree this is most of the speed.

`none` turns them off and runs every rule over every candidate file.

That makes it a diagnostic. If a rule finds something with
`--optimizations none` and nothing without, the rule is fine and the prefilter
wrongly skipped the file; that is a bug in opengrep worth reporting. Any value
other than these two is rejected, and opengrep exits 2.

## Examples

### Running without the prefilter

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --optimizations none app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)

$ opengrep scan --config rule.yaml --optimizations some app.py 2>&1 >/dev/null | grep -o 'unsupported value.*'
unsupported value "some"
```
