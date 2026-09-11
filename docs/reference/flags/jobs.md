<!-- reference
id: flag-jobs
kind: flag
name: --jobs
aliases: [-j]
summary: How many cores run rules in parallel.
commands: [scan, ci]
value: `INT`
default: the number of cores detected
related: [flag-max-memory, flag-timeout, flag-optimizations]
-->
# `--jobs`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `-j`
- **Value:** `INT`
- **Default:** `the number of cores detected`
- **See also:** [`--max-memory`](max-memory.md), [`--timeout`](timeout.md), [`--optimizations`](optimizations.md), [`--incremental-output`](incremental-output.md), [`--time`](time.md)
<!-- END GENERATED: facts -->

Opengrep splits the work across as many cores as it finds. `--jobs N` fixes
the number instead.

Lower it when a scan must leave room for something else, or when memory is
short: each worker holds its own analysis, so the cap set by
[`--max-memory`](max-memory.md) applies per worker, and fewer workers means
less memory in total. `--jobs 1` also makes the logs readable, since nothing
interleaves, which helps when tracking down a slow or crashing rule.

The findings do not depend on the job count.

## Examples

### Scanning on one core

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
$ opengrep scan --config rule.yaml --jobs 1 app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
