<!-- reference
id: flag-max-memory
kind: flag
name: --max-memory
summary: Memory a single file's analysis may use before it is abandoned.
commands: [scan, ci, test]
value: `INT`, in MiB
default: 0, no limit
related: [flag-timeout, flag-max-target-bytes, flag-jobs]
-->
# `--max-memory`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md)
- **Value:** `INT`, in MiB
- **Default:** `0, no limit`
- **See also:** [`--timeout`](timeout.md), [`--max-target-bytes`](max-target-bytes.md), [`--jobs`](jobs.md), [`--max-match-per-file`](max-match-per-file.md), [`--timeout-threshold`](timeout-threshold.md)
<!-- END GENERATED: facts -->

Caps the memory used while analysing one file, and while running the interfile
analysis. Passing the cap abandons that file: opengrep warns
`ExceededMemoryLimit on <file>`, counts the file as partially analysed, and
carries on. The scan still exits 0, so a file dropped this way is easy to
miss unless you read the summary or pass [`--strict`](strict.md).

`0`, the default, means no limit. The value is in MiB.

Memory and cores go together: each of the [`--jobs`](jobs.md) workers holds
its own analysis, so lowering the job count is the other way to keep a scan
inside a memory budget.

## Examples

### A limit too small to analyse anything

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
$ seq 20000 | sed 's/.*/x& = f(&)/' > big.py && echo 'eval(1)' >> big.py
$ opengrep scan --config rule.yaml --files-with-matches big.py
big.py
$ opengrep scan --config rule.yaml --files-with-matches --max-memory 1 big.py
$ opengrep scan --config rule.yaml --max-memory 1 big.py 2>&1 >/dev/null | grep ExceededMemoryLimit
[00.05][WARNING]: ExceededMemoryLimit on big.py
```

With the cap in place the third command prints nothing: the file was
abandoned, so it has no findings to report. The warning says which file was
dropped, and the summary counts it as partially analysed.
