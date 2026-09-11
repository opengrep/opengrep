<!-- reference
id: flag-max-target-bytes
kind: flag
name: --max-target-bytes
summary: Skip files larger than this when walking a directory.
commands: [scan, ci]
value: `VALUE`, a number of bytes or a size such as `1.5MB`
default: 1000000
related: [flag-exclude-minified-files, flag-exclude, flag-max-memory]
-->
# `--max-target-bytes`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `VALUE`, a number of bytes or a size such as `1.5MB`
- **Default:** `1000000`
- **See also:** [`--exclude-minified-files`](exclude-minified-files.md), [`--exclude`](exclude.md), [`--max-memory`](max-memory.md)
<!-- END GENERATED: facts -->

Files bigger than this are not scanned. The default is a million bytes, which
keeps generated bundles and checked-in data files out of a scan. A zero or
negative value turns the filter off.

The value is a number of bytes, or a size with a unit, as in `1.5MB`.

Like the other path filters, it applies to the files opengrep finds by walking
a directory. A file named on the command line is scanned however large it is.
The summary says how many files were left out, as in
`skipped: 1 files larger than 100 bytes`.

## Examples

### Leaving a large file out

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`src/a.py`**
```python title="src/a.py"
eval(1)
```

**Command and result:**
```console
$ seq 500 | sed 's/.*/# pad/' > big.py && echo 'eval(1)' >> big.py
$ opengrep scan --config rule.yaml --files-with-matches .
big.py
src/a.py
$ opengrep scan --config rule.yaml --files-with-matches --max-target-bytes 100 .
src/a.py
$ opengrep scan --config rule.yaml --files-with-matches --max-target-bytes 100 big.py
big.py
```

The last command scans `big.py` despite its size, because it was named on the
command line.
