<!-- reference
id: flag-exclude-minified-files
kind: flag
name: --exclude-minified-files
aliases: [--no-exclude-minified-files]
summary: Skip files that look minified.
commands: [scan]
default: false
related: [flag-max-target-bytes, flag-exclude]
-->
# `--exclude-minified-files`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `--no-exclude-minified-files`
- **Default:** `false`
- **See also:** [`--max-target-bytes`](max-target-bytes.md), [`--exclude`](exclude.md)
<!-- END GENERATED: facts -->

Skips files that look minified. Such a file is usually generated, matches
rules in ways no one wants to read, and is slow to analyse.

A file counts as minified when its first 4096 bytes are less than 7%
whitespace (spaces, tabs, carriage returns and newlines), or average more than
1000 bytes per line. The rest of the file is not looked at. Files of 1000
bytes or less are never skipped, so a short file made of one long URL is
still scanned.

Minified files are scanned by default, so this is the flag to add rather than
one to turn off. `--no-exclude-minified-files` asks for the default back.

A size limit is a blunter way to the same end; see
[`--max-target-bytes`](max-target-bytes.md).

## Examples

### Leaving a minified file out

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`normal.py`**
```python title="normal.py"
eval(1)
```

**Command and result:**
```console
$ python3 -c "open('min.py','w').write('eval(1);' * 400 + '\n')"
$ opengrep scan --config rule.yaml --files-with-matches .
min.py
normal.py
$ opengrep scan --config rule.yaml --files-with-matches --exclude-minified-files .
normal.py
```

### Small files, and only the start of a file

Both generated files below have no whitespace in their first bytes. The first
is 1000 bytes long and is kept; the second is one byte longer and is skipped.
The third starts with 4800 bytes of ordinary lines, so the long line after
them does not count.

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
$ python3 -c "open('a.py','w').write('eval(1);' * 125)"
$ python3 -c "open('b.py','w').write('eval(1);' * 125 + '\n')"
$ python3 -c "open('c.py','w').write('eval(1)\n' * 600 + 'eval(1);' * 2000 + '\n')"
$ opengrep scan --config rule.yaml --files-with-matches --exclude-minified-files .
a.py
c.py
```
