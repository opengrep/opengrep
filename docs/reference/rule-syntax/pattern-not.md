<!-- reference
id: key-pattern-not
kind: rule-key
name: pattern-not
summary: Drop the matches that also match this pattern.
related: [key-patterns, key-pattern-not-inside, key-pattern-not-regex]
-->
# `pattern-not`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md), [`pattern-not-inside`](pattern-not-inside.md), [`pattern-not-regex`](pattern-not-regex.md)
<!-- END GENERATED: facts -->

`pattern-not` is an item of [`patterns`](patterns.md). It drops the matches of
the other items that its own pattern matches as well, which is how a rule
leaves out the safe forms of a dangerous call.

It is a negative item: it cannot stand at the top of a rule or among the
alternatives of [`pattern-either`](pattern-either.md), and a `patterns` list
needs at least one positive item next to it.

## Examples

### A shell command that is not a fixed string

**`shell-with-input.yaml`**
```yaml title="shell-with-input.yaml"
rules:
  - id: shell-with-input
    patterns:
      - pattern: subprocess.run($CMD, shell=True, ...)
      - pattern-not: subprocess.run("...", shell=True, ...)
    message: a shell command built at run time
    languages: [python]
    severity: ERROR
```

**`shell-with-input.py`**
```python title="shell-with-input.py"
import subprocess

def archive(name):
    # ruleid: shell-with-input
    subprocess.run("tar czf " + name + ".tgz data/", shell=True)
    # ok: shell-with-input
    subprocess.run("tar czf backup.tgz data/", shell=True)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
