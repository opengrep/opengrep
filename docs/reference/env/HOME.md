<!-- reference
id: env-HOME
kind: env
name: HOME
summary: The home directory; opengrep keeps only the language server's cache there.
value: a directory
related: [cmd-lsp]
-->
# `HOME`

<!-- BEGIN GENERATED: facts -->
- **Value:** a directory
- **See also:** [`opengrep lsp`](../commands/lsp.md)
<!-- END GENERATED: facts -->

Opengrep reads `HOME` to find the user's home directory. `XDG_CONFIG_HOME`
takes its place when it names a directory that exists, and `USERPROFILE` does
on Windows. The name has no `OPENGREP_` form.

The only thing opengrep keeps under that directory is the cache of
[`opengrep lsp`](../commands/lsp.md), in `.semgrep/cache`. That comes from the
source; the language server needs a client to drive it, so no example here
exercises it. Scans, tests and validation write nothing to the home directory.

Programs opengrep runs may read `HOME` for their own reasons: git, when
cloning a `git+` config, looks for its configuration there.

## Examples

### A scan leaves the home directory alone

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
$ mkdir home
$ HOME=$PWD/home opengrep scan --config rule.yaml app.py > /dev/null 2>&1
$ ls -A home | wc -l | tr -d ' '
0
```
