<!-- reference
id: flag-skip-invalid-configs
kind: flag
name: --skip-invalid-configs
summary: Skip files in a rules directory that are not rule configs, instead of stopping.
commands: [scan]
related: [flag-config, cmd-validate, flag-strict]
-->
# `--skip-invalid-configs`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`--config`](config.md), [`opengrep validate`](../commands/validate.md), [`--strict`](strict.md)
<!-- END GENERATED: facts -->

A directory of rules often holds YAML that is not a rule: a CI workflow, a
configuration file for something else. Loading such a directory normally stops
the scan with exit status 7, because one of the files did not parse as a rule
config.

With this flag opengrep warns about each such file and scans with the rules it
could load. It applies to rules loaded from a directory or from a
`git+<url>` repository. A config file you name yourself still has to be
valid: naming a broken file and asking for it to be skipped would leave
nothing to run.

To find out which files are the problem, run
[`opengrep validate`](../commands/validate.md) over the directory.

## Examples

### A workflow file among the rules

**`rules/good.yaml`**
```yaml title="rules/good.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`rules/workflow.yml`**
```yaml title="rules/workflow.yml"
name: CI
on: push
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rules/ app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
$ opengrep scan --config rules/ --skip-invalid-configs app.py
app.py

  warn  rules.find-eval
  found eval

    1 │ eval(1)
```
