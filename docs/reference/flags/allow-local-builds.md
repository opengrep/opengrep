<!-- reference
id: flag-allow-local-builds
kind: flag
name: --allow-local-builds
summary: Let opengrep build the project to work out its dependencies; it has nothing to act on today.
commands: [scan, ci]
related: [flag-config]
-->
# `--allow-local-builds`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--config`](config.md)
<!-- END GENERATED: facts -->

Experimental. It allows opengrep to build the project it is scanning, so that
dependencies and the relationships between them can be worked out where
lockfiles are missing or say too little.

Building a project runs code from that project and from its dependencies. On
code you do not trust, that is the risk you would be taking, and it is why the
flag exists rather than the behaviour being the default.

In opengrep the flag has nothing to act on: the rules that use dependency
information are supply-chain rules, written with
`r2c-internal-project-depends-on`, and opengrep rejects them. A scan
configured with such a rule reports that and runs the remaining rules.

## Examples

### What happens to a rule that needs dependencies

**`dep.yaml`**
```yaml title="dep.yaml"
rules:
  - id: dep-rule
    r2c-internal-project-depends-on:
      depends-on-either:
        - namespace: pypi
          package: requests
          version: "< 2.0"
    pattern: requests.get(...)
    message: old requests
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
requests.get("/")
```

**Command and result:**
```console
$ opengrep scan --config dep.yaml app.py 2>&1 | grep 'does not support supply-chain'
Opengrep does not support supply-chain (dependency) rules: this rule matches on the project's dependencies with 'r2c-internal-project-depends-on'
```
