<!-- reference
id: flag-junit-xml-output
kind: flag
name: --junit-xml-output
summary: Also write the findings as a JUnit XML report to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-junit-xml, flag-output]
-->
# `--junit-xml-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--junit-xml`](junit-xml.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the JUnit XML report that [`--junit-xml`](junit-xml.md) prints to FILE,
in addition to what the run prints on standard output. CI systems that show
test results can then show each finding as a failed test, while the log keeps
the readable report.

Give the flag several times to write several copies. The file is written even
when the scan finds nothing. [`--output`](output.md) describes the rules that
all output files follow.

## Examples

### A test report

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
$ opengrep scan --config rule.yaml --junit-xml-output results.xml app.py > /dev/null 2>&1
$ python3 -c "import xml.etree.ElementTree as ET; print([(t.get('name'), t.get('file'), t.get('line')) for t in ET.parse('results.xml').iter('testcase')])"
[('find-eval', 'app.py', '1')]
```

The command prints the rule, file and line of each test case in the file.
