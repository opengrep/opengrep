<!-- reference
id: flag-junit-xml
kind: flag
name: --junit-xml
summary: Print the findings as a JUnit XML report, so a CI system shows them as failed tests.
commands: [scan, ci]
related: [flag-json, flag-sarif, flag-gitlab-sast, flag-output]
-->
# `--junit-xml`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--json`](json.md), [`--sarif`](sarif.md), [`--gitlab-sast`](gitlab-sast.md), [`--output`](output.md), [`--junit-xml-output`](junit-xml-output.md)
<!-- END GENERATED: facts -->

Prints the findings as JUnit XML, the format most CI systems already know how
to display. The document holds one `testsuite` named `opengrep results`, and
one `testcase` per finding: `name` is the rule id, `classname` and `file` are
the path, and `line` is where the match starts. Inside each is a `failure`
whose `type` is the rule's severity and whose `message` is the rule's message,
with the matched lines as its body.

A scan with no findings therefore looks like a test run with no failures.
The `time` attributes hold how long the scan took.

[`--junit-xml-output`](junit-xml-output.md) writes the same document to a
file, which is the usual way to use it: the job prints its normal report and
leaves the XML for the CI system to collect.

## Examples

### The shape of the report

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

The `time` attributes are dropped here, because they differ from run to run.

**Command and result:**
```console
$ opengrep scan --config rule.yaml --junit-xml app.py 2>/dev/null | sed 's/ time="[^"]*"//g'
<?xml version="1.0" encoding="UTF-8"?>
<testsuites disabled="0" errors="0" failures="1" tests="1"><testsuite disabled="0" errors="0" failures="1" name="opengrep results" skipped="0" tests="1"><testcase name="find-eval" classname="app.py" file="app.py" line="1"><failure type="WARNING" message="found eval">eval(1)</failure></testcase></testsuite></testsuites>
```
