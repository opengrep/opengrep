<!-- reference
id: env-OPENGREP_PR_ID
kind: env
name: OPENGREP_PR_ID
aliases: [SEMGREP_PR_ID]
summary: Mark an opengrep ci run as a pull request, which makes its event pull_request.
value: a pull or merge request id
related: [cmd-ci]
-->
# `OPENGREP_PR_ID`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_PR_ID`
- **Value:** a pull or merge request id
- **See also:** [`opengrep ci`](../commands/ci.md), [`OPENGREP_AUDIT_ON`](OPENGREP_AUDIT_ON.md)
<!-- END GENERATED: facts -->

Tells [`opengrep ci`](../commands/ci.md) the id of the pull or merge request
the run belongs to, and with it the event name: `pull_request` when the
variable is set, `unknown` otherwise.

That holds when no CI provider is detected, and on Azure Pipelines, Bitbucket,
Buildkite, CircleCI, Jenkins and Travis, where this variable is read before the
provider's own pull-request variable. GitHub Actions and GitLab CI take the
event from their own variables, `GITHUB_EVENT_NAME` and `CI_PIPELINE_SOURCE`,
and this one does not change it.

The event name matters for [`--audit-on`](../flags/audit-on.md), which exits 0
despite blocking findings only when the event is one it names. The id itself
appears in no output.

## Examples

### A run that counts as a pull request

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
$ git init -q && git add . && git commit -qm init
$ OPENGREP_PR_ID=17 opengrep ci --config rule.yaml 2>&1 >/dev/null | awk -F ' · ' 'NR == 1 { print $2 " · " $3 }'
git · pull_request
$ opengrep ci --config rule.yaml --audit-on pull_request > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
$ OPENGREP_PR_ID=17 opengrep ci --config rule.yaml --audit-on pull_request > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
```

### Providers that honour it, and one that does not

The provider is detected from its own variables, so setting them is enough to
see the difference.

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
$ git init -q && git add . && git commit -qm init
$ CIRCLECI=true OPENGREP_PR_ID=17 opengrep ci --config rule.yaml 2>&1 >/dev/null | awk -F ' · ' 'NR == 1 { print $2 " · " $3 }'
circleci · pull_request
$ GITLAB_CI=true CI_PIPELINE_SOURCE=push OPENGREP_PR_ID=17 opengrep ci --config rule.yaml 2>&1 >/dev/null | awk -F ' · ' 'NR == 1 { print $2 " · " $3 }'
gitlab-ci · push
```
