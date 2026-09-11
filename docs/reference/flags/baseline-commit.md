<!-- reference
id: flag-baseline-commit
kind: flag
name: --baseline-commit
summary: Report only the findings that are not already present in the given commit.
commands: [scan, ci]
value: `COMMIT`, any git revision
env: [env-OPENGREP_BASELINE_COMMIT, env-OPENGREP_BASELINE_REF]
related: []
-->
# `--baseline-commit`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), `ci`
- **Value:** `COMMIT`, any git revision
- **Environment:** `OPENGREP_BASELINE_COMMIT`, `OPENGREP_BASELINE_REF`
<!-- END GENERATED: facts -->

Reports only the findings that are new relative to COMMIT. COMMIT is any
revision git understands, such as a hash, a branch, a tag or `HEAD~1`. The
current directory must be in a git repository. With a baseline, opengrep does
the following:

1. It takes the merge base of COMMIT and `HEAD` as the baseline. When COMMIT
   is an ancestor of `HEAD`, the merge base is COMMIT itself. For a branch
   name, it is the point where the current branch forked from that branch, so
   new commits on the other branch do not matter.
2. It lists the files that differ between the baseline and the git index,
   which holds the committed and staged changes. Unstaged changes and
   untracked files are not part of the list.
3. It scans those files.
4. It scans the same files as they were in the baseline, in a separate
   checkout.
5. It reports the findings of step 3 that step 4 did not have. Two findings
   are the same when they have the same rule id, file path and matched code.
   Line numbers do not count, so code that moved is not reported again.

When git cannot compute the merge base, opengrep stops with exit status 2.
That happens outside a git repository, and in shallow clones that lack the
history.

The summary on standard error says `Scan was limited to files changed since
baseline commit.` An empty value, as in `--baseline-commit ""`, means no
baseline.

When the flag is not given, the baseline comes from the first of these
variables that is set: `OPENGREP_BASELINE_COMMIT`, `SEMGREP_BASELINE_COMMIT`,
`OPENGREP_BASELINE_REF`, `SEMGREP_BASELINE_REF`.

## Examples

### Only new findings

```yaml title="rule.yaml"
rules:
  - id: no-print
    pattern: print(...)
    message: use logging instead of print
    languages: [python]
    severity: INFO
```

```python title="a.py"
print("old")
```

```console
$ git init -q && git add . && git commit -qm base
$ echo 'print("new")' > b.py && git add b.py && git commit -qm change
$ opengrep scan --config rule.yaml --baseline-commit HEAD~1 .


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    b.py
     ❱ no-print
          use logging instead of print

            1┆ print("new")
```
