<!-- reference
id: env-GIT_SSH_COMMAND
kind: env
name: GIT_SSH_COMMAND
summary: The ssh command git uses to clone a git+ssh rule repository; opengrep sets a non-prompting one when it is unset.
value: a shell command
related: [flag-config]
-->
# `GIT_SSH_COMMAND`

<!-- BEGIN GENERATED: facts -->
- **Value:** a shell command
- **See also:** [`--config`](../flags/config.md)
<!-- END GENERATED: facts -->

A [`--config`](../flags/config.md) of the form `git+ssh://…` makes opengrep
clone that repository with git, which reaches it with the ssh command named
here. When the variable is set, opengrep leaves it alone, so it is the place for
a deploy key (`ssh -i deploy_key`) or other ssh options.

When it is unset, opengrep sets it to `ssh -oBatchMode=yes` for the clone, and
it always sets `GIT_TERMINAL_PROMPT=0`. Neither ssh nor git may then stop to ask
for a password, a passphrase or a host key: a clone that cannot authenticate
fails at once, instead of leaving the scan waiting. That default comes from the
source; the examples below use a command of their own.

A clone that fails, for whatever reason, is reported as
`Could not clone git repo … (ensure it is accessible non-interactively:
ssh-agent / credential helper)`. The name is read as it is, with no
`OPENGREP_` form.

## Examples

### A custom ssh command

The command here only leaves a file behind and fails, which is enough to show
that git ran it.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ GIT_SSH_COMMAND="sh -c 'touch $PWD/ssh-ran; exit 1' --" opengrep scan --config git+ssh://git@example.invalid/rules.git app.py > /dev/null 2>&1
$ ls ssh-ran
ssh-ran
$ GIT_SSH_COMMAND="sh -c 'exit 1' --" opengrep scan --config git+ssh://git@example.invalid/rules.git app.py 2>&1 | grep 'Could not clone'
[00.11][ERROR]: Could not clone git repo ssh://git@example.invalid/rules.git (ensure it is accessible non-interactively: ssh-agent / credential helper)
```
