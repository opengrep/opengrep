<!-- reference
id: flag-replacement
kind: flag
name: --replacement
summary: The fix for a command-line pattern, as a rule's fix key would give it.
commands: [scan]
value: `EXPRESSION`
related: [flag-pattern, flag-autofix, flag-dryrun, key-fix]
-->
# `--replacement`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Value:** `EXPRESSION`
- **See also:** [`--pattern`](pattern.md), [`--autofix`](autofix.md), [`--dryrun`](dryrun.md), `fix`
<!-- END GENERATED: facts -->

Gives a fix for the pattern passed with [`-e`/`--pattern`](pattern.md), the
way a rule's `fix:` key does. Metavariables bound by the pattern can be used
in it, so `-e 'eval($X)' --replacement 'safe($X)'` rewrites the argument into
a call to `safe`.

It only means anything with a command-line pattern; there is nothing for it to
fix in a rule file, which carries its own `fix:`. As with a rule's fix, the
change reaches your files only with [`--autofix`](autofix.md), and
[`--dryrun`](dryrun.md) shows it without writing.

## Examples

### A fix for a one-off search

**`app.py`**
```python title="app.py"
eval(1)
print(2)
```

**Command and result:**
```console
$ opengrep scan -e 'eval($X)' -l python --replacement 'safe($X)' --autofix --dryrun app.py
app.py

  error  -
  eval(1)

    1 │ safe(1)

    fix: safe(1)

$ cat app.py
eval(1)
print(2)
```

The file is unchanged: `--dryrun` showed the fix instead of writing it.
