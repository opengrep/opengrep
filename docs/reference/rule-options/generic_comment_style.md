<!-- reference
id: opt-generic_comment_style
kind: option
name: generic_comment_style
summary: With spacegrep, remove comments of one style from the target before matching.
value: `c`, `cpp` or `shell`
related: []
-->
# `generic_comment_style`

<!-- BEGIN GENERATED: facts -->
- **Value:** `c`, `cpp` or `shell`
<!-- END GENERATED: facts -->

With `languages: [generic]` and the default engine, spacegrep, the target is
matched as it is, comments included. `generic_comment_style` removes comments
from the target first, so that a pattern does not match code that is
commented out:

| Value | Removes |
|---|---|
| `c` | `/* ... */` |
| `cpp` | `/* ... */` and `// ...` |
| `shell` | `# ...` |

Comments in the pattern are not removed. The option has no effect with
[`generic_engine: aliengrep`](generic_engine.md).

## Examples

### A setting that is commented out

The rule `debug-on` removes shell comments.

**`settings.yaml`**
```yaml title="settings.yaml"
rules:
  - id: debug-on-default
    pattern: debug = true
    message: debug mode is on
    languages: [generic]
    severity: WARNING
  - id: debug-on
    pattern: debug = true
    message: debug mode is on
    languages: [generic]
    severity: WARNING
    options:
      generic_comment_style: shell
```

**`settings.conf`**
```text title="settings.conf"
debug = true
# debug = true
```

**Command and result:**
```console
$ opengrep scan --config settings.yaml settings.conf
settings.conf

  warn  debug-on
  debug mode is on

    1 │ debug = true

  warn  debug-on-default
  debug mode is on

    1 │ debug = true

  warn  debug-on-default
  debug mode is on

    2 │ # debug = true
```
