<!-- reference
id: flag-scan-unknown-extensions
kind: flag
name: --scan-unknown-extensions
aliases: [--skip-unknown-extensions]
summary: Scan a file named on the command line even when its extension names no language.
commands: [scan, ci]
default: false
related: [flag-lang, flag-include, flag-force-exclude]
-->
# `--scan-unknown-extensions`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--skip-unknown-extensions`
- **Default:** `false`
- **See also:** [`--lang`](lang.md), [`--include`](include.md), [`--force-exclude`](force-exclude.md)
<!-- END GENERATED: facts -->

Opengrep chooses a language for each file from its extension. A file named on
the command line whose extension names no language is skipped like any other,
and a scan of nothing else reports `nothing to scan`.

`--scan-unknown-extensions` makes such a file bypass language detection. It is
then analysed as `--lang` says, or, with no `--lang`, with the languages of
the rules being run. This applies only to files named on the command line,
never to files found by walking a directory, where the extension still
decides. `--skip-unknown-extensions` is the default.

## Examples

### A file whose extension names no language

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`script.unknown`**
```title="script.unknown"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml script.unknown 2>&1 >/dev/null
nothing to scan

0 files · 0 findings
$ opengrep scan --config rule.yaml --scan-unknown-extensions script.unknown
script.unknown

  warn  find-eval
  found eval

    1 │ eval(1)
```

The first command prints nothing on standard output: the message quoted here
is on standard error.
