<!-- reference
id: key-fix
kind: rule-key
name: fix
summary: Replacement code for each match, applied by --autofix.
related: [flag-autofix, flag-dryrun, key-fix-regex, cmd-test]
-->
# `fix`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`--autofix`](../flags/autofix.md), [`--dryrun`](../flags/dryrun.md), [`fix-regex`](fix-regex.md), [`opengrep test`](../commands/test.md), [`--replacement`](../flags/replacement.md)
<!-- END GENERATED: facts -->

`fix` is the code that replaces each match of the rule. Metavariables in it
are replaced by the code they matched, so the fix can keep the parts of the
original that stay. [`--autofix`](../flags/autofix.md) rewrites the files
with it, and [`--dryrun`](../flags/dryrun.md) together with `--autofix` shows
the fixed line under each finding without writing anything.

[`opengrep test`](../commands/test.md) checks a fix when the target `x.js`
has a file `x.fixed.js` next to it: it applies the fix to `x.js` and compares
the result with `x.fixed.js`.

## Examples

### `innerHTML` to `textContent`

**`inner-html.yaml`**
```yaml title="inner-html.yaml"
rules:
  - id: inner-html
    pattern: $EL.innerHTML = $VALUE
    fix: $EL.textContent = $VALUE
    message: assigning to innerHTML can run injected scripts
    languages: [javascript]
    severity: WARNING
```

**`inner-html.js`**
```javascript title="inner-html.js"
function showComment(el, comment) {
  // ruleid: inner-html
  el.innerHTML = comment;
}
```

**`inner-html.fixed.js`**
```javascript title="inner-html.fixed.js"
function showComment(el, comment) {
  // ruleid: inner-html
  el.textContent = comment;
}
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
1/1: ✓ All fix tests passed
$ opengrep scan --config inner-html.yaml --autofix --dryrun inner-html.js
inner-html.js

  warn  inner-html
  assigning to innerHTML can run injected scripts

    3 │ el.textContent = comment;

    fix: el.textContent = comment

```
