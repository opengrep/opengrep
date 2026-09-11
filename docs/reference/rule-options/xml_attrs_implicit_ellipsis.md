<!-- reference
id: opt-xml_attrs_implicit_ellipsis
kind: option
name: xml_attrs_implicit_ellipsis
summary: Let an XML pattern match elements with more attributes than it lists.
value: `true` or `false`
default: true
related: []
-->
# `xml_attrs_implicit_ellipsis`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`xml_singleton_loose_matching`](xml_singleton_loose_matching.md)
<!-- END GENERATED: facts -->

An element in an XML pattern matches an element that has at least the
attributes it lists, in any order, as if they were followed by `...`.

With `xml_attrs_implicit_ellipsis: false`, the element must have exactly those
attributes, unless the pattern writes the `...` itself.

## Examples

### An input with more attributes

The rules `password` and `password-dots` turn the option off; the second adds
`...` to its pattern.

**`form.yaml`**
```yaml title="form.yaml"
rules:
  - id: password-default
    pattern: <input type="password"/>
    message: password field
    languages: [xml]
    severity: INFO
  - id: password
    pattern: <input type="password"/>
    message: password field
    languages: [xml]
    severity: INFO
    options:
      xml_attrs_implicit_ellipsis: false
  - id: password-dots
    pattern: <input type="password" .../>
    message: password field
    languages: [xml]
    severity: INFO
    options:
      xml_attrs_implicit_ellipsis: false
```

**`form.xml`**
```xml title="form.xml"
<form>
<!-- ruleid: password-default, password, password-dots -->
<input type="password"/>
<!-- ruleid: password-default, password-dots -->
<input type="password" name="secret"/>
</form>
```

**Command and result:**
```console
$ opengrep test .
3/3: ✓ All tests passed
No tests for fixes found.
```
