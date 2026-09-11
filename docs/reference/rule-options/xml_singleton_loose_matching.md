<!-- reference
id: opt-xml_singleton_loose_matching
kind: option
name: xml_singleton_loose_matching
summary: Let a self-closing element and an element with open and close tags match each other.
value: `true` or `false`
default: true
related: [opt-xml_attrs_implicit_ellipsis, opt-xml_children_ordered]
-->
# `xml_singleton_loose_matching`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`xml_attrs_implicit_ellipsis`](xml_attrs_implicit_ellipsis.md), [`xml_children_ordered`](xml_children_ordered.md)
<!-- END GENERATED: facts -->

In XML patterns, and in the XML-like syntax of other languages, an element
written with open and close tags, `<item id="$X"></item>`, also matches the
self-closing `<item id="1"/>`, and the other way round.

With `xml_singleton_loose_matching: false`, each form matches only itself.

## Examples

### Two ways to write an empty element

The rule `item` turns the option off.

**`items.yaml`**
```yaml title="items.yaml"
rules:
  - id: item-default
    pattern: <item id="$X"></item>
    message: item $X
    languages: [xml]
    severity: INFO
  - id: item
    pattern: <item id="$X"></item>
    message: item $X
    languages: [xml]
    severity: INFO
    options:
      xml_singleton_loose_matching: false
```

**`items.xml`**
```xml title="items.xml"
<root>
<!-- ruleid: item-default, item -->
<item id="1"></item>
<!-- ruleid: item-default -->
<item id="2"/>
</root>
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
