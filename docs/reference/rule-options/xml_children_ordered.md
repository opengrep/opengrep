<!-- reference
id: opt-xml_children_ordered
kind: option
name: xml_children_ordered
summary: Require the children of an XML element to appear in the order the pattern gives.
value: `true` or `false`
default: true
related: []
-->
# `xml_children_ordered`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`xml_singleton_loose_matching`](xml_singleton_loose_matching.md)
<!-- END GENERATED: facts -->

The children of an element in an XML pattern match children in the same
order. With `xml_children_ordered: false`, they match in any order.

## Examples

### Children in the other order

The rule `list` turns the option off.

**`lists.yaml`**
```yaml title="lists.yaml"
rules:
  - id: list-default
    pattern: <list><a/><b/></list>
    message: list with a and b
    languages: [xml]
    severity: INFO
  - id: list
    pattern: <list><a/><b/></list>
    message: list with a and b
    languages: [xml]
    severity: INFO
    options:
      xml_children_ordered: false
```

**`lists.xml`**
```xml title="lists.xml"
<root>
<!-- ruleid: list-default, list -->
<list><a/><b/></list>
<!-- ruleid: list -->
<list><b/><a/></list>
</root>
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
