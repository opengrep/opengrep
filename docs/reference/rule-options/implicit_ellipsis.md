<!-- reference
id: opt-implicit_ellipsis
kind: option
name: implicit_ellipsis
summary: Let a record or class pattern match targets with more fields than it lists.
value: `true` or `false`
default: true
related: []
-->
# `implicit_ellipsis`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
<!-- END GENERATED: facts -->

A pattern for a record, such as the object literal `{method: 'POST'}`, or for
a class body, matches a target that has at least the fields it lists, in any
order, as if it ended with `...`.

With `implicit_ellipsis: false`, the target must have exactly the fields of
the pattern, unless the pattern writes the `...` itself.

## Examples

### Extra fields in an object

The rules `post-fetch` and `post-fetch-dots` turn the option off; the second
adds `...` to its pattern.

**`fetch.yaml`**
```yaml title="fetch.yaml"
rules:
  - id: post-fetch-default
    pattern: "fetch($URL, {method: 'POST'})"
    message: POST request
    languages: [javascript]
    severity: WARNING
  - id: post-fetch
    pattern: "fetch($URL, {method: 'POST'})"
    message: POST request
    languages: [javascript]
    severity: WARNING
    options:
      implicit_ellipsis: false
  - id: post-fetch-dots
    pattern: "fetch($URL, {method: 'POST', ...})"
    message: POST request
    languages: [javascript]
    severity: WARNING
    options:
      implicit_ellipsis: false
```

**`fetch.js`**
```javascript title="fetch.js"
// ruleid: post-fetch-default, post-fetch, post-fetch-dots
fetch(url, {method: 'POST'});
// ruleid: post-fetch-default, post-fetch-dots
fetch(url, {method: 'POST', body: data});
// ruleid: post-fetch-default, post-fetch-dots
fetch(url, {body: data, method: 'POST'});
```

**Command and result:**
```console
$ opengrep test .
3/3: ✓ All tests passed
No tests for fixes found.
```

### Extra methods in a class

**`models.yaml`**
```yaml title="models.yaml"
rules:
  - id: saveable-default
    pattern: |
      class $C:
          def save(self): ...
    message: class with a save method
    languages: [python]
    severity: WARNING
  - id: saveable
    pattern: |
      class $C:
          def save(self): ...
    message: class with a save method
    languages: [python]
    severity: WARNING
    options:
      implicit_ellipsis: false
```

**`models.py`**
```python title="models.py"
# ruleid: saveable-default, saveable
class Note:
    def save(self): pass

# ruleid: saveable-default
class User:
    def save(self): pass
    def load(self): pass
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
