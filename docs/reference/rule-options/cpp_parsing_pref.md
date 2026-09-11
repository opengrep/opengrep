<!-- reference
id: opt-cpp_parsing_pref
kind: option
name: cpp_parsing_pref
summary: Read an ambiguous C++ pattern such as foo $X(...); as a function declaration or a constructed variable.
value: `as_vardef_with_ctor` or `as_fundef`
default: as_vardef_with_ctor
related: []
-->
# `cpp_parsing_pref`

<!-- BEGIN GENERATED: facts -->
- **Value:** `as_vardef_with_ctor` or `as_fundef`
- **Default:** `as_vardef_with_ctor`
<!-- END GENERATED: facts -->

In C++, `foo bar(x);` can declare a function `bar` that takes an `x` and
returns a `foo`, or define a variable `bar` of type `foo` built with the
argument `x`. In a function body opengrep reads it as a variable; elsewhere,
as in a class body, as a declaration. A pattern has no such context, and this
option chooses how a pattern like `foo $X(...);` is read:

- `as_vardef_with_ctor`: as a variable built with a constructor, which matches
  `foo obj(1);` in a function body;
- `as_fundef`: as a function declaration, which matches `foo bar(x);` in a
  class body.

A rule without an `options:` key reads `foo $X(...);` as a declaration. A rule
with an `options:` key, even one that sets only another option, reads it as a
variable, since `as_vardef_with_ctor` is the option's default.

In this version, a pattern whose type is `std::string` or `string` matches
nothing when read with `as_vardef_with_ctor`.

## Examples

### One pattern, three readings

**`ctor.yaml`**
```yaml title="ctor.yaml"
rules:
  - id: foo-default
    pattern: foo $X(...);
    message: foo $X
    languages: [cpp]
    severity: INFO
  - id: foo-ctor
    pattern: foo $X(...);
    message: foo $X
    languages: [cpp]
    severity: INFO
    options:
      cpp_parsing_pref: as_vardef_with_ctor
  - id: foo-fundef
    pattern: foo $X(...);
    message: foo $X
    languages: [cpp]
    severity: INFO
    options:
      cpp_parsing_pref: as_fundef
  - id: foo-other-option
    pattern: foo $X(...);
    message: foo $X
    languages: [cpp]
    severity: INFO
    options:
      symbolic_propagation: true
```

**`ctor.cpp`**
```cpp title="ctor.cpp"
class Widget {
  // ruleid: foo-default, foo-fundef
  foo bar(x);
  void build() {
    // ruleid: foo-ctor, foo-other-option
    foo obj(1);
  }
};
```

**Command and result:**
```console
$ opengrep test .
4/4: ✓ All tests passed
No tests for fixes found.
```

`foo-other-option` does not mention `cpp_parsing_pref`, but its `options:` key
gives it the default reading, as a variable.
