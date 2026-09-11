<!-- reference
id: opt-implicit_return
kind: option
name: implicit_return
summary: Let a return pattern match the value a function returns without the return keyword.
value: `true` or `false`
default: true
related: []
-->
# `implicit_return`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
<!-- END GENERATED: facts -->

A pattern `return e` also matches `e` where `e` is returned without the
`return` keyword:

- the last expression of a function, in languages where that expression is
  the function's value, such as Ruby, Rust, Scala and Julia, including the
  last expression of each branch that ends the function;
- the body of a lambda, such as a Python `lambda` or a Kotlin lambda.

With `implicit_return: false`, only an explicit `return` matches.

## Examples

### The last expression of a Ruby method

The rule `returns-user` turns the option off.

**`users.yaml`**
```yaml title="users.yaml"
rules:
  - id: returns-user-default
    pattern: return User.find(...)
    message: returns a user
    languages: [ruby]
    severity: WARNING
  - id: returns-user
    pattern: return User.find(...)
    message: returns a user
    languages: [ruby]
    severity: WARNING
    options:
      implicit_return: false
```

**`users.rb`**
```ruby title="users.rb"
def find_user(id)
  # ruleid: returns-user-default
  User.find(id)
end

def find_user_explicitly(id)
  # ruleid: returns-user-default, returns-user
  return User.find(id)
end

def find_user_if_given(id)
  if id
    # ruleid: returns-user-default
    User.find(id)
  else
    nil
  end
end

def find_and_log(id)
  # ok: returns-user-default
  user = User.find(id)
  log(user)
end
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
