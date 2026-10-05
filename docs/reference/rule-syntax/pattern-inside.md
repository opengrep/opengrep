<!-- reference
id: key-pattern-inside
kind: rule-key
name: pattern-inside
summary: Keep only the matches that lie inside code matching this pattern.
related: [key-pattern-not-inside, key-patterns]
-->
# `pattern-inside`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`pattern-not-inside`](pattern-not-inside.md), [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`pattern-inside` is an item of [`patterns`](patterns.md). It keeps the matches
of the other items that lie inside code matched by its own pattern, and drops
the rest. The pattern usually describes the surroundings with an ellipsis
standing for the part where the match may be: the body of a function, the
statements after an assignment.

Metavariables bound in `pattern-inside` are shared with the other items of the
same `patterns`, so a context can name a variable that the main pattern then
has to use.

## Examples

### Inside a request handler

**`handler-shell.yaml`**
```yaml title="handler-shell.yaml"
rules:
  - id: handler-shell
    patterns:
      - pattern-inside: |
          @app.route(...)
          def $HANDLER(...):
              ...
      - pattern: os.system(...)
    message: os.system in a request handler
    languages: [python]
    severity: ERROR
```

**`handler-shell.py`**
```python title="handler-shell.py"
import os
from flask import Flask

app = Flask(__name__)

@app.route("/backup")
def backup():
    # ruleid: handler-shell
    os.system("tar czf /tmp/backup.tgz /srv/data")

def nightly():
    # ok: handler-shell
    os.system("tar czf /tmp/backup.tgz /srv/data")
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A metavariable bound by the context

**`flask-debug.yaml`**
```yaml title="flask-debug.yaml"
rules:
  - id: flask-debug
    patterns:
      - pattern-inside: |
          $APP = Flask(...)
          ...
      - pattern: $APP.run(..., debug=True, ...)
    message: Flask app $APP runs with the debugger enabled
    languages: [python]
    severity: ERROR
```

**`flask-debug.py`**
```python title="flask-debug.py"
from flask import Flask

app = Flask(__name__)
server = make_server()

# ruleid: flask-debug
app.run(debug=True)
# ok: flask-debug
server.run(debug=True)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
