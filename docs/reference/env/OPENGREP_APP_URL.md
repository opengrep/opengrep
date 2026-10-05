<!-- reference
id: env-OPENGREP_APP_URL
kind: env
name: OPENGREP_APP_URL
aliases: [SEMGREP_APP_URL]
summary: Another name for the registry base URL, used when OPENGREP_URL is not set.
value: a URL
related: []
-->
# `OPENGREP_APP_URL`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_APP_URL`
- **Value:** a URL
- **See also:** [`OPENGREP_URL`](OPENGREP_URL.md)
<!-- END GENERATED: facts -->

Another name for [`OPENGREP_URL`](OPENGREP_URL.md). Opengrep reads it only
when `OPENGREP_URL` and `SEMGREP_URL` are both unset, and uses it as the base
URL of the rule registry.

## Examples

### Which name wins

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ OPENGREP_APP_URL=http://app.invalid opengrep scan --config p/python app.py 2>&1 | grep -o 'http://[a-z.]*/c/p/python'
http://app.invalid/c/p/python
$ OPENGREP_URL=http://registry.invalid OPENGREP_APP_URL=http://app.invalid opengrep scan --config p/python app.py 2>&1 | grep -o 'http://[a-z.]*/c/p/python'
http://registry.invalid/c/p/python
```

With both set, the download goes to `registry.invalid`, the address in
`OPENGREP_URL`.
