<!-- reference
id: env-OPENGREP_URL
kind: env
name: OPENGREP_URL
aliases: [SEMGREP_URL]
summary: The base URL of the rule registry, for configs such as p/python and auto.
value: a URL
default: https://semgrep.dev
related: [env-OPENGREP_APP_URL, flag-config]
-->
# `OPENGREP_URL`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_URL`
- **Value:** a URL
- **Default:** `https://semgrep.dev`
- **See also:** [`OPENGREP_APP_URL`](OPENGREP_APP_URL.md), [`--config`](../flags/config.md), [`HTTPS_PROXY`](HTTPS_PROXY.md)
<!-- END GENERATED: facts -->

A [`--config`](../flags/config.md) value that names a registry entry — `p/NAME`,
`r/NAME`, `s/NAME`, `auto` or `r2c` — is downloaded from the registry, at
`<base>/c/<value>`. This variable sets the base. When it is unset,
[`OPENGREP_APP_URL`](OPENGREP_APP_URL.md) is used, and when neither is set, the
base is `https://semgrep.dev`.

Point it at a mirror or an internal copy of the registry. Configs that are
files, directories, URLs or `git+` repositories do not involve the registry
and ignore it.

## Examples

### Where a registry config is fetched from

The host `registry.invalid` does not exist, so the download fails at once, and
the error shows the address opengrep tried.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ OPENGREP_URL=http://registry.invalid opengrep scan --config p/python app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from http://registry.invalid/c/p/python: Failure: resolution failed: name resolution failed
$ OPENGREP_URL=http://registry.invalid opengrep scan --config p/python app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 2
```
