<!-- reference
id: env-HTTP_PROXY
kind: env
name: HTTP_PROXY
aliases: [http_proxy]
summary: The proxy for opengrep's plain http downloads; https downloads use HTTPS_PROXY.
value: a proxy URL with a scheme, such as `http://proxy.example.com:3128`
related: []
-->
# `HTTP_PROXY`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `http_proxy`
- **Value:** a proxy URL with a scheme, such as `http://proxy.example.com:3128`
- **See also:** [`ALL_PROXY`](ALL_PROXY.md), [`HTTPS_PROXY`](HTTPS_PROXY.md)
<!-- END GENERATED: facts -->

Does what [`HTTPS_PROXY`](HTTPS_PROXY.md) does, for downloads over plain
`http`. It does not cover `https`, which is how the registry and most rule URLs
are reached, so behind a proxy you will usually want both variables set, or
[`ALL_PROXY`](ALL_PROXY.md).

The lowercase `http_proxy` is read too; when both are set, `HTTP_PROXY` wins.
The name has no `OPENGREP_` form.

## Examples

### Which downloads it covers

The host `proxy.invalid` does not exist, so a download sent to it fails when
opengrep looks the proxy up.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ HTTP_PROXY=http://proxy.invalid:3128 opengrep scan --config http://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from http://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
$ HTTP_PROXY=http://proxy.invalid:3128 opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: TLS to non-TCP currently unsupported: host=example.invalid endp=(Unknown "name resolution failed")
```

The `http` download went to the proxy; the `https` one did not use it and
failed on `example.invalid` itself.
