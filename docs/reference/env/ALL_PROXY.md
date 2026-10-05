<!-- reference
id: env-ALL_PROXY
kind: env
name: ALL_PROXY
aliases: [all_proxy]
summary: The proxy for opengrep's downloads over both http and https, unless a scheme has its own.
value: a proxy URL with a scheme, such as `http://proxy.example.com:3128`
related: [env-HTTP_PROXY]
-->
# `ALL_PROXY`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `all_proxy`
- **Value:** a proxy URL with a scheme, such as `http://proxy.example.com:3128`
- **See also:** [`HTTP_PROXY`](HTTP_PROXY.md), [`HTTPS_PROXY`](HTTPS_PROXY.md)
<!-- END GENERATED: facts -->

Names one proxy for every download opengrep makes, over `http` and `https`.
[`HTTPS_PROXY`](HTTPS_PROXY.md) and [`HTTP_PROXY`](HTTP_PROXY.md) win over it
for their own scheme, and hosts listed in [`NO_PROXY`](NO_PROXY.md) are
reached directly.

The lowercase `all_proxy` is read too; when both are set, `ALL_PROXY` wins.
The name has no `OPENGREP_` form.

## Examples

### Both schemes through one proxy

The host `proxy.invalid` does not exist, so a download sent to it fails when
opengrep looks the proxy up.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ ALL_PROXY=http://proxy.invalid:3128 opengrep scan --config http://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from http://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
$ ALL_PROXY=http://proxy.invalid:3128 opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
```
