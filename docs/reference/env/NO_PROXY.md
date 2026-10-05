<!-- reference
id: env-NO_PROXY
kind: env
name: NO_PROXY
aliases: [no_proxy]
summary: Hosts that opengrep's downloads reach directly, without the proxy.
value: a comma-separated list of host names or domain suffixes, or `*`
related: [env-HTTPS_PROXY]
-->
# `NO_PROXY`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `no_proxy`
- **Value:** a comma-separated list of host names or domain suffixes, or `*`
- **See also:** [`HTTPS_PROXY`](HTTPS_PROXY.md)
<!-- END GENERATED: facts -->

Lists the hosts that are reached directly even when
[`HTTPS_PROXY`](HTTPS_PROXY.md), [`HTTP_PROXY`](HTTP_PROXY.md) or
[`ALL_PROXY`](ALL_PROXY.md) is set. Entries are separated by commas, and spaces
around them are ignored.

An entry matches the host itself and every host under it: `example.com`,
`.example.com` and `com` all match `rules.example.com`, while `example.com`
does not match `notexample.com`. A port is not part of
the match, so an entry such as `example.com:443` matches nothing. `*` sends
every download directly.

The lowercase `no_proxy` is read too; when both are set, `NO_PROXY` wins. The
name has no `OPENGREP_` form.

## Examples

### One host past the proxy

The host `proxy.invalid` does not exist, so a download sent to it fails when
opengrep looks the proxy up.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ HTTPS_PROXY=http://proxy.invalid:3128 opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
$ HTTPS_PROXY=http://proxy.invalid:3128 NO_PROXY=example.invalid opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: TLS to non-TCP currently unsupported: host=example.invalid endp=(Unknown "name resolution failed")
$ HTTPS_PROXY=http://proxy.invalid:3128 NO_PROXY=other.invalid opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
```

Only the second download skips the proxy and goes to `example.invalid`.
