<!-- reference
id: env-HTTPS_PROXY
kind: env
name: HTTPS_PROXY
aliases: [https_proxy]
summary: The proxy for opengrep's https downloads, such as rules from a URL or the registry.
value: a proxy URL with a scheme, such as `http://proxy.example.com:3128`
related: [env-HTTP_PROXY, env-ALL_PROXY, env-NO_PROXY, env-OPENGREP_URL]
-->
# `HTTPS_PROXY`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `https_proxy`
- **Value:** a proxy URL with a scheme, such as `http://proxy.example.com:3128`
- **See also:** [`HTTP_PROXY`](HTTP_PROXY.md), [`ALL_PROXY`](ALL_PROXY.md), [`NO_PROXY`](NO_PROXY.md), [`OPENGREP_URL`](OPENGREP_URL.md)
<!-- END GENERATED: facts -->

Opengrep goes to the network to fetch rules: a
[`--config`](../flags/config.md) that is a URL, or a registry entry such as
`p/python`. An `https` download goes through the proxy this variable names,
which opengrep asks to open a tunnel to the server.

The lowercase `https_proxy` is read too; when both are set, `HTTPS_PROXY`
wins. It covers `https` URLs only, so a plain `http` download needs
[`HTTP_PROXY`](HTTP_PROXY.md), or [`ALL_PROXY`](ALL_PROXY.md) for both. The
registry, `https://semgrep.dev` by default, is reached over `https`. Hosts
listed in [`NO_PROXY`](NO_PROXY.md) are reached directly. The name has no
`OPENGREP_` form.

The value must include the scheme: `127.0.0.1:3128` alone fails the download.

Rules from a `git+` repository are fetched by git, which reads its own proxy
settings.

## Examples

### A download sent to the proxy

The host `proxy.invalid` does not exist, so a download sent to it fails when
opengrep looks the proxy up, which shows where it went.

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: TLS to non-TCP currently unsupported: host=example.invalid endp=(Unknown "name resolution failed")
$ HTTPS_PROXY=http://proxy.invalid:3128 opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
$ https_proxy=http://proxy.invalid:3128 opengrep scan --config https://example.invalid/rules.yaml app.py 2>&1 | grep 'Failed to download'
[00.00][ERROR]: Failed to download config from https://example.invalid/rules.yaml: Failure: resolution failed: name resolution failed
```

The first download goes straight to `example.invalid`, and fails there. The
other two go to the proxy.
