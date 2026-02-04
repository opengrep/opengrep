<br />
<p align="center">
  <a href="https://github.com/opengrep">
    <picture>
      <source media="(prefers-color-scheme: light)" srcset="images/opengrep-github-banner.svg">
      <source media="(prefers-color-scheme: dark)" srcset="images/opengrep-github-banner.svg">
      <img src="https://raw.githubusercontent.com/opengrep/opengrep/main/images/opengrep-github-banner.svg" width="100%" alt="Opengrep logo"/>
    </picture>
  </a>
</p>

### Welcome to Opengrep, a fork of Semgrep, under the LGPL 2.1 license

**Opengrep is the most advanced open source SAST engine.**

Let's make secure software development a shared standard. Opengrep provides every developer and organisation with open and advanced static code analysis.

Opengrep is backed by a consortium of 10+ AppSec organisations, including: [Aikido](https://www.aikido.dev/), [Arnica](https://www.arnica.io), [Amplify](https://amplify.security/), [Endor Labs](https://www.endorlabs.com/), [Jit](https://www.jit.io/), [Kodem](https://www.kodemsecurity.com/), [Legit](https://www.legitsecurity.com/), [Mobb](https://www.mobb.ai/), [Orca Security](https://orca.security/), and [Phoenix Security](https://phoenix.security/). To learn more, read the manifesto at [opengrep.dev](https://opengrep.dev/).

## Why Opengrep?

Opengrep was created when Semgrep moved critical features behind a commercial licence. We believe advanced static analysis should remain open and accessible to all.

**Key advantages:**
- **Compatible with Semgrep rules** - your existing rules and rulesets work unchanged
- **Standard outputs** - JSON and SARIF formats for easy integration
- **Open governance** - contributions accepted on merit, not commercial interest
- **Long-term assurance** - committed to open-source under LGPL 2.1

## Key Improvements

Opengrep has introduced significant improvements since the fork. Highlights include:

**Superior Taint Analysis** (`--taint-intrafile`):
- Constructor and field assignment tracking
- Inter-method taint flow
- Higher-order function support across 12 languages
- Collection method tainting (map, filter, reduce, etc.)

See the [Intrafile Tainting Tutorial](https://github.com/opengrep/opengrep/wiki/Intrafile-tainting-tutorial) and [Higher-Order Functions Tutorial](https://github.com/opengrep/opengrep/wiki/Higher-order-functions-tutorial) for details.

**Language Support:**
- **Visual Basic** - not available in Semgrep CE or Pro
- **Apex, Elixir** - not in Semgrep CE
- **Improved**: Clojure (tainting support), PHP 8.4, C# 14

**Distribution:**
- Self-contained binaries via Nuitka (no Python required)
- Signed releases with Cosign

See [OPENGREP.md](OPENGREP.md) for the full list of improvements since the fork.

# Opengrep: Fast and Powerful Code Pattern Search

Opengrep is an ultra-fast static analysis tool for searching code patterns with the power of semantic grep. Analyze large code bases at the speed of thought with intuitive pattern matching and customizable rules. Find and fix security vulnerabilities, fast – ship more secure code.

Opengrep supports 30+ languages, including:

Apex · Bash · C · C++ · C# · Clojure · Dart · Dockerfile · Elixir · Go · HTML · Java · JavaScript · JSON · Jsonnet · JSX · Julia · Kotlin · Lisp · Lua · OCaml · PHP · Python · R · Ruby · Rust · Scala · Scheme · Solidity · Swift · Terraform · TSX · TypeScript · Visual Basic · XML · YAML · Generic (ERB, Jinja, etc.)

## Installation

### Quick Install (Recommended)

#### Linux / macOS

```bash
curl -fsSL https://raw.githubusercontent.com/opengrep/opengrep/main/install.sh | bash
```

Or if you've cloned the repo:

```bash
./install.sh
```

#### Windows (PowerShell)

```powershell
irm https://raw.githubusercontent.com/opengrep/opengrep/main/install.ps1 | iex
```

Or with a specific version:

```powershell
& ([scriptblock]::Create((irm https://raw.githubusercontent.com/opengrep/opengrep/main/install.ps1))) -Version v1.16.0
```

### Manual Install

Binaries are available on the [releases page](https://github.com/opengrep/opengrep/releases).

## Getting started

Create `rules/demo-rust-unwrap.yaml` with the following content:

```yml
rules:
- id: unwrapped-result
  pattern: $VAR.unwrap()
  message: "Unwrap detected - potential panic risk"
  languages: [rust]
  severity: WARNING
```

and `code/rust/main.rs` with the following content (that contains a risky unwrap):

```rust
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        return Err("Division by zero".to_string());
    }
    Ok(a / b)
}

fn main() {
    let result = divide(10, 0).unwrap(); // Risky unwrap!
    println!("Result: {}", result);
}
```

You should now have: 

``` shell
.
├── code
│   └── rust
│       └── main.rs
└── rules
    └── demo-rust-unwrap.yaml
```

Now run: 

```bash
❯ opengrep scan -f rules code/rust

┌──────────────┐
│ Opengrep CLI │
└──────────────┘


Scanning 1 file (only git-tracked) with 1 Code rule:

  CODE RULES
  Scanning 1 file.

  PROGRESS

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 100% 0:00:00


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    code/rust/main.rs
    ❯❯ rules.unwrapped-result
          Unwrap detected - potential panic risk

            9┆ let result = divide(10, 0).unwrap(); // Risky unwrap!



┌──────────────┐
│ Scan Summary │
└──────────────┘

Ran 1 rule on 1 file: 1 finding.
```

To obtain SARIF output: 

```bash
❯ opengrep scan --sarif-output=sarif.json -f rules code
  ...
❯ cat sarif.json | jq
{
  "version": "2.1.0",
  "runs": [
    {
      "invocations": [
        {
          "executionSuccessful": true,
          "toolExecutionNotifications": []
        }
      ],
      "results": [
        {
          "fingerprints": {
            "matchBasedId/v1": "a0ff5ed82149206a74ee7146b075c8cb9e79c4baf86ff4f8f1c21abea6ced504e3d33bb15a7e7dfa979230256603a379edee524cf6a5fd000bc0ab29043721d8_0"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "code/rust/main.rs",
                  "uriBaseId": "%SRCROOT%"
                },
                "region": {
                  "endColumn": 40,
                  "endLine": 9,
                  "snippet": {
                    "text": "    let result = divide(10, 0).unwrap(); // Risky unwrap!"
                  },
                  "startColumn": 18,
                  "startLine": 9
                }
              }
            }
          ],
          "message": {
            "text": "Unwrap detected - potential panic risk"
          },
          "properties": {},
          "ruleId": "rules.unwrapped-result"
        }
      ],
      "tool": {
        "driver": {
          "name": "Opengrep OSS",
          "rules": [
            {
              "defaultConfiguration": {
                "level": "warning"
              },
              "fullDescription": {
                "text": "Unwrap detected - potential panic risk"
              },
              "help": {
                "markdown": "Unwrap detected - potential panic risk",
                "text": "Unwrap detected - potential panic risk"
              },
              "id": "rules.unwrapped-result",
              "name": "rules.unwrapped-result",
              "properties": {
                "precision": "very-high",
                "tags": []
              },
              "shortDescription": {
                "text": "Opengrep Finding: rules.unwrapped-result"
              }
            }
          ],
          "semanticVersion": "1.100.0"
        }
      }
    }
  ],
  "$schema": "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
}
```

## Documentation

- [Wiki](https://github.com/opengrep/opengrep/wiki) - tutorials and language guides
- [Intrafile Tainting Tutorial](https://github.com/opengrep/opengrep/wiki/Intrafile-tainting-tutorial)
- [Higher-Order Functions Tutorial](https://github.com/opengrep/opengrep/wiki/Higher-order-functions-tutorial)
- [C# Support](https://github.com/opengrep/opengrep/wiki/Support-for-C%23) (C# 12/13/14)
- [PHP Support](https://github.com/opengrep/opengrep/wiki/Support-for-Php) (PHP 7.1-8.4)
- [Visual Basic Support](https://github.com/opengrep/opengrep/wiki/Support-for-Visual-Basic)

## Community

- [X / Twitter](https://x.com/opengrep)
- [Reddit](https://www.reddit.com/r/opengrep)
- [Manifesto](https://opengrep.dev/) - why we forked
- [Open roadmap sessions](https://lu.ma/opengrep) - join the conversation

## More

- [Contributing](CONTRIBUTING.md)
- [Build instructions for developers](INSTALL.md)
- [License (LGPL-2.1)](LICENSE)

---

_Opengrep is a fork of Semgrep v1.100.0, created by Semgrep Inc. Opengrep is not affiliated with or endorsed by Semgrep Inc._
