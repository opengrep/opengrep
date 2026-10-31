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

### Welcome to Opengrep, the most advanced open source SAST engine

Let's make secure software development a shared standard. Opengrep provides every developer and organisation with open and advanced static code analysis.

Opengrep is backed by a consortium of AppSec organisations, including: [Aikido](https://www.aikido.dev/), [Amplify](https://amplify.security/), [Endor Labs](https://www.endorlabs.com/), [Kodem](https://www.kodemsecurity.com/), and [Orca Security](https://orca.security/). For more information, go to [opengrep.dev](https://opengrep.dev/).

# Overview

Opengrep finds code by its structure and meaning, not just its text. Rules look like the code they match, so they are quick to write and read, and they run across large codebases in seconds. Use them to find and fix security vulnerabilities before they ship.

**Analysis**
- **Semantic pattern matching** - metavariables and ellipses let a single pattern match code written in many different ways
- **Taint analysis across functions** (`--taint-intrafile`) - follows untrusted data through constructors, fields, method calls, higher-order functions and collection methods such as `map`, `filter` and `reduce` (see the [Intrafile Tainting](https://github.com/opengrep/opengrep/wiki/Intrafile-tainting-tutorial) and [Higher-Order Functions](https://github.com/opengrep/opengrep/wiki/Higher-order-functions-tutorial) tutorials)
- **Taint analysis across files** (`--taint-interfile`) - builds a call graph of the whole project and follows data from a source in one file to a sink in another

**Languages**
- 30+ languages, including Visual Basic and Crystal, which no other Semgrep-compatible engine supports
- Cross-function taint analysis for 20+ languages
- Up-to-date syntax, such as PHP 8.5 and C# 14

Apex · Bash · C · C++ · C# · Clojure · Crystal · Dart · Dockerfile · Elixir · Go · HTML · Java · JavaScript · JSON · Jsonnet · JSX · Julia · Kotlin · Lisp · Lua · OCaml · PHP · Python · R · Ruby · Rust · Scala · Scheme · Solidity · Swift · Terraform · TSX · TypeScript · Visual Basic · XML · YAML · Generic (ERB, Jinja, etc.)

**Rules and output**
- **Your Semgrep rules work as they are** - bring your existing rules and rulesets
- **Standard outputs** - text, JSON and SARIF, with metavariable values, fingerprints and, on request, the enclosing class or function of each match
- **Fine-grained control** - per-rule timeouts, timeouts that scale with file size, per-file match limits and custom ignore annotations

**Distribution**
- **Self-contained binaries** for Linux, macOS and Windows, with no Python or other runtime to install
- **Multicore scanning** with OCaml 5, on every platform
- **Signed releases** with Cosign

**Open source**
- **Open governance** - contributions are accepted on merit
- **LGPL 2.1** - open source for good

See [OPENGREP.md](OPENGREP.md) for a detailed list of features and fixes.

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

On Windows, the package holds `opengrep.exe` and the DLLs it needs; keep them
together in the directory you install them to.

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
1 file · 1 rule

code/rust/main.rs

  warn  rules.unwrapped-result
  Unwrap detected - potential panic risk

    9 │ let result = divide(10, 0).unwrap(); // Risky unwrap!

1 finding in 1 file
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

## Log file

Set `OPENGREP_LOG_FILE` (or `SEMGREP_LOG_FILE`) to a path to get a copy of
what Opengrep prints on stderr, at the same level: warnings and errors by
default, more with `--verbose` or `--debug`. The file is truncated on each
run and its directory is created. Nothing is written when the variable is
not set. A path that cannot be written produces a warning and the run
continues without the copy.

## Documentation

- [Wiki](https://github.com/opengrep/opengrep/wiki) - tutorials and language guides
- [Intrafile Tainting Tutorial](https://github.com/opengrep/opengrep/wiki/Intrafile-tainting-tutorial)
- [Higher-Order Functions Tutorial](https://github.com/opengrep/opengrep/wiki/Higher-order-functions-tutorial)
- [C# Support](https://github.com/opengrep/opengrep/wiki/Support-for-C%23) (C# 12/13/14)
- [PHP Support](https://github.com/opengrep/opengrep/wiki/Support-for-Php) (PHP 7.1-8.5)
- [Visual Basic Support](https://github.com/opengrep/opengrep/wiki/Support-for-Visual-Basic)

## Community

- [X / Twitter](https://x.com/opengrep)
- [Reddit](https://www.reddit.com/r/opengrep)
- [opengrep.dev](https://opengrep.dev/) - more information
- [Open roadmap sessions](https://lu.ma/opengrep) - join the conversation

## More

- [Contributing](CONTRIBUTING.md)
- [Build instructions for developers](INSTALL.md)
- [License (LGPL-2.1)](LICENSE)

---

_Opengrep is a fork of Semgrep v1.100.0, created by Semgrep Inc. Opengrep is not affiliated with or endorsed by Semgrep Inc._
