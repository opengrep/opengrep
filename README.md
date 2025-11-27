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

_Opengrep is a fork of Semgrep, created by Semgrep Inc. Opengrep is not affiliated with or endorsed by Semgrep Inc._

Let's make secure software development a shared standard. Opengrep provides every developer and organization with open and advanced static code analysis. 

Opengrep is initiated by a collective of AppSec organizations, including: Aikido.dev, Arnica, Amplify, Endor, Jit, Kodem, Mobb, and Orca Security. To join as a sponsor or contributor, open an [issue](https://github.com/opengrep/opengrep/issues). To learn more, read the manifesto at [opengrep.dev](https://opengrep.dev/). We aim to make SAST widely accessible, advance the engine with new impactful features, and ensure it remains open and vendor-neutral for the long-term.

Opengrep is open to any individual or organization to leverage and contribute, [join the open roadmap sessions](https://lu.ma/opengrep).

# Opengrep: Fast and Powerful Code Pattern Search

Opengrep is an ultra-fast static analysis tool for searching code patterns with the power of semantic grep. Analyze large code bases at the speed of thought with intuitive pattern matching and customizable rules. Find and fix security vulnerabilities, fast – ship more secure code.

Opengrep supports 30+ languages, including:

Apex · Bash · C · C++ · C# · Clojure · Dart · Dockerfile · Elixir · Go · HTML · Java · JavaScript · JSON · Jsonnet · JSX · Julia · Kotlin · Lisp · Lua · OCaml · PHP · Python · R · Ruby · Rust · Scala · Scheme · Solidity · Swift · Terraform · TSX · TypeScript · Visual Basic · XML · YAML · Generic (ERB, Jinja, etc.)

## Installation

You can install Opengrep using our official install script.

### Quick Install (Recommended)

```bash
curl -fsSL https://raw.githubusercontent.com/opengrep/opengrep/main/install.sh | bash
```

If you've cloned the repo and `install.sh` is in the root directory, you can run:

```bash
./install.sh
```

which will install the latest version of Opengrep.

You can also install manually:
* Binaries available in the [release page](https://github.com/opengrep/opengrep/releases).

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

## More

- [Contributing](CONTRIBUTING.md)
- [Build instructions for developers](INSTALL.md)
- [License (LGPL-2.1)](LICENSE)
