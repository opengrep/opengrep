# Since the Fork

**Opengrep** is a fork of Semgrep version 1.100.0. Since the fork, it has introduced a number of new features and bug fixes. This document outlines the most important changes and improvements.

## Summary

### Main New Features

- Switched to **OCaml 5.3.0** with large-scale refactoring to support multicore execution.

- Added support for **Windows**.

- Added support for languages that are not available in Semgrep CE: **Apex** and **Elixir**.

### Features

- **Self-contained binaries** for multiple architectures, built using **Nuitka** for fast and self-contained executables.

- **Install script** for macOS and Linux.

- **Release binaries signed** with Cosign to ensure authenticity.

- **Metavariable values and fingerprints** are now included in both JSON and SARIF outputs.

- Added support for reporting the **enclosing context** of a match (e.g., class, function, module) in the JSON output. Use the flag: `--output-enclosing-context`

- **Dynamic timeouts** that scale with file size. Enabled via `--dynamic-timeout`, but can also be controlled per rule using `dynamic_timeout: true` together with `--allow-rule-timeout-control`. The behaviour can be finetuned with `--dynamic-timeout-unit-kb` (rule option: `dynamic_timeout_unit_kb`) and `--dynamic-timeout-max-multiplier` (rule option `dynamic_timeout_max_multiplier`); see the CLI man page for details.

- Added a per-rule **limit on the number of reported matches**. Use the rule option: `max-match-per-file`.

- Taint analysis now supports **per-rule timeout configuration**. Use the rule option: `taint-fixpoint-timeout`. (This should be mostly be reserved for cases where results are not very stable on consecutive runs, and should remain relatively low, for example between 0.2 and 2 seconds.)

- **Postprocessing** (autofix and `nosem` annotations) now works with **incremental output**. Use the flag: `--incremental-output-postprocess.`

- Added option to **inline metavariable values** in the `metadata` section of the JSON output. Use the flag: `--inline-metavariables`.

- Support for **custom ignore annotations** (instead of default `nosem` / `nosemgrep`). Use the option: `--opengrep-ignore-pattern=<VAL>.`

- Added a CLI option for specifying a custom **ignore file name**: `--semgrepignore-filename=<VAL>.`

- Improved control over **file inclusion/exclusion**. Use `--force-exclude` to apply `--include` / `--exclude` rules even on explicitly passed file targets instead of just on directories.

- The `test` command now accepts **multiple target files**.

- Multiple **performance** improvements.

### Important bug fixes and language feature support

- **Taint tracking**: Fix bug in propagating taints through composites and dictionaries [#367](https://github.com/opengrep/opengrep/pull/367)

- **Matching**: Fix wrong ranges reported for a number of languages [#272](https://github.com/opengrep/opengrep/issues/272)

- **C/C++**: Fix failure in to-ast-generic [#385](https://github.com/opengrep/opengrep/pull/385)

- **C/C++**: Make C/C++ parser more lenient when dealing with preprocessor directives [#393](https://github.com/opengrep/opengrep/pull/393)

- **C#**: Fix string literals in the parser [#186](https://github.com/opengrep/opengrep/pull/186)

- **Kotlin**: Fix string templates [#191](https://github.com/opengrep/opengrep/pull/191)

- **Kotlin**: Enable taint tracking through the Elvis operator (`?:`) [#334](https://github.com/opengrep/opengrep/pull/334)

- **Kotlin**: Enable taint tracking through scope functions (let, also, use, takeIf, takeUnless) [#332](https://github.com/opengrep/opengrep/pull/332)

- **PHP**: Add union types to PHP menhir parser [#201](https://github.com/opengrep/opengrep/pull/201)

- **PHP**: Add arrow functions to the menhir parser [#205](https://github.com/opengrep/opengrep/pull/205)

- **PHP**: Interpolated strings parsed as normal strings [#296](https://github.com/opengrep/opengrep/pull/296)

- **PHP**: Add `match` and `enum` in the primary parser [#306](https://github.com/opengrep/opengrep/pull/306)

- **Ruby**: Improve Ruby tainting [#324](https://github.com/opengrep/opengrep/pull/324)

- **Scala**: Support metavariabless as elements in interpolated strings [#403](https://github.com/opengrep/opengrep/pull/403)

- **Typescript**: Fix bug related to lambdas [#378](https://github.com/opengrep/opengrep/pull/378)

- **Typescript**: Fix naming in the presence of typed patterns [#395](https://github.com/opengrep/opengrep/pull/395)

## Acknowledgments

We'd like to thank all external contributors and our industry partners for their invaluable support.
