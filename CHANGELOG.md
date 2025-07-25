# Changelog

## [1.8.2](https://github.com/opengrep/opengrep/releases/tag/v1.8.2) - 25-07-2025

### Bug fixes

* Fix bug in propagating taints through composites and dictionaries by @corneliuhoffman in #367

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.8.1...v1.8.2


## [1.8.1](https://github.com/opengrep/opengrep/releases/tag/v1.8.1) - 24-07-2025

### Bug fixes

* Fix thread-safety bug in Apex parser by @maciejpirog in #365
* Improve deduplication and sorting of results to remove nondetermism by @maciejpirog in #364

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.8.0...v1.8.1


## [1.8.0](https://github.com/opengrep/opengrep/releases/tag/v1.8.0) - 23-07-2025

### New features

* Add new rule option `max-match-per-file` by @dimitris-m in #359 and #357
* Add new rule option `taint-fixpoint-timeout` by @dimitris-m in #360
* Release opengrep-core by @spotdemo4 in #349

### Bug fixes

* For variables, calculate the range in the `extra` section using the original token by @maciejpirog in #356

### Tech debt

* Eliminated references in `AST_to_IL` by @corneliuhoffman in #351
* Eliminated more references by @corneliuhoffman in #358

### New Contributors
* @spotdemo4 made their first contribution in #349

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.7.0...v1.8.0


## [1.7.0](https://github.com/opengrep/opengrep/releases/tag/v1.7.0) - 14-07-2025

### New features

* Add support for Apex by @maciejpirog in #337

### Improvements

* Kotlin: enable taint tracking through the Elvis operator (`?:`) by @fxbru in #334
* Elixir: add `defp` private functions by @dimitris-m in #346

### Bug fixes

* Fix: ensure we fallback to python for `ci` by @dimitris-m in #345
* Remove metrics in the bench command by @corneliuhoffman in #352
* Cleanup metrics and settings related code on the Python CLI by @dimitris-m in #342

### Tech debt

* Refactor: remove duplicate `metavar_string_of_any` by @dimitris-m in #344

### New Contributors
* @fxbru made their first contribution in #334

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.6.0...v1.7.0


## [1.6.0](https://github.com/opengrep/opengrep/releases/tag/v1.6.0) - 10-07-2025

### Removed features

* Remove semgrep-specific functionality including the `--metrics` parameter by @dimitris-m in #329 and #336

### Improvements

* Ensure that python has the expected version in the manylinux binary by @dimitris-m in #322
* Make tests invariant to version by @dimitris-m in #325
* Align --profile table output by @dimitris-m in #327

### Bug fixes

* Fix: install script flow for cosign by @dimitris-m in #321
* Improve Ruby tainting by @corneliuhoffman in #324
* Fix baseline scan when using --experimental by @dimitris-m in #326
* Fix: pin memprof-limits by @dimitris-m in #328
* fix: detect musl in install script by @securityPirate in #330
* Ensure fingerprint is unique by @dimitris-m in #338

### New Contributors
* @securityPirate made their first contribution in #330

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.5.0...v1.6.0


## [1.5.0](https://github.com/opengrep/opengrep/releases/tag/v1.5.0) - 03-07-2025

### New features

* Binary install script for Mac OSX and Linux by @Gable-github in #294 with improvements by @dimitris-m in #309
* Optionally expand metavariables in output metadata using `--inline-metavariables` by @corneliuhoffman in #310

### Improvements

* Build self-contained binaries using Nuitka by @dimitris-m in #311
* Add Cosign signing of binaries by @corneliuhoffman in #315
* Associate Containerfiles with the dockerfile language by @chrisnovakovic in #314
* Add match and enum in the primary PHP parser by @corneliuhoffman in #306

### Bug fixes

* Fix a segfault: replace pcre with pcre2 in Eval_generic by @dimitris-m in #308
* Create _opam in container when no GHA cache hit by @dimitris-m in #318
* Windows: fix settings.yml permission issue by @dimitris-m in #319

### Tech debt

* Remove opentelemetry and curl dependencies by @dimitris-m in #317

### New Contributors
* @Gable-github made their first contribution in #294
* @chrisnovakovic made their first contribution in #314

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.4.2...v1.5.0


## [1.4.2](https://github.com/opengrep/opengrep/releases/tag/v1.4.2) - 23-06-2025

### Bug fixes

* Fix #291: cwd can begin with lowecase drive letter on windows by @dimitris-m in #300

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.4.1...v1.4.2


## [1.4.1](https://github.com/opengrep/opengrep/releases/tag/v1.4.1) - 16-06-2025

### Bug fixes

* Fix #295: PHP interpolated strings parsed as normal strings by @corneliuhoffman in #296

### New Contributors

* @corneliuhoffman made their first contribution in #296

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.4.0...v1.4.1


## [1.4.0](https://github.com/opengrep/opengrep/releases/tag/v1.4.0) - 09-06-2025

### New features

* Add new `--semgrepignore-filename` flag by @tom-paz in #288

### Improvements

* Static linking for Mac OSX by @dimitris-m in #279

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.3.1...v1.4.0


## [1.3.1](https://github.com/opengrep/opengrep/releases/tag/v1.3.1) - 03-06-2025

### Bug fixes

* Adjust paths on windows: ensure that tagets in WSL can be scanned by @dimitris-m in #280
* Bump alpine image for aarch64 build by @dimitris-m in #281

### Improvements

* Reorder fields in classes for name resolution and constant propagation by @maciejpirog in #277

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.3.0...v1.3.1


## [1.3.0](https://github.com/opengrep/opengrep/releases/tag/v1.3.0) - 27-05-2025

### New features

* New `--force-exclude` flag: apply `--exclude` on file targets by @dimitris-m in #270
* New `--incremental-output-postprocess` flag: enable post-processing (autofix, nosem) for incremental output by @dimitris-m in #274

### Bug fixes

* Fix autofix in javascript template strings by @dimitris-m in #255
* Fix: --opengrep-ignore-pattern declared twice in python cli by @maciejpirog in #260
* Fix opengrep ignore pattern by @dimitris-m in #265
* Bug: name resolution in blocks by @maciejpirog in #268
* Adjust ranges of parenthesized expressions in Java by @maciejpirog in #258
* Fix ranges of parenthesized expressions in C# by @maciejpirog in #271
* Fix parenthesized expressions in Rust by @maciejpirog in #273
* Fix ranges of of parenthesized expressions in Kotlin by @maciejpirog in #275

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.2.2...v1.3.0


## [1.2.2](https://github.com/opengrep/opengrep/releases/tag/v1.2.2) - 12-05-2025

### Bug fixes

* Fix #112: combine consecutive strings in templates by @dimitris-m in #245
* Fix #249: synchronised incremental outputs by @dimitris-m in #251

### Improvements

* Pull performance fixes from semgrep by @maciejpirog in #248
* Add script to compare running time of two executables by @maciejpirog in #247
* Remove search for proprietary binary by @dimitris-m in #252
* Adapt version bump script by @dimitris-m in #253

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.2.1...v1.2.2


## [1.2.1](https://github.com/opengrep/opengrep/releases/tag/v1.2.1) - 02-05-2025

### Bug fixes

* Fix #241: missing `opengrep_ignore_pattern` in CI command by @dimitris-m in #242

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.2.0...v1.2.1


## [1.2.0](https://github.com/opengrep/opengrep/releases/tag/v1.2.0) - 01-05-2025

### New features

* Allow multiple targets in the test command by @maciejpirog in #238
* Add custom ignore pattern support for code comments by @tom-paz in #216 with improvements by @maciejpirog in #232: this adds a new command-line flag `--opengrep-ignore-pattern=VAL` that lets users specify a custom ignore pattern that will override the default ones

### Improvements

* Update .semgrepignore with latest from Semgrep by @dimitris-m in #225
* Adapt perf benchmarks in #223 and add new script to compare performance in #226 and #234 by @maciejpirog 

### Bug fixes

* Fix for issue #92: relax C# parser which previously led to some code not being scanned by @dimitris-m in #231 and #239
* Windows: add two missing DLLs by @dimitris-m in #236
* Show more enclosing context information when focusing on a metavar by @maciejpirog in #233

## New Contributors

* @tom-paz from Kodem Security made his first contribution in https://github.com/opengrep/opengrep/pull/216

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.5...v1.2.0


## [1.1.5](https://github.com/opengrep/opengrep/releases/tag/v1.1.5) - 18-04-2025

### Improvements

* Performance improvements by @maciejpirog and @dimitris-m in #221
* Add workflows for Intel Mac by @dimitris-m in #219

### Bug fixes

* Ensure that `min-version:` in rules is respected by @dimitris-m in #220

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.4...v1.1.5


## [1.1.4](https://github.com/opengrep/opengrep/releases/tag/v1.1.4) - 14-04-2025

### Improvements

* PHP: Add arrow functions to the menhir parser by @maciejpirog in #205

### Bug fixes

* Fix logging mutex by @dimitris-m and @maciejpirog in #208

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.3...v1.1.4


## [1.1.3](https://github.com/opengrep/opengrep/releases/tag/v1.1.3) - 10-04-2025

### Improvements

* Fix string templates in Kotlin by @maciejpirog in #191
* Add union types to PHP menhir parser by @maciejpirog in #201

### Bug fixes

* Fix: .semgrepignore not working on windows when path is not relative by @dimitris-m in #194

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.2...v1.1.3


## [1.1.2](https://github.com/opengrep/opengrep/releases/tag/v1.1.2) - 02-04-2025

### Improvements

* Fix string literals in parser for C# by @maciejpirog in #186

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.1...v1.1.2


## [1.1.1](https://github.com/opengrep/opengrep/releases/tag/v1.1.1) - 31-03-2025

### Bug fixes

* Elixir: allow pairs to be ellipsis by @dimitris-m in #181; now patterns such as `%{..., some_item: $V, ...}` work as expected.

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.1.0...v1.1.1


## [1.1.0](https://github.com/opengrep/opengrep/releases/tag/v1.1.0) - 31-03-2025

### New features

* Add support for reporting enclosure of matches by @maciejpirog in #169 and #178
* Resurrection of Elixir by @mbacarella and @dimitris-m in #150

### Details

There's now a new flag `--output-enclosing-context` that can be added to the `scan` command, adding information about the surrounding context of the matched fragments of code, such as the innermost function and/or class in which the match occurs.

This is only available for json output, so the `--json`flag must also be passed, and it's an experimental feature so it also requires `--experimental`.

Elixir support has been restored and will continue to be improved.

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.0.2...v1.1.0


## [1.0.2](https://github.com/opengrep/opengrep/releases/tag/v1.0.2) - 24-03-2025

### Improvements

* Produce aarch64 linux binaries by @dimitris-m in #171
* Use opengrep's fork of semgrep-interfaces by @maciejpirog in #168

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.0.1...v1.0.2


## [1.0.1](https://github.com/opengrep/opengrep/releases/tag/v1.0.1) - 19-03-2025

### Bug fixes

- Fix tree-sitter parsers for lisp, clojure and terraform in #166
- Pass jobs parameter to scan with --test in #164

### Improvements

- Pin Github actions to commit SHA in #162 and #163

### Contributors

@dimitris-m

**Full Changelog**: https://github.com/opengrep/opengrep/compare/v1.0.0...v1.0.1


## [1.0.0](https://github.com/opengrep/opengrep/releases/tag/v1.0.0) - 18-03-2025

### Highlights

- Windows support is now in beta, without any restrictions and with full parallelism enabled.
- Self-contained binaries for x86 Linux, arm64 Mac and x86 Windows.
- SARIF output has been re-enabled.
- Fingerprint and metavars fields are exposed again.

### Improvements

- Transitioned to OCaml 5.3.0, making use of the new multicore features.
- Timeouts and memory limits have been re-implemented and now they also work on Windows.
- Reduced memory footprint thanks to several optimisations.

### Contributors

@dimitris-m @mbacarella @madelinelawren @pritchyspritch @nir-valtman @maciejpirog @jesse-merhi @hansott @Kirill89 @HenriqueOCabral @willem-delbare

