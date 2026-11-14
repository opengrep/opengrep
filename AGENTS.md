# AGENTS.md

Instructions for coding agents working on opengrep. Read this file in full
before any change.

## What opengrep is

- Opengrep is a hard fork of Semgrep CE (forked at Semgrep 1.100.0), under
  the LGPL 2.1 licence. It is a separate product: call it opengrep in code, 
  messages and documentation. "Semgrep" appears only in literal code artefacts 
  (module paths such as `src/osemgrep/`, `semgrep_output`, `.semgrepignore`,
  `nosemgrep`) or when referring to upstream.
- The build produces one OCaml executable, `bin/opengrep`. There is no Python
  wrapper and no `cli/` directory; opengrep is not distributed on PyPI.
  `src/main/Main.ml` runs the low-level engine CLI when the first argument is
  `--core` (`opengrep --core -help` lists its options) and the opengrep CLI
  otherwise.
- Opengrep has no account, login, app token, cloud platform, metrics or
  telemetry. 
- Performance is a feature. A time or memory regression is a defect.

## Layout

| Path | Contents |
|---|---|
| `languages/<lang>/` | front ends: `tree-sitter/Parse_<lang>_tree_sitter.ml`, `menhir/`, `generic/` (conversion to the generic AST), `ast/` |
| `languages/<lang>/tree-sitter/semgrep-<lang>` | tree-sitter grammars (git submodules) |
| `languages/{cpp,go,java,javascript,ocaml,php,python}/menhir/` | Menhir grammars, maintained in this repository |
| `libs/ast_generic/` | the generic AST (`AST_generic.ml`) |
| `libs/commons/`, `libs/paths/` | shared helpers |
| `src/naming/` | name resolution on the generic AST (`Naming_AST.ml`) |
| `src/matching/` | the pattern matcher (`Generic_vs_generic.ml`) |
| `src/analyzing/`, `src/il/` | lowering to IL (`AST_to_IL.ml`), CFG |
| `src/tainting/` | the taint engine (`Dataflow_tainting.ml`, signatures) |
| `src/call_graph/` | the call graph and per-language data (`Lang_config.ml`) |
| `src/project_index/` | the project index that builds the interfile call graph |
| `src/engine/` | rule evaluation, search and taint modes, interfile dispatch |
| `src/core_cli/` | the engine CLI (`opengrep --core`) |
| `src/osemgrep/` | the opengrep CLI (`scan`, `test`, `show`, ...) |
| `src/tests/Test.ml` | the test executable, symlinked as `./test` |
| `tests/` | fixtures and snapshots |
| `docs/interfile/` | design of interfile taint analysis |
| `docs/MEMTRACE.md` | memory and allocation tracing |

## Build and test

Prerequisites: OCaml 5.5.1 with `dune` and `opam`, a C toolchain with the
PCRE and gmp libraries, `make` and `git`. First setup:

```sh
git submodule update --init --recursive
make setup
make core
```

The commands:

```sh
make core                 # builds bin/opengrep
make build-core-test      # builds the test executable ./test
./test -s <substring>     # runs the tests whose name contains <substring>
make core-test            # the full suite
make opengrep-diff        # builds a tool that compares two JSON scan outputs
```

- `./test -s` takes one literal substring: no regular expression, no list, no
  repeated `-s`. Run several selections as separate commands.
- `make core-test` builds the test executable, runs `./test -j0`, then the
  inline tests of `libs/commons`, `libs/lib_parsing`, `src/call_graph`,
  `src/engine`, `src/tainting`, `src/fixing` and the expect tests of
  `src/osemgrep`, through `scripts/run-core-test`. It is the only full check,
  and it is what CI runs (`.github/workflows/build-test-core-x86.yml`:
  `make core` then `make core-test` on pull requests to `main`).
- While iterating, run targeted selections (`./test -s` on the affected
  fixtures). Run `make core-test` once, before a commit.
- Expect test diffs in `src/osemgrep` are applied with `dune promote`.
  Snapshot output under `tests/snapshots/` is checked with `./test status`
  and accepted with `./test approve`, only after the new output has been
  read and confirmed correct.
- Never run a bare `dune build` or `dune runtest` in place of the make
  targets: the Makefile includes
  `libs/ocaml-tree-sitter-core/tree-sitter-config.mk`, without which the
  build fails on the tree-sitter headers. The one exception is
  `dune build opam/semgrep.opam` after a dependency edit in `dune-project`
  (see Documentation and dependencies).
- Never pipe build or test output (`make core | tail` reports the exit code
  of `tail`). Redirect to a file and check the command's own exit status.
- Never delete anything under `_build` or `bin` (`bin` is a symlink into
  `_build`). If a clean build is needed: `dune clean` followed by `make core`.
- After switching branches, run `git submodule update --init --recursive`.

### Worktrees

A worktree has its own submodules and its own tree-sitter configuration:

```sh
git worktree add -b <branch> <path> main
cd <path>
git submodule update --init --recursive
(cd libs/ocaml-tree-sitter-core && ./configure && ./scripts/install-tree-sitter-lib)
make core
```

## Code standards

- OCaml: annotate every function argument with its type; qualify record
  fields with the module where ambiguous; no polymorphic `=` (use
  `String.equal`, `Int.equal`, `Fpath.equal`); use libraries already in the
  opam switch; add no dependency without a maintainer's approval.
- Functional style: fold, map, recursion and pipelines over `ref`, `while`
  and mutation. Keep the style of the code being edited.
- No global mutable state: no module-level `ref` or `Hashtbl`. The engine
  runs worker domains (OCaml 5), so such state is a data race. State lives in
  a record owned by one caller and passed down, or the function is stateless.
- DRY: search `libs/commons`, `libs/paths`, `src/core` and the file itself
  before writing a helper; define a helper once, at the level its callers can
  reach, and replace the inlined copies in the files you touch.
- Small, reviewable changes; minimal churn. Fix a defect at its source, not
  at its call sites. No optional argument added to avoid updating callers, no 
  ignored error result, no exception handler wider than the failure it handles, 
  no string matching where the data is typed.
- Never hand-roll what a library in the switch already implements; use the
  library's implementation.
- For all libraries used, do so idiomatically.
- Names: the established computer science or language specification term
  (binding, scope, shadowing, precedence, dispatch, receiver, linearisation),
  never an invented one. Identifiers are plain English and say what the value
  is; `stuff`, `thing`, `tmp` are renamed when an edit touches their function.
- Language semantics are the specification. A language's scoping, binding
  and resolution rules are implemented as faithfully as possible. A language
  fact lives in `Lang_config` or in the front end, never as a guard at a use
  site. No bound, tie break, fallback or per-language special case in generic
  code without a derivation from the language's rules.
- No proxies: a property is decided from the fact itself, carried in the
  data, never from something that correlates with it (a lookup by name where
  a binding exists is a proxy).
- If you need to change a tree-sitter grammar and are not a maintainer, link 
  your submodule fork in the branch and ensure that you notify maintainers in 
  the PR description, so they can eventually update the opengrep org submodule. 
  In such cases the maintainers will regenerate the parser(s) involved.
- The generic AST should distinguish every form of a language, so patterns can
  tell them apart. Equating two forms belongs in the engine (the call graph,
  the index, the IL lowering), not in the converter. 
- Performance: prefer solutions that are asymptotically cheap on the hot path
  (matching, taint set operations, the dataflow fixpoint). No per-call
  normalisation, and no syscall (`stat`, `open`, `realpath`, `getcwd`, a
  subprocess) per match ; syscalls belong to small inputs such as the scanning 
  roots and the command line. Memory and time regressions (which can be caused
  by allocation) are investigated with memtrace (`docs/MEMTRACE.md`).
- Robustness: rely on documented exit statuses and structured data, never on
  the text of another program's error output.
- Precision and recall must not drop below main's. Run the binary built from
  the change and the binary built from main on the same inputs:
  - minimal probes: one construct each, with a negative control, for every
    construct whose analysis the change affects, in every language where it
    occurs;
  - real repositories with rule sets, comparing the two JSON outputs with
    `opengrep-diff`.
  Classify every finding reported by only one binary as a true or false
  positive by reading the code, under the language's semantics. A true
  positive of main missing from the change is a recall regression; a false
  positive of the change absent from main is a precision regression. A
  regression whose cause also exists on main is still a regression; report it
  with that cause. A false positive of main that the change removes gets a
  test.
- Do not edit the CHANGELOG.md file; only maintainers edit this file and it
  only receives edits in release PRs.

## Comments

- New comments only where an expert reader needs a mechanism explained that
  the code does not show. Written for experts, in plain English, without redundant 
  information. No comment that restates the code.
- Never delete an existing comment when editing; move it with the code it
  describes. A comment that a change makes false is deleted or reported,
  never rewritten into something vague.
- No TODO, FIXME or XXX unless if you are a maintainer. No commit SHAs,
  no issue numbers, no idioms, no speculation that the code cannot prove, no
  claims such as "sound", "complete" or "correct by construction".

## Language

Applies to comments, identifiers, test titles, error messages, commit
messages and documentation.

- Plain, direct engineering English: complete sentences, concrete nouns and 
  verbs, no metaphors, no marketing words, no qualifiers, no anthropomorphic 
  verbs ("knows", "wants", "gains").
- No em dash or en dash anywhere in the tree; use a comma, a colon or a new
  sentence.
- Banned words: "spell", "spelling", etc; the verb "name" in any form (write 
  "refers to", "carries", "returns", "prints").
- An error message states what failed and, when known, what to do; it never
  refers to an internal tool, module or function.

## Tests

- Every fix comes with a minimal fixture that fires on the fixed flow and a
  control that stays clean. A test exercises one construct, without
  unrelated wrapping.
- A failing test means the engine must be fixed, not the test. Do not rewrite,
  weaken, skip or replace a failing fixture with a form that happens to pass.
- A change that the compiler proves cannot affect runtime (a rename, removal
  of an unused binding) needs `make core`, not a test run.
- Fixture locations:
  - `tests/patterns/<lang>/`: a target `foo.<ext>` with a sibling pattern
    `foo.sgrep` of the same base name; one `.sgrep` per target.
  - `tests/rules/`: a rule `foo.yaml` next to its target `foo.<ext>`, with
    `ruleid:` and `ok:` annotations. Under `tests/rules/cross_function_tainting/`
    the harness enables intrafile taint.
  - `tests/tainting_rules/<lang>/`: taint rules and targets per language.
  - `tests/interfile/<lang>/<case>/`: a directory with `rule.yaml` and
    annotated sources, discovered automatically (`./test -s interfile`).
  - `tests/snapshots/`: expected output, updated through `./test approve`.
- A red suite is a full stop: no commit until every failure is understood and
  resolved. A green suite is not enough when a known defect remains in the
  area of the change.

## Git

- Work on a feature branch. Never commit to `main`; never push without the
  authors's permission; no destructive operation (`git reset --hard`,
  `git push --force`) without permission.
- Stage with `git add -u` and new files one by one by exact path. Never
  `git add -A`, `git add .`, a glob or a directory: the checkout may hold 
  untracked files that should not be committed.
- Commit subject: `<area>: <what the code does now>`, lowercase, for example
  `taint: a call trace is shared between taints, never copied`.
- Commit body: at most three or four lines, present tense, stating what the
  code does now, in plain English for a reader with a PhD in computer
  science. Refer to code entities by their identifiers (`File_timeout`, not
  "a file timeout"). No story of the old behaviour, no rationale for an
  obvious change, no metrics, no line numbers, no references to later commits
  or to discussions the reader cannot see, no `@` (it notifies GitHub
  accounts), no references to other repositories' issues.
- Issue numbers may appear in commit messages.
- Semi-linear history (CONTRIBUTING.md): rebase the branch on the target
  branch, never merge `main` into it; CI rejects merge commits on a pull
  request branch. Every commit must build.

## Documentation and dependencies

- Document user-facing features (commands, options, environment variables)
  in README.md when opening a pull request: purpose, usage, options.
- `opam/semgrep.opam` is generated from `dune-project` and `make core` does
  not regenerate it. After editing dependencies in `dune-project`, run:

```sh
dune build opam/semgrep.opam
```

## Final checks and agent review workflow

- Evaluate if the solution is honestly what the user approved and if the 
  standards set here were respected, and adjust accordingly.
- Prefer principled changes instead of narrow fixes, passing tests does not
  mean automatically that the solution is good.
- Before each commit, unless otherwise stated by the user, launch N (>=1)
  adversarial review agents, verify each review finding, and if fixes were 
  applied then repeat the review cycle until nothing needs fixing.

## Opening a PR 

- Pull request descriptions should state what is contributed, why (which issues 
  it closes), and what reviewers need to know. They should be high level and no 
  more than 2-3 paragraphs with a few optional bullet points as needed.
  Examples can be added to help understand any changes in the engine.
- Do not word-wrap the PR text.
