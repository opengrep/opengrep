<!-- reference
id: internal
kind: internal
name: Internal and debugging interfaces
summary: Interfaces meant for opengrep's own development and tests.
covers: [flag-develop, flag-dump-ast, flag-profile, flag-x-ls, flag-x-ls-long, flag-x-ignore-semgrepignore-files, env-OPENGREP_IN_TEST, env-OPENGREP_IN_DOCKER, env-OPENGREP_PROJIDX_BATCH, env-OPENGREP_FAIL_OPEN_URL, env-OPENGREP_GHA_MIN_FETCH_DEPTH, env-PYTEST_*, env-GITHUB_*, env-GH_TOKEN, env-GITLAB_CI, env-CI_*, env-CIRCLECI, env-CIRCLE_*, env-BUILDKITE, env-BUILDKITE_*, env-TRAVIS, env-TRAVIS_*, env-JENKINS_URL, env-GIT_URL, env-GIT_URL_1, env-GIT_BRANCH, env-GIT_COMMIT, env-BUILD_*, env-BITBUCKET_*, env-SYSTEM_*, key-semgrep-internal-*, key-r2c-internal-*]
-->
# Internal and debugging interfaces

<!-- BEGIN GENERATED: stamp -->
<!-- END GENERATED: stamp -->

These interfaces exist for opengrep's own development, tests, and packaging.
They are listed so the reference is complete, and they may change or disappear
in any release.

## `opengrep --core`

The opengrep binary also contains the low-level engine CLI, historically called
`semgrep-core` and later `opengrep-core`. It runs when the first argument is
`--core`, and `opengrep --core -help` lists its options. The `opengrep`
commands build on the same engine, and this reference does not document the
engine CLI. The engine CLI appends the whitespace-separated words of
`OPENGREP_CORE_EXTRA` (or `SEMGREP_CORE_EXTRA`) to its own arguments.

## `opengrep show` dump commands

`opengrep show` prints internal representations with `dump-config`,
`dump-rule`, `dump-rule-v2`, `dump-patterns-of-rule`, `dump-ast`, `dump-il`,
`dump-il-pp`, `dump-cst`, `dump-pattern`, `dump-intrafile-graph`,
`dump-interfile-graph` and `dump-taint-signatures`. `opengrep show --help`
describes their arguments.

## Flags

| Flag | Purpose |
|---|---|
| `--develop` | Enables features under development. |
| `--dump-ast` | Prints the AST of the target instead of scanning (with `-e`/`--lang`). |
| `--profile` | Collects and prints profiling information. |
| `--x-ls`, `--x-ls-long` | Lists the files a scan would consider, instead of scanning. |
| `--x-ignore-semgrepignore-files` | Ignores `.semgrepignore` files. |

## Environment variables

| Variable | Purpose |
|---|---|
| `OPENGREP_IN_TEST` | Set by opengrep's test suite. |
| `OPENGREP_IN_DOCKER` | Set in opengrep's Docker image. |
| `OPENGREP_PROJIDX_BATCH` | Batch size of the project index. Read only under this name, with no `SEMGREP_` alias. |
| `OPENGREP_FAIL_OPEN_URL` | Not used: its only mention in the sources is commented out. |
| `OPENGREP_GHA_MIN_FETCH_DEPTH` | Deprecated: minimum git fetch depth for `opengrep ci` on GitHub Actions. |
| `PYTEST_OPENGREP_LOG_*`, `PYTEST_SEMGREP_LOG_*` | Log settings for test runners that clear the environment except `PYTEST_*`. They take precedence over [`OPENGREP_LOG_LEVEL`](env/OPENGREP_LOG_LEVEL.md) and the related variables. |
| `GITHUB_*`, `GH_TOKEN`, `GITLAB_CI`, `CI_*`, `CIRCLECI`, `CIRCLE_*`, `BUILDKITE`, `BUILDKITE_*`, `TRAVIS`, `TRAVIS_*`, `JENKINS_URL`, `GIT_URL`, `GIT_URL_1`, `GIT_BRANCH`, `GIT_COMMIT`, `BUILD_*`, `BITBUCKET_*`, `SYSTEM_*` | Set by CI providers. `opengrep ci` reads them to detect the provider, the repository, the branch and the pull request. |

## Rule keys

Rule keys starting with `semgrep-internal-` or `r2c-internal-` are internal. A
rule using `r2c-internal-project-depends-on` (supply-chain rules) is rejected.
