# Pull Request Rules

Use this checklist before opening a PR or marking a draft PR ready for review.

## Branch And History

- [ ] Rebase your branch on the target branch, usually `main`.
- [ ] Do not merge `main` into your PR branch.
- [ ] Keep the PR focused on one feature, bug fix, or refactor.
- [ ] Split large work into smaller PRs when reviewers cannot validate it as one change.
- [ ] Keep commits clean and informative before requesting review.
- [ ] Mention issue numbers in commit messages or the PR description when the PR closes or relates to an issue.

## PR Description

- [ ] Explain what changed.
- [ ] Explain why the change is needed.
- [ ] List the main files or areas reviewers should inspect.
- [ ] Call out behavior changes, compatibility risks, migration steps, and known limitations.
- [ ] Include exact test commands you ran and their results.
- [ ] If the PR intentionally leaves follow-up work, list it clearly.

## Tests And Verification

- [ ] Add tests for new features and bug fixes.
- [ ] Add regression tests for every bug or review concern fixed by the PR.
- [ ] Run the smallest relevant local test target before pushing.
- [ ] Run broader tests when touching shared behavior, parsing, matching, targeting, tainting, output formats, CI, or release code.
- [ ] Make sure CI is green after the latest push.
- [ ] Re-run CI after rebasing.

## Generated Code And Snapshots

- [ ] Regenerate checked-in generated files when their source files change.
- [ ] Update snapshots only when the output change is intended.
- [ ] Review generated diffs for unrelated churn before pushing.
- [ ] Document the command used to regenerate files or snapshots in the PR description.

## Submodules And External Refs

- [ ] Do not point submodules at personal or organization forks in a PR intended for `main`.
- [ ] Land required submodule changes upstream first, or clearly mark the PR as blocked.
- [ ] Pin submodules to commits available from the upstream submodule repository.
- [ ] Verify `git submodule update --init --recursive` works from a clean checkout.
- [ ] Mention related submodule PRs in the main PR description.

## Language And Parser Changes

- [ ] Preserve syntax that users may need to match, even when the generic AST has no perfect representation.
- [ ] Add parser tests for representative syntax, not only minimal happy paths.
- [ ] Add pattern tests for matching behavior users will rely on.
- [ ] Add tests for modules/imports, qualified names, declarations, expressions, literals, comments, and error handling where relevant.
- [ ] For tree-sitter parsers, follow the repository's domain-local safety pattern for parser state.
- [ ] Avoid swallowing converter exceptions without diagnostics that help reviewers debug failures.
- [ ] Mark unsupported syntax as `TODO` tests only when the limitation is explicit and acceptable for the PR.

## Matching, Taint, Targeting, And Output Changes

- [ ] Add tests that cover false positives and false negatives.
- [ ] Include diff-scan tests when changing target discovery or ignore behavior.
- [ ] Include permission, size-limit, minified-file, dirty-file, and explicit-file cases when changing targeting.
- [ ] Include output snapshots for SARIF, JSON, or text output changes.
- [ ] Verify changes do not silently alter unrelated languages or output formats.

## Compatibility And Release Risk

- [ ] Keep public interfaces backward compatible unless the breaking change is intentional and documented.
- [ ] Update docs, help text, snapshots, and changelog entries when user-facing behavior changes.
- [ ] Avoid unrelated formatting, version bumps, workflow changes, or dependency churn.
- [ ] Call out performance-sensitive changes and include before/after measurements when performance is part of the claim.

## Before Requesting Review

- [ ] The branch is current with the target branch.
- [ ] CI is green on the latest commit.
- [ ] The PR is not blocked by unpublished downstream changes.
- [ ] Reviewers can reproduce your local verification from the PR description.
- [ ] Known limitations are documented instead of discovered by reviewers.
- [ ] The PR is small enough to review effectively, or it has a clear staged merge plan.
