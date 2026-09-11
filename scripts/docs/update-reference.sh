#!/usr/bin/env bash
# Bring docs/reference up to date, or fill in entries that are still missing.
#
#   update-reference.sh              update the reference for the built opengrep
#   update-reference.sh --fill [N]   document the next batch of missing entries
#   update-reference.sh --dry-run    print what would be sent to Claude Code
#
# The reference is its own store. This script only collects what changed since
# the reference was written, hands that to Claude Code, and checks the result.
# The examples run against bin/opengrep, so the binary must be current.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

BIN=bin/opengrep
REF=(python3 scripts/docs/reference.py)
SKILL=.claude/skills/opengrep-reference/SKILL.md

# The sources whose changes can reach the documented behaviour.
WATCHLIST=(
  src/osemgrep/cli/CLI.ml
  src/osemgrep/cli_scan/Scan_CLI.ml
  src/osemgrep/cli_ci/Ci_CLI.ml
  src/osemgrep/cli_test/Test_CLI.ml
  src/osemgrep/cli_show/Show_CLI.ml
  src/osemgrep/cli_validate/Validate_CLI.ml
  src/osemgrep/cli_lsp/Lsp_CLI.ml
  src/osemgrep/cli_install_ci/Install_ci_CLI.ml
  src/osemgrep/core/CLI_common.ml
  src/osemgrep/configuring/Semgrep_envvars.ml
  libs/commons/Cmdliner_.ml
  libs/commons/Opengrep_env.ml
  src/core/Log_semgrep.ml
  src/core/Version.ml
  src/parsing/Parse_rule.ml
  src/parsing/Parse_rule_formula.ml
  interfaces/Rule_options.atd
)

mode=update
batch=""
dry_run=false
while [ $# -gt 0 ]; do
  case "$1" in
    --fill)
      mode=fill
      shift
      if [[ ${1:-} =~ ^[0-9]+$ ]]; then batch=$1; shift; fi
      ;;
    --dry-run) dry_run=true; shift ;;
    -h | --help) sed -n '2,9p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

[ -x "$BIN" ] || { echo "$BIN not found: run make first" >&2; exit 1; }

binary_version=$("$BIN" --version)
source_version=$(sed -n 's/^let version = "\(.*\)"$/\1/p' src/core/Version.ml)
if [ "$binary_version" != "$source_version" ]; then
  echo "bin/opengrep is $binary_version but src/core/Version.ml says" \
    "$source_version: run make so the examples run against this release" >&2
  exit 1
fi

read -r documented_version documented_commit < <("${REF[@]}" stamp)

inputs=$(mktemp)
trap 'rm -f "$inputs"' EXIT
{
  echo "opengrep is $binary_version; the reference is written for" \
    "$documented_version at commit $documented_commit."
  echo
  echo "=== changes in the watched sources since $documented_commit ==="
  # against the working tree, so changes that are not committed yet count too
  git diff --stat "$documented_commit" -- "${WATCHLIST[@]}" 2>&1 || true
  echo
  echo "=== what the checker reports now ==="
  if [ "$mode" = fill ]; then
    "${REF[@]}" check 2>&1 || true
  else
    # the coverage warnings are the worklist of --fill, not of an update
    "${REF[@]}" check 2>&1 | grep -v '^warning: undocumented:' || true
  fi
} >"$inputs"

if [ "$mode" = fill ]; then
  task="Follow the 'Filling in entries' procedure of the skill. Document ${batch:-one batch of} entries that belong together, taken from the undocumented lists below."
else
  task="Follow the 'Updating for a new release' procedure of the skill. Change only the entries that the differences below actually affect."
fi

prompt="Read $SKILL and follow it. $task

Every claim needs an example that the checker runs; never copy --help text
without checking it against the sources and the real binary.

The pages are user documentation and must read like it: their text never
mentions the checker, no-check, the scripts, BUG.md or how examples are
verified. The reason an example is not run goes in an HTML comment above its
label; a requirement readers need to know is said in plain prose.

The state of the reference right now:

$(cat "$inputs")

Change only docs/reference, BUG.md and, if the tooling itself needs it,
scripts/docs/reference.py. When you are done, run
'python3 scripts/docs/reference.py index' and then 'check', and report the
entries you wrote, what the checker says, and any bugs you found."

if $dry_run; then
  printf '%s\n' "$prompt"
  exit 0
fi

claude -p "$prompt" \
  --permission-mode acceptEdits \
  --allowedTools "${ALLOWED_TOOLS:-Read Edit Write Grep Glob Bash}"

if [ "$mode" = update ]; then
  "${REF[@]}" index --stamp
else
  "${REF[@]}" index
fi

status=0
"${REF[@]}" check || status=$?
echo
echo "=== what changed ==="
git diff --stat -- docs/reference BUG.md scripts/docs
exit $status
