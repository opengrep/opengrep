#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$ROOT/testapp1"
APP_FILE="$APP_DIR/app.py"
RULE_FILE="$APP_DIR/interfile_rule.yaml"
BINARY="$ROOT/bin/opengrep"
SWITCH_NAME="${OPENGREP_OPAM_SWITCH:-opengrep-5.3.0}"

cd "$ROOT"

if [ ! -f "$ROOT/libs/ocaml-tree-sitter-core/configure" ]; then
  echo "Initializing submodules"
  git submodule update --init --recursive
fi

need_brew_install=0
for pkg in pkgconf libev pcre; do
  if ! brew list "$pkg" >/dev/null 2>&1; then
    need_brew_install=1
    break
  fi
done

if [ "$need_brew_install" -eq 1 ]; then
  echo "Installing missing Homebrew packages"
  brew install pkgconf libev pcre
fi

if ! opam switch list --short | grep -Fxq "$SWITCH_NAME"; then
  echo "Creating OPAM switch $SWITCH_NAME"
  opam switch create "$SWITCH_NAME" ocaml-base-compiler.5.3.0 -y
fi

need_opam_install=0
for pkg in cohttp-lwt-unix ppx_deriving.show; do
  if ! opam exec --switch "$SWITCH_NAME" -- ocamlfind query "$pkg" >/dev/null 2>&1; then
    need_opam_install=1
    break
  fi
done

if [ "$need_opam_install" -eq 1 ]; then
  echo "Installing repository OCaml dependencies in $SWITCH_NAME"
  OPAMSWITCH="$SWITCH_NAME" make install-deps
fi

echo "Building OpenGrep in switch $SWITCH_NAME"
OPAMSWITCH="$SWITCH_NAME" opam exec --switch "$SWITCH_NAME" -- make minimal-build

if [ ! -x "$BINARY" ]; then
  echo "Expected binary at $BINARY" >&2
  exit 1
fi

single_json="$(mktemp)"
multi_json="$(mktemp)"
trap 'rm -f "$single_json" "$multi_json"' EXIT

echo "Scanning only app.py; expected findings: 0"
"$BINARY" scan --no-git-ignore --json --config "$RULE_FILE" "$APP_FILE" > "$single_json"

echo "Scanning the whole testapp1 directory; expected findings: 1"
"$BINARY" scan --no-git-ignore --json --config "$RULE_FILE" "$APP_DIR" > "$multi_json"

python3 - "$single_json" "$multi_json" "$APP_FILE" <<'PY'
import json
import os
import sys

single_json, multi_json, app_file = sys.argv[1:]

with open(single_json, "r", encoding="utf-8") as fh:
    single = json.load(fh)
with open(multi_json, "r", encoding="utf-8") as fh:
    multi = json.load(fh)

single_results = single.get("results", [])
multi_results = multi.get("results", [])

if len(single_results) != 0:
    raise SystemExit(
        f"Expected 0 findings when scanning only {app_file}, got {len(single_results)}"
    )

if len(multi_results) != 1:
    raise SystemExit(
        f"Expected 1 finding when scanning {os.path.dirname(app_file)}, got {len(multi_results)}"
    )

result = multi_results[0]
if not result["path"].endswith("testapp1/app.py"):
    raise SystemExit(
        f"Expected finding on testapp1/app.py, got {result['path']}"
    )

if result["check_id"] != "testapp1.python-interfile-taint":
    raise SystemExit(
        f"Unexpected rule id {result['check_id']}"
    )

print("Verified:")
print("  single-file scan findings: 0")
print("  multi-file scan findings: 1")
print(f"  finding path: {result['path']}")
print(f"  check id: {result['check_id']}")
PY

echo
echo "Rendered scan output:"
"$BINARY" scan --no-git-ignore --config "$RULE_FILE" "$APP_DIR"
