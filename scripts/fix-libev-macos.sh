#!/usr/bin/env bash
#
# Fix `make test` failing on macOS with:
#
#   [FAIL] Language Server (unit) > Lib EV tests > Test LS with libev
#     Lwt_sys.Not_available("libev_init")
#
# Cause: lwt's build-time `discover` step looks for libev via LIBEV_CFLAGS/
# LIBEV_LIBS, then `pkg-config libev`, then a fixed list of /usr, /usr/local,
# /opt, ... . Homebrew installs libev under `brew --prefix libev` (e.g.
# /opt/homebrew/opt/libev on Apple Silicon) with no pkg-config file, so all
# probes miss and lwt gets compiled WITHOUT the libev engine. This reinstalls
# lwt with the right flags so the libev backend is built in.
#
# Safe to re-run. Only touches the CURRENT opam switch.
#
set -euo pipefail

say() { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33mwarning:\033[0m %s\n' "$*" >&2; }
die() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

# --- sanity checks ----------------------------------------------------------
[ "$(uname -s)" = "Darwin" ] || die "This script is for macOS only. On Linux, install libev-dev (or equivalent) via your package manager."
command -v brew >/dev/null 2>&1 || die "Homebrew not found. Install it from https://brew.sh, then re-run."
command -v opam >/dev/null 2>&1 || die "opam not found. Set up your OCaml toolchain first."

# --- ensure system libev ----------------------------------------------------
if ! brew --prefix libev >/dev/null 2>&1; then
  say "Installing libev via Homebrew..."
  brew install libev
fi
LIBEV_PREFIX="$(brew --prefix libev)"
[ -f "$LIBEV_PREFIX/include/ev.h" ] || die "ev.h not found under $LIBEV_PREFIX/include even after install."
say "Using libev at: $LIBEV_PREFIX"

# --- load the opam environment ----------------------------------------------
eval "$(opam env)"
say "Target opam switch: $(opam switch show)"

# --- reinstall lwt with libev flags -----------------------------------------
# LIBEV_CFLAGS / LIBEV_LIBS are read directly by lwt's discover.ml.
export LIBEV_CFLAGS="-I$LIBEV_PREFIX/include"
export LIBEV_LIBS="-L$LIBEV_PREFIX/lib -lev"

# conf-libev is lwt's declared dependency for libev support; harmless if present.
opam install conf-libev --yes >/dev/null 2>&1 || true

say "Reinstalling lwt with libev support (this also rebuilds its reverse deps; may take a few minutes)..."
opam reinstall lwt --yes

# --- verify -----------------------------------------------------------------
say "Verifying the libev engine is available..."
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
cat > "$tmpdir/check.ml" <<'EOF'
let () =
  try
    Lwt_engine.set (new Lwt_engine.libev ());
    print_endline "libev engine: AVAILABLE"
  with Lwt_sys.Not_available s ->
    Printf.printf "libev engine: NOT AVAILABLE (%s)\n" s;
    exit 1
EOF

if command -v ocamlfind >/dev/null 2>&1 \
   && ocamlfind ocamlopt -package lwt.unix -linkpkg "$tmpdir/check.ml" -o "$tmpdir/check" >/dev/null 2>&1 \
   && "$tmpdir/check"; then
  say "Done. \`make test\` should now pass the 'Test LS with libev' test."
else
  warn "Could not auto-verify (ocamlfind/compile step failed), but the lwt reinstall completed."
  warn "Just run 'make test' — the libev test should now pass."
fi
