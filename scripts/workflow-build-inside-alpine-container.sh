#!/usr/bin/env sh

set -ex

export OPAMROOT=/workspace/_opam
export DUNE_CACHE_ROOT=/workspace/_dune
export OPAMCONFIRMLEVEL=unsafe-yes
export OPAMCOLOR=always

OPAM_VERSION=${OPAM_VERSION:-"2.5.1"}
OCAML_VERSION=${OCAML_VERSION:-"5.3.0"}
SHOULD_INIT_OPAM=${SHOULD_INIT_OPAM:-true}

# Install build dependencies. Note: opam is fetched separately below from the
# upstream release so we get a known, current version regardless of what Alpine
# packages ship. gnupg is needed to verify the opam signature.
apk add --no-cache \
    git build-base zip bash libffi-dev \
    libpsl-static zstd-static libidn2-static \
    libunistring-static tar zstd gnupg curl

# Install opam from the official upstream release. Alpine's packaged opam lags
# behind, so we pin to OPAM_VERSION and fetch the matching static binary for
# the host architecture, then verify the signature against the upstream key.
case "$(uname -m)" in
    x86_64)  OPAM_ARCH="x86_64" ;;
    aarch64) OPAM_ARCH="arm64" ;;
    *)       echo "Unsupported architecture: $(uname -m)" >&2; exit 1 ;;
esac
OPAM_BASE="opam-${OPAM_VERSION}-${OPAM_ARCH}-linux"
OPAM_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/${OPAM_BASE}"
OPAM_TMP="$(mktemp -d)"
mkdir -m 0700 "${OPAM_TMP}/gnupg"
wget -qO "${OPAM_TMP}/${OPAM_BASE}"     "${OPAM_URL}"
wget -qO "${OPAM_TMP}/${OPAM_BASE}.sig" "${OPAM_URL}.sig"
wget -qO "${OPAM_TMP}/opam-pubkey.pgp"  "https://opam.ocaml.org/opam-dev-pubkey.pgp"
gpg --homedir "${OPAM_TMP}/gnupg" --batch --quiet \
    --import "${OPAM_TMP}/opam-pubkey.pgp"
gpg --homedir "${OPAM_TMP}/gnupg" --batch --verify \
    "${OPAM_TMP}/${OPAM_BASE}.sig" "${OPAM_TMP}/${OPAM_BASE}"
install -m 0755 "${OPAM_TMP}/${OPAM_BASE}" /usr/local/bin/opam
rm -rf "${OPAM_TMP}"
opam --version

make install-deps-ALPINE-for-semgrep-core

git config --global --add safe.directory $(pwd)
git config --global fetch.parallel 50

# Install tree-sitter
cd libs/ocaml-tree-sitter-core || exit
./configure
./scripts/install-tree-sitter-lib
cd - || exit

echo "Building in $(pwd)"

# Initialize opam, only if SHOULD_INIT_OPAM is true, since we are in a container
# in GHA and we may have had a cache hit on _opam.
if [ "$SHOULD_INIT_OPAM" = true ]; then
  opam init --yes --disable-sandboxing --root=$OPAMROOT --compiler=$OCAML_VERSION
  opam install dune
  make install-opam-deps
else
  echo "OPAM switch already exists, skipping creation: SHOULD_INIT_OPAM=$SHOULD_INIT_OPAM"
fi

eval $(opam env)

opam exec -- make core

opam exec -- make core-test

mkdir artifacts
cp bin/opengrep-core artifacts/
tar czf artifacts.tgz artifacts
