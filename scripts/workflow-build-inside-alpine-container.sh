#!/usr/bin/env sh

set -ex

export OPAMROOT=/workspace/_opam
export DUNE_CACHE_ROOT=/workspace/_dune
export OPAMCONFIRMLEVEL=unsafe-yes
export OPAMCOLOR=always

# OPAM_VERSION="2.3.0"
OCAML_VERSION=${OCAML_VERSION:-"5.3.0"}
SHOULD_INIT_OPAM=${SHOULD_INIT_OPAM:-true}

# Install opam and other dependencies.
apk add --no-cache \
    opam git build-base zip bash libffi-dev \
    libpsl-static zstd-static libidn2-static \
    libunistring-static tar zstd

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
