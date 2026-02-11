###############################################################################
# Overview
###############################################################################
# Dockerfile to build the opengrep Docker image.
#
# First, we build a fully *static* 'opengrep-core' binary on Alpine. This
# binary does not even depend on Glibc because Alpine uses Musl instead
# which can be statically linked.
#
# Then 'opengrep-core' alone is copied to another Alpine-based container
# which takes care of the Python CLI wrapping (pysemgrep).
#
# We use this two-step process because *building* opengrep-core itself
# requires lots of tools (ocamlc, gcc, make, etc.), with big containers,
# but those tools are not necessary when *running* opengrep.
# See https://docs.docker.com/build/building/multi-stage/
#
# Build with:  docker build -t opengrep .
# Run with:    docker run --rm -v "${PWD}:/src" opengrep scan --config auto .
#
# In case of problems, if you need to debug the docker image, run
# 'docker build .', identify the SHA of the build image and run
# 'docker run -it <sha> /bin/bash' to explore before the failing point.

###############################################################################
# Step0: collect files needed to build opengrep-core
###############################################################################

# The repository contains source code for multiple build artifacts
# (pysemgrep, opengrep-core, etc.). To maximize Docker cache hits we only
# copy the folders needed to build opengrep-core.

FROM busybox:stable AS source-files
WORKDIR /src/opengrep
COPY . .
# Remove files not needed for the core build
# coupling: see the (dirs ...) directive in the toplevel dune file
RUN rm -rf cli .github .circleci Dockerfile
# We *do* need the cli's semgrep_interfaces folder, however
COPY cli/src/semgrep/semgrep_interfaces cli/src/semgrep/semgrep_interfaces

###############################################################################
# Step1: build opengrep-core
###############################################################################
# Using ocamlpro/ocaml:5.3.0 — the same base image used by the CI workflow
# (.github/workflows/build-test-core-x86.yml). This avoids building OCaml
# from scratch and significantly speeds up the Docker build.

FROM ocamlpro/ocaml:5.3.0 AS core-builder
USER root

# System packages matching CI's "Add system packages" step.
# Static link deps (libidn2, libunistring, libpsl, zstd) are needed because
# src/main/flags.sh links them on Alpine for the static opengrep-core binary.
RUN apk add --no-cache bash build-base zip libffi-dev \
    libpsl-static zstd-static libidn2-static libunistring-static

WORKDIR /src/opengrep

# Create a local opam switch using the system OCaml (same as CI)
RUN opam switch --no-install create . ocaml-system

# Copy just enough for dependency installation (maximises docker cache hits).
# note: semgrep.opam lives under opam/ in the opengrep fork
COPY --from=source-files /src/opengrep/Makefile ./Makefile
COPY --from=source-files /src/opengrep/scripts ./scripts
COPY --from=source-files /src/opengrep/opam/semgrep.opam ./opam/semgrep.opam
COPY --from=source-files /src/opengrep/libs/ocaml-tree-sitter-core/tree-sitter.opam ./libs/ocaml-tree-sitter-core/tree-sitter.opam
COPY --from=source-files /src/opengrep/dev ./dev

# Install deps: Alpine system packages, pin memprof-limits, then opam deps.
# memprof-limits must be pinned from a custom branch before opam can resolve.
RUN make install-deps-ALPINE-for-semgrep-core && \
    ./scripts/install-memprof-limits-dev.sh && \
    make install-opam-deps

# Copy full source and build
COPY --from=source-files /src/opengrep ./

# Create the bin symlink that the Makefile expects
RUN ln -sf _build/install/default/bin bin

# Build opengrep-core (same as CI: install-deps-for-semgrep-core then make core)
RUN make install-deps-for-semgrep-core && \
    opam exec -- make core && \
    ./bin/opengrep-core -version

###############################################################################
# Step2: Python CLI wrapper + opengrep-core binary
###############################################################################
# We change container, bringing the 'opengrep-core' binary with us.

#coupling: the 'semgrep-oss' name is used in 'make build-docker'
FROM python:3.11-alpine AS semgrep-oss

WORKDIR /pysemgrep

# Update to the latest packages for the base image for CVE fixes.
RUN apk upgrade --no-cache && \
    apk add --no-cache --virtual=.run-deps\
# Here is why we need the apk packages below:
# - git, git-lfs, openssh: so that the opengrep docker image can be used in
#   Github actions (GHA) and get git submodules and use ssh to get those
#   submodules
# - bash: many users customize their call to opengrep via bash script
# - jq: useful to process the JSON output of opengrep
# - curl: useful to connect to some webhooks
	git git-lfs openssh \
	bash jq curl

# We just need the Python code in cli/.
# The opengrep-core binary is copied from the builder container below.
COPY cli ./

ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
    PIP_NO_CACHE_DIR=true \
    PYTHONIOENCODING=utf8 \
    PYTHONUNBUFFERED=1

# Workaround for pip 24 failing to find the wheel module.
# TODO: this seems to work in the pro Dockerfile so we can probably remove now
RUN pip install --force-reinstall -v "pip==23.3.2"

# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base make &&\
     pip install /pysemgrep &&\
     apk del .build-deps

# Get opengrep-core from step1
COPY --from=core-builder /src/opengrep/_build/install/default/bin/opengrep-core /usr/local/bin/opengrep-core

# The CLI looks for opengrep-core in PATH (see entrypoint.py).
# Keep semgrep-core and osemgrep as compatibility symlinks.
RUN ln -s opengrep-core /usr/local/bin/semgrep-core && \
    ln -s opengrep-core /usr/local/bin/osemgrep && \
    rm -rf /pysemgrep

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

ENV SEMGREP_IN_DOCKER=1 \
    SEMGREP_USER_AGENT_APPEND="Docker"

# The command we tell people to run for testing opengrep in Docker is
#   docker run --rm -v "${PWD}:/src" opengrep scan --config=auto .
WORKDIR /src

RUN adduser -D -u 1000 -h /home/semgrep semgrep \
    && chown semgrep /src

# Workaround for rootless containers as git operations may fail due to dubious
# ownership of /src
RUN printf "[safe]\n	directory = /src"  > ~root/.gitconfig
RUN printf "[safe]\n	directory = /src"  > ~semgrep/.gitconfig && \
	chown semgrep:semgrep ~semgrep/.gitconfig

CMD ["semgrep", "--help"]
LABEL maintainer="support@opengrep.dev"
