#!/usr/bin/env bash
# Opengrep installation script

set -euo pipefail

if [[ "$0" == "bash" || "$0" == "-bash" ]]; then
  SCRIPT_NAME="install.sh (via stdin)"
else
  SCRIPT_NAME="$0"
fi

print_usage() {
    echo "Usage:"
    echo "  $SCRIPT_NAME [-v <version>] [--verify-signatures]"
    echo "      Install the latest or specified version (default: latest)"
    echo
    echo "  $SCRIPT_NAME -l"
    echo "      List the latest 3 available versions"
    echo
    echo "  $SCRIPT_NAME -h"
    echo "      Show this help message"
    echo
    echo "Options:"
    printf "  %-22s %s\n" "-v <version>" "Specify version to install (optional)"
    printf "  %-22s %s\n" "--verify-signatures" "Require Cosign verification of signature"
    printf "  %-22s %s\n" "-l" "List latest 3 versions (no install)"
    printf "  %-22s %s\n" "-h" "Display help (no install)"
    echo
    echo "Notes:"
    echo "  - '--verify-signatures' can be used with or without '-v'."
    echo "  - '-l' and '-h' cannot be combined with other options."
}

check_has_curl() {
    command -v curl > /dev/null 2>&1 || {
        echo >&2 "Required tool curl could not be found. Aborting."
        exit 1
    }
}

retry() {
    local retry_count=3
    local retry_delay=2

    local n=1
    until "$@"; do
        if (( n >= retry_count )); then
            printf 'attempt %d/%d failed for: %s\n' "$n" "$retry_count" "$*" >&2
            return 1
        fi
        printf 'attempt %d/%d failed for: %s; retrying in %ds\n' "$n" "$retry_count" "$*" "$retry_delay" >&2
        sleep "$retry_delay"
        ((n++))
    done
}


# Function to get available versions - already checked when running main
get_available_versions() {
    check_has_curl
    retry curl -s --fail https://api.github.com/repos/opengrep/opengrep/releases \
      | grep '"tag_name":' \
      | sed -E 's/.*"([^"]+)".*/\1/'
}

# Function to validate version
validate_version() {
    local VERSION="$1"
    local AVAILABLE_VERSIONS
    AVAILABLE_VERSIONS=$(get_available_versions)

    if [ -z "$AVAILABLE_VERSIONS" ]; then
        echo "Error: Unable to fetch available versions. Please check your internet connection." 1>&2
        return 1
    fi

    if echo "$AVAILABLE_VERSIONS" | grep -q "^$VERSION$"; then
        return 0
    else
        echo "Error: Version $VERSION not found"
        echo "Available versions (latest 3):"
        echo "$AVAILABLE_VERSIONS" | head -3
        exit 1
    fi
}

validate_signature() {
    local P="$1"
    if $HAS_COSIGN; then
        echo "Verifying signatures for ${P}/opengrep.cert"
        if cosign verify-blob \
            --cert "$P/opengrep.cert" \
            --signature "${P}/opengrep.sig" \
            --certificate-identity-regexp "https://github.com/opengrep/opengrep.+" \
            --certificate-oidc-issuer "https://token.actions.githubusercontent.com" \
            "${P}/opengrep"; then
            echo "Signature valid."
            exit 0
        else
            if [[ "$VERIFY_SIGNATURES" == true ]]; then
                echo "Error: Signature validation error. Deleting downloaded package ${P}."
                exit 1
            else
                echo "Warning: Signature validation error; the package is still installed."
                echo "If this was not intended, delete and rerun with --verify-signatures"
                exit 0
            fi
        fi
    fi
}

# carefully cleanup the expected files; we don't want a programming error to run
# rm -rf on any directory that is not intended...
cleanup_on_failure() {
    local P="$1"
    echo "An error occurred during the installation. Cleaning up ${P}..."
    rm -f "${P}/opengrep" || true
    rm -f "${P}/opengrep.sig" || true
    rm -f "${P}/opengrep.cert" || true
    rmdir "${P}" || true
    exit 1
}

main() {
    local REQUESTED_VERSION="$1"
    local VERIFY_SIGNATURES="$2"
    local FINAL_VERSION="$REQUESTED_VERSION"

    PREFIX="${HOME}/.opengrep/cli"
    LATEST="${PREFIX}/latest"

    OS="${OS:-$(uname -s)}"
    ARCH="${ARCH:-$(uname -m)}"
    DIST=""

    check_has_curl

    # check and set "os_arch"
    if [ "$OS" = "Linux" ]; then
        if ldd --version 2>&1 | grep -qi musl; then
            if [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "amd64" ]; then
                DIST="opengrep_musllinux_x86"
            elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
                DIST="opengrep_musllinux_aarch64"
            fi
        else
            if [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "amd64" ]; then
                DIST="opengrep_manylinux_x86"
            elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
                DIST="opengrep_manylinux_aarch64"
            fi
        fi
    elif [ "$OS" = "Darwin" ]; then
        if [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "amd64" ]; then
            DIST="opengrep_osx_x86"
        elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
            DIST="opengrep_osx_arm64"
        fi
    fi

    if [ -z "${DIST}" ]; then
        echo "Operating system '${OS}' / architecture '${ARCH}' is unsupported." 1>&2
        exit 1
    fi

    if [ "$REQUESTED_VERSION" = "latest" ]; then
        # Use GitHub's latest release redirect
        URL="https://github.com/opengrep/opengrep/releases/latest/download/${DIST}"

        # Get the final version from the redirect
        REDIRECT_URL=$(curl -sI "$URL" 2>/dev/null | grep -i '^location:' | sed 's/location: //i' | tr -d '\r\n' || true)
        if [ -n "$REDIRECT_URL" ]; then
            # Extract version from the redirect URL
            FINAL_VERSION=$(echo "$REDIRECT_URL" | sed -E 's|.*/download/([^/]+)/.*|\1|')
        else
            echo "Failed to resolve latest version. The redirect from $URL did not work." 1>&2
            echo "Please check your internet connection or specify a version with -v" 1>&2
            exit 1
        fi

        echo
        echo "*** Installing Opengrep ${FINAL_VERSION} for ${OS} (${ARCH}) ***"
        echo
    else
        URL="https://github.com/opengrep/opengrep/releases/download/${REQUESTED_VERSION}/${DIST}"
        echo
        echo "*** Installing Opengrep ${REQUESTED_VERSION} for ${OS} (${ARCH}) ***"
        echo
    fi

    # Now set INST with the final version
    INST="${PREFIX}/${FINAL_VERSION}"

    # check if binary already exists
    if [ -f "${INST}/opengrep" ]; then
        echo "Destination binary ${INST}/opengrep already exists."
        rm -f "${LATEST}" || exit 1
        ln -s "${INST}" "${LATEST}" || exit 1
        echo "Updated symlink from ${LATEST}/opengrep to point to ${INST}/opengrep."
        if $VERIFY_SIGNATURES; then
            echo "Signature verification skipped for existing installation."
        fi
    else
        # cleanup on error
        trap '[ "$?" -eq 0 ] || cleanup_on_failure $INST' EXIT

        # Download to temp file first
        TEMP_FILE=$(mktemp)
        trap 'rm -f $TEMP_FILE' EXIT

        if ! retry curl --fail --location --progress-bar "${URL}" > "${TEMP_FILE}"; then
            # Download failed - if user provided a version, validate it to show available versions
            if [ "$REQUESTED_VERSION" != "latest" ]; then
                echo
                echo "Error: Failed to download version ${REQUESTED_VERSION}" 1>&2
                # This will show available versions and exit
                validate_version "$REQUESTED_VERSION"
            fi
            rm -f "$TEMP_FILE"
            exit 1
        fi

        # Only create directory after successful download
        mkdir -p "${INST}"
        if [ ! -d "${INST}" ]; then
            echo "Failed to create install directory ${INST}." 1>&2
            rm -f "$TEMP_FILE"
            exit 1
        fi

        # Move temp file to final location
        mv "$TEMP_FILE" "${INST}/opengrep"

        local SIG_EXISTS=true

        # Try downloading .cert
        CERT_STATUS=$(curl --location --silent --show-error --write-out "%{http_code}" \
            --output "${INST}/opengrep.cert" "${URL}.cert")

        if [[ "$CERT_STATUS" == "404" ]]; then
            SIG_EXISTS=false
            rm -f "${INST}/opengrep.cert" # It's there but contains "Not found".
            echo "Warning: Certificate file not found at ${URL}.cert"
        elif [[ "$CERT_STATUS" != 200 ]]; then
          echo "Error: Failed to download ${URL}.cert: HTTP status $CERT_STATUS."
          exit 1
        else
            # Only attempt .sig if .cert was found
            SIG_STATUS=$(curl --location --silent --show-error --write-out "%{http_code}" \
                --output "${INST}/opengrep.sig" "${URL}.sig")

            if [[ "$SIG_STATUS" == "404" ]]; then
                SIG_EXISTS=false
                echo "Error: Signature file not found at ${URL}.sig, but ${URL}.cert was found."
                exit 1  # we donwloaded .cert, so exit with error
            elif [[ "$SIG_STATUS" != 200 ]]; then
              echo "Error: Failed to download ${URL}.sig: HTTP status $SIG_STATUS."
              exit 1
            fi
        fi

        # check signature if SIG_EXIST
        if [[ "$SIG_EXISTS" == true ]]; then
            validate_signature "${INST}"
        else
            if [[ "$VERIFY_SIGNATURES" == true ]]; then
                echo "Error: No signature / certificate found for ${VERSION} but --verify-signatures was requested."
                exit 1
            else
                echo "Warning: No signature / certificate found for ${VERSION}. Skipping signature verification."
                echo "Warning: The package is still installed. It is likely that signature verification was added after this version."
            fi
        fi

        # make executable by all users
        chmod a+x "${INST}/opengrep" || exit 1

        if [ ! -f "${INST}/opengrep" ]; then
            echo "Failed to download binary at ${INST}/opengrep" 1>&2
            exit 1
        fi

        echo "Testing binary..."
        # Test by calling --version on the downloaded binary
        TEST=$("${INST}/opengrep" --version 2> /dev/null || true)
        if [ -z "$TEST" ]; then
            echo "Failed to execute installed binary: ${INST}/opengrep." 1>&2
            exit 1
        fi

        echo
        echo "Successfully installed Opengrep binary at ${INST}/opengrep"

        rm -f "${LATEST}" || exit 1
        ln -s "${INST}" "${LATEST}" || exit 1
        echo "with a symlink from ${LATEST}/opengrep"
    fi

    # Always update the symlink to point to the requested version
    rm -f "${LATEST}" || exit 1
    ln -s "${INST}" "${LATEST}" || exit 1

    LOCALBIN="${HOME}/.local/bin"

    # Only need to create the symlink from .local/bin once if not created before.
    # For all subsequent installations, the ./local/bin symlink will still point
    # to the updated symlink (created above).
    if [ -d "${LOCALBIN}" ] && [ -w "${LOCALBIN}" ]; then
        # Only create the symlink if it doesn't already exist
        if [ ! -f "${LOCALBIN}/opengrep" ]; then
            ln -s "${LATEST}/opengrep" "${LOCALBIN}/opengrep"
            echo "Created symlink from ${LOCALBIN}/opengrep to ${LATEST}/opengrep"
        fi

        echo
        echo "To launch Opengrep now, type:"
        # Do not assume that ~/.local/bin is in the PATH even if it exists,
        # as it is not always the case.
        if echo "$PATH" | tr ':' '\n' | grep -Fxq "$HOME/.local/bin"; then
            echo "opengrep"
        else
            echo "${LOCALBIN}/opengrep"
        fi
        echo
    else
        echo
        echo "To launch Opengrep now, type:"
        echo "${LATEST}/opengrep"
        echo
        echo "Hint: Append the following line to your shell profile:"
        echo "export PATH='${LATEST}':\$PATH"
        echo
    fi
}

# Argument parsing
if command -v cosign &> /dev/null; then
    HAS_COSIGN=true
else
    HAS_COSIGN=false
fi

HELP=false
LIST=false
VERIFY_SIGNATURES=false
VERSION=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h)
            HELP=true
            shift
            ;;
        -l)
            LIST=true
            shift
            ;;
        --verify-signatures)
            VERIFY_SIGNATURES=true
            shift
            ;;
        -v)
            if [[ -z "${2-}" ]] || ! [[ -n "$2" && "$2" != -* ]]; then
                echo "Error: -v requires a version argument."
                exit 1
            else
                VERSION="$2"
                shift 2
            fi
            ;;
        *)
            echo "Error: Unknown option: $1."
            print_usage
            exit 1
            ;;
    esac
done

if { { $VERIFY_SIGNATURES || [[ -n "$VERSION" ]] || $LIST; } && $HELP; } || { { $VERIFY_SIGNATURES || [[ -n "$VERSION" ]] || $HELP; } && $LIST; }; then
    echo "Error: incorrect arguments:"
    print_usage
    exit 1
fi

if $VERIFY_SIGNATURES && ! $HAS_COSIGN; then
    echo "Error: cosign is required for --verify-signatures but is not installed."
    echo "Go to https://github.com/sigstore/cosign to install it or run without the --verify-signatures flag to install without signature verification."
    exit 1
elif ! $HAS_COSIGN; then
    echo "Warning: cosign is required for --verify-signatures but is not installed. Skipping signature validation."
    echo "Go to https:/github.com/sigstore/cosign to install it."
fi

if "$HELP"; then
    print_usage
    exit 0
fi

# Only fetch available versions if listing
if $LIST; then
    AVAILABLE_VERSIONS="$(get_available_versions)"
    echo "Available versions (latest 3):"
    printf '%s\n' "$AVAILABLE_VERSIONS" | head -3
    exit 0
fi

shift $((OPTIND - 1))

# If no version specified, we'll use "latest" and let curl resolve it
if [ -z "$VERSION" ]; then
    VERSION="latest"
fi

main "$VERSION" "$VERIFY_SIGNATURES"
