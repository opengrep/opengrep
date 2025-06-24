#!/usr/bin/env bash
set -euo pipefail

# Opengrep installation script

print_usage() {
    echo "Usage: $0 [-v version] [-l] [-h]"
    echo "  -v    Specify version to install (optional, default: latest)"
    echo "  -l    List available versions (latest 3)"
    echo "  -h    Show this help message"
}

retry() {
    # This facilitates arbitrary retry logic for the provided command
    #
    # USAGE:
    #  # Use the defaults
    #  retry curl --fail https://example.com
    #  # Provide an updated count
    #  retry 10 curl --fail https://example.com
    #  # Provide an updated count and delay
    #  retry 5 10 curl --fail https://example.com
    local retry_count=3
    local retry_delay=2

    # Update the default values if provided
    [[ $1 =~ ^[0-9]+$ ]] && { retry_count=$1; shift; }
    [[ $1 =~ ^[0-9]+$ ]] && { retry_delay=$1; shift; }

    local n=1
    until "$@"; do
        if (( n >= retry_count )); then
            printf '⚠️  attempt %d/%d failed for: %s\n' "$n" "$retry_count" "$*" >&2
            return 1
        fi
        printf '⚠️  attempt %d/%d failed for: %s; retrying in %ds\n' "$n" "$retry_count" "$*" "$retry_delay" >&2
        sleep "$retry_delay"
        ((n++))
    done
}


# Function to get available versions - already checked when running main
get_available_versions() {
    command -v curl >/dev/null 2>&1 || { echo >&2 "Required tool curl could not be found. Aborting."; exit 1; }
    # This extracts and validates the version tag(s) without requiring jq
    retry curl -s --fail https://api.github.com/repos/opengrep/opengrep/releases \
      | grep '"tag_name":' \
      | sed -E 's/.*"([^"]+)".*/\1/' \
      | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$'
}

# Function to validate version
validate_version() {
    local version="$1"

    # Fetch available versions
    AVAILABLE_VERSIONS="$(get_available_versions 2>/dev/null || echo "")"

    if [ -z "$AVAILABLE_VERSIONS" ]; then
        echo "Error: Unable to fetch available versions. Please check your internet connection." 1>&2
        return 1
    fi

    if ! printf '%s\n' "$AVAILABLE_VERSIONS" | grep -qx "$version"; then
        echo "Error: Version $version does not appear to be a valid opengrep release" 1>&2
        echo "Available versions (latest 3):" 1>&2
        printf '%s\n' "$AVAILABLE_VERSIONS" | head -3 1>&2
        return 1
    else
        echo "Error: Version $version appears to be valid, but the installation failed for unknown reasons" 1>&2
        return 1
    fi
}


main () {
    local REQUESTED_VERSION="$1"
    local FINAL_VERSION="$REQUESTED_VERSION"

    PREFIX="${HOME}/.opengrep/cli"
    LATEST="${PREFIX}/latest"

    OS="${OS:-$(uname -s)}"
    ARCH="${ARCH:-$(uname -m)}"
    DIST=""

    command -v curl >/dev/null 2>&1 || { echo >&2 "Required tool curl could not be found. Aborting."; exit 1; }

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
    else
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

        # make executable by all users
        chmod a+x "${INST}/opengrep" || exit 1

        if [ ! -f "${INST}/opengrep" ]; then
            echo "Failed to download binary at ${INST}/opengrep" 1>&2
            exit 1
        fi

        # Test by calling --version on the downloaded binary
        TEST=$("${INST}/opengrep" --version 2>/dev/null || true)
        if [ -z "$TEST" ]; then
            echo "Failed to execute installed binary: ${INST}/opengrep." 1>&2
            exit 1
        fi

        echo
        echo "Successfully installed Opengrep binary to ${INST}/opengrep"
    fi

    # Always update the symlink to point to the requested version
    rm -f "${LATEST}" || exit 1
    ln -s "${INST}" "${LATEST}" || exit 1

    LOCALBIN="${HOME}/.local/bin"

    # only need create the symlink from .local/bin once if not created before
    # for all subsequent installations, the ./local/bin symlink will still point to the updated symlink (created above)
    if [ -d "${LOCALBIN}" ] && [ -w "${LOCALBIN}" ]; then
        # Only create the symlink if it doesn't already exist
        if [ ! -f "${LOCALBIN}/opengrep" ]; then
            ln -s "${LATEST}/opengrep" "${LOCALBIN}/opengrep"
            echo "Created symlink from ${LATEST}/opengrep to ${LOCALBIN}/opengrep"
        fi
        echo
        echo "To launch Opengrep now, type:"
        echo "opengrep"
        echo
        echo "To check Opengrep version, type:"
        echo "opengrep --version"
        echo
    else
        echo
        echo "Hint: Append the following line to your shell profile:"
        echo "export PATH='${LATEST}':\$PATH"
        echo
    fi
}

# Argument parsing
VERSION=""
SHOW_HELP=0
SHOW_LIST=0

while getopts "v:hl" opt; do
    case $opt in
        v)
            VERSION="$OPTARG"
            ;;
        h)
            SHOW_HELP=1
            ;;
        l)
            SHOW_LIST=1
            ;;
        \?)
            print_usage
            exit 1
            ;;
    esac
done

if [ "$SHOW_HELP" -eq 1 ]; then
    print_usage
    exit 0
fi

# Only fetch available versions if listing
if [ "$SHOW_LIST" -eq 1 ]; then
    AVAILABLE_VERSIONS="$(get_available_versions)"
    echo "Available versions (latest 3):"
    printf '%s\n' "$AVAILABLE_VERSIONS" | head -3
    exit 0
fi

shift $((OPTIND -1))

# If no version specified, we'll use "latest" and let curl resolve it
if [ -z "$VERSION" ]; then
    VERSION="latest"
fi


main "$VERSION"
