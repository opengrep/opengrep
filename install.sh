#!/usr/bin/env bash -e
# Opengrep installation script 

print_usage() {
    echo "Usage: $0 [-v version] [-l] [-h]"
    echo "  -v    Specify version to install (optional, default: latest)"
    echo "  -l    List available versions (latest 3)"
    echo "  -h    Show this help message"
}

# Function to get available versions - already checked when running main
get_available_versions() {
    command -v curl >/dev/null 2>&1 || { echo >&2 "Required tool curl could not be found. Aborting."; exit 1; }
    curl -s https://api.github.com/repos/opengrep/opengrep/releases | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
}

# Function to get latest release version
get_latest_version() {
    command -v curl >/dev/null 2>&1 || { echo >&2 "Required tool curl could not be found. Aborting."; exit 1; }
    curl -s https://api.github.com/repos/opengrep/opengrep/releases | grep '"tag_name":' | head -1 | sed -E 's/.*"([^"]+)".*/\1/'
}

# Function to validate version
validate_version() {
    local version="$1"
    local available_versions
    available_versions=$(get_available_versions)
    if echo "$available_versions" | grep -q "^$version$"; then
        return 0
    else
        echo "Error: Version $version not found"
        echo "Available versions (latest 3):"
        echo "$available_versions" | head -3
        exit 1
    fi
}


main () {
    local VERSION="$1"

    PREFIX="${HOME}/.opengrep/cli"
    INST="${PREFIX}/${VERSION}"
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

    URL="https://github.com/opengrep/opengrep/releases/download/${VERSION}/${DIST}"
    echo
    echo "*** Installing Opengrep version ${VERSION} for ${DIST} ***"
    echo

    # check if binary already exists
    if [ -f "${INST}/opengrep" ]; then
        echo "Destination binary ${INST}/opengrep already exists."
    else
        mkdir -p "${INST}"
        if [ ! -d "${INST}" ]; then
            echo "Failed to create install directory ${INST}." 1>&2
            exit 1
        fi

        curl --fail --location --progress-bar "${URL}" > "${INST}/opengrep"

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
        
        ln -s "${INST}" "${LATEST}" || exit 1
        echo "with a symlink from ${LATEST}/opengrep"
    fi

    LOCALBIN="${HOME}/.local/bin"
    SYMLINK_CREATED=0

    if [ -d "${LOCALBIN}" ] && [ -w "${LOCALBIN}" ] && [ ! -f "${LOCALBIN}/opengrep" ]; then
        ln -s "${LATEST}/opengrep" "${LOCALBIN}/opengrep" && SYMLINK_CREATED=1
        if [ $SYMLINK_CREATED -eq 1 ]; then
            echo "Created a symlink: ${LOCALBIN}/opengrep → ${LATEST}/opengrep"
        fi
    fi

    if [ $SYMLINK_CREATED -eq 0 ]; then
        echo
        echo "Hint: Append the following line to your shell profile:"
        echo "export PATH='${LATEST}':\$PATH"
        echo
    fi

    if [ $SYMLINK_CREATED -eq 1 ]; then
        echo
        echo "To launch Opengrep now, type"
        echo "opengrep"
    else
        echo
        echo "To launch Opengrep now, type"
        echo "${LATEST}/opengrep"
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

if [ "$SHOW_LIST" -eq 1 ]; then
    echo "Available versions (latest 3):"
    get_available_versions | head -3
    exit 0
fi

shift $((OPTIND -1))

if [ -z "$VERSION" ]; then
    VERSION=$(get_latest_version)
else
    validate_version "$VERSION"
fi


main "$VERSION"