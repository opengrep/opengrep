#!/bin/bash
# Script for installing opengrep on Linux and MacOS
set -e

# Color output
YELLOW='\033[0;33m'
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Global variable for temporary directory
TEMP_DIR=""

# Cleanup function
cleanup() {
    local exit_code=$?
    printf "%b" "\n${YELLOW}Cleaning up...${NC}\n"
    
    # Remove temporary directory if it exists
    if [ -n "$TEMP_DIR" ] && [ -d "$TEMP_DIR" ]; then
        rm -rf "$TEMP_DIR"
        printf "%b" "${YELLOW}Removed temporary directory: $TEMP_DIR${NC}\n"
    fi
    
    # If script was interrupted or failed, show message
    if [ $exit_code -ne 0 ]; then
        printf "%b" "${RED}Installation was interrupted or failed.${NC}\n"
        printf "%b" "${YELLOW}Please run the script again to complete installation.${NC}\n"
    fi
    
    exit $exit_code
}

# Set up trap for cleanup
trap cleanup EXIT
trap 'trap - EXIT; cleanup' INT TERM

# Function to get available versions
get_available_versions() {
    local versions
    if command -v curl &> /dev/null; then
        versions=$(curl -s https://api.github.com/repos/opengrep/opengrep/releases | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/')
    elif command -v wget &> /dev/null; then
        versions=$(wget -qO- https://api.github.com/repos/opengrep/opengrep/releases | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/')
    else
        echo "Neither curl nor wget found. Please install either one."
        exit 1
    fi
    echo "$versions"
}

# Function to validate version
validate_version() {
    local version=$1
    local available_versions
    available_versions=$(get_available_versions)
    
    if echo "$available_versions" | grep -q "^$version$"; then
        return 0
    else
        printf "%b" "${RED}Error: Version $version not found${NC}\n"
        printf "%b" "${YELLOW}Available versions (latest 3):${NC}\n"
        echo "$available_versions" | head -3
        exit 1
    fi
}

# Function to get latest release version
get_latest_version() {
    local latest_version
    if command -v curl &> /dev/null; then
        latest_version=$(curl -s https://api.github.com/repos/opengrep/opengrep/releases | grep '"tag_name":' | head -1 | sed -E 's/.*"([^"]+)".*/\1/')
    elif command -v wget &> /dev/null; then
        latest_version=$(wget -qO- https://api.github.com/repos/opengrep/opengrep/releases| grep '"tag_name":' | head -1 | sed -E 's/.*"([^"]+)".*/\1/')
    else
        echo "Neither curl nor wget found. Please install either one."
        exit 1
    fi
    if [ -z "$latest_version" ]; then
        echo "Failed to detect latest version"
        exit 1
    fi
    echo "$latest_version"
}

# Function to detect OS and architecture
detect_system() {
    local os
    local arch
    local variant=""
    # Detect OS
    case "$(uname -s)" in
        Darwin*)
            os="osx"
            ;;
        Linux*)
            os="linux"
            # Detect Linux variant
            if ldd --version 2>&1 | grep -q "musl"; then
                variant="musllinux"
            else
                variant="manylinux"
            fi
            ;;
        *)
            echo "Unsupported operating system"
            exit 1
            ;;
    esac
    # Detect architecture
    case "$(uname -m)" in
        x86_64)
            arch="x86"
            ;;
        aarch64|arm64)
            arch="arm64"
            ;;
        *)
            echo "Unsupported architecture"
            exit 1
            ;;
    esac
    # Construct system identifier
    if [ "$os" = "linux" ]; then
        echo "${variant}_${arch}"
    else
        echo "${os}_${arch}"
    fi
}

# Function to download and install the package
install_package() {
    local system=$1
    local version=$2
    local base_url="https://github.com/opengrep/opengrep/releases/download/${version}"
    local binary_name="opengrep"
    
    # Create temporary directory and store in global variable
    TEMP_DIR=$(mktemp -d)
    printf "%b" "${YELLOW}Created temporary directory: $TEMP_DIR${NC}\n"
    
    cd "$TEMP_DIR" || exit 1
    printf "%b" "${YELLOW}Downloading OpenGrep ${version} for $system...${NC}\n"
    
    # Construct download URL based on system
    local download_url="${base_url}/opengrep_${system}"
    
    # Download binary
    if command -v curl &> /dev/null; then
        curl -L "$download_url" -o "$binary_name"
    elif command -v wget &> /dev/null; then
        wget "$download_url" -O "$binary_name"
    else
        echo "Neither curl nor wget found. Please install either one."
        exit 1
    fi
    
    # Make binary executable
    chmod +x "$binary_name"
    
    # Check if /usr/local/bin exists and is writable
    if [ ! -d "/usr/local/bin" ]; then
        sudo mkdir -p /usr/local/bin
    fi
    
    # Move binary to /usr/local/bin with sudo
    printf "%b" "${YELLOW}Installing OpenGrep to /usr/local/bin...${NC}"
    sudo mv "$binary_name" /usr/local/bin/
    sudo chmod +x "/usr/local/bin/$binary_name"
    
    printf "%b" "${GREEN}OpenGrep has been successfully installed!${NC}"
    printf "%b" "${GREEN}You can now use 'opengrep' command.${NC}"
}

# Function to print usage
print_usage() {
    echo "Usage: $0 [-v version]"
    echo "  -v    Specify version to install (optional)"
    echo "  -h    Show this help message"
    echo "  -l    List available versions"
}

# Main execution
VERSION=""

# Parse command line arguments
while getopts "v:hl" opt; do
    case $opt in
        v)
            VERSION="$OPTARG"
            ;;
        h)
            print_usage
            exit 0
            ;;
        l)
            echo -e "${YELLOW}Available versions (latest 3):${NC}"
            get_available_versions | head -3
            exit 0
            ;;
        \?)
            print_usage
            exit 1
            ;;
    esac
done

printf "%b" "${YELLOW}Detecting system...${NC}\n"
system=$(detect_system)
printf "%b" "${YELLOW}Detected system: $system${NC}\n"

if [ -z "$VERSION" ]; then
    printf "%b" "${YELLOW}No version specified, getting latest version...${NC}\n"
    VERSION=$(get_latest_version)
else
    printf "%b" "${YELLOW}Validating version $VERSION...${NC}\n"
    validate_version "$VERSION"
fi

printf "%b" "${YELLOW}Installing version: $VERSION${NC}\n"
install_package "$system" "$VERSION"
