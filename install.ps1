# Opengrep installation script for Windows PowerShell
#
# NOTES
# ============================
#
# 1. Junctions
#    Uses directory junctions (mklink /J) which do not require admin privileges.
#    Falls back to copying files if junction creation fails.
#
# 2. Binary Naming
#    Windows: Binary is named 'opengrep.exe'
#
# 3. Architecture Support
#    Currently only x86_64 builds are available. ARM64 is not yet supported
#    (though it will run under x86 emulation on Windows ARM64).
#
# 4. PATH Integration
#    Provides instructions for manually adding to PATH. No automatic
#    modification of user PATH to avoid requiring elevation.
#
# 5. Parameter Naming (PowerShell conventions)
#    -v <version>        ->  -Version <version>
#    --verify-signatures ->  -VerifySignatures
#    -l                  ->  -List
#    -h                  ->  -Help
#
# 6. Installation Path
#    %USERPROFILE%\.opengrep\cli\<version>\opengrep.exe
#
# USAGE:
# ======
#
# Local execution:
#   .\install.ps1                       # Install latest version
#   .\install.ps1 -Version v1.15.0      # Install specific version
#   .\install.ps1 -VerifySignatures     # Verify with cosign
#   .\install.ps1 -List                 # List available versions
#   .\install.ps1 -Help                 # Show help
#
# Remote execution (one-liner):
#   irm https://raw.githubusercontent.com/opengrep/opengrep/main/install.ps1 | iex
#
# Remote execution with parameters:
#   & ([scriptblock]::Create((irm https://raw.githubusercontent.com/opengrep/opengrep/main/install.ps1))) -Version v1.15.0
#
# Note: You may need to adjust your execution policy:
#   Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
#

#Requires -Version 5.1

[CmdletBinding()]
param(
    [Parameter()]
    [string]$Version,

    [Parameter()]
    [switch]$VerifySignatures,

    [Parameter()]
    [switch]$List,

    [Parameter()]
    [switch]$Help
)

$ErrorActionPreference = "Stop"

$ScriptName = if ($MyInvocation.MyCommand.Name) { $MyInvocation.MyCommand.Name } else { "install.ps1" }

function Print-Usage {
    Write-Host "Usage:"
    Write-Host "  $ScriptName [-Version <version>] [-VerifySignatures]"
    Write-Host "      Install the latest or specified version (default: latest)"
    Write-Host ""
    Write-Host "  $ScriptName -List"
    Write-Host "      List the latest 3 available versions"
    Write-Host ""
    Write-Host "  $ScriptName -Help"
    Write-Host "      Show this help message"
    Write-Host ""
    Write-Host "Options:"
    Write-Host "  -Version <version>    Specify version to install (optional)"
    Write-Host "  -VerifySignatures     Require Cosign verification of signature"
    Write-Host "  -List                 List latest 3 versions (no install)"
    Write-Host "  -Help                 Display help (no install)"
    Write-Host ""
    Write-Host "Notes:"
    Write-Host "  - '-VerifySignatures' can be used with or without '-Version'."
    Write-Host "  - '-List' and '-Help' cannot be combined with other options."
}

function Get-AvailableVersions {
    try {
        $response = Invoke-RestMethod -Uri "https://api.github.com/repos/opengrep/opengrep/releases" -UseBasicParsing
        return $response | ForEach-Object { $_.tag_name }
    }
    catch {
        Write-Error "Failed to fetch available versions: $_"
        exit 1
    }
}

function Validate-Version {
    param([string]$VersionToValidate)

    $availableVersions = Get-AvailableVersions
    if ($availableVersions -contains $VersionToValidate) {
        return $true
    }
    else {
        Write-Host "Error: Version $VersionToValidate not found" -ForegroundColor Red
        Write-Host "Available versions (latest 3):"
        $availableVersions | Select-Object -First 3 | ForEach-Object { Write-Host "  $_" }
        exit 1
    }
}

function Test-CosignInstalled {
    $cosign = Get-Command cosign -ErrorAction SilentlyContinue
    return $null -ne $cosign
}

function Get-CosignMajorVersion {
    try {
        $versionOutput = & cosign version 2>&1
        $versionLine = $versionOutput | Where-Object { $_ -match "GitVersion" }
        if ($versionLine -match "v?(\d+)") {
            return [int]$Matches[1]
        }
    }
    catch {
        return 0
    }
    return 0
}

function Validate-Signature {
    param([string]$InstallPath)

    if ($script:HasCosign) {
        Write-Host "Verifying signatures for $InstallPath\opengrep.cert"
        $result = & cosign verify-blob `
            --cert "$InstallPath\opengrep.cert" `
            --signature "$InstallPath\opengrep.sig" `
            --certificate-identity-regexp "https://github.com/opengrep/opengrep.+" `
            --certificate-oidc-issuer "https://token.actions.githubusercontent.com" `
            "$InstallPath\opengrep.exe" 2>&1

        if ($LASTEXITCODE -eq 0) {
            Write-Host "Signature valid."
        }
        else {
            Write-Host "Error: Signature validation error." -ForegroundColor Red
            Write-Host $result
            exit 1
        }
    }
    else {
        Write-Host "Warning: cosign needed for signature validation; the package will still be installed." -ForegroundColor Yellow
        Write-Host "If this was not intended, delete and rerun with -VerifySignatures or install cosign."
    }
}

function Cleanup-OnFailure {
    param([string]$InstallPath)

    Write-Host "An error occurred during the installation. Cleaning up $InstallPath..." -ForegroundColor Yellow
    Remove-Item -Path "$InstallPath\opengrep.exe" -ErrorAction SilentlyContinue
    Remove-Item -Path "$InstallPath\opengrep.sig" -ErrorAction SilentlyContinue
    Remove-Item -Path "$InstallPath\opengrep.cert" -ErrorAction SilentlyContinue
    Remove-Item -Path $InstallPath -ErrorAction SilentlyContinue
}

function Update-LatestLink {
    param(
        [string]$LatestPath,
        [string]$TargetPath
    )

    # Remove existing latest directory/junction if it exists
    if (Test-Path $LatestPath) {
        $item = Get-Item $LatestPath -Force
        if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
            # It's a junction/symlink, remove it
            cmd /c rmdir "$LatestPath" 2>$null
        }
        else {
            Remove-Item -Path $LatestPath -Recurse -Force
        }
    }

    # Create a directory junction (does not require admin privileges)
    cmd /c mklink /J "$LatestPath" "$TargetPath" >$null 2>&1
    if ($LASTEXITCODE -ne 0) {
        # Fallback: copy the directory if junction creation fails
        Write-Host "Warning: Could not create directory junction, copying files instead." -ForegroundColor Yellow
        Copy-Item -Path $TargetPath -Destination $LatestPath -Recurse -Force
    }
}

function Main {
    param(
        [string]$VersionToInstall,
        [bool]$DoVerifySignatures
    )

    $prefix = Join-Path $env:USERPROFILE ".opengrep\cli"
    $inst = Join-Path $prefix $VersionToInstall
    $latest = Join-Path $prefix "latest"

    # Windows architecture detection
    $arch = $env:PROCESSOR_ARCHITECTURE
    if (-not $arch) {
        $arch = (Get-CimInstance Win32_Processor).Architecture
    }

    # Determine distribution name
    # Currently only x86_64 Windows builds are available
    if ($arch -eq "AMD64" -or $arch -eq "x86_64") {
        $dist = "opengrep_windows_x86.exe"
    }
    elseif ($arch -eq "ARM64") {
        Write-Host "Error: ARM64 Windows builds are not yet available." -ForegroundColor Red
        Write-Host "You may try running the x86_64 build under emulation."
        exit 1
    }
    else {
        Write-Host "Error: Architecture '$arch' is unsupported." -ForegroundColor Red
        exit 1
    }

    $url = "https://github.com/opengrep/opengrep/releases/download/$VersionToInstall/$dist"

    # Check if binary already exists
    $binaryPath = Join-Path $inst "opengrep.exe"
    if (Test-Path $binaryPath) {
        Write-Host "Destination binary $binaryPath already exists."
        Update-LatestLink -LatestPath $latest -TargetPath $inst
        Write-Host "Updated link from $latest\opengrep.exe to point to $binaryPath."
        if ($DoVerifySignatures) {
            Write-Host "Signature verification skipped for existing installation."
        }
    }
    else {
        Write-Host ""
        Write-Host "*** Installing Opengrep $VersionToInstall for Windows ($arch) ***"

        # Create install directory
        if (-not (Test-Path $inst)) {
            New-Item -ItemType Directory -Path $inst -Force | Out-Null
        }

        try {
            # Download the binary
            Write-Host "Downloading $url..."
            $progressPreference = 'SilentlyContinue'  # Speeds up Invoke-WebRequest
            Invoke-WebRequest -Uri $url -OutFile $binaryPath -UseBasicParsing

            $sigExists = $true

            # Try downloading .cert
            $certPath = Join-Path $inst "opengrep.cert"
            $certUrl = "$url.cert"
            try {
                Invoke-WebRequest -Uri $certUrl -OutFile $certPath -UseBasicParsing
            }
            catch {
                if ($_.Exception.Response.StatusCode -eq 404) {
                    $sigExists = $false
                    Remove-Item -Path $certPath -ErrorAction SilentlyContinue
                    Write-Host "Warning: Certificate file not found at $certUrl" -ForegroundColor Yellow
                }
                else {
                    throw "Failed to download $certUrl`: $_"
                }
            }

            if ($sigExists) {
                # Only attempt .sig if .cert was found
                $sigPath = Join-Path $inst "opengrep.sig"
                $sigUrl = "$url.sig"
                try {
                    Invoke-WebRequest -Uri $sigUrl -OutFile $sigPath -UseBasicParsing
                }
                catch {
                    if ($_.Exception.Response.StatusCode -eq 404) {
                        Write-Host "Error: Signature file not found at $sigUrl, but $certUrl was found." -ForegroundColor Red
                        throw "Signature file missing"
                    }
                    else {
                        throw "Failed to download $sigUrl`: $_"
                    }
                }
            }

            # Check signature if it exists
            if ($sigExists) {
                Validate-Signature -InstallPath $inst
            }
            else {
                if ($DoVerifySignatures) {
                    Write-Host "Error: No signature / certificate found for $VersionToInstall but -VerifySignatures was requested." -ForegroundColor Red
                    Write-Host "Error: It is likely that signature verification was added after this version."
                    throw "Signature verification failed"
                }
                else {
                    Write-Host "Warning: No signature / certificate found for $VersionToInstall. Skipping signature verification." -ForegroundColor Yellow
                    Write-Host "Warning: The package will still be installed. It is likely that signature verification was added after this version."
                }
            }

            # Verify the binary exists
            if (-not (Test-Path $binaryPath)) {
                throw "Failed to download binary at $binaryPath"
            }

            Write-Host "Testing binary..."
            # Test by calling --version on the downloaded binary
            $testOutput = & $binaryPath --version 2>&1
            if (-not $testOutput -or $LASTEXITCODE -ne 0) {
                throw "Failed to execute installed binary: $binaryPath"
            }

            Write-Host ""
            Write-Host "Successfully installed Opengrep binary at $binaryPath"

            Update-LatestLink -LatestPath $latest -TargetPath $inst
            Write-Host "with a link from $latest\opengrep.exe"
        }
        catch {
            Cleanup-OnFailure -InstallPath $inst
            throw
        }
    }

    # Add to PATH guidance
    $latestBinary = Join-Path $latest "opengrep.exe"

    Write-Host ""
    Write-Host "To launch Opengrep now, type:"
    Write-Host "  $latestBinary"
    Write-Host ""
    Write-Host "To add Opengrep to your PATH permanently, run (as Administrator):"
    Write-Host "  `$currentPath = [Environment]::GetEnvironmentVariable('Path', 'User')"
    Write-Host "  [Environment]::SetEnvironmentVariable('Path', `"`$currentPath;$latest`", 'User')"
    Write-Host ""
    Write-Host "Or add '$latest' to your PATH manually via System Properties."
    Write-Host ""
}

# --- Main script execution ---

# Check for cosign
$script:HasCosign = Test-CosignInstalled
if ($script:HasCosign) {
    $cosignMajor = Get-CosignMajorVersion
    if ($cosignMajor -lt 2) {
        Write-Host "Warning: cosign version is less than 2.0.0, signature validation may fail." -ForegroundColor Yellow
    }
}

# Validate argument combinations
if ($Help -and ($List -or $Version -or $VerifySignatures)) {
    Write-Host "Error: incorrect arguments:" -ForegroundColor Red
    Print-Usage
    exit 1
}

if ($List -and ($Version -or $VerifySignatures)) {
    Write-Host "Error: incorrect arguments:" -ForegroundColor Red
    Print-Usage
    exit 1
}

if ($VerifySignatures -and -not $script:HasCosign) {
    Write-Host "Error: cosign is required for -VerifySignatures but is not installed." -ForegroundColor Red
    Write-Host "Go to https://github.com/sigstore/cosign to install it or run without the -VerifySignatures flag to install without signature verification."
    exit 1
}
elseif (-not $script:HasCosign) {
    Write-Host "Warning: cosign is required for -VerifySignatures but is not installed. Skipping signature validation." -ForegroundColor Yellow
    Write-Host "Go to https://github.com/sigstore/cosign to install it."
}

if ($Help) {
    Print-Usage
    exit 0
}

if ($List) {
    Write-Host "Available versions (latest 3):"
    Get-AvailableVersions | Select-Object -First 3 | ForEach-Object { Write-Host "  $_" }
    exit 0
}

# Determine version to install
if (-not $Version) {
    $Version = (Get-AvailableVersions | Select-Object -First 1)
    if (-not $Version) {
        Write-Host "Error: Could not determine latest version." -ForegroundColor Red
        exit 1
    }
}
else {
    Validate-Version -VersionToValidate $Version
}

Main -VersionToInstall $Version -DoVerifySignatures $VerifySignatures.IsPresent
