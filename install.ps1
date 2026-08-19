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
#    Currently only x86_64 builds are available. On ARM64 Windows, the x86_64
#    build is installed and runs under emulation.
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

# Session-global under irm | iex, like SecurityProtocol below; both are
# restored at the end of the run.
$originalErrorActionPreference = $ErrorActionPreference
$ErrorActionPreference = "Stop"

# PowerShell 5.1's SystemDefault protocol negotiation can be aborted by
# github.com ("connection was closed unexpectedly"), which also masks HTTP
# status codes such as 404. The same happens when TLS 1.3 is enabled, as
# .NET Framework's TLS 1.3 support is unreliable; force TLS 1.2 only.
# The setting is process-global, so it is restored at the end of the run:
# under the documented one-liners the script executes in the caller's
# session, which must not stay pinned to TLS 1.2.
$originalSecurityProtocol = [Net.ServicePointManager]::SecurityProtocol
if ($PSVersionTable.PSEdition -ne 'Core') {
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
}

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

# The version is interpolated into the install path and download URL, so accept
# only a release tag. Same semver shape as validate-inputs in rolling-release,
# with the suffix restricted to tag characters so no path separator gets through.
# [.] rather than \. to match the regex literal in install.sh; \z rather
# than $ because .NET $ accepts a trailing newline where bash rejects it.
$ReleaseTagRegex = '^v[0-9]+[.][0-9]+[.][0-9]+(-[A-Za-z0-9._-]+)?\z'

function Test-VersionFormat {
    param([string]$Version)

    if ($Version -cnotmatch $ReleaseTagRegex) {
        throw "Invalid version '$Version'. Expected a release tag such as v1.27.1."
    }
}

function Throw-FetchFailed {
    param([string]$Reason)

    if ($Reason) { throw "Failed to fetch available versions from GitHub: $Reason" }
    throw "Failed to fetch available versions from GitHub."
}

# Lists the Count most recent releases, newest first, marking pre-releases.
# Throws on failure so a caller mid-install can still clean up.
function Get-VersionList {
    param([int]$Count)

    try {
        $response = Invoke-RestMethod -Uri "https://api.github.com/repos/opengrep/opengrep/releases?per_page=$Count" -UseBasicParsing
    }
    catch {
        $reason = $_.Exception.Message
        if ($_.Exception.Response) {
            $status = [int]$_.Exception.Response.StatusCode
            # GitHub API error bodies explain the failure in a "message" field.
            $detail = $null
            try { $detail = (ConvertFrom-Json $_.ErrorDetails.Message).message } catch {}
            $reason = if ($detail) { "HTTP status $status ($detail)." } else { "HTTP status $status." }
        }
        Throw-FetchFailed $reason
    }
    # Tags that are not release tags are not listed.
    $versions = @($response | Where-Object { $_.tag_name -cmatch $ReleaseTagRegex } | ForEach-Object {
        if ($_.prerelease) { "$($_.tag_name) (pre-release)" } else { $_.tag_name }
    })
    if ($versions.Count -eq 0) {
        Throw-FetchFailed
    }
    return $versions
}

function Show-VersionList {
    param([int]$Count)

    Write-Host "Available versions (latest $Count):"
    Get-VersionList -Count $Count | ForEach-Object { Write-Host "  $_" }
}

# The /releases/latest web redirect never points to a pre-release or draft,
# and does not use the rate-limited API.
function Get-LatestVersion {
    try {
        $response = Invoke-WebRequest -Uri "https://github.com/opengrep/opengrep/releases/latest" -Method Head -UseBasicParsing
        $finalUri = if ($PSVersionTable.PSEdition -eq 'Core') {
            # PowerShell 7+ (HttpResponseMessage)
            $response.BaseResponse.RequestMessage.RequestUri.AbsoluteUri
        }
        else {
            # PowerShell 5.1 (HttpWebResponse)
            $response.BaseResponse.ResponseUri.AbsoluteUri
        }
        if ($finalUri -match '/releases/tag/([^/]+)$') {
            return $Matches[1]
        }
        throw "unexpected final URL $finalUri"
    }
    catch {
        throw "Failed to determine the latest version: $_"
    }
}

function Find-Cosign {
    # Check known binary names: official installs use 'cosign',
    # winget and manual downloads use 'cosign-windows-amd64'.
    foreach ($name in @('cosign', 'cosign-windows-amd64')) {
        $cmd = Get-Command $name -ErrorAction SilentlyContinue
        if ($null -ne $cmd) {
            return $cmd.Source
        }
    }
    return $null
}

function Test-CosignInstalled {
    return $null -ne (Find-Cosign)
}

function Get-CosignMajorVersion {
    $cosignPath = Find-Cosign
    if ($null -eq $cosignPath) { return $null }
    try {
        $versionOutput = & $cosignPath version 2>&1
        $versionLine = $versionOutput | Where-Object { $_ -match "GitVersion" }
        if ($versionLine -match "v?(\d+)") {
            return [int]$Matches[1]
        }
    }
    catch {}
    return $null
}

function Validate-Signature {
    param([string]$InstallPath)

    if ($script:HasCosign) {
        Write-Host "Verifying signatures for $InstallPath\opengrep.cert"
        $cosignPath = Find-Cosign
        # Run cosign in a child scope with $ErrorActionPreference = "Continue"
        # so that stderr text (e.g. "Verified OK") is not treated as a
        # terminating ErrorRecord under PS 5.1's global "Stop" preference.
        $result = & {
            $ErrorActionPreference = "Continue"
            & $cosignPath verify-blob `
                --cert "$InstallPath\opengrep.cert" `
                --signature "$InstallPath\opengrep.sig" `
                --certificate-identity-regexp "https://github.com/opengrep/opengrep.+" `
                --certificate-oidc-issuer "https://token.actions.githubusercontent.com" `
                "$InstallPath\opengrep.exe" 2>&1
        } | Out-String

        if ($LASTEXITCODE -eq 0) {
            Write-Host "Signature valid."
        }
        else {
            Write-Host $result.Trim()
            throw "Signature validation error."
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
    Remove-Item -Path "$InstallPath\opengrep.exe.download" -ErrorAction SilentlyContinue
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
        Write-Host "Warning: ARM64 Windows builds are not yet available. Installing x86_64 build (runs under emulation)." -ForegroundColor Yellow
        $dist = "opengrep_windows_x86.exe"
    }
    else {
        throw "Architecture '$arch' is unsupported."
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
            # The download also validates the version, avoiding the rate-limited
            # GitHub API: a 404 means the tag does not exist or has no asset for
            # this platform. A temporary name, renamed only on success, so an
            # interrupted download is never mistaken for an installed binary.
            $downloadPath = "$binaryPath.download"
            try {
                Invoke-WebRequest -Uri $url -OutFile $downloadPath -UseBasicParsing
            }
            catch {
                if ($_.Exception.Response.StatusCode -eq 404) {
                    Write-Host "Error: Version $VersionToInstall not found, or it has no $dist asset." -ForegroundColor Red
                    # A listing failure must not mask the diagnosis above.
                    try { Show-VersionList -Count 3 } catch { Write-Host "  ($_)" }
                    throw "Version not found"
                }
                throw "Failed to download $url`: $_"
            }
            Move-Item -Force -Path $downloadPath -Destination $binaryPath

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
            # Test by calling --version on the downloaded binary.
            # We route stderr through a temp file rather than using `2>&1`, so
            # that harmless runtime warnings (e.g. requests' RequestsDependency-
            # Warning) are not surfaced by PowerShell as NativeCommandError
            # records and misinterpreted as failures. We still surface them to
            # the user via Write-Host, and rely on $LASTEXITCODE to decide
            # whether the binary actually ran.
            $stderrFile = New-TemporaryFile
            try {
                $testOutput = & $binaryPath --version 2>$stderrFile
                $testExit   = $LASTEXITCODE
                $testStderr = (Get-Content -Raw -ErrorAction SilentlyContinue $stderrFile)
            }
            finally {
                Remove-Item -Force -ErrorAction SilentlyContinue $stderrFile
            }
            if ($testStderr) {
                Write-Host $testStderr
            }
            if ($testExit -ne 0 -or -not $testOutput) {
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

# Failures are thrown and reported once in the catch below; `exit` happens
# only at the very end, and only when running as a script file. Under the
# documented one-liners the script executes at session scope, where an
# early exit would close the caller's console; the failure is rethrown
# instead, so callers still observe a terminating error.
$failed = $false
try {
    # Check for cosign
    $script:HasCosign = Test-CosignInstalled

    # Validate argument combinations
    if (($Help -and ($List -or $Version -or $VerifySignatures)) -or
        ($List -and ($Version -or $VerifySignatures))) {
        Print-Usage
        throw "incorrect arguments"
    }

    if ($VerifySignatures -and -not $script:HasCosign) {
        throw "cosign is required for -VerifySignatures but is not installed.`nGo to https://github.com/sigstore/cosign to install it or run without the -VerifySignatures flag to install without signature verification."
    }
    elseif (-not $script:HasCosign) {
        Write-Host "Warning: cosign is required for -VerifySignatures but is not installed. Skipping signature validation." -ForegroundColor Yellow
        Write-Host "Go to https://github.com/sigstore/cosign to install it."
    }
    elseif ($script:HasCosign) {
        $cosignMajor = Get-CosignMajorVersion
        if ($null -eq $cosignMajor) {
            if ($VerifySignatures) {
                throw "could not determine cosign version and -VerifySignatures was requested.`nYour cosign binary may have been built without version metadata (e.g. distro packages).`nInstall cosign from https://github.com/sigstore/cosign or run without -VerifySignatures."
            }
            else {
                Write-Host "Warning: could not determine cosign version. Signature validation may not work correctly." -ForegroundColor Yellow
            }
        }
        elseif ($cosignMajor -lt 2) {
            Write-Host "Warning: cosign version is less than 2.0.0, signature validation may fail." -ForegroundColor Yellow
        }
    }

    if ($Help) {
        Print-Usage
    }
    elseif ($List) {
        Show-VersionList -Count 3
    }
    else {
        # Determine version to install; an explicit empty -Version is
        # rejected by the format check, as in install.sh
        if (-not $PSBoundParameters.ContainsKey('Version')) {
            $Version = Get-LatestVersion
        }
        Test-VersionFormat -Version $Version
        Main -VersionToInstall $Version -DoVerifySignatures $VerifySignatures.IsPresent
    }
}
catch {
    if ($MyInvocation.MyCommand.Path) {
        Write-Host "Error: $_" -ForegroundColor Red
        $failed = $true
    }
    else {
        throw
    }
}
finally {
    $ErrorActionPreference = $originalErrorActionPreference
    if ($PSVersionTable.PSEdition -ne 'Core') {
        [Net.ServicePointManager]::SecurityProtocol = $originalSecurityProtocol
    }
}

if ($failed) {
    exit 1
}
