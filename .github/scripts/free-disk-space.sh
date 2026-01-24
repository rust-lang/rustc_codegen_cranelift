#!/bin/bash
# Script to free disk space for CI runners.
# Ported from rust-lang/rust (src/ci/scripts/free-disk-space-linux.sh)

set -euo pipefail

# Print a line of the specified character
printSeparationLine() {
    for ((i = 0; i < 80; i++)); do
        printf "%s" "$1"
    done
    printf "\n"
}

# Get available disk space in KB
getAvailableSpace() {
    df -a | awk 'NR > 1 {avail+=$4} END {print avail}'
}

# Format byte count to human readable
formatByteCount() {
    numfmt --to=iec-i --suffix=B --padding=7 "${1}000"
}

# Macro to output saved space
printSavedSpace() {
    local before=${1}
    local title=${2:-}

    local after
    after=$(getAvailableSpace)
    local saved=$((after - before))

    if [ "$saved" -lt 0 ]; then
        echo "::warning::Saved space is negative: $saved. Using '0' as saved space."
        saved=0
    fi

    echo ""
    printSeparationLine "*"
    if [ -n "${title}" ]; then
        echo "=> ${title}: Saved $(formatByteCount "$saved")"
    else
        echo "=> Saved $(formatByteCount "$saved")"
    fi
    printSeparationLine "*"
    echo ""
}

# Print output of df with caption
printDF() {
    local caption=${1}

    printSeparationLine "="
    echo "${caption}"
    echo ""
    echo "$ df -h"
    echo ""
    df -h
    printSeparationLine "="
}

# Remove unused files and directories
removeUnusedFilesAndDirs() {
    local to_remove=(
        "/usr/share/java"
        "/usr/local/aws-sam-cli"
        "/usr/local/doc/cmake"
        "/usr/local/julia"*
        "/usr/local/lib/android"
        "/usr/local/share/chromedriver-"*
        "/usr/local/share/chromium"
        "/usr/local/share/cmake-"*
        "/usr/local/share/edge_driver"
        "/usr/local/share/emacs"
        "/usr/local/share/gecko_driver"
        "/usr/local/share/icons"
        "/usr/local/share/powershell"
        "/usr/local/share/vcpkg"
        "/usr/local/share/vim"
        "/usr/share/apache-maven-"*
        "/usr/share/gradle-"*
        "/usr/share/kotlinc"
        "/usr/share/miniconda"
        "/usr/share/php"
        "/usr/share/ri"
        "/usr/share/swift"
        # binaries
        "/usr/local/bin/azcopy"
        "/usr/local/bin/bicep"
        "/usr/local/bin/ccmake"
        "/usr/local/bin/cmake-"*
        "/usr/local/bin/cmake"
        "/usr/local/bin/cpack"
        "/usr/local/bin/ctest"
        "/usr/local/bin/helm"
        "/usr/local/bin/kind"
        "/usr/local/bin/kustomize"
        "/usr/local/bin/minikube"
        "/usr/local/bin/packer"
        "/usr/local/bin/phpunit"
        "/usr/local/bin/pulumi-"*
        "/usr/local/bin/pulumi"
        "/usr/local/bin/stack"
        # Haskell runtime
        "/usr/local/.ghcup"
        # Azure
        "/opt/az"
        "/usr/share/az_"*
    )

    if [ -n "${AGENT_TOOLSDIRECTORY:-}" ]; then
        to_remove+=("${AGENT_TOOLSDIRECTORY}")
    fi

    for element in "${to_remove[@]}"; do
        if [ ! -e "$element" ]; then
            echo "::warning::Directory or file $element does not exist, skipping."
        fi
    done

    # Remove all files and directories at once to save time.
    sudo rm -rf "${to_remove[@]}" || true
}

# Clean unused apt packages
cleanPackages() {
    local packages=(
        '^aspnetcore-.*'
        '^dotnet-.*'
        '^llvm-.*'
        '^mongodb-.*'
        'firefox'
        'libgl1-mesa-dri'
        'mono-devel'
        'php.*'
        'azure-cli'
        'google-chrome-stable'
        'google-cloud-cli'
        'google-cloud-sdk'
        'powershell'
    )

    WAIT_DPKG_LOCK="-o DPkg::Lock::Timeout=60"
    sudo apt-get ${WAIT_DPKG_LOCK} -qq remove -y --fix-missing "${packages[@]}" || true
    sudo apt-get ${WAIT_DPKG_LOCK} autoremove -y || echo "::warning::apt-get autoremove failed"
    sudo apt-get ${WAIT_DPKG_LOCK} clean || echo "::warning::apt-get clean failed"
}

# Clean Docker images
cleanDocker() {
    echo "=> Removing docker images..."
    sudo docker image prune --all --force || true
}

# Remove Swap storage
cleanSwap() {
    sudo swapoff -a || true
    sudo rm -rf /mnt/swapfile || true
}

# Execute operation and measure space change
execAndMeasureSpaceChange() {
    local operation=${1}
    local title=${2}

    local before
    before=$(getAvailableSpace)
    $operation

    printSavedSpace "$before" "$title"
}

# Main execution
AVAILABLE_INITIAL=$(getAvailableSpace)

printDF "BEFORE CLEAN-UP:"
echo ""
execAndMeasureSpaceChange cleanPackages "Unused packages"
execAndMeasureSpaceChange cleanDocker "Docker images"
execAndMeasureSpaceChange cleanSwap "Swap storage"
execAndMeasureSpaceChange removeUnusedFilesAndDirs "Unused files and directories"

# Output saved space statistic
echo ""
printDF "AFTER CLEAN-UP:"

echo ""
echo ""

printSavedSpace "$AVAILABLE_INITIAL" "Total saved"
