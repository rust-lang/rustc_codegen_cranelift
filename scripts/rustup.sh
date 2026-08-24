#!/usr/bin/env bash

set -e

TOOLCHAIN=${TOOLCHAIN:-$(date +%Y-%m-%d)}

case $1 in
    "prepare")
        echo "=> Installing new nightly"
        rustup toolchain install --profile minimal "nightly-${TOOLCHAIN}" # Sanity check to see if the nightly exists
        sed -i "s/\"nightly-.*\"/\"nightly-${TOOLCHAIN}\"/" rust-toolchain.toml

        echo "=> Uninstalling all old nightlies"
        for nightly in $(rustup toolchain list | grep nightly | grep -v "$TOOLCHAIN" | grep -v nightly-x86_64); do
            rustup toolchain uninstall "$nightly"
        done

        ./clean_all.sh

        ./y.sh prepare
        ;;
    "commit")
        git add rust-toolchain.toml
        git commit -m "Rustup to $(rustc -V)"
        ;;
    "push")
        branch=sync_cg_clif-$(date +%Y-%m-%d)
        rustc-josh-sync push "${branch}" bjorn3
	;;
    "pull")
        RUST_VERS=$(curl "https://static.rust-lang.org/dist/$TOOLCHAIN/channel-rust-nightly-git-commit-hash.txt")
        echo "Pulling $RUST_VERS ($TOOLCHAIN)"

        branch=rustc-pull-$(date +%Y-%m-%d)
        git checkout -b "${branch}"
        rustc-josh-sync pull --upstream-commit "${RUST_VERS}"
        ;;
    *)
        echo "Unknown command '$1'"
        echo "Usage: ./rustup.sh prepare|commit|push|pull"
        ;;
esac
