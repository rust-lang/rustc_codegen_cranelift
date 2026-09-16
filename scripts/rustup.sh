#!/usr/bin/env bash

set -e

TOOLCHAIN=${TOOLCHAIN:-$(date +%Y-%m-%d)}

case $1 in
    "prepare")
        ./clean_all.sh

        ./y.sh prepare
        ;;
    "push")
        username=${2:-bjorn3}
        branch=sync_cg_clif-$(date +%Y-%m-%d)
        rustc-josh-sync push "$branch" "$username"
	;;
    "pull")
        git checkout -b sync_from_rust
        rustc-josh-sync pull
        ;;
    *)
        echo "Unknown command '$1'"
        echo "Usage: ./rustup.sh prepare|pull|push <fork>"
        ;;
esac
