#!/usr/bin/env bash
set -e

./y.exe build --sysroot none "$@"

scripts/tests.sh no_sysroot

./y.exe build "$@"

scripts/tests.sh base_sysroot
scripts/tests.sh extended_sysroot
