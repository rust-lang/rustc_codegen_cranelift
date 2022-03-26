# Note to people running shellcheck: this file should only be sourced, not executed directly.

set -e

export LD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH"
export DYLD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$DYLD_LIBRARY_PATH"
export PATH="$(rustc --print sysroot)/bin;$PATH"
ls $(rustc --print sysroot)/bin
