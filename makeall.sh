#!/usr/bin/env bash

# This runs against all the Makefiles on Linux.

if test "$BASH" = "" || "$BASH" -uc "a=();true \"\${a[@]}\"" 2>/dev/null; then
    # Bash 4.4, Zsh
    set -euo pipefail
else
    # Bash 4.3 and older chokes on empty arrays with set -u.
    set -eo pipefail
fi
set -o noclobber
shopt -s nullglob globstar

make FC=gfortran BUILD=debug test
make clean
make FC=gfortran BUILD=release test
make clean
make FC=ifx BUILD=debug test
make clean
make FC=ifx BUILD=release test
make clean
make FC=ifort BUILD=debug test
make clean
make FC=ifort BUILD=release test
make clean
make FC=nvfortran BUILD=debug test
make clean
make FC=nvfortran BUILD=release test
make clean

bmake -f BSDmakefile FC=gfortran BUILD=debug test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=gfortran BUILD=release test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifx BUILD=debug test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifx BUILD=release test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifort BUILD=debug test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifort BUILD=release test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=nvfortran BUILD=debug test
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=nvfortran BUILD=release test
bmake -f BSDmakefile clean

pdpmake -f PDPmakefile test
pdpmake -f PDPmakefile clean
