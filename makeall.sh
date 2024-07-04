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
set -x

make FC=gfortran BUILD=debug check
make clean
make FC=gfortran BUILD=release check
make clean
make FC=ifx BUILD=debug check
make clean
make FC=ifx BUILD=release check
make clean
make FC=ifort BUILD=debug check
make clean
make FC=ifort BUILD=release check
make clean
make FC=nvfortran BUILD=debug check
make clean
make FC=nvfortran BUILD=release check
make clean

bmake -f BSDmakefile FC=gfortran BUILD=debug check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=gfortran BUILD=release check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifx BUILD=debug check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifx BUILD=release check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifort BUILD=debug check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=ifort BUILD=release check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=nvfortran BUILD=debug check
bmake -f BSDmakefile clean
bmake -f BSDmakefile FC=nvfortran BUILD=release check
bmake -f BSDmakefile clean

pdpmake -f PDPmakefile check
pdpmake -f PDPmakefile clean
