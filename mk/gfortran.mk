# `-fsanitize=leak` doesn't work on gfortran 9. Add it after upgrading.
# `-Werror=uninitialized` seems to require `-fcheck=all` (not used in the release build), or else there's a false-positive compile-time error, at least for gfortran 9. Check if this is true for later versions. `-Wno-uninitialized` eliminates this false-positive and should be removed in the future if gfortran eliminates this problem.

FFLAGS = -Wall -Wextra -Werror -pedantic-errors -Wno-maybe-uninitialized -std=f2018 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DFLAGS = -Og -g -fbacktrace -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow,denormal --coverage
RFLAGS = -O2 -Wno-uninitialized -fopt-info-missed=$(MISSED) -fopenmp

# <https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html>

# TODO: `-funroll`
