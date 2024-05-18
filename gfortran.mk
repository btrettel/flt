# -fsanitize=leak doesn't work on gfortran 9. Add it after upgrading.

FFLAGS   = -Wall -Wextra -Werror -pedantic-errors -Wno-maybe-uninitialized -std=f2018 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DBGFLAGS = -Og -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal --coverage
RFLAGS   = -O2

# <https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html>
