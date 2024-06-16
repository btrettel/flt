FFLAGS = -qlanglvl=2008pure -qwarn64 -qinfo=all -qsuppress=1512-045:1521-004:1516-329 -qsclk=micro
DFLAGS = -g2 -qcheck=all -qflttrap=inexact:invalid:overflow:underflow:zerodivide
RFLAGS = -O2 -qreport=hotlist -qarch=qauto

# `-qsuppress` explanations:
# `1512-045`: IBM XL Fortran doesn't seem to support `g0` edit descriptors, so I disable the message for this.
# `1521-004`: Who cares about host association?
# `1516-329`: Seems to have false positives for using variables before setting them.

# `-qsclk=micro`: Needed for test `timeit (absolute)` to pass.

# `-qfree=f90`: free form, default for xlf2008
# `-qsmp=omp`: Will this enable parallelization for `do concurrent`?
# `-qhalt=i`: I would like to include this, but reducing the level to even `-qhalt=e` causes FLT to not compile despite not printing any messages!?!
