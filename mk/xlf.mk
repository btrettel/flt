FFLAGS = -qlanglvl=2008pure
DFLAGS = -g2 -qcheck=all -qflttrap=inexact:invalid:overflow:underflow:zerodivide
RFLAGS = -O2 -qreport=hotlist -qarch=qauto

# `-qfree=f90`: free form, default for xlf2008
# `-qsmp=omp`: Will this enable parallelization for `do concurrent`?
