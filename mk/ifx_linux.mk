# `-check uninit` has false positives with namelists for the moment.
# <https://community.intel.com/t5/Intel-Fortran-Compiler/Known-bug-with-check-all-or-check-uninit-in-ifx-2024-0-0-for/m-p/1545825>

FFLAGS   = -warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand:f18 -diag-error-limit=1 -init=snan,arrays
DBGFLAGS = -O0 -g -traceback -debug full -check all,nouninit -fpe0
RFLAGS   = -O2 -qopt-report -fiopenmp

# TODO: Ask question about `do concurrent` on Intel Fortran forum. What's the right compiler flag? `-parallel` isn't for ifx contrary to what was said there. Update this afterward.
