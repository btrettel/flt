# ifort is here due to possible performance benefits on x86.
# `-init=snan,arrays` seems to lead to false positives with ifort.

FFLAGS   = -diag-disable=10448 -warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand f18 -diag-error-limit=1
DBGFLAGS = -O0 -g -traceback -debug full -check all -fpe0
RFLAGS   = -O2 -qopenmp -parallel
