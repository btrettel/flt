# GNU Make on Linux

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# defaults
BUILD = debug
FC    = gfortran

#############
# Compilers #
#############

# gfortran
ifeq ($(FC),gfortran)
FFLAGS   = -Wall -Wextra -Werror -pedantic-errors -Wno-maybe-uninitialized -std=f2018 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DBGFLAGS = -Og -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal --coverage
# -fsanitize=leak doesn't work on gfortran 9.

# ifx
# `-check uninit` has false positives with namelists for the moment.
# <https://community.intel.com/t5/Intel-Fortran-Compiler/Known-bug-with-check-all-or-check-uninit-in-ifx-2024-0-0-for/m-p/1545825>
else ifeq ($(FC),ifx)
FFLAGS   = -warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand:f18 -diag-error-limit=1 -init=snan,arrays
DBGFLAGS = -O0 -g -traceback -debug full -check all,nouninit -fpe0

# ifort
# ifort is here due to possible performance benefits on x86.
# `-init=snan,arrays` seems to lead to false positives with ifort.
else ifeq ($(FC),ifort)
FFLAGS   = ifort FFLAGS='-diag-disable=10448 -warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand f18 -diag-error-limit=1
DBGFLAGS = -O0 -g -traceback -debug full -check all -fpe0

# nvfortran
else ifeq ($(FC),nvfortran)
FFLAGS   = -Minform=inform -Werror
DBGFLAGS = -g

# LATER: FC=lfortran, particularly for the style suggestions
# FFLAGS   = --link-with-gcc
# DBGFLAGS =

endif

ifeq ($(BUILD),debug)
FFLAGS += $(DBGFLAGS)
else ifeq ($(BUILD),debug)
FFLAGS += $(RFLAGS)
else
$(error Set BUILD to either debug or release. BUILD=$(BUILD))
endif

include linux.mk
include common.mk
