# # $File$
# 
# Summary: Makefile for dimcheck, including tests
# Standard: Fortran 90, ELF90 subset
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Last updated: $Date$
# Revision: $Revision$
# Project: [dimcheck](https://github.com/btrettel/dimcheck)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# -fsanitize=leak doesn't work on trident for some reason. It does work on bison.
FC        = gfortran
FFLAGS    = -Wall -Wextra -Werror -pedantic-errors -std=f95 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DBGFLAGS  = -Og -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal --coverage
BINEXT    = 
RUN       = ./
OBIN      = dimgen$(BINEXT)
OFLAG     = -o 
TESTBIN   = tests$(BINEXT)
OBJEXT    = o
OBJFLAGS  = -c -o 
DEPS      = dimmod.$(OBJEXT)
DBGBIN    = dimgen-dbg$(BINEXT)
DBGOBJEXT = -dbg.$(OBJEXT)
DBGDEPS   = dimmod$(DBGOBJEXT)
SRC       = gen.f90
TESTDEPS  = 
TESTSRC   = tests.f90
RM        = rm -rfv

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT) $(DBGOBJEXT)

$(OBIN): $(DEPS) $(SRC)
	$(FC) $(OFLAG)$(OBIN) $(FFLAGS) $(DEPS) $(SRC)

.f90.$(OBJEXT):
	$(FC) $(OBJFLAGS)$@ $(FFLAGS) $<

.PHONY: clean
clean:
	$(RM) *.cmdx *.cmod *.d *.dbg *.ERR *.exe *.EXE fail *.FPI *.fpl *.FPT *.gcda *.gcno *.gcov html-cov/ *.ilm *.info *.lib *.map *.mod *.MOD modtable.txt modsema4.txt *.o *.obj *.pc *.pcl *.s *.stb $(OBIN) $(DBGBIN) $(TESTBIN) *.log fort.* *.jsonl

$(DBGBIN): $(SRC) $(DBGDEPS)
	$(FC) $(OFLAG)$(DBGBIN) $(FFLAGS) $(DBGFLAGS) $(DBGDEPS) $(SRC)

.PHONY: debug
debug: $(DBGBIN)

.f90$(DBGOBJEXT):
	$(FC) $(OBJFLAGS)$@ $(FFLAGS) $(DBGFLAGS) $<

$(TESTBIN): $(TESTSRC) $(DBGDEPS) $(TESTDEPS)
	$(FC) $(OFLAG)$(TESTBIN) $(FFLAGS) $(DBGFLAGS) $(DBGDEPS) $(TESTDEPS) $(TESTSRC)

.PHONY: test
test: $(TESTBIN)
	$(RM) *.gcda *.gcno *.gcov html-cov/
	$(RUN)$(TESTBIN)
	test ! -e fort.*

tests.$(OBJEXT): dimmod.$(OBJEXT)

tests$(DBGOBJEXT): dimmod$(DBGOBJEXT)
