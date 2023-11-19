# # $File$
# 
# Summary: Makefile for flt, including tests
# Standard: Fortran 90, ELF90 subset
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Last updated: $Date$
# Revision: $Revision$
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

FC       = gfortran
FFLAGS   = -Wall -Wextra -Werror -pedantic-errors -std=f95 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DBGFLAGS = -Og -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal --coverage
# -fsanitize=leak doesn't work on trident for some reason. It does work on bison.
BINEXT   = 
RUN      = ./
RM       = rm -rfv
OFLAG    = -o 
OBJEXT   = o
OBJFLAGS = -c -o 

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT) $(DBGOBJEXT)

.PHONY: clean
clean:
	$(RM) *.$(OBJEXT) *$(DBGOBJEXT)

.f90$(DBGOBJEXT):
	$(FC) $(OBJFLAGS)$@ $(FFLAGS) $(DBGFLAGS) src/$<

test_dimmod$(BINEXT): dimmod$(DBGOBJEXT)
	$(FC) $(OFLAG)test_dimmod$(BINEXT) $(FFLAGS) $(DBGFLAGS) tests/test_dimmod.f90

dimmod.jsonl: test_dimmod$(BINEXT)
	$(RM) *.gcda *.gcno *.gcov html-cov/
	$(RUN)test_dimmod$(BINEXT)
	test ! -e fort.*

.PHONY: test
test: dimmod.jsonl
