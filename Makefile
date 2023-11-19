# # $File$
# 
# Summary: Makefile for all components of flt, including tests
# Standard: Fortran 90, ELF90 subset
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Last updated: $Date$
# Revision: $Revision$
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# TODO: Add code coverage.
# TODO: Add other compilers, particularly ELF90.

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

#############
# Compilers #
#############

# gfortran

FC        = gfortran
FFLAGS    = -Wall -Wextra -Werror -pedantic-errors -std=f95 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
DBGFLAGS  = -Og -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal
# --coverage
# -fsanitize=leak doesn't work on trident for some reason. It does work on bison.
BINEXT    = 
RUN       = ./
RM        = rm -rfv
OFLAG     = -o 
OBJEXT    = o
OBJFLAGS  = -c -o 
DBGOBJEXT = -dbg.$(OBJEXT)

# ELF90

ELF90RM = *.exe *.lib *.map *.mod modtable.txt test/*.obj

#FC        = wine elf90
#FFLAGS    = -npause -fullwarn -winconsole
#DBGFLAGS  = 
#BINEXT    = .exe
#RUN       = wine 
#RM        = rm -rfv
#OFLAG     = -out 
#OBJEXT    = lib
#OBJFLAGS  = 
#DBGOBJEXT = .lib

###############
# Boilerplate #
###############

.PHONY: all
all: test

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT) $(DBGOBJEXT)

.PHONY: clean
clean:
	$(RM) $(ELF90RM) test_* *.mod src/*.$(OBJEXT) src/*$(DBGOBJEXT)

.f90$(DBGOBJEXT):
	$(FC) $(OBJFLAGS)$@ $(FFLAGS) $(DBGFLAGS) $<

.PHONY: test
test: dimmod.jsonl

###################
# Other compilers #
###################

.PHONY: elf90
elf90:
	$(MAKE) test FC='wine elf90' FFLAGS='-npause -fullwarn -winconsole' DBGFLAGS='' BINEXT='.exe' RUN='wine ' OFLAG='-out ' OBJEXT='lib' OBJFLAGS='' DBGOBJEXT='.lib'

############
# dimcheck #
############

src/dimmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

DIMMOD_TEST_DEPS = src/dimmod$(DBGOBJEXT) src/prec$(DBGOBJEXT)

test_dimmod$(BINEXT): $(DIMMOD_TEST_DEPS)
	$(FC) $(OFLAG)test_dimmod$(BINEXT) $(FFLAGS) $(DBGFLAGS) $(DIMMOD_TEST_DEPS) test/test_dimmod.f90

dimmod.jsonl: test_dimmod$(BINEXT)
	$(RUN)test_dimmod$(BINEXT)
	test ! -e fort.*
