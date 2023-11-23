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
# TODO: Add other compilers.

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

ELF90RM = *.exe *.lib *.map *.mod modtable.txt test/*.obj

FAILDBGOBJ = src/fail$(DBGOBJEXT)

###############
# Boilerplate #
###############

.PHONY: all
all: test

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT) $(DBGOBJEXT)

.PHONY: clean
clean:
	$(RM) $(ELF90RM) *.jsonl *.mod test_* src/*.$(OBJEXT) src/*$(DBGOBJEXT)

.f90$(DBGOBJEXT):
	$(FC) $(OBJFLAGS)$@ $(FFLAGS) $(DBGFLAGS) $<

.PHONY: test
test: asserts.jsonl dimmod.jsonl testmod.jsonl

###################
# Other compilers #
###################

.PHONY: elf90
elf90:
	$(MAKE) test FC='wine elf90' FFLAGS='-npause -fullwarn -winconsole' DBGFLAGS='' BINEXT='.exe' RUN='wine ' OFLAG='-out ' OBJEXT='lib' OBJFLAGS='' DBGOBJEXT='.lib' FAILDBGOBJ='src/fail_elf.lib'

################
# Dependencies #
################

src/asserts$(DBGOBJEXT): src/logging$(DBGOBJEXT) src/prec$(DBGOBJEXT)

src/dimmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/fail$(DBGOBJEXT):

src/logging$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/prec$(DBGOBJEXT):

src/testmod$(DBGOBJEXT): src/asserts$(DBGOBJEXT) $(FAILDBGOBJ) src/logging$(DBGOBJEXT) src/prec$(DBGOBJEXT)

###########
# asserts #
###########

TEST_ASSERTS_DEPS = src/asserts$(DBGOBJEXT) $(FAILDBGOBJ) src/prec$(DBGOBJEXT) src/logging$(DBGOBJEXT) src/testmod$(DBGOBJEXT)

test_asserts$(BINEXT): $(TEST_ASSERTS_DEPS)
	$(FC) $(OFLAG)test_asserts$(BINEXT) $(FFLAGS) $(DBGFLAGS) $(TEST_ASSERTS_DEPS) test/test_asserts.f90

asserts.jsonl: test_asserts$(BINEXT)
	$(RUN)test_asserts$(BINEXT)
	test ! -e fort.*

############
# dimcheck #
############

TEST_DIMMOD_DEPS = src/asserts$(DBGOBJEXT) src/dimmod$(DBGOBJEXT) $(FAILDBGOBJ) src/prec$(DBGOBJEXT) src/logging$(DBGOBJEXT) src/testmod$(DBGOBJEXT)

test_dimmod$(BINEXT): $(TEST_DIMMOD_DEPS)
	$(FC) $(OFLAG)test_dimmod$(BINEXT) $(FFLAGS) $(DBGFLAGS) $(TEST_DIMMOD_DEPS) test/test_dimmod.f90

dimmod.jsonl: test_dimmod$(BINEXT)
	$(RUN)test_dimmod$(BINEXT)
	test ! -e fort.*

###########
# testmod #
###########

TEST_TESTMOD_DEPS = src/asserts$(DBGOBJEXT) $(FAILDBGOBJ) src/prec$(DBGOBJEXT) src/logging$(DBGOBJEXT) src/testmod$(DBGOBJEXT)

test_testmod$(BINEXT): $(TEST_TESTMOD_DEPS)
	$(FC) $(OFLAG)test_testmod$(BINEXT) $(FFLAGS) $(DBGFLAGS) $(TEST_TESTMOD_DEPS) test/test_testmod.f90

testmod.jsonl: test_testmod$(BINEXT)
	$(RUN)test_testmod$(BINEXT)
	test ! -e fort.*
