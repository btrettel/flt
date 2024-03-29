# # Makefile
# 
# Summary: Makefile for all components of flt, including tests
# Standard: POSIX (tested on GNU Make and bmake)
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# TODO: Add code coverage.
# TODO: Valgrind to detect uninitialized variables and more. <https://stackoverflow.com/a/52455413>, <https://stackoverflow.com/a/44989219>
# TODO: Add linters before compilation. Lint each file before compiling it.
# TODO: Camfort
# TODO: Figure out how to automate parts like `test/test_ga.f90` in `test_ga$(BINEXT):`
# TODO: Check other Makefiles to see which flags you use there.

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# Add later: dimmod.nml ga.nml rngmod.nml
NML = checks.nml nmllog.nml prec.nml unittest.nml
.PRECIOUS: $(NML)

#############
# Compilers #
#############

# gfortran

FC        = gfortran
FFLAGS    = -Wall -Wextra -Werror -pedantic-errors -Wno-maybe-uninitialized -std=f2018 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
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

###############
# Boilerplate #
###############

.PHONY: all
all:
	$(MAKE) test # gfortran
	$(MAKE) clean
	$(MAKE) ifx
	$(MAKE) clean
	$(MAKE) nvfortran
	$(MAKE) clean
	@echo "***************************************"
	@echo "* All tests passed for all compilers. *"
	@echo "***************************************"

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT) $(DBGOBJEXT)

.PHONY: clean
clean:
	$(RM) *.nml *.mod test_* src/*.$(OBJEXT) src/*$(DBGOBJEXT) *.dbg

# TODO: `.f90$(OBJEXT):`

.f90$(DBGOBJEXT):
	$(FC) $(OBJFLAGS) $@ $(FFLAGS) $(DBGFLAGS) $<

.PHONY: test
test: $(NML)
	@echo "*********************"
	@echo "* All tests passed. *"
	@echo "*********************"
	@echo "Compiler: $(FC)"

###################
# Other compilers #
###################

# `-check uninit` has false positives on Linux for the moment.
# <https://community.intel.com/t5/Intel-Fortran-Compiler/Known-bug-with-check-all-or-check-uninit-in-ifx-2024-0-0-for/m-p/1545825>
.PHONY: ifx
ifx:
	$(MAKE) test FC=ifx FFLAGS='-warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand:f18 -diag-error-limit=1 -init=snan,arrays' DBGFLAGS='-O0 -g -traceback -debug full -check all,nouninit -fpe0'

.PHONY: nvfortran
nvfortran:
	$(MAKE) test FC=nvfortran FFLAGS='-Minform=inform -Werror' DBGFLAGS='-g'

# LATER: lfortran, particularly for the style suggestions
#.PHONY: lfortran
#lfortran:
#	$(MAKE) test FC=lfortran FFLAGS='--link-with-gcc' DBGFLAGS=''

################
# Dependencies #
################

src/checks$(DBGOBJEXT): src/nmllog$(DBGOBJEXT) src/prec$(DBGOBJEXT)

src/nmllog$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/prec$(DBGOBJEXT):

src/unittest$(DBGOBJEXT): src/checks$(DBGOBJEXT) src/nmllog$(DBGOBJEXT) src/prec$(DBGOBJEXT)

##########
# checks #
##########

test_checks$(BINEXT): src/checks$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_checks.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_checks.f90

checks.nml: test_checks$(BINEXT)
	$(RUN)test_checks$(BINEXT)
	test ! -e fort.*

##########
# nmllog #
##########

test_nmllog$(BINEXT): src/nmllog$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_nmllog.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_nmllog.f90

nmllog.nml: test_nmllog$(BINEXT)
	$(RUN)test_nmllog$(BINEXT)
	test ! -e fort.*

########
# prec #
########

test_prec$(BINEXT): src/prec$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_prec.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_prec.f90

prec.nml: test_prec$(BINEXT)
	$(RUN)test_prec$(BINEXT)
	test ! -e fort.*

############
# unittest #
############

test_unittest$(BINEXT): src/unittest$(DBGOBJEXT) test/test_unittest.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_unittest.f90

unittest.nml: test_unittest$(BINEXT) prec.nml
	$(RUN)test_unittest$(BINEXT)
	test ! -e fort.*
