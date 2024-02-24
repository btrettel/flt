# # Makefile
# 
# Summary: Makefile for all components of flt, including tests
# Standard: POSIX (tested on GNU Make and bmake)
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# TODO: Add linters before compilation. Lint each file before compiling it.
# TODO: Camfort
# LATER: lfortran, particularly for the style suggestions
# TODO: Figure out how to automate parts like `test/test_ga.f90` in `test_ga$(BINEXT):`
# TODO: nvfortran to replace flang-7. <https://docs.nvidia.com/hpc-sdk//index.html>
# TODO: Add code coverage.
# TODO: Valgrind to detect uninitialized variables. https://stackoverflow.com/a/52455413
# TODO: Check other Makefiles to see which flags you use there.

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
FFLAGS    = -Wall -Wextra -Werror -pedantic-errors -std=f2008 -Wconversion -Wconversion-extra -fimplicit-none -fmax-errors=1 -fno-unsafe-math-optimizations -finit-real=snan -finit-integer=-2147483647 -finit-logical=true -finit-derived -Wimplicit-interface -Wunused -ffree-line-length-132
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
	$(MAKE) test
	$(MAKE) clean
	$(MAKE) ifort
	$(MAKE) clean
	$(MAKE) ifx
	$(MAKE) clean
	$(MAKE) flang-7
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
test: checks.nml dimmod.nml ga.nml nmllog.nml prec.nml rngmod.nml unittest.nml
	@echo "*********************"
	@echo "* All tests passed. *"
	@echo "*********************"
	@echo "Compiler: $(FC)"

###################
# Other compilers #
###################

# `-init=snan,arrays` leads to false positives. Probably of no consequence as ifort is being retired. There is no problem with ifx.
.PHONY: ifort
ifort:
	$(MAKE) test FC=ifort FFLAGS='-warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand f08 -diag-error-limit=1' DBGFLAGS='-O0 -g -traceback -debug full -check all -fpe0'

.PHONY: ifx
ifx:
	$(MAKE) test FC=ifx FFLAGS='-warn errors -warn all -diag-error=remark,warn,error -fltconsistency -stand:f08 -diag-error-limit=1 -init=snan,arrays' DBGFLAGS='-O0 -g -traceback -debug full -check all -fpe0'

.PHONY: flang-7
flang-7:
	$(MAKE) test FC=flang-7 FFLAGS='-Wdeprecated' DBGFLAGS='-g'

#.PHONY: lfortran
#lfortran:
#	$(MAKE) test FC=lfortran FFLAGS='--link-with-gcc' DBGFLAGS=''

################
# Dependencies #
################

src/checks$(DBGOBJEXT): src/nmllog$(DBGOBJEXT) src/prec$(DBGOBJEXT)

src/dimmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/ga$(DBGOBJEXT): src/prec$(DBGOBJEXT) src/rngmod$(DBGOBJEXT)

src/nmllog$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/prec$(DBGOBJEXT):

src/rngmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/unittest$(DBGOBJEXT): src/checks$(DBGOBJEXT) src/nmllog$(DBGOBJEXT) src/prec$(DBGOBJEXT)

##########
# checks #
##########

test_checks$(BINEXT): src/checks$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_checks.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_checks.f90

checks.nml: test_checks$(BINEXT)
	$(RUN)test_checks$(BINEXT)
	#python3 test/passed.py $@
	#python3 test/test_checks.py
	test ! -e fort.*

############
# dimcheck #
############

test_dimmod$(BINEXT): src/dimmod$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_dimmod.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_dimmod.f90

dimmod.nml: test_dimmod$(BINEXT)
	$(RUN)test_dimmod$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

######
# ga #
######

test_ga$(BINEXT): src/ga$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_ga.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_ga.f90

ga.nml: test_ga$(BINEXT)
	$(RUN)test_ga$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

##########
# nmllog #
##########

test_nmllog$(BINEXT): src/nmllog$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_nmllog.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_nmllog.f90

nmllog.nml: test_nmllog$(BINEXT)
	$(RUN)test_nmllog$(BINEXT)
	#python3 test/passed.py $@
	#python3 test/test_logging.py
	test ! -e fort.*

########
# prec #
########

test_prec$(BINEXT): src/prec$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_prec.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_prec.f90

prec.nml: test_prec$(BINEXT)
	$(RUN)test_prec$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

##########
# rngmod #
##########

test_rngmod$(BINEXT): src/rngmod$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_rngmod.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_rngmod.f90

rngmod.nml: test_rngmod$(BINEXT)
	$(RUN)test_rngmod$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

############
# unittest #
############

test_unittest$(BINEXT): src/unittest$(DBGOBJEXT) test/test_unittest.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_unittest.f90

unittest.nml: test_unittest$(BINEXT)
	$(RUN)test_unittest$(BINEXT)
	#python3 test/passed.py $@
	#python3 test/test_unittest.py
	test ! -e fort.*
