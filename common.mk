# Summary: Common Makefile for all components of flt, including tests
# Standard: POSIX (tested on GNU Make and bmake)
# Preprocessor: none
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# Add later: dimmod.nml ga.nml
TESTNML = autodiff.nml checks.nml genunits_data.nml genunits_io.nml nmllog.nml prec.nml purerng.nml timer.nml units.nml unittest.nml
.PRECIOUS: $(TESTNML)

###############
# Boilerplate #
###############

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT)

.f90.$(OBJEXT):
	$(FC) $(OBJFLAGS) $@ $(FFLAGS) $<

.PHONY: test
test: $(TESTNML)
	@echo =====================
	@echo = All tests passed. =
	@echo =====================
	@echo Compiler: $(FC)

.PHONY: clean
clean:
	$(RM) *.nml *.mod *.gcda *.gcno test_* src$(DIR_SEP)*.$(OBJEXT) *.dbg src$(DIR_SEP)*.gcda src$(DIR_SEP)*.gcno $(COV) html-cov$(DIR_SEP) src$(DIR_SEP)units.f90 genunits$(BINEXT)

################
# Dependencies #
################

src/autodiff.$(OBJEXT): src/prec.$(OBJEXT)

src/checks.$(OBJEXT): src/prec.$(OBJEXT)

src/nmllog.$(OBJEXT): src/prec.$(OBJEXT)

src/prec.$(OBJEXT):

src/genunits_data.$(OBJEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT)

src/genunits_io.$(OBJEXT): src/checks.$(OBJEXT) src/nmllog.$(OBJEXT) src/prec.$(OBJEXT) src/genunits_data.$(OBJEXT)

src/purerng.$(OBJEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT)

src/timer.$(OBJEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT)

src/units.$(OBJEXT): src/prec.$(OBJEXT)

src/unittest.$(OBJEXT): src/checks.$(OBJEXT) src/nmllog.$(OBJEXT) src/prec.$(OBJEXT) src/timer.$(OBJEXT)

############
# autodiff #
############

test_autodiff$(BINEXT): src/autodiff.$(OBJEXT) src/nmllog.$(OBJEXT) src/unittest.$(OBJEXT) test/test_autodiff.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_autodiff.f90

autodiff.nml: test_autodiff$(BINEXT)
	$(RUN)test_autodiff$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

##########
# checks #
##########

test_assert_false$(BINEXT): src/checks.$(OBJEXT) test/test_assert_false.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_assert_false.f90

test_assert_false_message$(BINEXT): src/checks.$(OBJEXT) test/test_assert_false_message.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_assert_false_message.f90

test_checks$(BINEXT): src/checks.$(OBJEXT) src/unittest.$(OBJEXT) test/test_checks.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_checks.f90

checks.nml: test_checks$(BINEXT) test_assert_false$(BINEXT) test_assert_false_message$(BINEXT)
	$(RUN)test_checks$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

############
# genunits #
############

genunits$(BINEXT): src/genunits_data.$(OBJEXT) src/genunits_io.$(OBJEXT) src/prec.$(OBJEXT) src/nmllog.$(OBJEXT) app/genunits.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) app/genunits.f90

src/units.f90: genunits$(BINEXT) test/genunits_input.nml
	$(RUN)genunits$(BINEXT) test/genunits_input.nml
	test ! -e fort.*
	test ! -e FORT.*

###############
# genunits_io #
###############

test_genunits_io$(BINEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT) src/genunits_data.$(OBJEXT) src/genunits_io.$(OBJEXT) src/unittest.$(OBJEXT) test/test_genunits_io.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_genunits_io.f90

genunits_io.nml: test_genunits_io$(BINEXT) test/genunits_input.nml
	$(RUN)test_genunits_io$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

#################
# genunits_data #
#################

test_genunits_data$(BINEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT) src/genunits_data.$(OBJEXT) src/unittest.$(OBJEXT) test/test_genunits_data.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_genunits_data.f90

genunits_data.nml: test_genunits_data$(BINEXT)
	$(RUN)test_genunits_data$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

##########
# nmllog #
##########

test_nmllog$(BINEXT): src/nmllog.$(OBJEXT) src/unittest.$(OBJEXT) test/test_nmllog.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_nmllog.f90

nmllog.nml: test_nmllog$(BINEXT)
	$(RUN)test_nmllog$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

########
# prec #
########

test_prec$(BINEXT): src/prec.$(OBJEXT) src/nmllog.$(OBJEXT) src/unittest.$(OBJEXT) test/test_prec.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_prec.f90

prec.nml: test_prec$(BINEXT)
	$(RUN)test_prec$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

###########
# purerng #
###########

test_purerng$(BINEXT): src/prec.$(OBJEXT) src/nmllog.$(OBJEXT) src/purerng.$(OBJEXT) src/unittest.$(OBJEXT) test/test_purerng.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_purerng.f90

purerng.nml: test_purerng$(BINEXT)
	$(RUN)test_purerng$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

#########
# timer #
#########

test_timer$(BINEXT): src/checks.$(OBJEXT) src/prec.$(OBJEXT) src/timer.$(OBJEXT) src/unittest.$(OBJEXT) test/test_timer.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_timer.f90

timer.nml: test_timer$(BINEXT)
	$(RUN)test_timer$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

#########
# units #
#########

test_units$(BINEXT): src/units.$(OBJEXT) src/prec.$(OBJEXT) src/nmllog.$(OBJEXT) src/unittest.$(OBJEXT) test/test_units.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_units.f90

test_units_fail_1$(BINEXT): src/prec.$(OBJEXT) src/units.$(OBJEXT) test/test_units_fail_1.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_units_fail_1.f90

test_units_fail_2$(BINEXT): src/prec.$(OBJEXT) src/units.$(OBJEXT) test/test_units_fail_2.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_units_fail_2.f90

units.nml: test_units$(BINEXT) test/test_units_fail_1.f90 test/test_units_fail_2.f90
	$(RUN)test_units$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*

############
# unittest #
############

test_unittest$(BINEXT): src/checks.$(OBJEXT) src/timer.$(OBJEXT) src/unittest.$(OBJEXT) test/test_unittest.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src/*.$(OBJEXT) test/test_unittest.f90

unittest.nml: test_unittest$(BINEXT) prec.nml
	$(RUN)test_unittest$(BINEXT)
	test ! -e fort.*
	test ! -e FORT.*
