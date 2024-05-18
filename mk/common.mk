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

src$(DIR_SEP)autodiff.$(OBJEXT): src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)checks.$(OBJEXT): src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)nmllog.$(OBJEXT): src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)prec.$(OBJEXT):

src$(DIR_SEP)genunits_data.$(OBJEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)genunits_io.$(OBJEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)genunits_data.$(OBJEXT)

src$(DIR_SEP)purerng.$(OBJEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)timer.$(OBJEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)units.$(OBJEXT): src$(DIR_SEP)prec.$(OBJEXT)

src$(DIR_SEP)unittest.$(OBJEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)timer.$(OBJEXT)

############
# autodiff #
############

test_autodiff$(BINEXT): src$(DIR_SEP)autodiff.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_autodiff.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_autodiff.f90

autodiff.nml: test_autodiff$(BINEXT)
	$(RUN)test_autodiff$(BINEXT)

##########
# checks #
##########

test_assert_false$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) test$(DIR_SEP)test_assert_false.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_assert_false.f90

test_assert_false_message$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) test$(DIR_SEP)test_assert_false_message.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_assert_false_message.f90

test_checks$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_checks.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_checks.f90

checks.nml: test_checks$(BINEXT) test_assert_false$(BINEXT) test_assert_false_message$(BINEXT)
	$(RUN)test_checks$(BINEXT)

############
# genunits #
############

genunits$(BINEXT): src$(DIR_SEP)genunits_data.$(OBJEXT) src$(DIR_SEP)genunits_io.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) app/genunits.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) app/genunits.f90

src$(DIR_SEP)units.f90: genunits$(BINEXT) test$(DIR_SEP)genunits_input.nml
	$(RUN)genunits$(BINEXT) test$(DIR_SEP)genunits_input.nml

###############
# genunits_io #
###############

test_genunits_io$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)genunits_data.$(OBJEXT) src$(DIR_SEP)genunits_io.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_genunits_io.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_genunits_io.f90

genunits_io.nml: test_genunits_io$(BINEXT) test$(DIR_SEP)genunits_input.nml
	$(RUN)test_genunits_io$(BINEXT)

#################
# genunits_data #
#################

test_genunits_data$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)genunits_data.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_genunits_data.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_genunits_data.f90

genunits_data.nml: test_genunits_data$(BINEXT)
	$(RUN)test_genunits_data$(BINEXT)

##########
# nmllog #
##########

test_nmllog$(BINEXT): src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_nmllog.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_nmllog.f90

nmllog.nml: test_nmllog$(BINEXT)
	$(RUN)test_nmllog$(BINEXT)

########
# prec #
########

test_prec$(BINEXT): src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_prec.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_prec.f90

prec.nml: test_prec$(BINEXT)
	$(RUN)test_prec$(BINEXT)

###########
# purerng #
###########

test_purerng$(BINEXT): src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)purerng.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_purerng.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_purerng.f90

purerng.nml: test_purerng$(BINEXT)
	$(RUN)test_purerng$(BINEXT)

#########
# timer #
#########

test_timer$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)timer.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_timer.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_timer.f90

timer.nml: test_timer$(BINEXT)
	$(RUN)test_timer$(BINEXT)

#########
# units #
#########

test_units$(BINEXT): src$(DIR_SEP)units.$(OBJEXT) src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)nmllog.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_units.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_units.f90

test_units_fail_1$(BINEXT): src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)units.$(OBJEXT) test$(DIR_SEP)test_units_fail_1.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_units_fail_1.f90

test_units_fail_2$(BINEXT): src$(DIR_SEP)prec.$(OBJEXT) src$(DIR_SEP)units.$(OBJEXT) test$(DIR_SEP)test_units_fail_2.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_units_fail_2.f90

units.nml: test_units$(BINEXT) test$(DIR_SEP)test_units_fail_1.f90 test$(DIR_SEP)test_units_fail_2.f90
	$(RUN)test_units$(BINEXT)

############
# unittest #
############

test_unittest$(BINEXT): src$(DIR_SEP)checks.$(OBJEXT) src$(DIR_SEP)timer.$(OBJEXT) src$(DIR_SEP)unittest.$(OBJEXT) test$(DIR_SEP)test_unittest.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) src$(DIR_SEP)*.$(OBJEXT) test$(DIR_SEP)test_unittest.f90

unittest.nml: test_unittest$(BINEXT) prec.nml
	$(RUN)test_unittest$(BINEXT)
