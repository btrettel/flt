# Summary: Common Makefile for FLT
# Standard: POSIX (tested on GNU Make, BSD Make, pdpmake, and Microsoft NMAKE)
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

###############
# Boilerplate #
###############

.SUFFIXES:
.SUFFIXES: .f90 .$(OBJEXT)

.PHONY: check
check: $(TESTNML)
	@echo =====================
	@echo = All tests passed. =
	@echo =====================
	@echo Compiler: $(FC)

.PHONY: clean
clean:
	-$(RM) $(CLEAN) $(CLEAN_MANUAL)

# jom needs explicit dependencies listed for all files.
src$(DIR_SEP)rev.$(OBJEXT): src$(DIR_SEP)rev.f90

###########################
# Portable Python scripts #
###########################

.PHONY: f90lint
f90lint:
	$(PYTHON) py$(DIR_SEP)f90lint.py config.ini

.PHONY: depends
depends:
	$(PYTHON) py$(DIR_SEP)depends.py config.ini
