# Summary: Makefile for GNU Make
# Standard: POSIX (tested on GNU Make, BSD Make, pdpmake, and Microsoft NMAKE)
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# Tested with GNU Make on Linux. Works with `make` on Ubuntu.

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# Detect the operating system so that this can work under both Windows and Linux.
# <https://stackoverflow.com/a/12099167/1124489>
OS ?=
ifeq ($(OS),Windows_NT)
OS_FILENAME = windows
else
# Despite saying Linux here, I expect this to work on Mac OS and BSD.
OS_FILENAME = linux
endif

# defaults
ifdef F90
FC=$(F90)
else
ifndef FC
FC = gfortran
# fort77 is GNU Make's default, which I'm overriding.
else ifeq ($(FC),fort77)
FC = gfortran
endif
endif
BUILD = debug
include mk/linux_defaults.mk

#############
# Compilers #
#############

ifeq ($(FC),gfortran)
include mk/gfortran.mk
else ifeq ($(FC),ifx)
include mk/ifx_$(OS_FILENAME).mk
else ifeq ($(FC),ifort)
include mk/ifort_$(OS_FILENAME).mk
else ifeq ($(FC),nvfortran)
include mk/nvfortran.mk
else ifeq ($(FC),lfortran)
include mk/lfortran.mk
else ifeq ($(FC),crayftn)
include mk/crayftn.mk
else ifeq ($(FC),ftn)
include mk/crayftn.mk
else ifeq ($(FC),xlf2008)
include mk/xlf.mk
else ifeq ($(FC),nagfor)
include mk/nagfor.mk
#else
#$(error Invalid FC: $(FC))
endif

ifeq ($(ARCH),native)
AFLAGS = $(NFLAGS)
endif

ifeq ($(BUILD),debug)
FFLAGS += $(DFLAGS) $(AFLAGS)
else ifeq ($(BUILD),release)
FFLAGS += $(RFLAGS) $(AFLAGS)
else
$(error Set BUILD to either debug or release. BUILD=$(BUILD))
endif

include mk/before.mk
include mk/common.mk
include mk/manual.mk
include mk/depends.mk
include mk/$(OS_FILENAME)_2.mk
