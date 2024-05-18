# GNU Make on Linux

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# defaults
BUILD = debug
include build/linux_1.mk

#############
# Compilers #
#############

# gfortran
ifeq ($(FC),gfortran)
include build/gfortran.mk
else ifeq ($(FC),ifx)
include build/ifx.mk
else ifeq ($(FC),ifort)
include build/ifort.mk
else ifeq ($(FC),nvfortran)
include build/nvfortran.mk
else
$(error Invalid FC: $(FC))
endif
# LATER: FC=lfortran, particularly for the style suggestions
# FFLAGS   = --link-with-gcc
# DBGFLAGS = -g
# RFLAGS   = --fast

ifeq ($(BUILD),debug)
FFLAGS += $(DBGFLAGS)
else ifeq ($(BUILD),release)
FFLAGS += $(RFLAGS)
else
$(error Set BUILD to either debug or release. BUILD=$(BUILD))
endif

include build/common.mk
include build/linux_2.mk
