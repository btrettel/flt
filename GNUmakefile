# GNU Make on Linux

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# defaults
BUILD = debug
include linux_1.mk

#############
# Compilers #
#############

# gfortran
ifeq ($(FC),gfortran)
include gfortran.mk
else ifeq ($(FC),ifx)
include ifx.mk
else ifeq ($(FC),ifort)
include ifort.mk
else ifeq ($(FC),nvfortran)
include nvfortran.mk
endif
# LATER: FC=lfortran, particularly for the style suggestions
# FFLAGS   = --link-with-gcc
# DBGFLAGS =

ifeq ($(BUILD),debug)
FFLAGS += $(DBGFLAGS)
else ifeq ($(BUILD),debug)
FFLAGS += $(RFLAGS)
else
$(error Set BUILD to either debug or release. BUILD=$(BUILD))
endif

include common.mk
include linux_2.mk
