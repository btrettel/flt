# GNU Make on Linux
# Works with `make` on Ubuntu.

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:
MAKEFLAGS = --warn-undefined-variables

# defaults
BUILD = debug
include mk/linux_1.mk

#############
# Compilers #
#############

# gfortran
ifeq ($(FC),gfortran)
include mk/gfortran.mk
else ifeq ($(FC),ifx)
include mk/ifx_linux.mk
else ifeq ($(FC),ifort)
include mk/ifort_linux.mk
else ifeq ($(FC),nvfortran)
include mk/nvfortran.mk
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

include mk/common.mk
include mk/depends.mk
include mk/linux_2.mk
