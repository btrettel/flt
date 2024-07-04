# Summary: Makefile for BSD Make
# Standard: POSIX (tested on GNU Make, BSD Make, pdpmake, and Microsoft NMAKE)
# Author: Ben Trettel (<http://trettel.us/>)
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

# Tested with a BSD Make on Linux. Probably will work on BSD too, though that is not tested. Works with `bmake -f BSDmakefile` on Ubuntu.

.POSIX:

# non-POSIX
# <https://innolitics.com/articles/make-delete-on-error/>
.DELETE_ON_ERROR:

# defaults
.ifdef F90
FC=$(F90)
.else
.ifndef FC
FC = gfortran
# f77 is bmake's default, which I'm overriding.
.elif $(FC) == f77
FC = gfortran
.endif
.endif
BUILD = debug
.include <mk/linux_defaults.mk>

#############
# Compilers #
#############

.if $(FC) == gfortran
.include <mk/gfortran.mk>
.elif $(FC) == ifx
.include <mk/ifx_linux.mk>
.elif $(FC) == ifort
.include <mk/ifort_linux.mk>
.elif $(FC) == nvfortran
.include <mk/nvfortran.mk>
.elif $(FC) == lfortran
.include <mk/lfortran.mk>
.elif $(FC) == crayftn
.include <mk/crayftn.mk>
.elif $(FC) == ftn
.include <mk/crayftn.mk>
.elif $(FC) == xlf2008
.include <mk/xlf.mk>
.elif $(FC) == nagfor
.include <mk/nagfor.mk>
#.else
#.error Invalid FC: $(FC)
.endif

.if $(ARCH) == native
AFLAGS = $(NFLAGS)
.endif

.if $(BUILD) == debug
FFLAGS += $(DFLAGS) $(AFLAGS)
.elif $(BUILD) == release
FFLAGS += $(RFLAGS) $(AFLAGS)
.else
.error Set BUILD to either debug or release. BUILD=$(BUILD)
.endif

.include <mk/before.mk>
.include <mk/common.mk>
.include <mk/depends.mk>
.include <mk/linux_2.mk>
