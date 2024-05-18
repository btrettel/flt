# BSD Make on Linux
# Probably will work on BSD too, though that is not tested.
# Works with `bmake -f BSDmakefile` on Ubuntu.

.POSIX:

.DELETE_ON_ERROR:

# defaults
BUILD = debug
.include <mk/linux_1.mk>

#############
# Compilers #
#############

# gfortran
.if $(FC) == gfortran
.include <mk/gfortran.mk>
.elif $(FC) == ifx
.include <mk/ifx_linux.mk>
.elif $(FC) == ifort
.include <mk/ifort_linux.mk>
.elif $(FC) == nvfortran
.include <mk/nvfortran.mk>
.else
.error Invalid FC: $(FC)
.endif
# LATER: FC=lfortran, particularly for the style suggestions
# FFLAGS   = --link-with-gcc
# DBGFLAGS = -g
# RFLAGS   = --fast

.if $(BUILD) == debug
FFLAGS += $(DBGFLAGS)
.elif $(BUILD) == release
FFLAGS += $(RFLAGS)
.else
.error Set BUILD to either debug or release. BUILD=$(BUILD)
.endif

.include <mk/common.mk>
.include <mk/linux_2.mk>
