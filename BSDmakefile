# BSD Make on Linux
# Probably will work on BSD too, though that is not tested.

.POSIX:

# defaults
BUILD = debug
.include <build/linux_1.mk>

#############
# Compilers #
#############

# gfortran
.if $(FC) == gfortran
.include <build/gfortran.mk>
.elif $(FC) == ifx
.include <build/ifx.mk>
.elif $(FC) == ifort
.include <build/ifort.mk>
.elif $(FC) == nvfortran
.include <build/nvfortran.mk>
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

.include <build/common.mk>
.include <build/linux_2.mk>
