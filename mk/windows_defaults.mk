# default compiler
FC = ifort

BINEXT   = .exe
RUN      = 
RM       = del /q
OFLAG    = /o
OBJEXT   = obj
OBJFLAGS = /c /o
MISSED   = missed.txt
CLEAN    = *.nml *.mod *.$(OBJEXT) src$(DIR_SEP)*.$(OBJEXT) $(MISSED) src$(DIR_SEP)units.f90 *$(BINEXT) *.pdb *.opt.yaml
CMP      = fc
PYTHON   = python

# The `^` escapes the backslash. Otherwise, `\` would be a new line in NMAKE.
DIR_SEP = ^\
