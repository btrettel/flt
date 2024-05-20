# default compiler
FC = ifort

BINEXT   = .exe
RUN      = 
RM       = del /q
OFLAG    = /o
OBJEXT   = obj
OBJFLAGS = /c /o
CLEAN    = *.nml *.mod src$(DIR_SEP)*.$(OBJEXT) src$(DIR_SEP)units.f90 *$(BINEXT) *.pdb
CMP      = fc
PYTHON   = python

# The `^` escapes the backslash. Otherwise, `\` would be a new line in NMAKE.
DIR_SEP = ^\
