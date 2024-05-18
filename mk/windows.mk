# default compiler
FC = ifort

BINEXT   = .exe
RUN      = 
RM       = del /q
OFLAG    = /o
OBJEXT   = obj
OBJFLAGS = /c /o

# The `^` escapes the backslash. Otherwise, `\` would be a new line in NMAKE.
DIR_SEP = ^\
