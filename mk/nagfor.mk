FFLAGS = -Error=Any_Warning -f2018 -info -quiet -I src
DFLAGS = -g
RFLAGS = -O2 -openmp -target=native

# `-I src`: Required because unlike other compilers, nagfor won't look for `include`d files in the same directory as the source code calling it.
