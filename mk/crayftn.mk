FFLAGS = -hlist=l -herror_on_warning -ec
DFLAGS = -G0 -O0 -hfp0 -Rbcdps -M1077
RFLAGS = -hlist=m -hconcurrent -O2 -hfp3
AFLAGS = 
NFLAGS = 

# What is `-h thread_do_concurrent`?
# <https://fortran-lang.discourse.group/t/do-concurrent-compiler-flags-to-enable-parallelization/4300/6?u=btrettel>

# `-fsanitize=address`: Seems to have false positives.
# `-M1077`: Disables a warning if OpenMP directives are used but OpenMP is not enabled.
