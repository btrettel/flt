FFLAGS = -hlist=l -herror_on_warning -ec
DFLAGS = -G0 -O0 -hpf1 -Rbcdps -fsanitize=address
RFLAGS = -hlist=m -hconcurrent -O2 -hfp3

# What is `-h thread_do_concurrent`?
# <https://fortran-lang.discourse.group/t/do-concurrent-compiler-flags-to-enable-parallelization/4300/6?u=btrettel>
