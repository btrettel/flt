FFLAGS   = -Minform=inform -Werror
DBGFLAGS = -g
RFLAGS   = -fast -Minfo=all -Mneginfo=all

# <https://docs.nvidia.com/hpc-sdk//compilers/hpc-compilers-user-guide/index.html#gs-perform-fast>

# TODO: `-stdpar=multicore` for CPUs
# TODO: `-stdpar=gpu` (or `-stdpar`) for GPUs
