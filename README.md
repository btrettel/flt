# flt

Misc. Fortran libraries and tools (all at various stages of development), intended for my own personal use:

- checks.f90: Module for procedures used for run-time checks.
- logging.f90: Module for structured logging in a JSON lines file.
- prec.f90: Module to set precisions, lengths, and mathematical constants.
- unittest.f90: Module for unit testing procedures.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: A subset of Fortran 2008 will be used to increase portability. Multiple compilers will be tested to ensure portability. Not all Fortran features have been implemented properly in some compilers (for example, parameterized derived types), so those features will be avoided.
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.
- Familiarity: Interfaces to flt modules should be based on interfaces for other successful modules/packages.
