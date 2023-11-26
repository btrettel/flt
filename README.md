# flt

Misc. Fortran libraries and tools (all at various stages of development):

- checks.f90: Module for procedures used for run-time checks.
- logging.f90: Module for structured logging in a JSON lines file.
- prec.f90: Module to set precisions, lengths, and mathematical constants.
- unittest.f90: Module for unit testing procedures.

flt is intended only for my own use. Certain parts (like fmutate in particular) are brittle and will only work for my own particular coding style.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: Typically the Fortran 90 standard will be used to maximize portability. Multiple compilers will be tested to ensure portability.
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.

## To-do

- dimmod.f90, dimgen.f90: Generates a module named `dimcheck` which provides compile-time checking of dimensions.
- fad.f90: Forward-mode automatic differentiation.
- fmutate: A primitive mutation tester for Fortran, written in Python. (A functional prototype is complete, but not yet copied to this repository from a private repository.)
- ga.f90: Module for derivative-free optimization of `real`s with a genetic algorithm.
- Run fmutate on flt to spot gaps in the tests.
