# flt

Misc. Fortran libraries and tools (all at various stages of development):

- checks.f90: Module for procedures used for run-time checks.
- ga.f90: Module for derivative-free optimization of `real`s with a genetic algorithm. (in progress)
- logging.f90: Module for structured logging in a JSON lines file.
- prec.f90: Module to set precisions, lengths, and mathematical constants.
- unittest.f90: Module for unit testing procedures.

flt is intended only for my own use. Certain parts (like fmutate in particular) are brittle and will only work for my own particular coding style.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: Typically the Fortran 90 standard will be used to maximize portability. Multiple compilers will be tested to ensure portability.
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.

## To-do

- dimmod.f90, dimgen.f90: Generates a module named `dimcheck` which provides compile-time checking of dimensions. (started, paused for now)
- fad.f90: Forward-mode automatic differentiation. (complete but not yet added)
- fps.f90: Module for Monte Carlo sensitivity analysis on floating point operations to help identify expressions contributing to floating point inaccuracy. This allows to find operations with inaccuracy worse than a threshold, rather than finding *all* inexact floating-point operations as tools like gfortran's `ffpe-trap=inexact` do. The latter approach leads to too many reported problems. Prioritizing floating-point errors by their magnitude makes sense.
    - parker_monte_1997-1
- fmut: A primitive mutation tester for Fortran, written in Fortran. (A functional prototype written in Python is complete, but too sloppy for me to release.)
    - Run fmut on flt to spot gaps in the tests.
- logging.f90
    - Test `stdout` optional argument.
    - Add `box_print` and other non-structured logging for the most important messages that I don't want to miss.
