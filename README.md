# flt

Misc. Fortran libraries and tools (all at various stages of development):

- checks.f90: Module for procedures used for run-time checks.
- ga.f90: Module for derivative-free optimization of `real`s with a genetic algorithm. (in progress)
- logging.f90: Module for structured logging in a JSON lines file.
- prec.f90: Module to set precisions, lengths, and mathematical constants.
- unittest.f90: Module for unit testing procedures.

flt is intended only for my own use. Certain parts (like fmut in particular) are brittle and will only work for my own particular coding style.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: A subset of Fortran 2003 will be used to maximize portability. Multiple compilers will be tested to ensure portability. Not all Fortran 2003 features have been implemented properly in some compilers (for example, parameterized derived types), so those features will be avoided.
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.

## To-do

General code cleanup:

- change `write(unit=*, fmt=*)` to `write(*, *)`?
- construct names for `do` loops and `if` statements

New modules and tools:

- dimmod.f90, dimgen.f90: Generates a module named `dimcheck` which provides compile-time checking of dimensions. (started, paused for now)
- fad.f90: Forward-mode automatic differentiation. (complete but not yet added)
- fps.f90: Module for Monte Carlo sensitivity analysis on floating point operations to help identify expressions contributing to floating point inaccuracy. This allows to find operations with inaccuracy worse than a threshold, rather than finding *all* inexact floating-point operations as tools like gfortran's `ffpe-trap=inexact` do. The latter approach leads to too many reported problems. Prioritizing floating-point errors by their magnitude makes sense.
    - parker_monte_1997-1
- f90lint: Simple linter for Fortran to enforce anything that can't be enforced with a regex linter.
- fmut: A primitive mutation tester for Fortran, written in Fortran. (A functional prototype written in Python is complete, but too sloppy for me to release.)
    - Run fmut on flt to spot gaps in the tests.
    - Make fmut distinguish between compilation errors and test failures
    - Mutate `function` return values.
    - Mutate subroutine `intent(out)` and `intent(in out)` values.
    - Pass in arrays of the wrong sizes to procedures to see if that's detected.
    - Thought after skimming horner_method_2021: Code coverage misses equation terms. A mutation tester which deletes random equation terms could be useful.
- logging.f90
    - Test `stdout` optional argument.
    - Add `box_print` and other non-structured logging for the most important messages that I don't want to miss.
- rng.f90: Includes a deterministic random number generator for testing purposes.
- semgrep static analysis
