# flt

Misc. Fortran libraries and tools (all at various stages of development):

- TODO: fmutate: A primitive mutation tester for Fortran, written in Python.
- dimmod.f90, dimgen.f90: Generates a module named `dimcheck` which provides compile-time checking of dimensions.
- TODO: ga.f90: Module for derivative-free optimization with a genetic algorithm.
- logging.f90: Module for structured logging in a JSON lines file.
- testmod.f90: Module for testing procedures.

flt is intended only for my own use. Certain parts (like fmutate in particular) are brittle and will only work for my own particular coding style.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: Typically the Fortran 90 standard will be used to maximize portability. Multiple compilers will be tested to ensure portability.

## To-do

- Move all tests from other repository
    - Put Python scripts that check JSONL files into this repository.
