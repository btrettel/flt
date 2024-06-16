# flt

Misc. Fortran libraries and tools (all at various stages of development), intended for my own personal use:

- autodiff.f90: Module for forward-mode automatic differentiation.
- checks.f90: Module for procedures used for run-time checks.
- depends.py: Python script to generate Makefile dependencies for Fortran modules.
- [f90lint.py](f90lint.md): Python script to do some static analysis not done by the other linters I have.
- nmllog.f90: Module for structured logging in namelist files. Also includes logger which can be used in `pure` procedures where the actual writing is done outside of the pure procedure.
- genunits.f90: Program to generate a module `units` which allows for compile-time physical dimension consistency checking. (Under active development.)
- prec.f90: Module to set precisions, lengths, and mathematical constants. Uses common convention of `WP` for real precision.
- purerng.f90: Module for pure random number generators. Interface similar to intrinsic `random_number` and `random_seed` (but not identical to avoid conflict with `size` intrinsic). Includes a deterministic "random" number generator to mock actual random number generators for testing.
- timer.f90: Module for wall-clock timers. Interface is similar to [StopWatch](https://math.nist.gov/StopWatch/).
- unittest.f90: Module for unit testing procedures. Interface is probably unique, but output has some similarities to Python's [unittest](https://docs.python.org/3/library/unittest.html) module.

Other files:

- f90.yaml: Modified rules file for the Fortran linter [flinter](https://pypi.org/project/flinter/).
- lint-wrapper.py: Runs the linters [flinter](https://pypi.org/project/flinter/) and [i-Code CNES](https://github.com/cnescatlab/i-CodeCNES/), reads their output, and has a useful exit code for Makefiles to detect linter suggestions.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: A subset of Fortran 2018 will be used to increase portability. Multiple compilers will be tested to ensure portability. Not all Fortran features have been implemented properly in some compilers (for example, parameterized derived types), so those features will be avoided.
    - The following compilers are used at present:
        - gfortran 13.2.0
        - ifx 2024.1.2
        - ifort 2021.12.0
        - nvfortran 24.5-1
        - xlf2008 17.1.1 (Linux on Power, in QEMU)
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.
- Familiarity: Interfaces to flt modules should be based on interfaces for other successful modules/packages, or standard Fortran if the module is intended to replace existing Fortran functionality.

## Building

Due to differences between make implementations and a desire to build this code on a Windows machine with only NMAKE, a variety of different Makefiles are available. These Makefiles `include` some common Makefiles in the mk directory.

To build and test all components on my Ubuntu computer (should be similar on most Linux machines):

GNU Make:

    make test

BSD Make:

    bmake -f BSDmakefile test

For both GNU Make and BSD Make, some macros can be set on the command line. The `FC` macro can be set to `gfortran` (default), `ifx`, `ifort`, or `nvfortran` to change which compiler is used. The `BUILD` macro can be set to `debug` (default) or `release`, which changes compiler flags and also disables assertions for `release`.

[Public domain POSIX make](https://frippery.org/make/) (used mostly to test POSIX compliance):

    pdpmake -f PDPmakefile test

On Windows, Microsoft NMAKE can be used, though not all tests will pass at present (that will be corrected):

    nmake -f Nmakefile test
