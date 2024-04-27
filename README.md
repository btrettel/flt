# flt

Misc. Fortran libraries and tools (all at various stages of development), intended for my own personal use:

- autodiff.f90: Module for forward-mode automatic differentiation.
- checks.f90: Module for procedures used for run-time checks.
- nmllog.f90: Module for structured logging in namelist files. Also includes logger which can be used in `pure` procedures where the actual writing is done outside of the pure procedure.
- prec.f90: Module to set precisions, lengths, and mathematical constants. Uses common convention of `WP` for real precision.
- purerng.f90: Module for pure random number generators. Interface similar to intrinsic `random_number` and `random_seed` (but not identical to avoid conflict with `size` intrinsic). Includes a deterministic "random" number generator to mock actual random number generators for testing.
- timer.f90: Module for wall-clock timers. Interface is similar to [StopWatch](https://math.nist.gov/StopWatch/).
- unittest.f90: Module for unit testing procedures. Interface is probably unique, but output has some similarities to Python's unittest module.

Other files:

- f90.yaml: Modified rules file for the Fortran linter[flinter](https://pypi.org/project/flinter/).
- lint-wrapper.py: Runs the linters [flinter](https://pypi.org/project/flinter/) and [i-Code CNES](https://github.com/cnescatlab/i-CodeCNES/), reads their output, and has a useful exit code for Makefiles to detect linter suggestions.

## Goals

- Correctness: All libraries and tools are planned to be thoroughly tested. I will fall short of this goal, but intend to approach it asymptotically in time.
- Portability: A subset of Fortran 2018 will be used to increase portability. Multiple compilers will be tested to ensure portability. Not all Fortran features have been implemented properly in some compilers (for example, parameterized derived types), so those features will be avoided.
    - The following compilers are used at present:
        - gfortran 9.4.0
        - ifx 2024.0.2
        - nvfortran 24.3-0
- Simplicity: No convoluted or opaque build system or algorithms. The simplest approach that works is usually what I'll pick.
- Familiarity: Interfaces to flt modules should be based on interfaces for other successful modules/packages, or standard Fortran if the module is intended to replace existing Fortran functionality.
