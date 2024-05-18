# Notes on the build system for FLT modules and programs

I have the following goals for the build system for FLT modules and programs:

1. Portability across different computer systems (Windows, Linux, BSD, more).
2. Allow easy testing the code on a variety of compilers.
3. No preprocessor.
4. Can be used at my work.

Given #1 and #4, I decided to target a "lowest common denominator" build system. I frequently have to use a computer with only Microsoft NMAKE. I am roughly targeting POSIX make for common.mk. As strictly POSIX make is quite limited, I have different headers for NMAKE, GNU Make, and BSD Make, which include the POSIX make common.mk file. This minimizes the amount of build-system dependent code I have to maintain.

As for why #2, I would like to be able to easily switch between multiple compilers to test portability and get better diagnostics as each compiler has different diagnostics. Switching between multiple compilers is not so easy with CMake or FPM at the moment, so those are not good options.

The justification for #3 is that Fortran has no standard preprocessor, so using a preprocessor portably is difficult. There certainly are places where I'd like to use a preprocessor, but until it's standardized and in common use, I won't be using a preprocessor for my Fortran projects.

Building with FPM is planned for the future, but is not a priority at the moment. At the moment, I am trying only to avoid deviations from the directory structure that FPM expects.

### Limitations of POSIX make and NMAKE

I would like to have separate release and debug output directories to avoid possibly mixing up debug and release object files. However, using only suffix rules for compliance with POSIX make, this is not possible as far as I'm aware. Separate release and debug directories also is inconsistent with FPM defaults.

Given the previous paragraph, I was tempted to try having different file extensions for debug and release object files. Unfortunately, NMAKE can't seem to work with the `.f90-dbg.$(OBJEXT):` suffix rule, requiring `.f90.dbg$(OBJEXT)`. But `.f90.dbg$(OBJEXT)` doesn't work with gfortran according to my notes. So I've decided to have simply one object file extension (`OBJEXT`).
