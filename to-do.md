# To-do

Priorities:

- ga.f90:
    - `config%openmp` to enable parallel `evaluate`
    - `write_restart_file` and `read_restart_file`
    - `stop_now` file can stop GA
    - `config%stop_f` option to make GA stop at a particular value of `f`
    - `config%stop_time`
    - different return codes depending on how `optimize` stopped
        - `rc <= 0`: success
            - 0: stopped due to `n_gener`
            - -1: stopped due to `stop_now` file
            - -2: stopped due to `stop_f`
            - -3: stopped due to `stop_time`
        - `rc >= 1`: failure
    - `integer` chromosome in addition
    - population statistics: mean, standard deviation
- returncodes.f90: A module containing `errno` codes, other internal return codes.
    - Make a table of `iostat` values in different Fortran compilers so that you know which values to pick to not conflict with any compiler.
        - <https://fortranwiki.org/fortran/show/iso_fortran_env>
            - `iostat_end`, `iostat_eor`
        - <https://www.scivision.dev/oneapi-fortran-iostat-codes/>
        - <https://www.ibm.com/docs/en/xl-fortran-linux/16.1.1?topic=inputoutput-conditions-iostat-values>
        - <https://groups.google.com/g/comp.lang.fortran/c/l8UJoI-x9PM>
- fmad.f90 and units.f90
    - `is_close`
- unittest.f90
    - maybe: Instead of `integer_eq`, `real_eq`, use generic `eq`?
    - Ensure that all test messages are unique.
    - Keep track of test results so that you know whether a test has ever failed, and thus whether it is discriminating. (bowes_how_2017 p. 3L)
        - Also track which assertions have never failed?
    - Add optional argument `brittle` to tests that may be brittle, so that their failures can be ignored if desired.
    - dataplot-like approach to ease adding tests (but use namelists instead of a single CSV file)
- f90lint: Simple linter for Fortran to enforce anything that can't be enforced with a regex linter.
    - Check that assertions have unique messages. List relevant variable values in error message.
    - Check that assertion messages follow proper template.
    - Change `skip_indexing` to `dont_lint`, make other changes to make the input file more clear.
    - check for `implicit none`
- `io.f90`
    - read and save CSV files
        - regex validation field for CSV
- Add Valgrind back to check-fc. Suppress namelist derived-type input problem in Valgrind.
    - <https://valgrind.org/docs/manual/manual-core.html#manual-core.suppress>
    - <https://stackoverflow.com/a/23897854/1124489>

Later:

- When work has ifx 2024.2, change `assert` to eliminate `full_message` by putting the message directly on the `error stop` line. Also see [compiler-bugs report0002](https://github.com/btrettel/compiler-bugs/tree/main/report0002).
