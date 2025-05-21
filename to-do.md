# To-do

Priorities:

- fmad.f90: Change `%dv` to `%d`
- Switch `make check` to `make test` for consistency with folder name.
- ga.f90:
    - Add more tests for flt ga.f90 in general
        - Add test to check that `pop%best_pop_indiv` is set correctly.
        - Add test for constrained optimization.
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
            - At end of optimization, report error if none satisfy constraints.
        - `rc >= 1`: failure
    - population statistics: mean, standard deviation
        - `call pop_stats(...)`
    - Assert that objective values are finite. This will help catch bugs in objective functions and ensure that the population statistics are meaningful.
    - Add ability to insert an arbitrary number of pre-specified seeds to the initial population.
    - Break `g_sum` into `g_sum_pre` for before the solver is run and `g_sum_post` for after the solver is run. Some constraint violations may make running the solver impossible. Individuals which have non-zero `g_sum_pre` will take `g_sum_post` values equal to the maximum `g_sum_post` value. This will incentivize satisfying the pre-constraints.
    - Report fraction of population not satisfying constraints in printout.
    - Check notes on testing optimization algorithms for more ideas.
    - `config%log = .true.` (`.false.` by default) will write `pop.nml` containing information about every individual.
    - Latin hypercube sampling for initial population
        - Probably better to use Halton sequence since I intend to use that for gradient-based optimization. The two are similar and this will save time.
        - <https://youtu.be/0ewU1frjoL8?list=PLj6pNSgoumyfNUw0T_dOv5g6QzDf6tHmq&t=1236>
        - <https://youtu.be/1BnRCDSWI3s?list=PLj6pNSgoumyfNUw0T_dOv5g6QzDf6tHmq&t=78>
        - martins_engineering_2021 p. 377-- (pdf pp. 384--)
        - Matlab `lhsdesign`
    - Test `standard_ga_config`
    - To improve convergence to the global optimum better:
        - Multi-start
        - luke_essentials_2013 p. 45: For `t < 2.0`, possibility of random selection.
- returncodes.f90: A module containing `errno` codes, other internal return codes.
- fmad.f90 and units.f90
    - `is_close`
- unittest.f90
    - Instead of `integer_eq`, `real_eq`, use generic `eq`.
    - Ensure that all test messages are unique.
    - Keep track of test results so that you know whether a test has ever failed, and thus whether it is discriminating. (bowes_how_2017 p. 3L)
        - Also track which assertions have never failed?
    - Add optional argument `brittle` to tests that may be brittle, so that their failures can be ignored if desired.
    - dataplot-like approach to ease adding tests (but use namelists instead of a single CSV file)
- f90lint: Simple linter for Fortran to enforce anything that can't be enforced with a regex linter.
    - Check that assertions have unique messages. List relevant variable values in error message.
    - Check that assertion messages follow proper template.
    - Change `skip_indexing` to `dont_lint`, make other changes to make the input file clearer.
    - check for `implicit none`
- `io.f90`
    - read and save CSV files
        - regex validation field for CSV
- Add Valgrind back to check-fc. Suppress namelist derived-type input problem in Valgrind.
    - <https://valgrind.org/docs/manual/manual-core.html#manual-core.suppress>
    - <https://stackoverflow.com/a/23897854/1124489>

Later:

- When work has ifx 2024.2, change `assert` to eliminate `full_message` by putting the message directly on the `error stop` line. Also see [compiler-bugs report0002](https://github.com/btrettel/compiler-bugs/tree/main/report0002).
