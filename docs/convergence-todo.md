# convergence.f90 to-do

- Print run-time for each convergence test in the table and a total time at the end.
- Python code to convert SymPy output to formatted Fortran code with `params%`, `PI`, numbers to `_WP`
- Have a feature to minimize the run-time of each convergence test, perhaps through some sort of optimizer. Reduce number of time steps for spatial convergence, etc.
- Make plots. Need to pass in variable names for plots? Probably is a good idea to pass in the variable name anyway to use instead of the variable number in the table printout.
- Output `delta` for plot for `de_solver`.
- convenience subroutine to calculate both de and de_dv given f, f_exact, ord, lower_index, upper_index
- Reduce boilerplate more:
    - procedure(s) to pick CFL number and/or time-step?
- Richardson extrapolation procedure?
- Check for ideas: marshall_scientific_2011
- Have ability to produce convergence rate plot like smitherman_calculation_2007 fig. 8. This could be useful to help pinpoint what is wrong when debugging.
