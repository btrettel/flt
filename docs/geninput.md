- geninput/nmlfuzz talk
- Kill four birds with one stone:
    1. generate boilerplate namelist input code with input validation
    2. generate documentation for input variables
    3. run fuzz tests
    4. do sensitivity analysis
- geninput
    - Limitations due to use of namelists:
        - Required variables must have a bound
            - This is because there is no way to know if a variable is defined or not in standard Fortran
            - TODO: `isnan` as a possibility for `real`s? It didn't seem reliable with optimizations as I recall.
