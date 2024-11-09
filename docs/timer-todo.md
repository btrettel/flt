# timer.f90 to-do

- Data type contains both CPU time and wall clock time for comparison?
- Tests
    - serial case where CPU and wall time should be close.
    - parallel case where CPU time should be a multiple of wall time
    - separate non-standard test code using `sleep(1)`
        - <https://gcc.gnu.org/onlinedocs/gcc-7.5.0/gfortran/SLEEP.html>
        - <https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/sleep.html>
