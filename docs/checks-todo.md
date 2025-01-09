# check.f90 to-do

- Make an assertion which takes an array to help avoid the problem of only the first failing assertion
    - `check` helps with this too, but an `assert_all` would be convenient if I simply want to check a bunch of things at once.
    - <https://blog.ploeh.dk/2022/11/07/applicative-assertions/>
    - Can make `assert_all` use `do_concurrent` to have a faster assertion.
- To-do routine in code to cause compilation to fail.
- <https://github.com/urbanjost/general-purpose-fortran/blob/master/src/M_verify.f90>
    - <https://urbanjost.github.io/general-purpose-fortran/docs/BOOK_M_verify.html>
