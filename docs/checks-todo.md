# check.f90 to-do

- For better error messages, do something like you do for unittest. For example, `assert_eq` takes the numbers to compare as arguments so they are printed out, `assert_true`, `assert_false`, etc. Make `assert` not public after this to discourage its use.
    - Not really needed. Just add one or more print statements later or use GDB to get the needed values.
- Make an assertion which takes an array to help avoid the problem of only the first failing assertion
    - `check` helps with this too, but an `assert_all` would be convenient if I simply want to check a bunch of things at once.
    - <https://blog.ploeh.dk/2022/11/07/applicative-assertions/>
    - Can make `assert_all` use `do_concurrent` to have a faster assertion.
- To-do routine in code to cause compilation to fail.
- <https://github.com/urbanjost/general-purpose-fortran/blob/master/src/M_verify.f90>
    - <https://urbanjost.github.io/general-purpose-fortran/docs/BOOK_M_verify.html>
