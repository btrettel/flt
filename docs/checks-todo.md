# check.f90 to-do

- Make an assertion which takes an array to help avoid the problem of only the first failing assertion
    - `check` helps with this too, but an `assert_all` would be convenient if I simply want to check a bunch of things at once.
    - <https://blog.ploeh.dk/2022/11/07/applicative-assertions/>
    - Can make `assert_all` use `do_concurrent` to have a faster assertion.
- To-do routine in code to cause compilation to fail.
- Make pure logger have a finalizer to check that it was printed. This might not be possible if the logger itself is allocatable.
- Maybe: Add `assert_always` for assertions that are never turned off.
