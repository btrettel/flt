# unittest.f90 and testing to-do

- Instead of `integer_eq`, `real_eq`, use generic `eq`.
- Ensure that all test messages are unique.
- Keep track of test results so that you know whether a test has ever failed, and thus whether it is discriminating. (bowes_how_2017 p. 3L)
    - Also track which assertions have never failed?
- Add optional argument `brittle` to tests that may be brittle, so that their failures can be ignored if desired.
- dataplot-like approach to ease adding tests (but use namelists instead of a single CSV file)
- Make `test_concurrent` more reliable. I think this problem might only appear for Intel. And is it only for release mode as an assertion should catch this? Why don't the assertions fail in that case?
    - ```./test_purerng
    real returned = -3999.7052
           > real = .0000000
    fail: test_concurrent, greater than zero```
- transient.py: Repeatedly run tests looking for transient failures, saving results when a transient failure is encountered.
- add names to deeply nested `if`s and `do`s in unittest
- Test `exit_code_eq` better. Check if the file is kept or not.
- Make sure that debug builds enable assertions by looking at the test log output.
- `CMP` and checksum based tests
- Add tests to compare speed of parallel vs. serial
