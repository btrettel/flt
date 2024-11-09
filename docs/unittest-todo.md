# unittest.f90 and testing to-do

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
