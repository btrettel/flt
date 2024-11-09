# purerng.f90 to-do

- <https://en.wikipedia.org/wiki/Quasi-Monte_Carlo_method>
- Add more rigorous tests for random number generators. (Low priority as I just implemented a common random number generator, which should be good enough to get started.)
- Make xoshiro256** only work with `REAL64` and `INT64` with radix 2 as it seems designed around those based on the bit manipulation.
- Change `I10` to `INT64` to work with xoshiro256**.
- Test that `INT64` has enough precision for the `lecuyer` RNG.
- RNG splitting
    - lecuyer_implementing_1991
    - Iterative: 20 lines
    - Decomposition: 41 lines
    - Russian peasant: 23 lines
- `set_determ`: Convenience function to convert `real` array to `RNG_DETERM` seed
- Create `stats` module with `mean` and `std` to do some basic tests on the `RNG_LECUYER` random number generator.
    - <https://stdlib.fortran-lang.org/page/specs/stdlib_stats.html>
    - <https://en.wikipedia.org/wiki/Variance#Unbiased_sample_variance>
    - <https://en.wikipedia.org/wiki/Continuous_uniform_distribution>
- Switch `random_seed` to use a return code rather than `error stop` to make it more easily tested?
