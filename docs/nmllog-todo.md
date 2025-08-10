# nmllog.f90 to-do

- `check_bounds(x, rc, lower, upper)`
    - Both `lower` and `upper` are optional, but at least one of the two must be `present`.
    - `integer` version can optionally use `<=`, etc.
- When nvfortran supports writing namelists to internal variables, support adding a custom namelist to the output. Then you can have custom variables in `nmllog` output.
