# genunits: Minimalist compile-time physical unit consistency checking for Fortran

Why write genunits? Why not use an existing solution?

- genunits is designed to be as general and easy to extend as possible.
    - Often, units I desire are missing from other packages. In particular, few other packages support rational unit exponents, which are very common in turbulence.
    - TODO: Unit-aware intrinsic functions can easily be added by changing the input file, and this is a first-class feature (it is the *only* way to add unit-aware intrinsic functions).
- As Van Snyder highlights, generating the full set of units within certain bounds requires a huge number of units. This leads to slow compilation times for many compilers. genunits avoids this by intelligently prioritizing which units to include.
- Fortran specifically:
    - PhysUnits checks units are run-time, which will have an impact on performance.
    - quaff is quite nice, but does not use type-bound operators, requiring a bit more work. The units coverage also isn't as good as I'd like, missing turbulence quantities.

## files

- app/genunits.f90
- src/genunits_data.f90
    - DONE: `parameter`s
        - `UNIT_PREFIX = "unit_"`
    - `type`s
        - DONE: `unit_type` type for a derived unit with exponents
        - DONE: `unit_system_type` type for part of `config`
        - MAYBE: `operation_type`: Have array of operations to write. This array can be fed to the module writer, and also to a test writer to write a complete set of tests for the module.
            - `op`: `+`, `-`, `*`, `/`, unary `-`, `<`, `<=`, `>`, `>=`, `==`, `/=`, `sqrt`, `cbrt`, `square`, other intrinsic functions
            - `left_unit` (empty for binary operators and functions)
            - `right_unit`
            - `result_unit`
    - procedures
        - `generate_unit_system`
        - `generate_interfaces`
- src/genunits_io.f90
    - `type`s
        - `config_type`
            - TODO: `tests`
            - TODO: `comparison`: Enable or disable generation of comparison operators. `.true.` by default.
            - DONE: `sqrt`
            - DONE: `cbrt`
            - DONE: `square`
            - TODO: `intrinsics`: Intrinsic functions with same input and output units, or `integer` output. `abs`, `maxval`, `minval`, `maxloc`, `minloc`
            - TODO: `custom`: Character array of additional custom intrinsic functions to create equivalents of. These must have unitless arguments and results. (So `sqrt` received separate treatment as that is not necessarily unitless.)
                - <https://cyber.dabamos.de/programming/modernfortran/intrinsic-procedures.html>
                - <https://gitlab.com/everythingfunctional/quaff/-/issues/17>
    - procedures (mostly subroutines to write Fortran code)
        - module writers
            - `write_module`: Calls all the other subroutines.
                - TODO: Split this up more per below.
            - `write_header`
            - TODO: `write_intrinsics`
            - TODO: `write_custom`
            - `print_interface_stats`
                - Print number of operators per type, exponentiation functions.
        - namelist readers (One subroutine per namelist group.)
            - TODO: `read_reject_namelists`
            - TODO: `read_custom_namelists`
        - test writers
            - Include tests for all combinations of operators, including those which should fail.
- src/units.f90 (computer generated)
- tests/units.nml

## Namelists

### `config`

- `output_file`
- `base_units`: An array of the base units of the unit system. `["kg", "m", "s"]` by default.
- `min_exponents`
- `max_exponents`
- `sqrt`: Adds `sqrt` functions. `.true.` by default.
- `cbrt`: Adds `cbrt` functions. `.true.` by default.
- `square`: Adds `square` functions. `.true.` by default.
- `max_n_units`
- `max_n_interfaces`
- `type_definition`
- `use_line`
- `tests`: Generates a file to test all interfaces. `.false.` by default

### `seed_unit`

Units to seed the unit system and print `use` statements for.

- `label`: The label to give the unit in the `use` statement.
- `e`: An array for the exponents of the unit.

### `reject_unit`

Units to be rejected, that is, not included in the unit system.

- `e`: An array for the exponents of the unit.

## Reducing the number of types to just those required

- Limit number of units, number of total interfaces
- Fewer base units
- Limited exponent range
- Smaller denominator
- Remove units which have no associated operators or functions.
- Remove units with less than a certain number of associated operators or functions.
- Rank all units by how many operators or functions they have, and keep the highest ranking ones that fit within the allowed number of operators and functions.
- Limiting to only units used or units that can appear from combinations.
    - To reduce the number of units farther, examine how many iterations it takes to obtain a particular derived unit. Units which require many iterations from the reference units are less likely to appear and more likely can be safely disregarded.
- Have namelist group `reject` to ensure that particular units do not appear in the `unit_system`, similar to namelist group to specify labeled units.
