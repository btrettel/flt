# gen_units: Minimalist compile-time physical unit consistency checking for Fortran

## to-do

- Put `linspace` into an experimental file.
- Put refactor into entirely new files to refer to the old ones as needed.

## Terminology

Based largely on <https://www.gmpreussner.com/research/dimensional-analysis-in-programming-languages>, which is similar to the ISO standard as described by mills_quantities_2007, but a bit more consistent.

- numerical value: `v`
    -  mills_quantities_2007 p. 3
- unit
    - mills_quantities_2007 p. 3
- physical dimension
    - quantity (which seems inconsistent with "quantity" as a base dimension)
- derived unit

## files

- app/gen_units.f90
- src/units_types.f90
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
- src/gen_units_mod.f90
    - `type`s
        - `config_type`
            - DONE: `output_file`
            - DONE: `base_units`
            - DONE: `type_definition`: Characters of type definition to use in derived time 
            - DONE: `use_line`: Characters of custom use line(s) to put in the header of the generated module. Empty by default.
            - TODO: `max_n_units`
            - TODO: `max_n_interfaces`
            - TODO: `tests`
            - TODO: `comparison`: Enable or disable generation of comparison operators. `.true.` by default.
            - TODO: `sqrt`
            - TODO: `cbrt`
            - TODO: `square`
            - TODO: `intrinsics`: Enable or disable generation of equivalents for many intrinsic operators. `.true.` by default.
            - TODO: `custom`: Character array of additional custom elemental functions to create equivalents of. Empty by default.
    - procedures (mostly subroutines to write Fortran code)
        - module writers
            - `write_module`: Calls all the other subroutines.
            - `write_header`
            - `write_intrinsics`
                - <https://gitlab.com/everythingfunctional/quaff/-/issues/17>
            - `print_interface_stats`
                - Print number of operators per type, exponentiation functions.
        - namelist readers (One subroutine per namelist group.)
            - DONE: `read_config_namelist`
            - TODO: `read_unit_namelists`
            - TODO: `read_reject_namelists`
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