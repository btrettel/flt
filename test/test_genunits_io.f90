! tests for the genunits_io module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_genunits_io

!use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT

use genunits_io, only: config_type, in_exponent_bounds, denominator_matches
use genunits_data, only: unit_type, unit_system_type
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests
type(config_type)       :: config
integer                 :: rc
type(unit_type)         :: unit
type(unit_system_type)  :: unit_system
integer                 :: i_base_unit

character(len=*), parameter :: TEST_INPUT_FILE = "test/genunits_input.nml"

call logger%open("genunits_io.nml")
call tests%start_tests(logger)

call config%read_config_namelist(TEST_INPUT_FILE, rc)

! TODO: `type_definition` and `use_line` are set to the defaults below.
! Later use a different input file to check every member of `config`.

! TODO: Test what the defaults are to make mutation testing happy.

! TODO: Test making `rc /= 0`.

call tests%character_eq(config%output_file, "src/units.f90", "read_config_namelist, config%output_file")
call tests%character_eq(config%type_definition, "real(WP)", "read_config_namelist, config%type_definition")
call tests%character_eq(config%use_line, "use prec, only: WP", "read_config_namelist, config%use_line")
call tests%real_eq(config%min_exponents(1), -3.0_WP, "read_config_namelist, config%min_exponents(1)")
call tests%real_eq(config%min_exponents(2), 0.0_WP, "read_config_namelist, config%min_exponents(2)")
call tests%real_eq(config%min_exponents(3), -2.0_WP, "read_config_namelist, config%min_exponents(3)")
call tests%real_eq(config%max_exponents(1), 3.0_WP, "read_config_namelist, config%max_exponents(1)")
call tests%real_eq(config%max_exponents(2), 1.0_WP, "read_config_namelist, config%max_exponents(2)")
call tests%real_eq(config%max_exponents(3), 1.0_WP, "read_config_namelist, config%max_exponents(3)")
call tests%character_eq(config%base_units(1), "m", "read_config_namelist, config%base_units(1)")
call tests%character_eq(config%base_units(2), "kg", "read_config_namelist, config%base_units(2)")
call tests%character_eq(config%base_units(3), "s", "read_config_namelist, config%base_units(3)")
call tests%integer_eq(rc, 0, "read_config_namelist, rc")

! `read_seed_unit_namelists`

call config%read_seed_unit_namelists(TEST_INPUT_FILE, rc)

call tests%integer_eq(size(config%seed_units), 10, "read_seed_unit_namelists, size(config%seed_units)")
call tests%integer_eq(size(config%seed_labels), 10, "read_seed_unit_namelists, size(config%seed_labels)")
call tests%integer_eq(rc, 0, "read_seed_unit_namelists, rc")
! TODO: Test more of `read_seed_unit_namelists`.

! `generate_system`

call config%generate_system(unit_system)

call tests%integer_eq(size(unit_system%units), config%max_n_units, "generate_system, size(unit_system%units)")
call tests%integer_eq(unit_system%n_base_units, 3, "generate_system, unit_system%n_base_units)")
call tests%integer_eq(size(unit_system%base_units), 3, "generate_system, size(unit_system%base_units))")

do i_base_unit = 1, unit_system%n_base_units
    call tests%integer_ge(len(trim(unit_system%base_units(i_base_unit))), 1, &
                            "generate_system, unit_system%base_units not empty strings")
end do

! TODO: test that none of the units are outside the bounds or violate the desired denominator

! `in_exponent_bounds`

unit%e = [1.0_WP, 0.0_WP, 0.0_WP]
call tests%logical_true(in_exponent_bounds(config, unit), "in_exponent_bounds, .true.")

unit%e = [10.0_WP, 0.0_WP, 0.0_WP]
call tests%logical_false(in_exponent_bounds(config, unit), "in_exponent_bounds, .false.")

! `denominator_matches`

call tests%logical_true(denominator_matches(1.0_WP, 2), "denominator_matches, .true. (1)")
call tests%logical_true(denominator_matches(2.5_WP, 6), "denominator_matches, .true. (2)")

call tests%logical_false(denominator_matches(0.5_WP, 1), "denominator_matches, .true. (1)")
call tests%logical_false(denominator_matches(5.0_WP/3.0_WP, 2), "denominator_matches, .true. (2)")

! `write_type`

!call config%write_type(OUTPUT_UNIT, 1, unit_system)

call tests%end_tests()
call logger%close()

end program test_genunits_io
