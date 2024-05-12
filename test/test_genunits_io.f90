! tests for the genunits_io module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_genunits_io

use genunits_io, only: config_type
use genunits_data, only: unit_system_type
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(config_type)       :: config
integer                 :: rc
type(unit_system_type)  :: unit_system

character(len=*), parameter :: TEST_INPUT_FILE = "test/units.nml"

call logger%open("genunits_io.nml")
call tests%start_tests(logger)

call config%read_config_namelist(TEST_INPUT_FILE, rc)

! TODO: `type_definition` and `use_line` are set to the defaults below.
! Later use a different input file to check every member of `config`.

! TODO: Test what the defaults are to make mutation testing happy.

! TODO: Test making `rc /= 0`.

call tests%character_eq(config%output_file, "src/pdim_types.f90", "read_config_namelist, config%output_file")
call tests%character_eq(config%type_definition, "real(kind=WP)", "read_config_namelist, config%type_definition")
call tests%character_eq(config%use_line, "use prec, only: WP", "read_config_namelist, config%use_line")
call tests%real_eq(config%min_exponents(1), 0.0_WP, "read_config_namelist, config%min_exponents(1)")
call tests%real_eq(config%min_exponents(2), 0.0_WP, "read_config_namelist, config%min_exponents(2)")
call tests%real_eq(config%min_exponents(3), -1.0_WP, "read_config_namelist, config%min_exponents(3)")
call tests%real_eq(config%max_exponents(1), 3.0_WP, "read_config_namelist, config%max_exponents(1)")
call tests%real_eq(config%max_exponents(2), 0.0_WP, "read_config_namelist, config%max_exponents(2)")
call tests%real_eq(config%max_exponents(3), 1.0_WP, "read_config_namelist, config%max_exponents(3)")
call tests%character_eq(config%base_units(1), "kg", "read_config_namelist, config%base_units(1)")
call tests%character_eq(config%base_units(2), "m", "read_config_namelist, config%base_units(2)")
call tests%character_eq(config%base_units(3), "s", "read_config_namelist, config%base_units(3)")
call tests%integer_eq(rc, 0, "read_config_namelist, rc")

call config%read_seed_unit_namelists(TEST_INPUT_FILE, rc)

call tests%integer_eq(size(config%seed_units), 5, "read_seed_unit_namelists, size(config%seed_units)")
call tests%integer_eq(size(config%seed_labels), 5, "read_seed_unit_namelists, size(config%seed_labels)")
call tests%integer_eq(rc, 0, "read_seed_unit_namelists, rc")
! TODO: Test more of `read_seed_unit_namelists`.

call config%generate_system(unit_system)

! TODO: test `in_exponent_bounds`

call tests%end_tests()
call logger%close()

end program test_genunits_io
