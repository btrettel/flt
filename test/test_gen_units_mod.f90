! tests for the gen_units_mod module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_gen_units_mod

use gen_units_mod, only: config_type, read_config_namelist
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(config_type)       :: config
integer                 :: rc

call logger%open("gen_units_mod.nml")
call tests%start_tests(logger)

call read_config_namelist("test/units.nml", config, rc)

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

call tests%end_tests()
call logger%close()

end program test_gen_units_mod