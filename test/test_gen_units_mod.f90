! tests for the gen_units_mod module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_gen_units_mod

use gen_units_mod, only: config_type, read_config_namelist
!use prec, only: WP
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

call tests%integer_eq(rc, 0, "read_config_namelist rc")

call tests%end_tests()
call logger%close()

end program test_gen_units_mod
