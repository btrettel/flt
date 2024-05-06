! tests for the test_units_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units_types

use units_types, only: unit_type, unit_system_type
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(unit_type)         :: unit
type(unit_system_type)  :: unit_system

call logger%open("units_types.nml")
call tests%start_tests(logger)

unit_system%base_units   = ["kg", "m ", "s "]
unit_system%n_base_units = size(unit_system%base_units)

call tests%integer_eq(unit_system%n_base_units, 3, "unit_system%n_base_units == 3")

allocate(unit%e(unit_system%n_base_units))
unit%e(1) = 1.0_WP
unit%e(2) = 1.5_WP
unit%e(3) = -2.0_WP

call tests%character_eq(unit%label(), "unit_p10000_p15000_m20000", "unit%label")
call tests%character_eq(unit%readable(unit_system), "kg^1.0000 m^1.5000 s^-2.0000", "unit%readable")

call tests%end_tests()
call logger%close()

end program test_units_types
