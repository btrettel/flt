! tests for the generated units_ad module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units_ad

use units_ad, only: unitless  => unit_p00000_p00000_p00000, &
                    si_length => unit_p10000_p00000_p00000
use nmllog, only: log_type
use unittest, only: test_results_type
use prec, only: WP
implicit none

type(test_results_type) :: tests

type(unitless)  :: u
type(si_length) :: x

integer, parameter :: N_DV = 2

call tests%start_tests("units_ad.nml")

! TODO: Won't work yet due to AD: x = unitless(2.0_WP)*x

call x%v%init(2.0_WP, 1, N_DV)
x = -1.0_WP*x
call tests%real_eq(x%v%v, -2.0_WP, "units value, -1.0_WP*x")
call tests%real_eq(x%v%d(1), -1.0_WP, "units dv(1), -1.0_WP*x")
call tests%real_eq(x%v%d(2), 0.0_WP, "units dv(2), -1.0_WP*x")

deallocate(x%v%d)
call u%v%init_const(2.0_WP, N_DV)
call x%v%init(1.0_WP, 2, N_DV)
x = u*x
call tests%real_eq(x%v%v, 2.0_WP, "units value, u*x")
call tests%real_eq(x%v%d(1), 0.0_WP, "units dv(1), u*x")
call tests%real_eq(x%v%d(2), 2.0_WP, "units dv(2), u*x")

call tests%end_tests()

end program test_units_ad
