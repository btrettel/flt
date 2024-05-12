! tests for the generated unitss module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units

use units, only: length   => unit_p10000_p00000_p00000, &
                 time     => unit_p00000_p00000_p10000, &
                 velocity => unit_p10000_p00000_m10000, &
                 area     => unit_p20000_p00000_p00000, &
                 volume   => unit_p30000_p00000_p00000, &
                 sqrt, cbrt, square
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

type(length)   :: x, y, z
type(time)     :: t
type(velocity) :: v
type(area)     :: a
type(volume)   :: vol

call logger%open("units.nml")
call tests%start_tests(logger)

x%v = 1.0_WP
y%v = -1.0_WP
z = x + y
call tests%real_eq(z%v, 0.0_WP, "units value, addition")

x%v = 1.0_WP
y%v = -1.0_WP
z = x - y
call tests%real_eq(z%v, 2.0_WP, "units value, subtraction")

v%v = -0.5_WP
t%v = 3.0_WP
x = v * t
call tests%real_eq(x%v, -1.5_WP, "units value, multiplication")

x%v = 1.0_WP
t%v = 2.0_WP
v = x / t
call tests%real_eq(v%v, 0.5_WP, "units value, division")

a%v = 4.0_WP
x   = sqrt(a)
call tests%real_eq(x%v, 2.0_WP, "units value, sqrt")

vol%v = 27.0_WP
x     = cbrt(vol)
call tests%real_eq(x%v, 3.0_WP, "units value, cbrt")

x%v = 4.0_WP
a   = square(x)
call tests%real_eq(a%v, 16.0_WP, "units value, square")

!call test_exit_code(tests, &
!                    "make test_units_fail_1", &
!                    "compile-time error for physical dimension mismatch, 1", &
!                    "test_units_fail_1.txt")

!call test_exit_code(tests, &
!                    "make test_units_fail_2", &
!                    "compile-time error for physical dimension mismatch, 2", &
!                    "test_units_fail_2.txt")

call tests%end_tests()
call logger%close()

end program test_units
