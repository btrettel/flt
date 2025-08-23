! tests for the prec module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_prec

use prec, only: I5, I9, WP, PI, ACCEPTABLE_LOG10_SPACING_JUMP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(test_results_type) :: tests

call tests%start_tests("prec.nml")

call tests%integer_ge(range(1), 5, "default integer exponent range")
call tests%integer_ge(range(1_I5), 5, "integer kind I5 exponent range")
call tests%integer_ge(range(1_I9), 9, "integer kind I9 exponent range")

call tests%integer_ge(precision(1.0), 6, "default real kind precision")
call tests%integer_ge(range(1.0), 37, "default real kind exponent range")
call tests%integer_ge(precision(1.0_WP), 15, "real kind WP precision")
call tests%integer_ge(range(1.0_WP), 307, "real kind WP exponent range")
call tests%integer_eq(ACCEPTABLE_LOG10_SPACING_JUMP, 3, "real kind WP ACCEPTABLE_LOG10_SPACING_JUMP")

! This needs to be modified when changing the precision.
call tests%real_eq(3.141592653589793_WP, PI, "PI value")

call tests%end_tests()

end program test_prec
