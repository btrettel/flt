! tests for the prec module
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_asserts

use prec, only: I5, I9, RP, PI
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

character(len=*), parameter :: LOG_FILENAME = "prec.nml"

call logger%open(LOG_FILENAME)
call tests%start_tests(logger)

call tests%integer_ge(range(1), 5, "default integer exponent range")
call tests%integer_ge(range(1_I5), 5, "integer kind I5 exponent range")
call tests%integer_ge(range(1_I9), 9, "integer kind I9 exponent range")
call tests%integer_ge(range(1.0_RP), 15, "real kind RP exponent range")

! This needs to be modified when changing the precision.
call tests%real_eq(3.141592653589793_RP, PI, "PI value")

call tests%end_tests()
call logger%close()

end program test_asserts
