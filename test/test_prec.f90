! # $File$
! 
! Summary: tests for the prec module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_asserts

use prec, only: I5, I9, RP, PI
use nmllog, only: start_log
use unittest, only: test_results_type
implicit none

type(test_results_type) :: test_data

character(len=*), parameter :: LOG_FILENAME = "prec.jsonl"

call test_data%start_tests(LOG_FILENAME)
call start_log(LOG_FILENAME)

call test_data%integer_greater_equal_test(range(1), 5, "default integer exponent range")
call test_data%integer_greater_equal_test(range(1_I5), 5, "integer kind I5 exponent range")
call test_data%integer_greater_equal_test(range(1_I9), 9, "integer kind I9 exponent range")
call test_data%integer_greater_equal_test(range(1.0_RP), 15, "real kind RP exponent range")

! This needs to be modified when changing the precision.
call test_data%real_equality_test(3.141592653589793_RP, PI, "PI value")

call test_data%end_tests()

end program test_asserts
