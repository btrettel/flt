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

use prec, only: RP, PI ! I5, I9, 
use logging, only: start_log
use unittest, only: test_type, start_tests, end_tests, real_equality_test!, integer_greater_equal_test
implicit none

type(test_type) :: test_data

character(len=*), parameter :: LOG_FILENAME = "prec.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

! TODO: Test exponent range of `RP`.

!call integer_greater_equal_test(range(1), 5, "default integer exponent range", test_data)
!call integer_greater_equal_test(range(1_I5), 5, "integer kind I5 exponent range", test_data)
!call integer_greater_equal_test(range(1_I9), 9, "integer kind I9 exponent range", test_data)

! This needs to be modified when changing the precision.
call real_equality_test(3.141592653589793_RP, PI, "PI value", test_data)

call end_tests(test_data)

stop

end program test_asserts
