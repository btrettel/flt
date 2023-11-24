! # $File$
! 
! Summary: tests for the prec module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_asserts

use prec, only: RP, PI
use logging, only: start_log
use testmod, only: test_type, start_tests, end_tests, real_equality_test
implicit none

type(test_type) :: test_data

character(len=*), parameter :: LOG_FILENAME = "prec.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

! Originally, I defined `PI` as `4.0_RP * atan(1.0_RP)` so that it automatically changed as the precision is changed with `RP`.
! But Fortran 95 doesn't allow initialization expressions containing non-integer and non-character arguments. So I now define
! `PI` as a number and test with the higher precision version, so that the test will fail if I switch to higher precision.
!call real_equality_test(3.141592653589793_RP, PI, "PI value", test_data)
call real_equality_test(4.0_RP * atan(1.0_RP), PI, "PI value", test_data)

call end_tests(test_data)

stop

end program test_asserts
