! # $File$
! 
! Summary: tests for the ga module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_ga

use prec, only: IP, RP, PI
use ga, only: rand_int, rand_cauchy, clip
use logging, only: start_log
use unittest, only: test_type, start_tests, end_tests, integer_equality_test, real_equality_test
implicit none

type(test_type)  :: test_data
integer(kind=IP) :: ri
real(kind=RP)    :: rr

character(len=*), parameter :: LOG_FILENAME = "ga.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

ri = rand_int(0_I5, 1_I5, 0.0_RP)
write(unit=*, fmt=*) ri

ri = rand_int(0_I5, 1_I5, 0.49_RP)
write(unit=*, fmt=*) ri

ri = rand_int(0_I5, 1_I5, 0.5_RP)
write(unit=*, fmt=*) ri

ri = rand_int(0_I5, 1_I5, 0.99_RP)
write(unit=*, fmt=*) ri

ri = rand_int(0_I5, 1_I5, 1.0_RP)
write(unit=*, fmt=*) ri

rr = rand_cauchy(0.0_RP, 1.0_RP, 0.75_RP)
write(unit=*, fmt=*) rr

call end_tests(test_data)

stop

end program test_ga
