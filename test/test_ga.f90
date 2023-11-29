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

use prec, only: I5, RP
use ga, only: bounds_type, rand_int, rand_cauchy, clip
use logging, only: start_log
use unittest, only: test_type, start_tests, end_tests, integer_equality_test, real_equality_test
implicit none

type(test_type)   :: test_data
integer(kind=I5)  :: ri
real(kind=RP)     :: rr
type(bounds_type) :: bounds

character(len=*), parameter :: LOG_FILENAME = "ga.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

ri = rand_int(0_I5, 1_I5, 0.0_RP)
call integer_equality_test(ri, 0_I5, "rand_int (1)", test_data)

ri = rand_int(0_I5, 1_I5, 0.49_RP)
call integer_equality_test(ri, 0_I5, "rand_int (2)", test_data)

ri = rand_int(0_I5, 1_I5, 0.5_RP)
call integer_equality_test(ri, 1_I5, "rand_int (3)", test_data)

ri = rand_int(0_I5, 1_I5, 0.99_RP)
call integer_equality_test(ri, 1_I5, "rand_int (4)", test_data)

ri = rand_int(0_I5, 1_I5, 1.0_RP)
call integer_equality_test(ri, 1_I5, "rand_int (5)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 0.75_RP)
call real_equality_test(rr, 1.0_RP, "rand_cauchy (1)", test_data)

! TODO: Add more tests for `rand_cauchy`.

bounds%lower = 0.0_RP
bounds%upper = 1.0_RP

rr = -5.0_RP
call clip(bounds, rr)
call real_equality_test(rr, 0.0_RP, "clip (below)", test_data)

rr = 0.5_RP
call clip(bounds, rr)
call real_equality_test(rr, 0.5_RP, "clip (no change)", test_data)

rr = 5.0_RP
call clip(bounds, rr)
call real_equality_test(rr, 1.0_RP, "clip (above)", test_data)

call end_tests(test_data)

stop

end program test_ga
