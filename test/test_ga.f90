! # $File$
! 
! Summary: tests for the ga module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_ga

use prec, only: RP
use ga, only: bounds_type, rand_int, rand_uniform, rand_cauchy, clip
use logging, only: start_log
use unittest, only: test_type, start_tests, end_tests, integer_equality_test, real_equality_test
implicit none

type(test_type)   :: test_data
integer           :: ri
real(kind=RP)     :: rr
type(bounds_type) :: bounds

character(len=*), parameter :: LOG_FILENAME = "ga.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

! `rand_int`

ri = rand_int(0, 1, 0.0_RP)
call integer_equality_test(ri, 0, "rand_int (1)", test_data)

ri = rand_int(0, 1, 0.49_RP)
call integer_equality_test(ri, 0, "rand_int (2)", test_data)

ri = rand_int(0, 1, 0.5_RP)
call integer_equality_test(ri, 1, "rand_int (3)", test_data)

ri = rand_int(0, 1, 0.99_RP)
call integer_equality_test(ri, 1, "rand_int (4)", test_data)

ri = rand_int(0, 1, 1.0_RP)
call integer_equality_test(ri, 1, "rand_int (5)", test_data)

! `rand_uniform`

rr = rand_uniform(-2.0_RP, 3.0_RP, 0.0_RP)
call real_equality_test(rr, -2.0_RP, "rand_uniform (r = 0)", test_data)

rr = rand_uniform(-2.0_RP, 3.0_RP, 0.5_RP)
call real_equality_test(rr, 0.5_RP, "rand_uniform (r = 0.5)", test_data)

rr = rand_uniform(-2.0_RP, 3.0_RP, 1.0_RP)
call real_equality_test(rr, 3.0_RP, "rand_uniform (r = 1)", test_data)

! `rand_cauchy`

!rr = rand_cauchy(0.0_RP, 1.0_RP, 0.75_RP)
!call real_equality_test(rr, 1.0_RP, "rand_cauchy (1)", test_data)

! rand_cauchy = m + b * tan(PI * (r - 0.5_RP))

! theta     sin         cos         tan         r
! -PI/2     -1          0           -inf         r - 1/2 = 1/2 ==> r = 1
! -PI/3     -sqrt(3)/2  1/2         -sqrt(3)    r - 1/2 = -1/3 ==> r = 1/6
! -PI/4     -sqrt(2)/2  sqrt(2)/2   -1          r - 1/2 = -1/4 ==> r = 1/4
! -PI/6     -1/2        sqrt(3)/2   -sqrt(3)/3  r - 1/2 = -1/6 ==> r = 1/3
! 0         0           1           0           1/2
! PI/6      1/2         sqrt(3)/2   sqrt(3)/3   r - 1/2 = 1/6 ==> r = 2/3
! PI/4      sqrt(2)/2   sqrt(2)/2   1           r - 1/2 = 1/4 ==> r = 3/4
! PI/3      sqrt(3)/2   1/2         sqrt(3)     r - 1/2 = 1/3 ==> r = 5/6
! PI/2      1           0           inf         r - 1/2 = 1/2 ==> r = 1

! TODO: Add tests for `rand_cauchy` for `r` approaching 0 and 1.

rr = rand_cauchy(0.0_RP, 1.0_RP, 1.0_RP/6.0_RP)
call real_equality_test(rr, -sqrt(3.0_RP), "rand_cauchy (r = 1/6)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 1.0_RP/4.0_RP)
call real_equality_test(rr, -1.0_RP, "rand_cauchy (r = 1/4)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 1.0_RP/3.0_RP)
call real_equality_test(rr, -sqrt(3.0_RP)/3.0_RP, "rand_cauchy (r = 1/3)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 1.0_RP/2.0_RP)
call real_equality_test(rr, 0.0_RP, "rand_cauchy (r = 1/2)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 2.0_RP/3.0_RP)
call real_equality_test(rr, sqrt(3.0_RP)/3.0_RP, "rand_cauchy (r = 2/3)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 3.0_RP/4.0_RP)
call real_equality_test(rr, 1.0_RP, "rand_cauchy (r = 3/4)", test_data)

rr = rand_cauchy(0.0_RP, 1.0_RP, 5.0_RP/6.0_RP)
call real_equality_test(rr, sqrt(3.0_RP), "rand_cauchy (r = 5/6)", test_data)

rr = rand_cauchy(2.0_RP, 1.0_RP, 3.0_RP/4.0_RP)
call real_equality_test(rr, 3.0_RP, "rand_cauchy (changed m)", test_data)

rr = rand_cauchy(0.0_RP, 0.5_RP, 3.0_RP/4.0_RP)
call real_equality_test(rr, 0.5_RP, "rand_cauchy (changed b)", test_data)

! `clip`

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
