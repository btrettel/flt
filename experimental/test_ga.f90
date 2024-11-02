! tests for the ga module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_ga

use prec, only: WP
use ga, only: bounds_type, rand_int, rand_uniform, rand_cauchy, clip
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
integer                 :: ri
real(WP)                :: rr
type(bounds_type)       :: bounds

call logger%open("ga.nml")
call tests%start_tests(logger)

! `rand_int`

ri = rand_int(0, 1, 0.0_WP)
call tests%integer_eq(ri, 0, "rand_int (1)")

ri = rand_int(0, 1, 0.49_WP)
call tests%integer_eq(ri, 0, "rand_int (2)")

ri = rand_int(0, 1, 0.5_WP)
call tests%integer_eq(ri, 1, "rand_int (3)")

ri = rand_int(0, 1, 0.99_WP)
call tests%integer_eq(ri, 1, "rand_int (4)")

ri = rand_int(0, 1, 1.0_WP)
call tests%integer_eq(ri, 1, "rand_int (5)")

! `rand_uniform`

rr = rand_uniform(-2.0_WP, 3.0_WP, 0.0_WP)
call tests%real_eq(rr, -2.0_WP, "rand_uniform (r = 0)")

rr = rand_uniform(-2.0_WP, 3.0_WP, 0.5_WP)
call tests%real_eq(rr, 0.5_WP, "rand_uniform (r = 0.5)")

rr = rand_uniform(-2.0_WP, 3.0_WP, 1.0_WP)
call tests%real_eq(rr, 3.0_WP, "rand_uniform (r = 1)")

! `rand_cauchy`

!rr = rand_cauchy(0.0_WP, 1.0_WP, 0.75_WP)
!call real_eq(rr, 1.0_WP, "rand_cauchy (1)")

! rand_cauchy = m + b * tan(PI * (r - 0.5_WP))

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

rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/6.0_WP)
call tests%real_eq(rr, -sqrt(3.0_WP), "rand_cauchy (r = 1/6)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/4.0_WP)
call tests%real_eq(rr, -1.0_WP, "rand_cauchy (r = 1/4)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/3.0_WP)
call tests%real_eq(rr, -sqrt(3.0_WP)/3.0_WP, "rand_cauchy (r = 1/3)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/2.0_WP)
call tests%real_eq(rr, 0.0_WP, "rand_cauchy (r = 1/2)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 2.0_WP/3.0_WP)
call tests%real_eq(rr, sqrt(3.0_WP)/3.0_WP, "rand_cauchy (r = 2/3)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 3.0_WP/4.0_WP)
call tests%real_eq(rr, 1.0_WP, "rand_cauchy (r = 3/4)")

rr = rand_cauchy(0.0_WP, 1.0_WP, 5.0_WP/6.0_WP)
call tests%real_eq(rr, sqrt(3.0_WP), "rand_cauchy (r = 5/6)")

rr = rand_cauchy(2.0_WP, 1.0_WP, 3.0_WP/4.0_WP)
call tests%real_eq(rr, 3.0_WP, "rand_cauchy (changed m)")

rr = rand_cauchy(0.0_WP, 0.5_WP, 3.0_WP/4.0_WP)
call tests%real_eq(rr, 0.5_WP, "rand_cauchy (changed b)")

! `clip`

bounds%lower = 0.0_WP
bounds%upper = 1.0_WP

rr = -5.0_WP
call clip(bounds, rr)
call tests%real_eq(rr, 0.0_WP, "clip (below)")

rr = 0.5_WP
call clip(bounds, rr)
call tests%real_eq(rr, 0.5_WP, "clip (no change)")

rr = 5.0_WP
call clip(bounds, rr)
call tests%real_eq(rr, 1.0_WP, "clip (above)")

call tests%end_tests()
call logger%close()

end program test_ga
