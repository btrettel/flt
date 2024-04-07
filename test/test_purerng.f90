! tests for the purerng module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_purerng

use prec, only: I10, WP
use nmllog, only: log_type
use unittest, only: test_results_type
use purerng, only: rng_type, lecuyer
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

type(rng_type) :: rng
real(kind=WP)  :: r

call logger%open("purerng.nml")
call tests%start_tests(logger)

allocate(rng%seed(2))

rng%seed = [2147483562_I10, 2147483398_I10]

call lecuyer(rng, r)

! Check that this random number generator is consistent with ifort and ifx.
! <https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/random-number.html>
! TODO: Not clear why ifort and ifx differ from this calculation.
! But the two are within the uncertainty that I would expect from the integer ratio.
call tests%real_eq(r, 3.920868039131165E-07_WP, "lecuyer rng matches ifx", abs_tol=1.0_WP/real(2147483563_I10, WP))
call tests%integer_eq(rng%seed(1), 2147443549_I10, "lecuyer rng seed(1) after matches ifx")
call tests%integer_eq(rng%seed(2), 2147442707_I10, "lecuyer rng seed(2) after matches ifx")

! TODO: test array version too. How do elemental subroutines work? I guess the seed means it must be sequential.

call tests%end_tests()
call logger%close()

end program test_purerng
