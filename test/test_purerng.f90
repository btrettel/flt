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
use purerng, only: rng_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

type(rng_type) :: rng
real(kind=WP)  :: r
integer        :: size, i

integer(kind=I10), allocatable :: seed(:)

integer, parameter           :: L = 2, N_PROPERTY = 100
integer(kind=I10), parameter :: LECUYER_M(L) = [2147483563_I10, 2147483399_I10]

call logger%open("purerng.nml")
call tests%start_tests(logger)

call rng%random_seed(size=size)
call tests%integer_eq(size, 2, "lecuyer, random_seed(size)")

allocate(seed(size))
call rng%random_seed(put=[2147483562_I10, 2147483398_I10])

call rng%random_seed(get=seed)
call tests%integer_eq(seed(1), 2147483562_I10, "lecuyer, random_seed(put), seed(1)")
call tests%integer_eq(seed(2), 2147483398_I10, "lecuyer, random_seed(put), seed(2)")

call rng%random_number(r)

! Check that this random number generator is consistent with ifort and ifx.
! <https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/random-number.html>
! TODO: Not clear why ifort and ifx differ from this calculation.
! But the two are within the uncertainty that I would expect from the integer ratio.
! Using the 4.656613e-10 multiplier from the paper makes the two closer, but not the same.
call tests%real_eq(r, 3.920868039131165E-07_WP, "lecuyer, harvest matches ifx", abs_tol=1.0_WP/real(2147483563_I10, WP))

call rng%random_seed(get=seed)
call tests%integer_eq(seed(1), 2147443549_I10, "lecuyer, rng seed(1) after matches ifx")
call tests%integer_eq(seed(2), 2147442707_I10, "lecuyer, rng seed(2) after matches ifx")

! Property test
do i = 1, N_PROPERTY
    call rng%random_seed()
    call rng%random_seed(get=seed)
    call tests%integer_ge(seed(1), 1_I10, "lecuyer, random_seed() seed(1) >= 1")
    call tests%integer_le(seed(1), LECUYER_M(1) - 1_I10, "lecuyer, random_seed() seed(2) <= LECUYER_M(1) - 1")
    call tests%integer_ge(seed(2), 1_I10, "lecuyer, random_seed() seed(2) >= 1")
    call tests%integer_le(seed(2), LECUYER_M(2) - 1_I10, "lecuyer, random_seed() seed(2) <= LECUYER_M(2) - 1")
end do

! TODO: test array `rng_types`, ranks 1 and 2

call tests%end_tests()
call logger%close()

end program test_purerng
