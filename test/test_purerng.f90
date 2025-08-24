! tests for the purerng module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_purerng

use unittest, only: test_results_type
implicit none

type(test_results_type) :: tests

call tests%start_tests("purerng.nml")

call test_unique(tests)
call test_lecuyer(tests)
call test_determ(tests)
call test_concurrent(tests)
call test_int(tests)
call test_uniform(tests)
call test_cauchy(tests)

call tests%end_tests()

contains

subroutine test_lecuyer(tests)
    use prec, only: I10, WP
    use purerng, only: rng_type, RNG_LECUYER
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    real(WP)       :: harvest
    integer        :: seed_size, rng_num, i

    integer(I10), allocatable :: seed(:)

    integer, parameter      :: L = 2, N_PROPERTY = 100!, N_STATS = 100000
    integer(I10), parameter :: LECUYER_M(L) = [2147483563_I10, 2147483399_I10]

    call rng%random_seed(seed_size=seed_size)
    call tests%integer_eq(seed_size, 2, "lecuyer, random_seed(size)")
    
    call rng%get_rng_num(rng_num)
    call tests%integer_eq(rng_num, RNG_LECUYER, "lecuyer, rng=RNG_LECUYER (default)")

    allocate(seed(seed_size))
    call rng%random_seed(put=[2147483562_I10, 2147483398_I10])

    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 2147483562_I10, "lecuyer, random_seed(put), seed(1)")
    call tests%integer_eq(seed(2), 2147483398_I10, "lecuyer, random_seed(put), seed(2)")

    call rng%random_number(harvest)

    ! Check that this random number generator is consistent with ifort and ifx.
    ! <https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/random-number.html>
    ! TODO: Not clear why ifort and ifx differ from this calculation.
    ! But the two are within the uncertainty that I would expect from the integer ratio.
    ! Using the 4.656613e-10 multiplier from the paper makes the two closer, but not the same.
    call tests%real_eq(harvest, 3.920868039131165E-07_WP, "lecuyer, harvest matches ifx", abs_tol=1.0_WP/real(2147483563_I10, WP))

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
    ! TODO: mean and standard deviation
    ! TODO: fig. 5
    ! TODO: Characterization tests from paper if there are any
end subroutine test_lecuyer

subroutine test_determ(tests)
    use prec, only: I10, WP
    use purerng, only: rng_type, RNG_DETERM
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    real(WP)       :: harvest
    integer        :: rng_num

    integer(I10), allocatable :: seed(:)
    
    call rng%set_determ([0.5_WP])
    
    call rng%get_rng_num(rng_num)
    call tests%integer_eq(rng_num, RNG_DETERM, "determ, rng=RNG_DETERM")
    
    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 2_I10, "determ, random_seed(put), seed(1)")
    call tests%integer_eq(seed(2), 500_I10, "determ, random_seed(put), seed(2)")
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.5_WP, "determ, harvest with seed of size 1 (1)")
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.5_WP, "determ, harvest with seed of size 1 (2)")
    
    call rng%random_seed(put=[2_I10, 100_I10, 200_I10, 300_I10])
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.1_WP, "determ, harvest with seed of size 3 (1)")
    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 3_I10, "determ, harvest with seed of size 3, index after 1")
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.2_WP, "determ, harvest with seed of size 3 (2)")
    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 4_I10, "determ, harvest with seed of size 3, index after 2")
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.3_WP, "determ, harvest with seed of size 3 (3)")
    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 2_I10, "determ, harvest with seed of size 3, index after 3")
    
    call rng%random_number(harvest)
    call tests%real_eq(harvest, 0.1_WP, "determ, harvest with seed of size 3 (4)")
    call rng%random_seed(get=seed)
    call tests%integer_eq(seed(1), 3_I10, "determ, harvest with seed of size 3, index after 4")
end subroutine test_determ

subroutine test_unique(tests)
    use purerng, only: RNG_DETERM, RNG_LECUYER
    
    type(test_results_type), intent(in out) :: tests
    
    call tests%integer_ne(RNG_DETERM, RNG_LECUYER, "RNG_DETERM /= RNG_LECUYER")
end subroutine test_unique

subroutine test_concurrent(tests)
    ! Test that PRNG works in `do concurrent` loop, particularly on nvfortran due to it's extra restrictions.
    
    ! TODO: Later switch to have different seeds for each once you have the parallel seed selection set up.
    ! Test that each random number is different in that case.
    
    use prec, only: WP
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    integer, parameter :: N_RNGS = 100
    
    type(rng_type) :: rng(N_RNGS)
    real(WP)       :: harvest(N_RNGS)
    integer        :: i_rng
    
    do i_rng = 1, N_RNGS ! SERIAL
        call rng(i_rng)%random_seed()
    end do
    
    do concurrent (i_rng = 1:N_RNGS)
        call rng(i_rng)%random_number(harvest(i_rng))
    end do
    
    ! TODO: Replace these pointless tests with tests that each `harvest` is different after adding parallel seed selection.
    do i_rng = 1, N_RNGS ! SERIAL
        call tests%real_gt(harvest(i_rng), 0.0_WP, "test_concurrent, greater than zero")
    end do
    ! TODO: Seems that this can fail if called at about the start of the hour due to the random seed initialization.
    ! Investigate later. Might be fixed by proper "jump"/"splitting" setup for the RNG.
end subroutine test_concurrent

subroutine test_int(tests)
    use prec, only: WP
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    integer        :: ri
    
    call rng%set_determ([0.0_WP])
    call rng%int(0, 1, ri)
    call tests%integer_eq(ri, 0, "rand_int (1)")
    
    call rng%set_determ([0.49_WP])
    call rng%int(0, 1, ri)
    call tests%integer_eq(ri, 0, "rand_int (2)")
    
    call rng%set_determ([0.5_WP])
    call rng%int(0, 1, ri)
    call tests%integer_eq(ri, 1, "rand_int (3)")
    
    call rng%set_determ([0.99_WP])
    call rng%int(0, 1, ri)
    call tests%integer_eq(ri, 1, "rand_int (4)")
    
    call rng%set_determ([1.0_WP])
    call rng%int(0, 1, ri)
    call tests%integer_eq(ri, 1, "rand_int (5)")
end subroutine test_int

subroutine test_uniform(tests)
    use prec, only: WP
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    real(WP)       :: rr
    
    call rng%set_determ([0.0_WP])
    call rng%uniform(-2.0_WP, 3.0_WP, rr)
    call tests%real_eq(rr, -2.0_WP, "rand_uniform (r = 0)")
    
    call rng%set_determ([0.5_WP])
    call rng%uniform(-2.0_WP, 3.0_WP, rr)
    call tests%real_eq(rr, 0.5_WP, "rand_uniform (r = 0.5)")
    
    call rng%set_determ([1.0_WP])
    call rng%uniform(-2.0_WP, 3.0_WP, rr)
    call tests%real_eq(rr, 3.0_WP, "rand_uniform (r = 1)")
end subroutine test_uniform

subroutine test_cauchy(tests)
    use prec, only: WP
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    real(WP)       :: rr
    
    ! rand_cauchy = m + b * tan(PI * (r - 0.5_WP))

    ! theta     sin         cos         tan         r
    ! -PI/2     -1          0           -inf        r - 1/2 = 1/2 ==> r = 1
    ! -PI/3     -sqrt(3)/2  1/2         -sqrt(3)    r - 1/2 = -1/3 ==> r = 1/6
    ! -PI/4     -sqrt(2)/2  sqrt(2)/2   -1          r - 1/2 = -1/4 ==> r = 1/4
    ! -PI/6     -1/2        sqrt(3)/2   -sqrt(3)/3  r - 1/2 = -1/6 ==> r = 1/3
    ! 0         0           1           0           1/2
    ! PI/6      1/2         sqrt(3)/2   sqrt(3)/3   r - 1/2 = 1/6 ==> r = 2/3
    ! PI/4      sqrt(2)/2   sqrt(2)/2   1           r - 1/2 = 1/4 ==> r = 3/4
    ! PI/3      sqrt(3)/2   1/2         sqrt(3)     r - 1/2 = 1/3 ==> r = 5/6
    ! PI/2      1           0           inf         r - 1/2 = 1/2 ==> r = 1

    ! TODO: Add tests for `rand_cauchy` for `r` approaching 0 and 1.

    ! Since new deterministic RNG can't return all rational numbers, some old tests are commented out.

!    rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/6.0_WP)
!    call tests%real_eq(rr, -sqrt(3.0_WP), "rand_cauchy (r = 1/6)")
    
    call rng%set_determ([0.25_WP])
    call rng%cauchy(0.0_WP, 1.0_WP, rr)
    call tests%real_eq(rr, -1.0_WP, "rand_cauchy (r = 1/4)")
    
!    rr = rand_cauchy(0.0_WP, 1.0_WP, 1.0_WP/3.0_WP)
!    call tests%real_eq(rr, -sqrt(3.0_WP)/3.0_WP, "rand_cauchy (r = 1/3)")
    
    call rng%set_determ([0.5_WP])
    call rng%cauchy(0.0_WP, 1.0_WP, rr)
    call tests%real_eq(rr, 0.0_WP, "rand_cauchy (r = 1/2)")

!    rr = rand_cauchy(0.0_WP, 1.0_WP, 2.0_WP/3.0_WP)
!    call tests%real_eq(rr, sqrt(3.0_WP)/3.0_WP, "rand_cauchy (r = 2/3)")
    
    call rng%set_determ([0.75_WP])
    call rng%cauchy(0.0_WP, 1.0_WP, rr)
    call tests%real_eq(rr, 1.0_WP, "rand_cauchy (r = 3/4)")

!    rr = rand_cauchy(0.0_WP, 1.0_WP, 5.0_WP/6.0_WP)
!    call tests%real_eq(rr, sqrt(3.0_WP), "rand_cauchy (r = 5/6)")
    
    call rng%set_determ([0.75_WP])
    call rng%cauchy(2.0_WP, 1.0_WP, rr)
    call tests%real_eq(rr, 3.0_WP, "rand_cauchy (changed m)")
    
    call rng%set_determ([0.75_WP])
    call rng%cauchy(0.0_WP, 0.5_WP, rr)
    call tests%real_eq(rr, 0.5_WP, "rand_cauchy (changed b)")
end subroutine test_cauchy

end program test_purerng
