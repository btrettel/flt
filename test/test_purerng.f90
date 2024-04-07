! tests for the purerng module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_purerng


use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

call logger%open("purerng.nml")
call tests%start_tests(logger)

call test_unique(tests)
call test_lecuyer(tests)
call test_determ(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_lecuyer(tests)
    use prec, only: I10, WP
    use purerng, only: rng_type, RNG_LECUYER
    
    type(test_results_type), intent(in out) :: tests
    
    type(rng_type) :: rng
    real(kind=WP)  :: harvest
    integer        :: seed_size, rng_num, i

    integer(kind=I10), allocatable :: seed(:)

    integer, parameter           :: L = 2, N_PROPERTY = 100!, N_STATS = 100000
    integer(kind=I10), parameter :: LECUYER_M(L) = [2147483563_I10, 2147483399_I10]

    call rng%random_seed(seed_size=seed_size)
    call tests%integer_eq(seed_size, 2, "lecuyer, random_seed(size)")
    
    call rng%get_rng_num(rng_num)
    call tests%integer_eq(rng_num, RNG_LECUYER, "lecuyer, rng=RNG_LECUYER")

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
    real(kind=WP)  :: harvest
    integer        :: rng_num

    integer(kind=I10), allocatable :: seed(:)
    
    call rng%set_rng_num(RNG_DETERM)
    
    call rng%get_rng_num(rng_num)
    call tests%integer_eq(rng_num, RNG_DETERM, "determ, rng=RNG_DETERM")
    
    call rng%random_seed(put=[2_I10, 500_I10])
    
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
    use purerng, only: rng_type, RNG_DETERM, RNG_LECUYER
    
    type(test_results_type), intent(in out) :: tests
    
    call tests%integer_ne(RNG_DETERM, RNG_LECUYER, "RNG_DETERM /= RNG_LECUYER")
end subroutine test_unique

end program test_purerng
