! Module for procedures used for convergence testing.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module convergence

use fmad, only: ad
use prec, only: WP
implicit none
private

public :: norm, convergence_test

interface norm
    ! <https://numpy.org/doc/stable/reference/generated/numpy.linalg.norm.html>
    ! <https://help.imsl.com/fortran/fnlmath/current/NORM.htm>
    ! <https://gcc.gnu.org/onlinedocs/gfortran/NORM2.html>
    
    module procedure norm_real_rank_1
    module procedure norm_ad_rank_1
end interface norm

contains

subroutine convergence_test(n_arr, solver_de, p_expected, message, tests, p_tol)
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    use checks, only: assert, assert_dimension
    use nmllog, only: CRITICAL_LEVEL
    use unittest, only: test_results_type
    use fmad, only: log
    
    integer, intent(in)                     :: n_arr(:) ! array of number of grid cells, time steps, Monte Carlo samples, etc.
    real(kind=WP), intent(in)               :: p_expected(:) ! expected order of convergence
    character(len=*), intent(in)            :: message
    type(test_results_type), intent(in out) :: tests
    real(kind=WP), intent(in), optional     :: p_tol
    
    interface
        subroutine solver_de(n, last, tests, de)!, de_dv)
            use unittest, only: test_results_type
            use fmad, only: ad
            use prec, only: WP
            
            integer, intent(in)                     :: n           ! number of grid cells, time steps, Monte Carlo samples, etc.
            logical, intent(in)                     :: last        ! for tests that should only be done on the last `n`
            type(test_results_type), intent(in out) :: tests
            type(ad), intent(out), allocatable      :: de(:)       ! discretization error for value
            !real(kind=WP), intent(out), allocatable :: de_dv(:, :) ! discretization error for derivatives
            
            ! This is not `pure` to make debugging easier.
            
            ! Exact or manufactured solutions are called in this function.
            
            ! Discretization error is calculated in here.
            ! A norm can be used or a local metric can be used.
            
            ! Additional tests can be added to be used with `tests`.
            
            ! Instead pass out `de` and calculate `de_dv` in `convergence_test`?
            ! Start as-is, later figure out how to refactor to simplify.
        end subroutine solver_de
    end interface
    
    integer                    :: i_n, n_n     ! index of `n_arr` and size of `n_arr`
    integer                    :: i_var, n_var ! index for dependent variables and number of dependent variables
    integer                    :: i_dv, n_dv   ! index for derivatives and number of derivatives
    integer                    :: stdout_level, n_failures
    type(ad), allocatable      :: de_i_n(:), de(:, :)
    ! TODO: real(kind=WP), allocatable :: de_dv_i_n(:), de_dv(:, :)
    type(ad), allocatable      :: p(:)
    real(kind=WP), allocatable :: p_tol_(:)
    character(len=6)           :: i_var_string, i_dv_string
    logical                    :: last
    
    if (present(p_tol)) then
        p_tol_ = p_tol
    else
        allocate(p_tol_(size(p_expected)))
        p_tol_ = 0.05_WP
    end if
    
    call assert(all(n_arr > 0), "convergence (convergence_test): n can not be zero or negative")
    call assert(len(message) > 0, "convergence (convergence_test): message can not be empty")
    call assert(p_tol_ > 0.0_WP, "convergence (convergence_test): p_tol is too small")
    
    n_n = size(n_arr)
    
    ! Suppress test messages while printing the table.
    stdout_level = tests%logger%stdout_level
    tests%logger%stdout_level = CRITICAL_LEVEL + 1
    n_failures = tests%n_failures
    
    print "(2a)", message, ":"
    print "(3a6, 2a14)", "n", "var #", "v/dv", "de", "p"
    ! MAYBE: Run convergence tests in parallel later?
    do i_n = 1, n_n ! SERIAL
        last = (i_n == n_n)
        call solver_de(n_arr(i_n), last, tests, de_i_n)!, de_dv_i_n)
        
        if (i_n == 1) then
            n_var = size(de_i_n)
            allocate(de(n_n, n_var))
            allocate(p(n_var))
            call assert_dimension(p, p_expected)
            
            do i_var = 1, n_var ! SERIAL
                call assert(de_i_n(i_var)%v >= 0.0_WP, "convergence (convergence_test): discretization error must be >= 0")
                de(i_n, i_var) = de_i_n(i_var)
                print "(2i6, a6, es14.5)", n_arr(i_n), i_var, "v", de(i_n, i_var)%v
            end do
        else
            do i_var = 1, n_var ! SERIAL
                call assert(de_i_n(i_var)%v >= 0.0_WP, "convergence (convergence_test): discretization error must be >= 0")
                de(i_n, i_var) = de_i_n(i_var)
                
                ! order of accuracy; see roy_review_2005 eq. 6 or 8
                p(i_var) = log(de(i_n, i_var) / de(i_n - 1, i_var)) &
                                / log(real(n_arr(i_n - 1), WP) / real(n_arr(i_n), WP))
                
                print "(2i6, a6, es14.5, f14.6)", n_arr(i_n), i_var, "v", de(i_n, i_var)%v, p(i_var)%v
            end do
        end if
    end do
    
    n_dv = size(p(1)%dv)
    
    ! Re-enable test failure messages.
    if (tests%n_failures > n_failures) then
        write(unit=ERROR_UNIT, fmt="(a)") "One or more test failures were suppressed during convergence test. Check the test log."
    end if
    tests%logger%stdout_level = stdout_level
    
    ! Check that the orders of accuracy are as expected.
    do i_var = 1, n_var ! SERIAL
        call assert(p_expected(i_var) > 0.0_WP, "convergence (convergence_test): p_expected is zero or negative, " &
                                                // "which probably isn't desired")
        
        write(unit=i_var_string, fmt="(i0)") i_var
        call tests%real_eq(p(i_var)%v, p_expected(i_var), message // ", p, var=" // trim(i_var_string), abs_tol=p_tol_(i_var))
        
        do i_dv = 1, n_dv ! SERIAL
            write(unit=i_dv_string, fmt="(i0)") i_dv
            call tests%real_eq(p(i_var)%dv(i_dv), 0.0_WP, message // ", p%dv(" // trim(i_dv_string) &
                                    // "), var=" // trim(i_var_string), abs_tol=p_tol_(i_var))
        end do
    end do
end subroutine convergence_test

pure function norm_real_rank_1(x, ord, lower, upper)
    use checks, only: assert
    
    real(kind=WP), intent(in)     :: x(:)
    integer, intent(in), optional :: ord, lower, upper
    
    ! `lower` and `upper` are used for cases where not all indices are to be summed over.
    ! For example, if you have ghost cells, those cells have fictitious data that should not be summed over.
    
    real(kind=WP) :: norm_real_rank_1
    
    integer :: ord_, lower_, upper_, i
    
    if (present(ord)) then
        ord_ = ord
    else
        ord_ = 2
    end if
    
    if (present(lower)) then
        lower_ = lower
    else
        lower_ = 1
    end if
    
    if (present(upper)) then
        upper_ = upper
    else
        upper_ = size(x)
    end if
    
    call assert(lower_ >= lbound(x, dim=1), "convergence (norm_ad_rank_1): lower index bound must be >= lbound")
    call assert(upper_ <= ubound(x, dim=1), "convergence (norm_ad_rank_1): upper index bound must be >= ubound")
    call assert(lower_ < upper_, "convergence (norm_ad_rank_1): lower index bound must be above upper index bound")
    
    norm_real_rank_1 = 0.0_WP
    if (ord == huge(1)) then
        ! $l_\infty$ norm
        
        do i = lower_, upper_ ! SERIAL
            norm_real_rank_1 = max(norm_real_rank_1, abs(x(i)))
        end do
    else
        do i = lower_, upper_ ! SERIAL
            norm_real_rank_1 = norm_real_rank_1 + abs(x(i))**ord_
        end do
        norm_real_rank_1 = norm_real_rank_1**(1.0_WP/real(ord_, WP))
    end if
    
    call assert(norm_real_rank_1 >= 0.0_WP, "convergence (norm_real_rank_1): negative norm?")
end function norm_real_rank_1

pure function norm_ad_rank_1(x, ord, lower, upper)
    use fmad, only: max, abs
    use checks, only: assert
    
    type(ad), intent(in)          :: x(:)
    integer, intent(in), optional :: ord, lower, upper
    
    type(ad) :: norm_ad_rank_1
    
    integer :: ord_, lower_, upper_, i
    
    if (present(ord)) then
        ord_ = ord
    else
        ord_ = 2
    end if
    
    if (present(lower)) then
        lower_ = lower
    else
        lower_ = 1
    end if
    
    if (present(upper)) then
        upper_ = upper
    else
        upper_ = size(x)
    end if
    
    call assert(lower_ >= lbound(x, dim=1), "convergence (norm_ad_rank_1): lower index bound must be >= lbound")
    call assert(upper_ <= ubound(x, dim=1), "convergence (norm_ad_rank_1): upper index bound must be >= ubound")
    call assert(lower_ < upper_, "convergence (norm_ad_rank_1): lower index bound must be above upper index bound")
    
    call norm_ad_rank_1%init_const(0.0_WP, size(x(1)%dv))
    if (ord == huge(1)) then
        ! $l_\infty$ norm
        
        do i = lower_, upper_ ! SERIAL
            norm_ad_rank_1 = max(norm_ad_rank_1, abs(x(i)))
        end do
    else
        do i = lower_, upper_ ! SERIAL
            norm_ad_rank_1 = norm_ad_rank_1 + abs(x(i))**ord_
        end do
        norm_ad_rank_1 = norm_ad_rank_1**(1.0_WP/real(ord_, WP))
    end if
    
    call assert(norm_ad_rank_1%v >= 0.0_WP, "convergence (norm_ad_rank_1): negative norm?")
end function norm_ad_rank_1

end module convergence
