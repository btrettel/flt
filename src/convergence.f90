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
            
            ! Exact or manufactured solutions are called in this function.
            
            ! Additional tests can be added to be used with `tests`.
            
            ! Instead pass out `de` and calculate `de_dv` in `convergence_test`?
            ! Start as-is, later figure out how to refactor to simplify.
        end subroutine solver_de
    end interface
    
    integer                    :: i_n, n_n     ! index of `n_arr` and size of `n_arr`
    integer                    :: i_var, n_var ! index for dependent variables and number of dependent variables
    integer                    :: i_dv, n_dv   ! index for derivatives and number of derivatives
    type(ad), allocatable      :: de_i_n(:), de(:, :)
    ! TODO: real(kind=WP), allocatable :: de_dv_i_n(:), de_dv(:, :)
    type(ad), allocatable      :: p(:)
    real(kind=WP), allocatable :: p_tol_(:)
    character(len=6)           :: i_var_string, i_dv_string
    logical                    :: last
    
    if (present(p_tol)) then
        p_tol_ = p_tol
        ! TODO: assert_dimension for `p_tol` and `p_expected`
    else
        allocate(p_tol_(size(p_expected)))
        p_tol_ = 0.05_WP
    end if
    
    n_n = size(n_arr)
    
    print "(2a)", message, ":"
    print "(5a12)", "n", "var #", "v/dv", "de", "p"
    do i_n = 1, n_n ! SERIAL
        last = (i_n == n_n)
        call solver_de(n_arr(i_n), last, tests, de_i_n)!, de_dv_i_n)
        
        if (i_n == 1) then
            n_var = size(de_i_n)
            allocate(de(n_n, n_var))
            allocate(p(n_var))
            ! TODO: assert_dimension for `p` and `p_expected`
            
            do i_var = 1, n_var ! SERIAL
                de(i_n, i_var) = de_i_n(i_var)
                print "(2i12, a12, es12.4)", n_arr(i_n), i_var, "v", de(i_n, i_var)%v
            end do
        else
            do i_var = 1, n_var ! SERIAL
                de(i_n, i_var) = de_i_n(i_var)
                
                ! order of accuracy; see roy_review_2005 eq. 6 or 8
                p(i_var) = log(de(i_n, i_var) / de(i_n - 1, i_var)) &
                                / log(real(n_arr(i_n - 1), WP) / real(n_arr(i_n), WP))
                
                print "(2i12, a12, es12.4, f12.4)", n_arr(i_n), i_var, "v", de(i_n, i_var)%v, p(i_var)%v
            end do
        end if
    end do
    
    n_dv = size(p(1)%dv)
    
    ! Check that the orders of accuracy are as expected.
    do i_var = 1, n_var ! SERIAL
        write(unit=i_var_string, fmt="(i0)") i_var
        call tests%real_eq(p(i_var)%v, p_expected(i_var), message // ", p, var=" // trim(i_var_string), abs_tol=p_tol_(i_var))
        
        do i_dv = 1, n_dv ! SERIAL
            write(unit=i_dv_string, fmt="(i0)") i_dv
            call tests%real_eq(p(i_var)%dv(i_dv), 0.0_WP, message // ", p%dv(" // trim(i_dv_string) &
                                    // "), var=" // trim(i_var_string), abs_tol=p_tol_(i_var))
        end do
    end do
end subroutine convergence_test

pure function norm_real_rank_1(x, ord)
    use checks, only: assert
    
    real(kind=WP), intent(in)     :: x(:)
    integer, intent(in), optional :: ord
    
    real(kind=WP) :: norm_real_rank_1
    
    integer :: ord_, i
    
    if (present(ord)) then
        ord_ = ord
    else
        ord_ = 2
    end if
    
    norm_real_rank_1 = 0.0_WP
    if (ord == huge(1)) then
        ! $l_\infty$ norm
        
        do i = 1, size(x) ! SERIAL
            norm_real_rank_1 = max(norm_real_rank_1, abs(x(i)))
        end do
    else
        do i = 1, size(x) ! SERIAL
            norm_real_rank_1 = norm_real_rank_1 + abs(x(i))**ord_
        end do
        norm_real_rank_1 = norm_real_rank_1**(1.0_WP/real(ord_, WP))
    end if
    
    call assert(norm_real_rank_1 >= 0.0_WP, "convergence (norm_real_rank_1): negative norm?")
end function norm_real_rank_1

pure function norm_ad_rank_1(x, ord)
    use fmad, only: max, abs
    use checks, only: assert
    
    type(ad), intent(in)          :: x(:)
    integer, intent(in), optional :: ord
    
    type(ad) :: norm_ad_rank_1
    
    integer :: ord_, i
    
    if (present(ord)) then
        ord_ = ord
    else
        ord_ = 2
    end if
    
    call norm_ad_rank_1%init_const(0.0_WP, size(x(1)%dv))
    if (ord == huge(1)) then
        ! $l_\infty$ norm
        
        do i = 1, size(x) ! SERIAL
            norm_ad_rank_1 = max(norm_ad_rank_1, abs(x(i)))
        end do
    else
        do i = 1, size(x) ! SERIAL
            norm_ad_rank_1 = norm_ad_rank_1 + abs(x(i))**ord_
        end do
        norm_ad_rank_1 = norm_ad_rank_1**(1.0_WP/real(ord_, WP))
    end if
    
    call assert(norm_ad_rank_1%v >= 0.0_WP, "convergence (norm_ad_rank_1): negative norm?")
end function norm_ad_rank_1

end module convergence
