! tests for the fmad module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_fmad

use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
use fmad, only: ad
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

integer, parameter :: N_DV = 2

call logger%open("fmad.nml")
call tests%start_tests(logger)

call test_scalars(tests)
call test_arrays(tests)
call test_sqrt(tests)
call test_tanh(tests)
call test_log(tests)
call test_exp(tests)
call test_merge(tests)
call test_max(tests)
call test_min(tests)
call test_abs(tests)
call test_comparison(tests)
call test_trig(tests)
call test_disabled(tests)
call test_fosm(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_scalars(tests)
    use fmad, only: f
    
    type(test_results_type), intent(in out) :: tests
    
    real(WP) :: a, b
    type(ad) :: x, y, z
    integer  :: c
    
    a = 7.0_WP
    b = 12.0_WP

    call x%init(5.0_WP, 1, N_DV)
    call y%init(1.5_WP, 2, N_DV)
    call z%init_const(-3.3_WP, N_DV)

    call tests%real_eq(x%v, 5.0_WP, "ad, value")
    call tests%real_eq(x%dv(1), 1.0_WP, "ad, derivative (dv 1)")
    call tests%real_eq(x%dv(2), 0.0_WP, "ad, derivative (dv 2)")

    call tests%real_eq(z%v, -3.3_WP, "init_const, value")
    call tests%real_eq(z%dv(1), 0.0_WP, "init_const, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "init_const, derivative (dv 2)")

    call tests%integer_eq(size(x%dv), N_DV, "size(x%dv) = N_DV")
    call tests%integer_eq(size(y%dv), N_DV, "size(y%dv) = N_DV")
    call tests%integer_eq(size(z%dv), N_DV, "size(z%dv) = N_DV")

    z = x + y
    call tests%real_eq(z%v, 6.5_WP, "ad+ad, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad+ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 1.0_WP, "ad+ad, derivative (dv 2)")

    z = x + a
    call tests%real_eq(z%v, 12.0_WP, "ad+real, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad+real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad+real, derivative (dv 2)")

    z = b + x
    call tests%real_eq(z%v, 17.0_WP, "ad+real, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad+real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad+real, derivative (dv 2)")

    z = +x
    call tests%real_eq(z%v, 5.0_WP, "+ad, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "+ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "+ad, derivative (dv 2)")

    z = x - y
    call tests%real_eq(z%v, 3.5_WP, "ad+ad, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad+ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), -1.0_WP, "ad+ad, derivative (dv 2)")

    z = x - a
    call tests%real_eq(z%v, -2.0_WP, "ad+ad, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad+ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad+ad, derivative (dv 2)")

    z = b - x
    call tests%real_eq(z%v, 7.0_WP, "ad+ad, value")
    call tests%real_eq(z%dv(1), -1.0_WP, "ad+ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad+ad, derivative (dv 2)")

    z = -x
    call tests%real_eq(z%v, -5.0_WP, "-ad, value")
    call tests%real_eq(z%dv(1), -1.0_WP, "-ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "-ad, derivative (dv 2)")

    z = x * y
    call tests%real_eq(z%v, 5.0_WP*1.5_WP, "ad*ad, value")
    call tests%real_eq(z%dv(1), 1.5_WP, "ad*ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 5.0_WP, "ad*ad, derivative (dv 2)")

    z = a * x
    call tests%real_eq(z%v, 7.0_WP*5.0_WP, "real*ad, value")
    call tests%real_eq(z%dv(1), 7.0_WP, "real*ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "real*ad, derivative (dv 2)")

    z = x * b
    call tests%real_eq(z%v, 5.0_WP*12.0_WP, "ad*real, value")
    call tests%real_eq(z%dv(1), 12.0_WP, "ad*real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad*real, derivative (dv 2)")

    z = x / y
    call tests%real_eq(z%v, 5.0_WP/1.5_WP, "ad/ad, value")
    call tests%real_eq(z%dv(1), 1.0_WP/1.5_WP, "ad/ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), -5.0_WP/(1.5_WP**2), "ad/ad, derivative (dv 2)")

    z = x / a
    call tests%real_eq(z%v, 5.0_WP/7.0_WP, "ad/real, value")
    call tests%real_eq(z%dv(1), 1.0_WP/7.0_WP, "ad/real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad/real, derivative (dv 2)")

    z = b / x
    call tests%real_eq(z%v, 12.0_WP/5.0_WP, "real/ad, value")
    call tests%real_eq(z%dv(1), -12.0_WP/(5.0_WP**2), "real/ad, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "real/ad, derivative (dv 2)")

    z = x**a
    call tests%real_eq(z%v, 5.0_WP**7.0_WP, "ad**real, value")
    call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "ad**real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad**real, derivative (dv 2)")

    c = 7
    z = x**c
    call tests%real_eq(z%v, 5.0_WP**7.0_WP, "ad**integer, value")
    call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "ad**integer, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad**integer, derivative (dv 2)")

    z = f(x, y)
    call tests%real_eq(z%v, (2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / 1.5_WP + 1.5_WP, "f(ad, ad), value")
    call tests%real_eq(z%dv(1), 2.0_WP - 2.0_WP * 5.0_WP / 1.5_WP, "f(ad, ad), derivative (dv 1)")
    call tests%real_eq(z%dv(2), -(2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / (1.5_WP**2) &
                                        + 2.0_WP * 5.0_WP / 1.5_WP + 1.0_WP, &
                                        "f(ad, ad), derivative (dv 2)")
end subroutine test_scalars

subroutine test_arrays(tests)
    use checks, only: assert_dimension
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: u(2), v(2), w(2), &
                a1(5),    b1(5),    &
                a2(5, 5), b2(5, 5), &
                a3(5, 5), b3(5, 5)
    
    ! Test array operations

    call u%init([5.0_WP, 7.0_WP], [1, 2], N_DV)
    call v%init_const([-2.0_WP, 0.0_WP], N_DV)

    w = u + 2.0_WP * v

    call tests%real_eq(w(1)%v, 1.0_WP, "ad array, 1, value")
    call tests%real_eq(w(1)%dv(1), 1.0_WP, "ad, 1, derivative (dv 1)")
    call tests%real_eq(w(1)%dv(2), 0.0_WP, "ad, 1, derivative (dv 2)")
    call tests%real_eq(w(2)%v, 7.0_WP, "ad array, 2, value")
    call tests%real_eq(w(2)%dv(1), 0.0_WP, "ad, 2, derivative (dv 1)")
    call tests%real_eq(w(2)%dv(2), 1.0_WP, "ad, 2, derivative (dv 2)")

    ! Make sure that `assert_dimension` works for `ad`.

    call assert_dimension(a1, b1) ! rank 1
    call assert_dimension(a2, b2) ! rank 2
    call assert_dimension(a3, b3) ! rank 3
end subroutine test_arrays

subroutine test_sqrt(tests)
    use fmad, only: sqrt
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(2.0_WP, 1, N_DV)
    y = sqrt(4.0_WP * x)

    call tests%real_eq(y%v, sqrt(8.0_WP), "ad sqrt, value")
    call tests%real_eq(y%dv(1), 1.0_WP / sqrt(2.0_WP), "ad sqrt, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad sqrt, derivative (dv 2)")
end subroutine test_sqrt

subroutine test_tanh(tests)
    use fmad, only: tanh
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(1.0_WP, 1, N_DV)
    y = tanh(2.0_WP * x)

    call tests%real_eq(y%v, tanh(2.0_WP), "ad tanh, value")
    call tests%real_eq(y%dv(1), 2.0_WP * (1 - tanh(2.0_WP)**2), "ad tanh, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad tanh, derivative (dv 2)")
end subroutine test_tanh

subroutine test_log(tests)
    use fmad, only: log
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(1.0_WP, 1, N_DV)
    y = 4.0_WP * log(2.0_WP * x)

    call tests%real_eq(y%v, 4.0_WP * log(2.0_WP), "ad log, value")
    call tests%real_eq(y%dv(1), 4.0_WP, "ad log, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad log, derivative (dv 2)")
end subroutine test_log

subroutine test_exp(tests)
    use fmad, only: exp
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(1.0_WP, 1, N_DV)
    y = 7.0_WP * exp(0.5_WP * x)

    call tests%real_eq(y%v, 7.0_WP*exp(0.5_WP), "ad exp, value")
    call tests%real_eq(y%dv(1), 3.5_WP * exp(0.5_WP), "ad exp, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad exp, derivative (dv 2)")
end subroutine test_exp

subroutine test_merge(tests)
    use fmad, only: merge
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y, z

    call x%init(1.0_WP, 1, N_DV)
    call y%init(-1.0_WP, 2, N_DV)
    
    z = merge(x, y, .true.)
    call tests%real_eq(z%v, 1.0_WP, "ad merge, .true., value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad merge, .true., derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad merge, .true., derivative (dv 2)")
    
    z = merge(x, y, .false.)
    call tests%real_eq(z%v, -1.0_WP, "ad merge, .false., value")
    call tests%real_eq(z%dv(1), 0.0_WP, "ad merge, .false., derivative (dv 1)")
    call tests%real_eq(z%dv(2), 1.0_WP, "ad merge, .false., derivative (dv 2)")
end subroutine test_merge

subroutine test_max(tests)
    use fmad, only: max
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y, z

    call x%init(1.0_WP, 1, N_DV)
    call y%init(-1.0_WP, 2, N_DV)
    
    z = max(x, y)
    call tests%real_eq(z%v, 1.0_WP, "ad max (1), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad max (1), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad max (1), derivative (dv 2)")
    
    deallocate(y%dv)
    call y%init(2.0_WP, 2, N_DV)
    z = max(x, y)
    call tests%real_eq(z%v, 2.0_WP, "ad max (2), value")
    call tests%real_eq(z%dv(1), 0.0_WP, "ad max (2), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 1.0_WP, "ad max (2), derivative (dv 2)")
    
    z = max(0.0_WP, x)
    call tests%real_eq(z%v, 1.0_WP, "ad max (3), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad max (3), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad max (3), derivative (dv 2)")
    
    deallocate(x%dv)
    call x%init(-1.0_WP, 2, N_DV)
    z = max(0.0_WP, x)
    call tests%real_eq(z%v, 0.0_WP, "ad max (4), value")
    call tests%real_eq(z%dv(1), 0.0_WP, "ad max (4), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad max (4), derivative (dv 2)")
    
    ! test at the switching point
    ! The first argument is used.
    deallocate(x%dv, y%dv)
    call x%init(1.0_WP, 1, N_DV)
    call y%init(1.0_WP, 2, N_DV)
    z = max(x, y)
    call tests%real_eq(z%v, 1.0_WP, "ad max (5), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad max (5), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad max (5), derivative (dv 2)")
end subroutine test_max

subroutine test_min(tests)
    use fmad, only: min
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y, z

    call x%init(1.0_WP, 1, N_DV)
    call y%init(-1.0_WP, 2, N_DV)
    
    z = min(x, y)
    call tests%real_eq(z%v, -1.0_WP, "ad min (1), value")
    call tests%real_eq(z%dv(1), 0.0_WP, "ad min (1), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 1.0_WP, "ad min (1), derivative (dv 2)")
    
    deallocate(x%dv)
    call x%init(-2.0_WP, 1, N_DV)
    z = min(x, y)
    call tests%real_eq(z%v, -2.0_WP, "ad min (2), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad min (2), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad min (2), derivative (dv 2)")
    
    z = min(0.0_WP, x)
    call tests%real_eq(z%v, -2.0_WP, "ad min (3), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad min (3), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad min (3), derivative (dv 2)")
    
    deallocate(x%dv)
    call x%init(1.0_WP, 2, N_DV)
    z = min(0.0_WP, x)
    call tests%real_eq(z%v, 0.0_WP, "ad min (4), value")
    call tests%real_eq(z%dv(1), 0.0_WP, "ad min (4), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad min (4), derivative (dv 2)")
    
    ! test at the switching point
    ! The first argument is used.
    deallocate(x%dv, y%dv)
    call x%init(1.0_WP, 1, N_DV)
    call y%init(1.0_WP, 2, N_DV)
    z = min(x, y)
    call tests%real_eq(z%v, 1.0_WP, "ad min (5), value")
    call tests%real_eq(z%dv(1), 1.0_WP, "ad min (5), derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "ad min (5), derivative (dv 2)")
end subroutine test_min

subroutine test_abs(tests)
    use fmad, only: abs
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(2.0_WP, 1, N_DV)
    y = abs(x)
    call tests%real_eq(y%v, 2.0_WP, "ad abs (positive), value")
    call tests%real_eq(y%dv(1), 1.0_WP, "ad abs (positive), derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad abs (positive), derivative (dv 2)")

    deallocate(x%dv)
    call x%init(-4.0_WP, 2, N_DV)
    y = abs(x)
    call tests%real_eq(y%v, 4.0_WP, "ad abs (negative), value")
    call tests%real_eq(y%dv(1), 0.0_WP, "ad abs (negative), derivative (dv 1)")
    call tests%real_eq(y%dv(2), -1.0_WP, "ad abs (negative), derivative (dv 2)")

    deallocate(x%dv)
    call x%init(0.0_WP, 1, N_DV)
    y = abs(x)
    call tests%real_eq(y%v, 0.0_WP, "ad abs (zero), value")
    call tests%real_eq(y%dv(1), 0.0_WP, "ad abs (zero), derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad abs (zero), derivative (dv 2)")
end subroutine test_abs

subroutine test_comparison(tests)
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(2.0_WP, 1, N_DV)
    call y%init(1.0_WP, 2, N_DV)
    call tests%logical_true(x > y, "ad > (1)")
    call tests%logical_true(x >= y, "ad >= (1)")
    call tests%logical_false(x < y, "ad < (1)")
    call tests%logical_false(x <= y, "ad <= (1)")
    
    deallocate(x%dv, y%dv)
    call x%init(2.0_WP, 1, N_DV)
    call y%init(2.0_WP, 2, N_DV)
    call tests%logical_false(x > y, "ad > (2)")
    call tests%logical_true(x >= y, "ad >= (2)")
    call tests%logical_false(x < y, "ad < (2)")
    call tests%logical_true(x <= y, "ad <= (2)")
    
    deallocate(x%dv, y%dv)
    call x%init(1.0_WP, 1, N_DV)
    call y%init(2.0_WP, 2, N_DV)
    call tests%logical_false(x > y, "ad > (3)")
    call tests%logical_false(x >= y, "ad >= (3)")
    call tests%logical_true(x < y, "ad < (3)")
    call tests%logical_true(x <= y, "ad <= (3)")
end subroutine test_comparison

subroutine test_trig(tests)
    use fmad, only: sin, cos, tan
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(0.0_WP, 1, N_DV)
    y = 2.0_WP * sin(x)
    call tests%real_eq(y%v, 0.0_WP, "ad sin, value")
    call tests%real_eq(y%dv(1), 2.0_WP, "ad sin, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad sin, derivative (dv 2)")
    
    deallocate(x%dv, y%dv)
    call x%init(0.0_WP, 1, N_DV)
    y = 3.0_WP*cos(x)
    call tests%real_eq(y%v, 3.0_WP, "ad cos, value")
    call tests%real_eq(y%dv(1), 0.0_WP, "ad cos, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad cos, derivative (dv 2)")
    
    deallocate(x%dv, y%dv)
    call x%init(0.0_WP, 1, N_DV)
    y = -tan(x)
    call tests%real_eq(y%v, 0.0_WP, "ad tan, value")
    call tests%real_eq(y%dv(1), -1.0_WP, "ad tan, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "ad tan, derivative (dv 2)")
    
    ! TODO: Add more tests where the function value is known exactly.
end subroutine test_trig

subroutine test_disabled(tests)
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    call x%init(-1.0_WP, 1, 0)
    call tests%real_eq(x%v, -1.0_WP, "ad disabled, value")
    call tests%integer_eq(size(x%dv), 0, "ad disabled, size(dv)")

    call y%init_const(2.0_WP, 0)
    call tests%real_eq(y%v, 2.0_WP, "ad disabled, const value")
    call tests%integer_eq(size(y%dv), 0, "ad disabled, const size(dv)")
end subroutine test_disabled

subroutine test_fosm(tests)
    use fmad, only: var, stdev
    
    type(test_results_type), intent(in out) :: tests
    
    integer, parameter :: N_X = 3
    
    type(ad) :: x(N_X)
    real(WP) :: sigmas(N_DV), y(N_X), z(N_X), &
                    y_expected(N_X), z_expected(N_X)

    call x%init_const(0.0_WP, N_DV)
    x(1)%dv(1) = -2.0_WP
    x(1)%dv(1) = 3.0_WP
    x(2)%dv(1) = 1.0_WP
    x(2)%dv(1) = 1.0_WP
    x(3)%dv(1) = 0.0_WP
    x(3)%dv(1) = 0.0_WP
    sigmas(1) = 1.5_WP
    sigmas(2) = 3.0_WP
    
    y = var(x, sigmas)
    z = stdev(x, sigmas)
    y_expected(1) = (x(1)%dv(1)*sigmas(1) + x(1)%dv(2)*sigmas(2))**2
    y_expected(2) = (x(2)%dv(1)*sigmas(1) + x(2)%dv(2)*sigmas(2))**2
    y_expected(3) = (x(3)%dv(1)*sigmas(1) + x(3)%dv(2)*sigmas(2))**2
    z_expected = sqrt(y_expected)
    
    call tests%real_eq(y(1), y_expected(1), "fmad var(1)")
    call tests%real_eq(y(2), y_expected(2), "fmad var(2)")
    call tests%real_eq(y(3), y_expected(3), "fmad var(3)")
    
    call tests%real_eq(z(1), z_expected(1), "fmad stdev(1)")
    call tests%real_eq(z(2), z_expected(2), "fmad stdev(2)")
    call tests%real_eq(z(3), z_expected(3), "fmad stdev(3)")
end subroutine test_fosm

end program test_fmad
