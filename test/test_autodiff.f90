! tests for the autodiff module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_autodiff

use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
use autodiff, only: ad
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

integer, parameter :: N_DV = 2

call logger%open("autodiff.nml")
call tests%start_tests(logger)

call test_scalars(tests)
call test_arrays(tests)
call test_sqrt(tests)

call tests%end_tests()
print *, "1"
call logger%close()
print *, "2"

contains

subroutine test_scalars(tests)
    use autodiff, only: f
    
    type(test_results_type), intent(in out) :: tests
    
    real(kind=WP) :: a, b
    type(ad)      :: x, y, z
    integer       :: c
    
    a = 7.0_WP
    b = 12.0_WP

    call x%init(5.0_WP, 1, N_DV)
    call y%init(1.5_WP, 2, N_DV)
    call z%init_const(-3.3_WP, N_DV)

    call tests%real_eq(x%v, 5.0_WP, "rd_var, value")
    call tests%real_eq(x%dv(1), 1.0_WP, "rd_var, derivative (dv 1)")
    call tests%real_eq(x%dv(2), 0.0_WP, "rd_var, derivative (dv 2)")

    call tests%real_eq(z%v, -3.3_WP, "rd_const, value")
    call tests%real_eq(z%dv(1), 0.0_WP, "rd_const, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd_const, derivative (dv 2)")

    call tests%integer_eq(size(x%dv), N_DV, "size(x%dv) = N_DV")
    call tests%integer_eq(size(y%dv), N_DV, "size(y%dv) = N_DV")
    call tests%integer_eq(size(z%dv), N_DV, "size(z%dv) = N_DV")

    z = x + y

    call tests%real_eq(z%v, 6.5_WP, "rd+rd, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 1.0_WP, "rd+rd, derivative (dv 2)")

    z = x + a

    call tests%real_eq(z%v, 12.0_WP, "rd+real, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "rd+real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd+real, derivative (dv 2)")

    z = b + x

    call tests%real_eq(z%v, 17.0_WP, "rd+real, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "rd+real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd+real, derivative (dv 2)")

    z = x - y

    call tests%real_eq(z%v, 3.5_WP, "rd+rd, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), -1.0_WP, "rd+rd, derivative (dv 2)")

    z = x - a

    call tests%real_eq(z%v, -2.0_WP, "rd+rd, value")
    call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dv 2)")

    z = b - x

    call tests%real_eq(z%v, 7.0_WP, "rd+rd, value")
    call tests%real_eq(z%dv(1), -1.0_WP, "rd+rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dv 2)")

    z = -x

    call tests%real_eq(z%v, -5.0_WP, "rd+rd, value")
    call tests%real_eq(z%dv(1), -1.0_WP, "rd+rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dv 2)")

    z = x * y

    call tests%real_eq(z%v, 5.0_WP*1.5_WP, "rd*rd, value")
    call tests%real_eq(z%dv(1), 1.5_WP, "rd*rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 5.0_WP, "rd*rd, derivative (dv 2)")

    z = a * x

    call tests%real_eq(z%v, 7.0_WP*5.0_WP, "real*rd, value")
    call tests%real_eq(z%dv(1), 7.0_WP, "real*rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "real*rd, derivative (dv 2)")

    z = x * b

    call tests%real_eq(z%v, 5.0_WP*12.0_WP, "rd*real, value")
    call tests%real_eq(z%dv(1), 12.0_WP, "rd*real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd*real, derivative (dv 2)")

    z = x / y

    call tests%real_eq(z%v, 5.0_WP/1.5_WP, "rd/rd, value")
    call tests%real_eq(z%dv(1), 1.0_WP/1.5_WP, "rd/rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), -5.0_WP/(1.5_WP**2), "rd/rd, derivative (dv 2)")

    z = x / a

    call tests%real_eq(z%v, 5.0_WP/7.0_WP, "rd/real, value")
    call tests%real_eq(z%dv(1), 1.0_WP/7.0_WP, "rd/real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd/real, derivative (dv 2)")

    z = b / x

    call tests%real_eq(z%v, 12.0_WP/5.0_WP, "real/rd, value")
    call tests%real_eq(z%dv(1), -12.0_WP/(5.0_WP**2), "real/rd, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "real/rd, derivative (dv 2)")

    z = x**a

    call tests%real_eq(z%v, 5.0_WP**7.0_WP, "rd**real, value")
    call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "rd**real, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd**real, derivative (dv 2)")

    c = 7

    z = x**c

    call tests%real_eq(z%v, 5.0_WP**7.0_WP, "rd**integer, value")
    call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "rd**integer, derivative (dv 1)")
    call tests%real_eq(z%dv(2), 0.0_WP, "rd**integer, derivative (dv 2)")

    z = f(x, y)

    call tests%real_eq(z%v, (2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / 1.5_WP + 1.5_WP, "f(rd, rd), value")
    call tests%real_eq(z%dv(1), 2.0_WP - 2.0_WP * 5.0_WP / 1.5_WP, "f(rd, rd), derivative (dv 1)")
    call tests%real_eq(z%dv(2), -(2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / (1.5_WP**2) &
                                        + 2.0_WP * 5.0_WP / 1.5_WP + 1.0_WP, &
                                        "f(rd, rd), derivative (dv 2)")
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

    call tests%real_eq(w(1)%v, 1.0_WP, "rd_var array, 1, value")
    call tests%real_eq(w(1)%dv(1), 1.0_WP, "rd_var, 1, derivative (dv 1)")
    call tests%real_eq(w(1)%dv(2), 0.0_WP, "rd_var, 1, derivative (dv 2)")
    call tests%real_eq(w(2)%v, 7.0_WP, "rd_var array, 2, value")
    call tests%real_eq(w(2)%dv(1), 0.0_WP, "rd_var, 2, derivative (dv 1)")
    call tests%real_eq(w(2)%dv(2), 1.0_WP, "rd_var, 2, derivative (dv 2)")

    ! Make sure that `assert_dimension` works for `ad`.

    call assert_dimension(a1, b1) ! rank 1
    call assert_dimension(a2, b2) ! rank 2
    call assert_dimension(a3, b3) ! rank 3
end subroutine test_arrays

subroutine test_sqrt(tests)
    use autodiff, only: sqrt
    
    type(test_results_type), intent(in out) :: tests
    
    type(ad) :: x, y

    ! `sqrt`

    call x%init(2.0_WP, 1, N_DV)
    y = sqrt(4.0_WP * x)

    call tests%real_eq(y%v, sqrt(8.0_WP), "rd_var sqrt, value")
    call tests%real_eq(y%dv(1), 1.0_WP / sqrt(2.0_WP), "rd_var sqrt, derivative (dv 1)")
    call tests%real_eq(y%dv(2), 0.0_WP, "rd_var sqrt, derivative (dv 2)")
end subroutine test_sqrt

end program test_autodiff
