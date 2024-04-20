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
use autodiff, only: ad, f
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

real(kind=WP) :: a, b
type(ad)      :: u(2), v(2), w(2), x, y, z
integer       :: c

integer, parameter :: N_DV = 2

call logger%open("autodiff.nml")
call tests%start_tests(logger)

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

call tests%end_tests()
call logger%close()

end program test_autodiff
