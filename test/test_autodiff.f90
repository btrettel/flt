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
use autodiff, only: rd, rd_var, rd_const, operator(+), operator(-), operator(*), operator(/), operator(**), NDVARS
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

! new declarations for autodiff.f90
real(kind=WP) :: a, b
type(rd)      :: x, y, z
integer       :: c

call logger%open("autodiff.nml")
call tests%start_tests(logger)

! tests for autodiff.f90
! ----------------------

a = 7.0_WP
b = 12.0_WP

x = rd_var(5.0_WP, 1)
y = rd_var(1.5_WP, 2)
z = rd_const(-3.3_WP)

call tests%real_eq(x%v, 5.0_WP, "rd_var, value")
call tests%real_eq(x%dv(1), 1.0_WP, "rd_var, derivative (dvar 1)")
call tests%real_eq(x%dv(2), 0.0_WP, "rd_var, derivative (dvar 2)")

call tests%real_eq(z%v, -3.3_WP, "rd_const, value")
call tests%real_eq(z%dv(1), 0.0_WP, "rd_const, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd_const, derivative (dvar 2)")

call tests%integer_eq(size(x%dv), NDVARS, "size(x%dv) = NDVARS")
call tests%integer_eq(size(y%dv), NDVARS, "size(y%dv) = NDVARS")
call tests%integer_eq(size(z%dv), NDVARS, "size(z%dv) = NDVARS")

z = x + y

call tests%real_eq(z%v, 6.5_WP, "rd+rd, value")
call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 1.0_WP, "rd+rd, derivative (dvar 2)")

z = x + a

call tests%real_eq(z%v, 12.0_WP, "rd+real, value")
call tests%real_eq(z%dv(1), 1.0_WP, "rd+real, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd+real, derivative (dvar 2)")

z = b + x

call tests%real_eq(z%v, 17.0_WP, "rd+real, value")
call tests%real_eq(z%dv(1), 1.0_WP, "rd+real, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd+real, derivative (dvar 2)")

z = x - y

call tests%real_eq(z%v, 3.5_WP, "rd+rd, value")
call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), -1.0_WP, "rd+rd, derivative (dvar 2)")

z = x - a

call tests%real_eq(z%v, -2.0_WP, "rd+rd, value")
call tests%real_eq(z%dv(1), 1.0_WP, "rd+rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dvar 2)")

z = b - x

call tests%real_eq(z%v, 7.0_WP, "rd+rd, value")
call tests%real_eq(z%dv(1), -1.0_WP, "rd+rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dvar 2)")

z = -x

call tests%real_eq(z%v, -5.0_WP, "rd+rd, value")
call tests%real_eq(z%dv(1), -1.0_WP, "rd+rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd+rd, derivative (dvar 2)")

z = x * y

call tests%real_eq(z%v, 5.0_WP*1.5_WP, "rd*rd, value")
call tests%real_eq(z%dv(1), 1.5_WP, "rd*rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 5.0_WP, "rd*rd, derivative (dvar 2)")

z = a * x

call tests%real_eq(z%v, 7.0_WP*5.0_WP, "real*rd, value")
call tests%real_eq(z%dv(1), 7.0_WP, "real*rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "real*rd, derivative (dvar 2)")

z = x * b

call tests%real_eq(z%v, 5.0_WP*12.0_WP, "rd*real, value")
call tests%real_eq(z%dv(1), 12.0_WP, "rd*real, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd*real, derivative (dvar 2)")

z = x / y

call tests%real_eq(z%v, 5.0_WP/1.5_WP, "rd/rd, value")
call tests%real_eq(z%dv(1), 1.0_WP/1.5_WP, "rd/rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), -5.0_WP/(1.5_WP**2), "rd/rd, derivative (dvar 2)")

z = x / a

call tests%real_eq(z%v, 5.0_WP/7.0_WP, "rd/real, value")
call tests%real_eq(z%dv(1), 1.0_WP/7.0_WP, "rd/real, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd/real, derivative (dvar 2)")

z = b / x

call tests%real_eq(z%v, 12.0_WP/5.0_WP, "real/rd, value")
call tests%real_eq(z%dv(1), -12.0_WP/(5.0_WP**2), "real/rd, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "real/rd, derivative (dvar 2)")

z = x**a

call tests%real_eq(z%v, 5.0_WP**7.0_WP, "rd**real, value")
call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "rd**real, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd**real, derivative (dvar 2)")

c = 7

z = x**c

call tests%real_eq(z%v, 5.0_WP**7.0_WP, "rd**integer, value")
call tests%real_eq(z%dv(1), 7.0_WP * (5.0_WP**6.0_WP), "rd**integer, derivative (dvar 1)")
call tests%real_eq(z%dv(2), 0.0_WP, "rd**integer, derivative (dvar 2)")

z = f(x, y)

call tests%real_eq(z%v, (2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / 1.5_WP + 1.5_WP, "f(rd, rd), value")
call tests%real_eq(z%dv(1), 2.0_WP - 2.0_WP * 5.0_WP / 1.5_WP, "f(rd, rd), derivative (dvar 1)")
call tests%real_eq(z%dv(2), -(2.0_WP * 5.0_WP * 1.5_WP - 5.0_WP**2) / (1.5_WP**2) &
                                    + 2.0_WP * 5.0_WP / 1.5_WP + 1.0_WP, &
                                    "f(rd, rd), derivative (dvar 2)")

call tests%end_tests()
call logger%close()

contains

function f(x, y)
    type(rd), intent(in) :: x, y
    type(rd)             :: f
    
    f = (2.0_WP * x * y - x**2) / y + y
    
    return
end function f

end program test_autodiff
