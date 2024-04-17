use autodiff, only: rd, rd_var, rd_const, operator(+), operator(-), operator(*), operator(/), operator(**), NDVARS

! new declarations for autodiff.f90
real(kind=RP)     :: a, b
type(rd)          :: x, y, z
integer(kind=IP)  :: c
type(rd), dimension(NDVARS)      :: dvars
real(kind=RP), dimension(NDVARS) :: ovars

! tests for autodiff.f90
! ----------------------

a = 7.0_RP
b = 12.0_RP

x = rd_var(5.0_RP, 1_IP)
y = rd_var(1.5_RP, 2_IP)
z = rd_const(-3.3_RP)

call real_equality_test(x%v, 5.0_RP, "rd_var, value", test_data)
call real_equality_test(x%dv(1_IP), 1.0_RP, "rd_var, derivative (dvar 1)", test_data)
call real_equality_test(x%dv(2_IP), 0.0_RP, "rd_var, derivative (dvar 2)", test_data)

call real_equality_test(z%v, -3.3_RP, "rd_const, value", test_data)
call real_equality_test(z%dv(1_IP), 0.0_RP, "rd_const, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2_IP), 0.0_RP, "rd_const, derivative (dvar 2)", test_data)

call integer_equality_test(size(x%dv), NDVARS, "size(x%dv) = NDVARS", test_data)
call integer_equality_test(size(y%dv), NDVARS, "size(y%dv) = NDVARS", test_data)
call integer_equality_test(size(z%dv), NDVARS, "size(z%dv) = NDVARS", test_data)

z = x + y

call real_equality_test(z%v, 6.5_RP, "rd+rd, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP, "rd+rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 1.0_RP, "rd+rd, derivative (dvar 2)", test_data)

z = x + a

call real_equality_test(z%v, 12.0_RP, "rd+real, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP, "rd+real, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd+real, derivative (dvar 2)", test_data)

z = b + x

call real_equality_test(z%v, 17.0_RP, "rd+real, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP, "rd+real, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd+real, derivative (dvar 2)", test_data)

z = x - y

call real_equality_test(z%v, 3.5_RP, "rd+rd, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP, "rd+rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), -1.0_RP, "rd+rd, derivative (dvar 2)", test_data)

z = x - a

call real_equality_test(z%v, -2.0_RP, "rd+rd, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP, "rd+rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd+rd, derivative (dvar 2)", test_data)

z = b - x

call real_equality_test(z%v, 7.0_RP, "rd+rd, value", test_data)
call real_equality_test(z%dv(1), -1.0_RP, "rd+rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd+rd, derivative (dvar 2)", test_data)

z = -x

call real_equality_test(z%v, -5.0_RP, "rd+rd, value", test_data)
call real_equality_test(z%dv(1), -1.0_RP, "rd+rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd+rd, derivative (dvar 2)", test_data)

z = x * y

call real_equality_test(z%v, 5.0_RP*1.5_RP, "rd*rd, value", test_data)
call real_equality_test(z%dv(1), 1.5_RP, "rd*rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 5.0_RP, "rd*rd, derivative (dvar 2)", test_data)

z = a * x

call real_equality_test(z%v, 7.0_RP*5.0_RP, "real*rd, value", test_data)
call real_equality_test(z%dv(1), 7.0_RP, "real*rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "real*rd, derivative (dvar 2)", test_data)

z = x * b

call real_equality_test(z%v, 5.0_RP*12.0_RP, "rd*real, value", test_data)
call real_equality_test(z%dv(1), 12.0_RP, "rd*real, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd*real, derivative (dvar 2)", test_data)

z = x / y

call real_equality_test(z%v, 5.0_RP/1.5_RP, "rd/rd, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP/1.5_RP, "rd/rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), -5.0_RP/(1.5_RP**2_IP), "rd/rd, derivative (dvar 2)", test_data)

z = x / a

call real_equality_test(z%v, 5.0_RP/7.0_RP, "rd/real, value", test_data)
call real_equality_test(z%dv(1), 1.0_RP/7.0_RP, "rd/real, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd/real, derivative (dvar 2)", test_data)

z = b / x

call real_equality_test(z%v, 12.0_RP/5.0_RP, "real/rd, value", test_data)
call real_equality_test(z%dv(1), -12.0_RP/(5.0_RP**2_IP), "real/rd, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "real/rd, derivative (dvar 2)", test_data)

z = x**a

call real_equality_test(z%v, 5.0_RP**7.0_RP, "rd**real, value", test_data)
call real_equality_test(z%dv(1), 7.0_RP * (5.0_RP**6.0_RP), "rd**real, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd**real, derivative (dvar 2)", test_data)

c = 7_IP

z = x**c

call real_equality_test(z%v, 5.0_RP**7.0_RP, "rd**integer, value", test_data)
call real_equality_test(z%dv(1), 7.0_RP * (5.0_RP**6.0_RP), "rd**integer, derivative (dvar 1)", test_data)
call real_equality_test(z%dv(2), 0.0_RP, "rd**integer, derivative (dvar 2)", test_data)

z = f(x, y)

call real_equality_test(z%v, (2.0_RP * 5.0_RP * 1.5_RP - 5.0_RP**2_IP) / 1.5_RP + 1.5_RP, "f(rd, rd), value", &
                            test_data)
call real_equality_test(z%dv(1), 2.0_RP - 2.0_RP * 5.0_RP / 1.5_RP, "f(rd, rd), derivative (dvar 1)", &
                            test_data)
call real_equality_test(z%dv(2), -(2.0_RP * 5.0_RP * 1.5_RP - 5.0_RP**2_IP) / (1.5_RP**2_IP) &
                                    + 2.0_RP * 5.0_RP / 1.5_RP + 1.0_RP, &
                                    "f(rd, rd), derivative (dvar 2)", test_data)

contains

function f(x, y)
    use general, only: RP, IP
    
    type(rd), intent(in) :: x, y
    type(rd)             :: f
    
    f = (2.0_RP * x * y - x**2_IP) / y + y
    
    return
end function f
