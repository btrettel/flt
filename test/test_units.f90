! tests for the generated units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units

use, intrinsic :: iso_fortran_env, only: compiler_version
use units, only: unitless     => unit_p00000_p00000_p00000, &
                 si_length    => unit_p10000_p00000_p00000, &
                 si_time      => unit_p00000_p00000_p10000, &
                 si_velocity  => unit_p10000_p00000_m10000, &
                 si_area      => unit_p20000_p00000_p00000, &
                 si_volume    => unit_p30000_p00000_p00000, &
                 si_density   => unit_m30000_p10000_p00000, &
                 si_frequency => unit_p00000_p00000_m10000, &
                 unit, sin, cos, tan, exp, log, abs, max, min, sqrt, cbrt, square
use prec, only: WP, CL
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

call logger%open("units.nml")
call tests%start_tests(logger)

call test_basic(tests)
call test_dtio(tests)
call test_unitless_real(tests)
call test_intrinsics(tests)
call test_timing(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_basic(tests)
    type(test_results_type), intent(in out) :: tests
    
    type(unitless)     :: u
    type(si_length)    :: x, y, z
    type(si_time)      :: t
    type(si_velocity)  :: v
    type(si_area)      :: a
    type(si_volume)    :: vol
    type(si_density)   :: rho
    type(si_frequency) :: f

    call tests%character_eq(unit(u), "1", "unit function (1)")
    call tests%character_eq(unit(x), "m", "unit function (m)")
    call tests%character_eq(unit(t), "s", "unit function (s)")
    call tests%character_eq(unit(a), "m2", "unit function (m2)")
    call tests%character_eq(unit(vol), "m3", "unit function (m3)")
    call tests%character_eq(unit(rho), "kg/m3", "unit function (kg/m3)")
    call tests%character_eq(unit(f), "1/s", "unit function (1/s)")

    x%v = 1.0_WP
    y%v = -1.0_WP
    z = x + y
    call tests%real_eq(z%v, 0.0_WP, "units value, addition")

    x%v = 1.0_WP
    y%v = -1.0_WP
    z = x - y
    call tests%real_eq(z%v, 2.0_WP, "units value, subtraction")

    v%v = -0.5_WP
    t%v = 3.0_WP
    x = v * t
    call tests%real_eq(x%v, -1.5_WP, "units value, multiplication")

    x%v = 1.0_WP
    t%v = 2.0_WP
    v = x / t
    call tests%real_eq(v%v, 0.5_WP, "units value, division")

    a%v = 4.0_WP
    x   = sqrt(a)
    call tests%real_eq(x%v, 2.0_WP, "units value, sqrt")

    vol%v = 27.0_WP
    x     = cbrt(vol)
    call tests%real_eq(x%v, 3.0_WP, "units value, cbrt")

    x%v = 4.0_WP
    a   = square(x)
    call tests%real_eq(a%v, 16.0_WP, "units value, square")

    x%v = 1.0_WP
    y   = +x
    call tests%real_eq(y%v, 1.0_WP, "units value, unary positive")

    x%v = 1.0_WP
    y   = -x
    call tests%real_eq(y%v, -1.0_WP, "units value, unary negative")

    x%v = 0.0_WP
    y%v = 1.0_WP
    ! The spaces after the operators in the quotes is to avoid a linter false positive.
    call tests%logical_true(x < y, "units, < , true")
    call tests%logical_false(y < x, "units, < , false")
    call tests%logical_true(x <= y, "units, <= , true")
    call tests%logical_false(y <= x, "units, <= , false")
    call tests%logical_true(y > x, "units, > , true")
    call tests%logical_false(x > y, "units, > , false")
    call tests%logical_true(y >= x, "units, >= , true")
    call tests%logical_false(x >= y, "units, >= , false")

    ! IBM XLF comment start
    ! It appears that recursive Make causes the right compiler to be used here!
    call tests%exit_code_ne("make test_units_fail_1", 0, &
                                "compile-time error for physical dimension mismatch, 1", "test_units_fail_1.txt")
    call tests%exit_code_ne("make test_units_fail_2", 0, &
                                "compile-time error for physical dimension mismatch, 2", "test_units_fail_2.txt")
    call tests%exit_code_ne("make test_units_fail_3", 0, &
                                "compile-time error for physical dimension mismatch, 3", "test_units_fail_3.txt")
    call tests%exit_code_ne("make test_units_fail_4", 0, &
                                "compile-time error for physical dimension mismatch, 4", "test_units_fail_4.txt")
    ! IBM XLF comment end

    ! If this compiles, then I can use constructors nicely.
    v = si_velocity(1.0_WP)

    ! TODO: The next one doesn't compile with nvfortran. File a bug report.
    ! v = si_length(1.0_WP) / si_time(1.0_WP)

    x%v = 2.0_WP
    x = -1.0_WP*x
    call tests%real_eq(x%v, -2.0_WP, "units value, -1.0_WP*x")

    ! TODO: The next one doesn't compile with nvfortran. File a bug report.
    !x%v = 1.0_WP
    !x = unitless(2.0_WP)*x
    !call tests%real_eq(x%v, 2.0_WP, "units value, unitless(2.0_WP)*x")
end subroutine test_basic

subroutine test_dtio(tests)
    !use, intrinsic :: iso_fortran_env, only: IOSTAT_END, IOSTAT_EOR
    
    type(test_results_type), intent(in out) :: tests
    
    type(si_volume)     :: vol
    integer             :: nml_unit, rc_nml
    character(len=2048) :: msg
    character(len=CL)   :: quantity_string
    
    namelist /dtio/ vol
    
    character(len=*), parameter :: TEST_FILENAME = "dtio_test.nml"
    
    vol%v = 12.345_WP
    
    ! TODO: Commented out for nvfortran due to a bug in nvfortran's derived type I/O for internal variables.
    ! TODO: Cray Fortran seems to not pass these tests either.
    if ((index(compiler_version(), "nvfortran") /= 0) &
        .and. (index(compiler_version(), "Cray Fortran") /= 0)) then
        write(unit=quantity_string, fmt="(dt'f'(6, 3))") vol
        call tests%character_eq(quantity_string, "12.345 m3", "derived type write")
        
        ! TODO: The remainder of this is disabled for nvfortran due to iostat being wrong for nvfortran.
        
        open(newunit=nml_unit, action="write", status="replace", position="rewind", file=TEST_FILENAME, delim="quote")
        write(unit=nml_unit, nml=dtio)
        close(nml_unit)
        
        ! Make sure that the test doesn't just look at the old value.
        vol%v = 0.0_WP
        rc_nml = 0
        msg = ""
        open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote")
        read(unit=nml_unit, nml=dtio, iostat=rc_nml)
        close(nml_unit)
        
        call tests%integer_eq(rc_nml, 0, "units namelist read (1), rc")
        if (rc_nml /= 0) then
            call tests%character_eq(msg, "", "units namelist read (2), iomsg")
        end if
        call tests%real_eq(vol%v, 12.345_WP, "units namelist write and read cycle")
        
        ! TODO: Update this so that it runs on Windows.
        vol%v = 0.0_WP
        rc_nml = 0
        msg = ""
        open(newunit=nml_unit, file="test/dtio_1.nml", status="old", action="read", delim="quote")
        read(unit=nml_unit, nml=dtio, iostat=rc_nml)
        close(nml_unit)
        
        call tests%integer_eq(rc_nml, 0, "units namelist read (2), rc")
        if (rc_nml /= 0) then
            call tests%character_eq(msg, "", "units namelist read (2), iomsg")
        end if
        call tests%real_eq(vol%v, 6.789_WP, "units namelist read (2)")
        
        ! TODO: Update this so that it runs on Windows.
        vol%v = 0.0_WP
        rc_nml = 0
        msg = ""
        open(newunit=nml_unit, file="test/dtio_2.nml", status="old", action="read", delim="quote")
        read(unit=nml_unit, nml=dtio, iostat=rc_nml)
        close(nml_unit)
        
        call tests%integer_eq(rc_nml, 2, "units namelist read (3), rc")
        call tests%real_eq(vol%v, 34.56_WP, "units namelist read (3)")
    end if
end subroutine test_dtio

subroutine test_unitless_real(tests)
    type(test_results_type), intent(in out) :: tests
    
    type(si_time)      :: t1, t2
    type(si_frequency) :: f
    type(unitless)     :: u1, u2
    
    t1%v = 1.0_WP
    u1%v = 1.0_WP
    
    u2 = 1.0_WP + u1
    call tests%real_eq(u2%v, 2.0_WP, "real + unitless")
    
    u2 = u1 + 2.0_WP
    call tests%real_eq(u2%v, 3.0_WP, "unitless + real")
    
    u2 = -1.0_WP - u1
    call tests%real_eq(u2%v, -2.0_WP, "real - unitless")
    
    u2 = u1 - 1.0_WP
    call tests%real_eq(u2%v, 0.0_WP, "unitless - real")
    
    t2 = 5.0_WP * t1
    call tests%real_eq(t2%v, 5.0_WP, "real * unitless")
    
    t2 = t1 * (-4.0_WP)
    call tests%real_eq(t2%v, -4.0_WP, "unitless * real")
    
    f = 2.0_WP / t1
    call tests%real_eq(f%v, 2.0_WP, "real / time")
    
    t2 = t1 / 2.0_WP
    call tests%real_eq(t2%v, 0.5_WP, "time / real")
    
    u1%v = 1.0_WP
    u2 = 4.0_WP / u1
    call tests%real_eq(u2%v, 4.0_WP, "real / unitless")
    
    u2 = u1 / 4.0_WP
    call tests%real_eq(u2%v, 0.25_WP, "unitless / real")
end subroutine test_unitless_real

subroutine test_intrinsics(tests)
    use prec, only: PI
    
    type(test_results_type), intent(in out) :: tests
    
    type(si_length) :: l1, l2, l3
    type(unitless)  :: u1, u2, u3
    
    u1%v = PI/6.0_WP
    u2   = sin(u1)
    call tests%real_eq(u2%v, 0.5_WP, "intrinsics, sin")
    
    u2   = cos(u1)
    call tests%real_eq(u2%v, sqrt(3.0_WP)/2.0_WP, "intrinsics, cos")
    
    u2   = tan(u1)
    call tests%real_eq(u2%v, sqrt(3.0_WP)/3.0_WP, "intrinsics, tan")
    
    u1%v = -2.0_WP
    u2   = exp(u1)
    call tests%real_eq(u2%v, exp(-2.0_WP), "intrinsics, exp")
    
    u1%v = 2.0_WP
    u2   = log(u1)
    call tests%real_eq(u2%v, log(2.0_WP), "intrinsics, log")
    
    u1%v = -2.0_WP
    u2   = abs(u1)
    call tests%real_eq(u2%v, 2.0_WP, "intrinsics, abs 1")
    
    u1%v = 4.0_WP
    u2   = abs(u1)
    call tests%real_eq(u2%v, 4.0_WP, "intrinsics, abs 2")
    
    l1%v = -1.0_WP
    l2   = abs(l1)
    call tests%real_eq(l2%v, 1.0_WP, "intrinsics, abs 3")
    
    l1%v = 3.0_WP
    l2   = abs(l1)
    call tests%real_eq(l2%v, 3.0_WP, "intrinsics, abs 4")
    
    u1%v = 0.0_WP
    u2%v = 1.0_WP
    u3   = min(u1, u2)
    call tests%real_eq(u3%v, 0.0_WP, "intrinsics, min 1")
    u3   = max(u1, u2)
    call tests%real_eq(u3%v, 1.0_WP, "intrinsics, max 1")
    
    l1%v = 0.0_WP
    l2%v = 1.0_WP
    l3   = min(l1, l2)
    call tests%real_eq(l3%v, 0.0_WP, "intrinsics, min 2")
    l3   = max(l1, l2)
    call tests%real_eq(l3%v, 1.0_WP, "intrinsics, max 2")
end subroutine test_intrinsics

subroutine test_timing(tests)
    type(test_results_type), intent(in out) :: tests
    
    real(kind=WP), allocatable :: u(:, :)
    
    call poisson_real(9, 9, 100, u)
    call tests%real_eq(u(3, 3), 8.010424579559166e-2_WP, "poisson_real, regression test")
end subroutine test_timing

subroutine poisson_real(mmax, nmax, itmax, u)
    integer, intent(in) :: mmax  ! number of interior $x$ grid points
    integer, intent(in) :: nmax  ! number of interior $y$ grid points
    integer, intent(in) :: itmax ! maximum number of iterations allowed
    real(kind=WP), allocatable, intent(out) :: u(:, :) ! numerical solution
    
    real(kind=WP), parameter :: A     = 1.0_WP ! $x$ dimension
    real(kind=WP), parameter :: B     = 1.0_WP ! $y$ dimension
    real(kind=WP), parameter :: OMEGA = 1.0_WP ! relaxation parameter
    real(kind=WP), parameter :: TOL   = 0.005_WP ! tolerance for maximum of absolute value of residual

    real(kind=WP) :: hx
    real(kind=WP) :: hy
    real(kind=WP) :: q

    real(kind=WP) :: hx2f(mmax + 2, nmax + 2) ! scaled forcing function
    integer       :: i ! number of SOR iterations performed
    real(kind=WP) :: x(mmax + 2)
    real(kind=WP) :: y(nmax + 2)
    real(kind=WP) :: u_old ! numerical solution for previous iteration
    real(kind=WP) :: u_sum, ave, rmax, res

    integer :: m, n ! loop indices

    allocate(u(mmax + 2, nmax + 2))

    ! Step 2: Set boundary conditions and initialize `u`

    hx = A / real(mmax + 1, WP)
    hy = B / real(nmax + 1, WP)
    q  = (hx / hy)**2

    u_sum       = 0.0_WP
    x(1)        = 0.0_WP
    x(mmax + 2) = A
    y(1)        = 0.0_WP
    y(nmax + 2) = B

    do m = 2, mmax + 1
        x(m) = x(m - 1) + hx
        
        u(m, 1) = x(m)**2
        u(m, nmax + 2) = x(m)**2 + 1.0_WP
        u_sum = u_sum + u(m, 1) + u(m, nmax + 2)
    end do

    do n = 2, nmax + 1
        y(n) = y(n - 1) + hy
        
        u(1, n) = y(n)**2
        u(mmax + 2, n) = 1.0_WP + y(n)**2
        u_sum = u_sum + u(1, n) + u(mmax + 2, n)
    end do

    ave = u_sum / (2.0_WP * real(mmax + nmax, WP))
!    write(unit=*, fmt="(a, f5.3)") " starting guess = ", ave

    do n = 2, nmax + 1
        do m = 2, mmax + 1
            ! initialize `u` to average of boundary values
            u(m, n) = ave
            
            hx2f(m, n) = -(hx**2) * 4.0_WP
        end do
    end do

    ! Step 3: Begin SOR iterations

    do i = 1, itmax
        ! Step 4. Calculate `i`th SOR iterate
        do n = 2, nmax + 1
            do m = 2, mmax + 1
                u_old = u(m, n)
                
                u(m, n) = (u(m - 1, n) &
                            + q * u(m, n - 1) &
                            + u(m + 1, n) &
                            + q * u(m, n + 1) &
                            + hx2f(m, n)) &
                            / (2.0_WP + 2.0_WP * q)
                
                u(m, n) = OMEGA * u(m, n) + (1.0_WP - OMEGA) * u_old
            end do
        end do
        
        ! Step 5. Calculate maximum residual for `i`th SOR iterate
        rmax = 0.0_WP
        do n = 2, nmax + 1
            do m = 2, mmax + 1
                res = (hx2f(m, n) &
                        + u(m - 1, n) &
                        + u(m + 1, n) &
                        - (2.0_WP + 2.0_WP * q) * u(m, n) &
                        + q * u(m, n - 1) &
                        + q * u(m, n + 1)) &
                        / (hx**2)
                rmax = max(abs(res), rmax)
            end do
        end do
        
!        write(unit=*, fmt="(a, i4, a, es15.6)") " i = ", i, " res = ", rmax
        
        ! Step 6. Compare maximum residual to tolerance
        if (rmax < TOL) then
            exit
        end if  
    end do

    ! Step 7. Output solution

!    write(unit=*, fmt="(a)", advance="no") "       x = "
!    do m = 2, mmax + 1
!        write(unit=*, fmt="(a, f5.3, a)", advance="no") " ", x(m), " "
!    end do
!    write(unit=*, fmt="(a)")

!    do n = nmax + 1, 2, -1
!        write(unit=*, fmt="(a, f5.3, a)", advance="no") " y = ", y(n), " "
!        do m = 2, mmax + 1
!            write(unit=*, fmt="(a, f5.3, a)", advance="no") " ", U(m, n), " "
!        end do
        
!        write(unit=*, fmt="(a)")
!    end do
end subroutine poisson_real

end program test_units
