! tests for the generated unitss module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units

use, intrinsic :: iso_fortran_env, only: compiler_version
use units, only: length   => unit_p10000_p00000_p00000, &
                 time     => unit_p00000_p00000_p10000, &
                 velocity => unit_p10000_p00000_m10000, &
                 area     => unit_p20000_p00000_p00000, &
                 volume   => unit_p30000_p00000_p00000, &
                 unit, sqrt, cbrt, square
use prec, only: WP, CL
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

type(length)   :: x, y, z
type(time)     :: t
type(velocity) :: v
type(area)     :: a
type(volume)   :: vol

character(len=CL) :: quantity_string

call logger%open("units.nml")
call tests%start_tests(logger)

call tests%character_eq(unit(x), "m", "unit function (m)")
call tests%character_eq(unit(t), "s", "unit function (s)")
call tests%character_eq(unit(a), "m2", "unit function (m2)")
call tests%character_eq(unit(vol), "m3", "unit function (m3)")

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
call tests%logical_true(x < y, "units, <, true")
call tests%logical_false(y < x, "units, <, false")
call tests%logical_true(x <= y, "units, <=, true")
call tests%logical_false(y <= x, "units, <=, false")
call tests%logical_true(y > x, "units, >, true")
call tests%logical_false(x > y, "units, >, false")
call tests%logical_true(y >= x, "units, >=, true")
call tests%logical_false(x >= y, "units, >=, false")

! IBM XLF comment start
! It appears that recursive Make causes the right compiler to be used here!
call tests%exit_code_ne("make test_units_fail_1", 0, &
                            "compile-time error for physical dimension mismatch, 1", "test_units_fail_1.txt")
call tests%exit_code_ne("make test_units_fail_2", 0, &
                            "compile-time error for physical dimension mismatch, 2", "test_units_fail_2.txt")
call tests%exit_code_ne("make test_units_fail_3", 0, &
                            "compile-time error for physical dimension mismatch, 3", "test_units_fail_3.txt")
! IBM XLF comment end

! If this compiles, then I can use constructors nicely.
v = velocity(1.0_WP)

! TODO: The next one doesn't work with nvfortran. File a bug report.
! v = length(1.0_WP) / time(1.0_WP)

call test_dtio(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_dtio(tests)
    type(test_results_type), intent(in out) :: tests
    
    type(volume) :: vol
    integer      :: nml_unit
    
    namelist /dtio/ vol
    
    character(len=*), parameter :: TEST_FILENAME = "dtio_test.nml"
    
    ! TODO: Commented out for nvfortran due to a bug in nvfortran's derived type I/O for internal variables.
    if (index(compiler_version(), "nvfortran") == 0) then
        vol%v = 12.345_WP
        write(unit=quantity_string, fmt="(dt'f'(6, 3))") vol
        call tests%character_eq(quantity_string, "12.345 m3", "derived type write")
    end if
    
    open(newunit=nml_unit, action="write", status="replace", position="rewind", file=TEST_FILENAME, delim="quote")
    write(unit=nml_unit, nml=dtio)
    close(nml_unit)
    
!    ! Make sure that the test doesn't just look at the old value.
!    vol%v = 0.0_WP
    
!    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote")
!    read(unit=nml_unit, nml=dtio)
!    close(nml_unit)
    
!    print *, vol
end subroutine test_dtio

end program test_units
