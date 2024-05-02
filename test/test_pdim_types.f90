! tests for the generator of a pdim_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_pdim_mod

use pdim_types, only: length   => pdim_p10000_p00000_p00000, &
                      time     => pdim_p00000_p00000_p10000, &
                      velocity => pdim_p10000_p00000_m10000
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

type(length)   :: x, y, z
type(time)     :: t
type(velocity) :: v
integer        :: rc_fail, failure_unit

character(len=*), parameter :: FAILURE_OUTPUT = "test_pdim_types_fail.txt"

call logger%open("pdim_types.nml")
call tests%start_tests(logger)

x%v = 1.0_WP
y%v = -1.0_WP
z = x + y
call tests%real_eq(z%v, 0.0_WP, "pdim_types value, addition")

x%v = 1.0_WP
y%v = -1.0_WP
z = x - y
call tests%real_eq(z%v, 2.0_WP, "pdim_types value, subtraction")

v%v = -0.5_WP
t%v = 3.0_WP
x = v * t
call tests%real_eq(v%v, -1.5_WP, "pdim_types value, multiplication")

x%v = 1.0_WP
t%v = 2.0_WP
v = x / t
call tests%real_eq(v%v, 0.5_WP, "pdim_types value, division")

! TODO: This will need to be updated for Windows.
call execute_command_line("make test_pdim_types_fail_1 2> " // FAILURE_OUTPUT, exitstat=rc_fail)
call tests%integer_ne(rc_fail, 0, "compile-time error for physical dimension mismatch, 1")

! Delete output file if test succeeded, otherwise keep it for examination.
if (rc_fail == 0) then
    open(newunit=failure_unit, file=FAILURE_OUTPUT, status="old", action="read")
    close(unit=failure_unit, status="delete")
end if

call tests%end_tests()
call logger%close()

! contains

! TODO subroutine for checking whether command line programs fail or not

end program test_pdim_mod
