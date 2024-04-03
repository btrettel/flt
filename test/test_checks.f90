! tests for the checks module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_checks

use checks, only: is_close, check, assert, same_shape_as
use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
implicit none

character(len=*), parameter :: ASSERT_FALSE_OUTPUT = "test_assert_false.txt"

type(log_type)          :: logger
type(test_results_type) :: tests
integer                 :: rc_check, rc_assert_false, test_assert_false_unit
logical                 :: test_assert_false_exists
character(len=80)       :: assert_false_line

real(kind=WP) :: a1(5), b1(5), c1(4), &
                    a2(5, 5), b2(5, 5), c2(4, 4), &
                    a3(5, 5), b3(5, 5), c3(4, 4)

call logger%open("checks.nml")
call tests%start_tests(logger)

call tests%logical_true(is_close(1.0_WP, 1.0_WP), "is_close, identical numbers (1)")

call tests%logical_true(is_close(15.0_WP, 15.0_WP), "is_close, identical numbers (2)")

call tests%logical_true(is_close(0.0001_WP, 0.0001_WP), "is_close, identical numbers (3)")

call tests%logical_false(is_close(1.0_WP, 10.0_WP), "is_close, different numbers (1)")

call tests%logical_false(is_close(5.0_WP, 1000.0_WP), "is_close, different numbers (2)")

call tests%logical_false(is_close(0.1_WP, 1000.0_WP), "is_close, different numbers (3)")

call tests%logical_true(is_close(1.0_WP, 1.0_WP + 5.0_WP * spacing(1.0_WP)), &
    "is_close, different numbers within tolerance (1)")

call tests%logical_true(is_close(100.0_WP, 100.0_WP + 5.0_WP * spacing(100.0_WP)), &
    "is_close, different numbers within tolerance (2)")

call tests%logical_true(is_close(0.1_WP, 0.1_WP + 5.0_WP * spacing(0.1_WP)), &
    "is_close, different numbers within tolerance (3)")

call tests%logical_false(is_close(1.0_WP, 1.0_WP + 20.0_WP * spacing(1.0_WP)), &
    "is_close, barely different numbers (1)")

call tests%logical_false(is_close(100.0_WP, 100.0_WP + 1000.0_WP * spacing(100.0_WP)), &
    "is_close, barely different numbers (2)")

call tests%logical_false(is_close(0.1_WP, 0.1_WP + 11.0_WP * spacing(0.1_WP)), &
    "is_close, barely different numbers (3)")

call tests%logical_true(is_close(0.0_WP, 0.0_WP), "is_close, both zero")

call tests%logical_false(is_close(0.0_WP, 100.0_WP * spacing(100.0_WP)), &
    "is_close, one zero, one different (1)")

call tests%logical_false(is_close(100.0_WP * spacing(0.0_WP), 0.0_WP), &
    "is_close, one zero, one different (2)")

call tests%logical_true(is_close(1.0_WP, 1.05_WP, abs_tol=0.1_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (1)")

call tests%logical_true(is_close(10.0_WP, 10.1_WP, abs_tol=0.2_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (2)")

call tests%logical_true(is_close(0.1_WP, 0.11_WP, abs_tol=0.02_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (3)")

call tests%logical_false(is_close(1.0_WP, 1.15_WP, abs_tol=0.1_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (1)")

call tests%logical_false(is_close(20.0_WP, 21.0_WP, abs_tol=0.5_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (2)")

call tests%logical_false(is_close(0.01_WP, 0.02_WP, abs_tol=0.005_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (3)")

call tests%logical_true(is_close(1.0_WP, 1.05_WP, abs_tol=0.0_WP, rel_tol=0.1_WP), &
        "is_close, close numbers with set rel_tol, inside rel_tol")

call tests%logical_false(is_close(1.0_WP, 1.15_WP, abs_tol=0.0_WP, rel_tol=0.1_WP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (1)")

call tests%logical_false(is_close(20.0_WP, 19.7_WP, abs_tol=0.0_WP, rel_tol=0.01_WP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (2)")

call tests%logical_false(is_close(0.0001_WP, 0.0003_WP, abs_tol=0.0_WP, rel_tol=0.1_WP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (3)")

call tests%logical_false(is_close(1.0_WP, 0.0_WP, abs_tol=1.0_WP, rel_tol=0.0_WP), &
        "is_close, close numbers with set abs_tol, just outside")

call tests%logical_false(is_close(1.0_WP, 0.0_WP, abs_tol=0.0_WP, rel_tol=1.0_WP), &
        "is_close, close numbers with set rel_tol, just outside")

rc_check = 0
call check(.true., logger, "check, .true.", rc_check)
call tests%integer_eq(rc_check, 0, "check, .true.")

rc_check = 0
call check(.false., logger, "check, .false.", rc_check)
call tests%integer_eq(rc_check, 1, "check, .false.")

! `assert(.true.)` does not terminate the program (no direct test performed)
call assert(.true.)
! The test passed if execution reaches here, so manually increment the test counters.
tests%n_tests = tests%n_tests + 1

! Check that `assert(.false.)` terminates with a non-zero exit code.
! TODO: This would need to be updated for Windows as I'd add .exe to the executable filename there.
! TODO: This also assumes Bash.
call execute_command_line("./test_assert_false 2> " // ASSERT_FALSE_OUTPUT, exitstat=rc_assert_false)
call tests%integer_ne(rc_assert_false, 0, "assert, .false., exit code")

inquire(file=ASSERT_FALSE_OUTPUT, exist=test_assert_false_exists)
call tests%logical_true(test_assert_false_exists, "assert, .false., output saved")

! Test default assertion failure message.
open(newunit=test_assert_false_unit, file=ASSERT_FALSE_OUTPUT, status="old", action="read")
read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
call tests%character_eq(assert_false_line, "ASSERTION FAILED.", "assert, .false., default assertion message")

! Delete saved file.
close(unit=test_assert_false_unit, status="delete")

! Check that `assert(.false., "Custom message.")` has the correct message.
! TODO: This would need to be updated for Windows as I'd add .exe to the executable filename there.
! TODO: This also assumes Bash.
call execute_command_line("./test_assert_false_message 2> " // ASSERT_FALSE_OUTPUT, exitstat=rc_assert_false)
call tests%integer_ne(rc_assert_false, 0, "assert, .false., message, exit code")

inquire(file=ASSERT_FALSE_OUTPUT, exist=test_assert_false_exists)
call tests%logical_true(test_assert_false_exists, "assert, .false., message, output saved")

! Test default assertion failure message.
open(newunit=test_assert_false_unit, file=ASSERT_FALSE_OUTPUT, status="old", action="read")
read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
call tests%character_eq(assert_false_line, "ASSERTION FAILED. Custom message.", "assert, .false., message, assertion message")

! Delete saved file.
close(unit=test_assert_false_unit, status="delete")

! `same_shape_as`

call tests%logical_true(same_shape_as(a1, b1), "same_shape_as, rank 1, .true.")
call tests%logical_false(same_shape_as(a1, c1), "same_shape_as, rank 1, .false.")

call tests%logical_true(same_shape_as(a2, b2), "same_shape_as, rank 2, .true.")
call tests%logical_false(same_shape_as(a2, c2), "same_shape_as, rank 2, .false.")

call tests%logical_true(same_shape_as(a3, b3), "same_shape_as, rank 3, .true.")
call tests%logical_false(same_shape_as(a3, c3), "same_shape_as, rank 3, .false.")

call tests%end_tests()
call logger%close()

end program test_checks
