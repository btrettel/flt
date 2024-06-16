! tests for the checks module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_checks

use build, only: DEBUG
use checks, only: TOL_FACTOR, abs_tolerance, is_close, all_close, assert, assert_dimension
use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
implicit none

character(len=*), parameter :: ASSERT_FALSE_OUTPUT = "test_assert_false.txt"

type(log_type)          :: logger
type(test_results_type) :: tests
integer                 :: test_assert_false_unit
logical                 :: is_close_array(2), test_assert_false_exists
character(len=80)       :: assert_false_line

real(kind=WP) :: a1(5),    b1(5),    &
                 a2(5, 5), b2(5, 5), &
                 a3(5, 5), b3(5, 5)

call logger%open("checks.nml")
call tests%start_tests(logger)

call tests%real_gt(abs_tolerance(0.0_WP, 0.0_WP), 0.0_WP, "abs_tolerance is greater than zero at arguments of 0")

call tests%real_eq(abs_tolerance(0.0_WP, 10.0_WP), TOL_FACTOR * spacing(10.0_WP), "abs_tolerance (1)")

call tests%real_eq(abs_tolerance(100.0_WP, 0.0_WP), TOL_FACTOR * spacing(100.0_WP), "abs_tolerance (2)")

call tests%logical_true(is_close(1.0_WP, 1.0_WP), "is_close, identical numbers (1)")

call tests%logical_true(is_close(15.0_WP, 15.0_WP), "is_close, identical numbers (2)")

call tests%logical_true(is_close(0.0001_WP, 0.0001_WP), "is_close, identical numbers (3)")

call tests%logical_false(is_close(1.0_WP, 10.0_WP), "is_close, different numbers (1)")

call tests%logical_false(is_close(5.0_WP, 1000.0_WP), "is_close, different numbers (2)")

call tests%logical_false(is_close(0.1_WP, 1000.0_WP), "is_close, different numbers (3)")

call tests%logical_true(is_close(1.0_WP, 1.0_WP + 0.5_WP * TOL_FACTOR * spacing(1.0_WP)), &
    "is_close, different numbers within tolerance (1)")

call tests%logical_true(is_close(100.0_WP, 100.0_WP + 0.5_WP * TOL_FACTOR * spacing(100.0_WP)), &
    "is_close, different numbers within tolerance (2)")

call tests%logical_true(is_close(0.1_WP, 0.1_WP + 0.5_WP * TOL_FACTOR * spacing(0.1_WP)), &
    "is_close, different numbers within tolerance (3)")

call tests%logical_false(is_close(1.0_WP, 1.0_WP + 2.0_WP * TOL_FACTOR * spacing(1.0_WP)), &
    "is_close, barely different numbers (1)")

call tests%logical_false(is_close(100.0_WP, 100.0_WP + 1000.0_WP * spacing(100.0_WP)), &
    "is_close, barely different numbers (2)")

call tests%logical_false(is_close(0.1_WP, 0.1_WP + (TOL_FACTOR + 1.0_WP) * spacing(0.1_WP)), &
    "is_close, barely different numbers (3)")

call tests%logical_true(is_close(0.0_WP, 0.0_WP), "is_close, both zero")

call tests%logical_false(is_close(0.0_WP, 100.0_WP * spacing(0.0_WP)), &
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

! `elemental` `is_close` tests
is_close_array = is_close([0.0_WP, 10.0_WP], [0.0_WP, 10.0_WP])
call tests%logical_true(all(is_close_array), "is_close, elemental (1)")

is_close_array = is_close([0.0_WP, 10.0_WP], [1.0_WP, 9.0_WP])
call tests%logical_false(all(is_close_array), "is_close, elemental (2)")

is_close_array = is_close([-1.0_WP, 5.0_WP], [2.0_WP, 5.0_WP])
call tests%logical_false(is_close_array(1), "is_close, elemental (3, index 1)")
call tests%logical_true(is_close_array(2), "is_close, elemental (3, index 2)")

! TODO: More tests for `all_close`.
call tests%logical_true(all_close([1.0_WP, 2.0_WP], [1.0_WP, 2.0_WP]), "all_close, .true.")
call tests%logical_false(all_close([1.0_WP, 2.0_WP], [-1.0_WP, 2.0_WP]), "all_close, .false.")

! `assert(.true.)` does not terminate the program (no direct test performed)
call assert(.true., "assert(.true.) test failed?")
! The test passed if execution reaches here, so manually increment the test counters.
tests%n_tests = tests%n_tests + 1

if (DEBUG) then ! IBM XLF comment start
    ! Check that `assert(.false., "Custom message.")` has the correct message.
    call tests%exit_code_ne("./test_assert_false", 0, "assert, .false., message, exit code", &
                                ASSERT_FALSE_OUTPUT, keep_file=.true.)

    inquire(file=ASSERT_FALSE_OUTPUT, exist=test_assert_false_exists)
    call tests%logical_true(test_assert_false_exists, "assert, .false., message, output saved")

    ! Test assertion failure message.
    open(newunit=test_assert_false_unit, file=ASSERT_FALSE_OUTPUT, status="old", action="read")
    read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
    read(unit=test_assert_false_unit, fmt="(a)") assert_false_line
    call tests%character_eq(assert_false_line, "ASSERTION FAILED. Custom message.", "assert, .false., message, assertion message")

    ! Delete saved file.
    close(unit=test_assert_false_unit, status="delete")
else
    ! Check that `assert(.false.)` does not terminate with a non-zero exit code for release mode.
    call tests%exit_code_eq("./test_assert_false", 0, "assert, .false., exit code (release)", ASSERT_FALSE_OUTPUT)
end if ! IBM XLF comment end

! `assert_dimension`

call assert_dimension(a1, b1) ! rank 1
call assert_dimension(a2, b2) ! rank 2
call assert_dimension(a3, b3) ! rank 3

if (DEBUG) then
    ! Check that `assert_dimension` terminates with a non-zero exit code for debug mode.
    call tests%exit_code_ne("./test_assert_dimension_false_1", 0, &
                                "assert_dimension, real, rank 1, .false., exit code", ASSERT_FALSE_OUTPUT)
    call tests%exit_code_ne("./test_assert_dimension_false_2", 0, &
                                "assert_dimension, real, rank 2, .false., exit code", ASSERT_FALSE_OUTPUT)
    call tests%exit_code_ne("./test_assert_dimension_false_3", 0, &
                                "assert_dimension, real, rank 3, .false., exit code", ASSERT_FALSE_OUTPUT)
else
    ! Check that `assert_dimension` does not terminate with a non-zero exit code for release mode.
    call tests%exit_code_eq("./test_assert_dimension_false_1", 0, &
                                "assert_dimension, real, rank 1, .false., exit code (release)", ASSERT_FALSE_OUTPUT)
    call tests%exit_code_eq("./test_assert_dimension_false_2", 0, &
                                "assert_dimension, real, rank 2, .false., exit code (release)", ASSERT_FALSE_OUTPUT)
    call tests%exit_code_eq("./test_assert_dimension_false_3", 0, &
                                "assert_dimension, real, rank 3, .false., exit code (release)", ASSERT_FALSE_OUTPUT)
end if

call tests%end_tests()
call logger%close()

end program test_checks
