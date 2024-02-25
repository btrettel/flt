! tests for unittest
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_unittest

use nmllog, only: log_type
use prec, only: RP
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests, failing_tests

character(len=*), parameter :: LOG_FILENAME = "unittest.nml"
integer, parameter          :: N_FAILING    = 12

call logger%open(LOG_FILENAME)
call tests%start_tests(logger)
call failing_tests%start_tests(logger) ! These are for tests which should fail.

call tests%integer_eq(tests%n_failures, 0, "tests%n_failures at start")
call tests%integer_eq(tests%n_tests, 1, "tests%n_tests at start")

call tests%integer_eq(failing_tests%n_failures, 0, "failing_tests%n_failures at start")
call tests%integer_eq(failing_tests%n_tests, 0, "failing_tests%n_tests at start")

call tests%logical_true(.true., "logical_true, .true.")

call tests%real_eq(1.0_RP, 1.0_RP, "real_eq, identical numbers (1)")

call tests%real_eq(15.0_RP, 15.0_RP, "real_eq, identical numbers (2)")

call tests%real_eq(0.0001_RP, 0.0001_RP, "real_eq, identical numbers (3)")

call tests%real_ne(1.0_RP, 10.0_RP, "real_ne, different numbers (1)")

call tests%real_ne(5.0_RP, 1000.0_RP, "real_ne, different numbers (2)")

call tests%real_ne(0.1_RP, 1000.0_RP, "real_ne, different numbers (3)")

call tests%real_eq(1.0_RP, 1.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_eq, different numbers within tolerance (1)")

call tests%real_eq(100.0_RP, 100.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_eq, different numbers within tolerance (2)")

call tests%real_eq(0.1_RP, 0.1_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_eq, different numbers within tolerance (3)")

call tests%real_ne(1.0_RP, 1.0_RP + 20.0_RP * epsilon(1.0_RP), &
    "real_ne, barely different numbers (1)")

call tests%real_ne(100.0_RP, 100.0_RP + 1000.0_RP * epsilon(1.0_RP), &
    "real_ne, barely different numbers (2)")

call tests%real_ne(0.1_RP, 0.1_RP + 11.0_RP * epsilon(1.0_RP), &
    "real_ne, barely different numbers (3)")

call tests%real_eq(0.0_RP, 0.0_RP, "real_eq, both zero")

call tests%real_ne(0.0_RP, 100.0_RP * epsilon(1.0_RP), &
    "real_ne, one zero, one different (1)")

call tests%real_ne(100.0_RP * epsilon(1.0_RP), 0.0_RP, &
    "real_ne, one zero, one different (2)")

call tests%integer_eq(1, 1, "integer_eq")

call tests%character_eq("a", "a", "character_eq")

call tests%real_eq(10.0_RP, 5.0_RP, "real_eq, large abs_tol set", abs_tol=5.1_RP)

call tests%integer_ge(2, 1, "integer_ge, greater")

call tests%integer_ge(2, 2, "integer_ge, equal")

call tests%integer_le(1, 2, "integer_le, greater")

call tests%integer_le(2, 2, "integer_le, equal")

call tests%integer_le(1, 2, "integer_ne, equal")

! tests which should fail

call failing_tests%logical_true(.false., "logical_true, failure")

call failing_tests%real_eq(1.0_RP, 0.0_RP, "real_eq, failure (greater)")

call failing_tests%real_eq(0.0_RP, 1.0_RP, "real_eq, failure (less)")

call failing_tests%real_ne(1.0_RP, 1.0_RP, "real_ne, failure")

call failing_tests%real_eq(10.0_RP, 5.0_RP, "real_eq, failure, abs_tol set", abs_tol=4.1_RP)

call failing_tests%integer_eq(1, 0, "integer_eq, failure (greater)")

call failing_tests%integer_eq(0, 1, "integer_eq, failure (less)")

call failing_tests%integer_ne(2, 2, "integer_ne, failure")

call failing_tests%integer_ge(1, 2, "integer_ge, failure")

call failing_tests%integer_le(2, 1, "integer_le, failure")

call failing_tests%character_eq("a", "b", "character_eq, failure (greater)")

call failing_tests%character_eq("b", "a", "character_eq, failure (less)")

! Now check that the expected number of tests that should fail did in fact fail, and update the total number of tests appropriately.

call tests%integer_eq(failing_tests%n_tests, N_FAILING, "correct number of tests expected to fail")

call tests%integer_eq(failing_tests%n_failures, N_FAILING, &
                                "correct number of tests expected to fail that fail")

tests%n_tests    = tests%n_tests + failing_tests%n_tests
tests%n_failures = tests%n_failures + (failing_tests%n_tests - failing_tests%n_failures)

call tests%end_tests()
call logger%close()

end program test_unittest
