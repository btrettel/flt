! # $File$
! 
! Summary: tests for unittest
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_unittest

use logging, only: start_log
use prec, only: RP
use unittest, only: test_results_type
implicit none

type(test_results_type) :: test_data, test_data_2

character(len=*), parameter :: LOG_FILENAME = "unittest.jsonl"
integer, parameter          :: N_FAILING    = 10

call test_data%start_tests(LOG_FILENAME)
call test_data_2%start_tests(LOG_FILENAME) ! These are for tests which should fail.
call start_log(LOG_FILENAME)

call test_data%integer_equality_test(test_data%number_of_failures, 0, "test_data%number_of_failures at start")
call test_data%integer_equality_test(test_data%number_of_tests, 1, "test_data%number_of_tests at start")

call test_data%integer_equality_test(test_data_2%number_of_failures, 0, "test_data_2%number_of_failures at start")
call test_data%integer_equality_test(test_data_2%number_of_tests, 0, "test_data_2%number_of_tests at start")

call test_data%logical_test(.true., "logical_test, .true.")

call test_data%real_equality_test(1.0_RP, 1.0_RP, "real_equality_test, identical numbers (1)")

call test_data%real_equality_test(15.0_RP, 15.0_RP, "real_equality_test, identical numbers (2)")

call test_data%real_equality_test(0.0001_RP, 0.0001_RP, "real_equality_test, identical numbers (3)")

call test_data%real_inequality_test(1.0_RP, 10.0_RP, "real_inequality_test, different numbers (1)")

call test_data%real_inequality_test(5.0_RP, 1000.0_RP, "real_inequality_test, different numbers (2)")

call test_data%real_inequality_test(0.1_RP, 1000.0_RP, "real_inequality_test, different numbers (3)")

call test_data%real_equality_test(1.0_RP, 1.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (1)")

call test_data%real_equality_test(100.0_RP, 100.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (2)")

call test_data%real_equality_test(0.1_RP, 0.1_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (3)")

call test_data%real_inequality_test(1.0_RP, 1.0_RP + 20.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (1)")

call test_data%real_inequality_test(100.0_RP, 100.0_RP + 1000.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (2)")

call test_data%real_inequality_test(0.1_RP, 0.1_RP + 11.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (3)")

call test_data%real_equality_test(0.0_RP, 0.0_RP, "real_equality_test, both zero")

call test_data%real_inequality_test(0.0_RP, 100.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, one zero, one different (1)")

call test_data%real_inequality_test(100.0_RP * epsilon(1.0_RP), 0.0_RP, &
    "real_inequality_test, one zero, one different (2)")

call test_data%integer_equality_test(1, 1, "integer_equality_test")

call test_data%string_equality_test("a", "a", "string_equality_test")

call test_data%real_equality_test(10.0_RP, 5.0_RP, "real_equality_test, large abs_tol set", abs_tol=5.1_RP)

call test_data%integer_greater_equal_test(2, 1, "integer_greater_equal_test, greater")

call test_data%integer_greater_equal_test(2, 2, "integer_greater_equal_test, equal")

! tests which should fail

call test_data_2%logical_test(.false., "logical_test, failure")

call test_data_2%real_equality_test(1.0_RP, 0.0_RP, "real_equality_test, failure (greater)")

call test_data_2%real_equality_test(0.0_RP, 1.0_RP, "real_equality_test, failure (less)")

call test_data_2%real_inequality_test(1.0_RP, 1.0_RP, "real_inequality_test, failure")

call test_data_2%real_equality_test(10.0_RP, 5.0_RP, "real_equality_test, failure, abs_tol set", abs_tol=4.1_RP)

call test_data_2%integer_equality_test(1, 0, "integer_equality_test, failure (greater)")

call test_data_2%integer_equality_test(0, 1, "integer_equality_test, failure (less)")

call test_data_2%string_equality_test("a", "b", "string_equality_test, failure (greater)")

call test_data_2%string_equality_test("b", "a", "string_equality_test, failure (less)")

call test_data_2%integer_greater_equal_test(1, 2, "integer_greater_equal_test, failure")

! Now check that the expected number of tests that should fail did in fact fail, and update the total number of tests appropriately.

call test_data%integer_equality_test(test_data_2%number_of_tests, N_FAILING, "correct number of tests expected to fail")

call test_data%integer_equality_test(test_data_2%number_of_failures, N_FAILING, &
                                "correct number of tests expected to fail that fail")

test_data%number_of_tests    = test_data%number_of_tests + test_data_2%number_of_tests
test_data%number_of_failures = test_data%number_of_failures + (test_data_2%number_of_tests - test_data_2%number_of_failures)

call test_data%end_tests()

end program test_unittest
