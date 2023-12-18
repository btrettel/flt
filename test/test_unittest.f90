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
use prec, only: I5, RP
use unittest, only: test_type, start_tests, end_tests, &
                    logical_test, real_equality_test, real_inequality_test, integer_equality_test, string_equality_test
implicit none

type(test_type) :: test_data, test_data_2

character(len=*), parameter :: LOG_FILENAME = "unittest.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_tests(LOG_FILENAME, test_data_2) ! These are for tests which should fail.
call start_log(LOG_FILENAME)

call integer_equality_test(test_data%number_of_failures, 0_I5, "test_data%number_of_failures at start", test_data)
call integer_equality_test(test_data%number_of_tests, 1_I5, "test_data%number_of_tests at start", test_data)

call integer_equality_test(test_data_2%number_of_failures, 0_I5, "test_data_2%number_of_failures at start", test_data)
call integer_equality_test(test_data_2%number_of_tests, 0_I5, "test_data_2%number_of_tests at start", test_data)

call logical_test(.true., "logical_test, .true.", test_data)

call real_equality_test(1.0_RP, 1.0_RP, "real_equality_test, identical numbers (1)", test_data)

call real_equality_test(15.0_RP, 15.0_RP, "real_equality_test, identical numbers (2)", test_data)

call real_equality_test(0.0001_RP, 0.0001_RP, "real_equality_test, identical numbers (3)", test_data)

call real_inequality_test(1.0_RP, 10.0_RP, "real_inequality_test, different numbers (1)", test_data)

call real_inequality_test(5.0_RP, 1000.0_RP, "real_inequality_test, different numbers (2)", test_data)

call real_inequality_test(0.1_RP, 1000.0_RP, "real_inequality_test, different numbers (3)", test_data)

call real_equality_test(1.0_RP, 1.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (1)", test_data)

call real_equality_test(100.0_RP, 100.0_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (2)", test_data)

call real_equality_test(0.1_RP, 0.1_RP + 5.0_RP * epsilon(1.0_RP), &
    "real_equality_test, different numbers within tolerance (3)", test_data)

call real_inequality_test(1.0_RP, 1.0_RP + 20.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (1)", test_data)

call real_inequality_test(100.0_RP, 100.0_RP + 1000.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (2)", test_data)

call real_inequality_test(0.1_RP, 0.1_RP + 11.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, barely different numbers (3)", test_data)

call real_equality_test(0.0_RP, 0.0_RP, "real_equality_test, both zero", test_data)

call real_inequality_test(0.0_RP, 100.0_RP * epsilon(1.0_RP), &
    "real_inequality_test, one zero, one different (1)", test_data)

call real_inequality_test(100.0_RP * epsilon(1.0_RP), 0.0_RP, &
    "real_inequality_test, one zero, one different (2)", test_data)

call integer_equality_test(1_I5, 1_I5, "integer_equality_test", test_data)

call string_equality_test("a", "a", "string_equality_test", test_data)

call real_equality_test(10.0_RP, 5.0_RP, "real_equality_test, large abs_tol set", test_data, abs_tol=5.1_RP)

! tests which should fail

call logical_test(.false., "logical_test, failure", test_data_2)

call real_equality_test(1.0_RP, 0.0_RP, "real_equality_test, failure (greater)", test_data_2)

call real_equality_test(0.0_RP, 1.0_RP, "real_equality_test, failure (less)", test_data_2)

call real_inequality_test(1.0_RP, 1.0_RP, "real_inequality_test, failure", test_data_2)

call real_equality_test(10.0_RP, 5.0_RP, "real_equality_test, failure, abs_tol set", test_data_2, abs_tol=4.1_RP)

call integer_equality_test(1_I5, 0_I5, "integer_equality_test, failure (greater)", test_data_2)

call integer_equality_test(0_I5, 1_I5, "integer_equality_test, failure (less)", test_data_2)

call string_equality_test("a", "b", "string_equality_test, failure (greater)", test_data_2)

call string_equality_test("b", "a", "string_equality_test, failure (less)", test_data_2)

! Now check that the expected number of tests that should fail did in fact fail, and update the total number of tests appropriately.

call integer_equality_test(test_data_2%number_of_tests, 9_I5, "correct number of tests expected to fail", test_data)
call integer_equality_test(test_data_2%number_of_failures, 9_I5, "correct number of tests expected to fail that fail", test_data)

test_data%number_of_tests    = test_data%number_of_tests + test_data_2%number_of_tests
test_data%number_of_failures = test_data%number_of_failures + (test_data_2%number_of_tests - test_data_2%number_of_failures)

call end_tests(test_data)

stop

end program test_unittest
