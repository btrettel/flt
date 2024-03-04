! tests for unittest
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_unittest

use nmllog, only: log_type, CRITICAL_LEVEL
use prec, only: RP
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests, failing_tests

integer, parameter :: N_FAILING = 13

call logger%open("unittest.nml")
call tests%start_tests(logger)
call failing_tests%start_tests(logger) ! These are for tests which should fail.

call tests%integer_eq(tests%n_failures, 0, "tests%n_failures at start")
call tests%integer_eq(tests%n_tests, 1, "tests%n_tests at start")

call tests%integer_eq(failing_tests%n_failures, 0, "failing_tests%n_failures at start")
call tests%integer_eq(failing_tests%n_tests, 0, "failing_tests%n_tests at start")

call tests%logical_true(.true., "logical_true, .true.")

call tests%logical_false(.false., "logical_true, .false.")

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

! Don't print these to stdout.
failing_tests%logger%stdout_level = CRITICAL_LEVEL + 1

call failing_tests%logical_true(.false., "logical_true, failure")

call failing_tests%logical_false(.true., "logical_false, failure")

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

call test_validate_timestamp(tests)
call test_read_unittest_nml(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_validate_timestamp(tests)
    use unittest, only: validate_timestamp
    use nmllog, only: CRITICAL_LEVEL
    
    type(test_results_type), intent(inout) :: tests
    
    character(len=:), allocatable :: timestamp
    type(test_results_type)       :: failing_tests
    
    integer :: n_tests, n_failing
    
    call failing_tests%start_tests(tests%logger)
    failing_tests%logger%stdout_level=CRITICAL_LEVEL
    n_tests   = 0
    n_failing = 0
    
    timestamp = "2024-02-24T18:50:53.194-05:00"
    call validate_timestamp(tests, timestamp, "test_validate_timestamp, valid timestamp")
    
    timestamp = "2024-02-24T18:50:53.194"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timestamp is too short")
    n_tests   = n_tests + 1
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-05:00aaa"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timestamp is too long")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "XXXX-02-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, year is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-XX-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, month is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-XXT18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, day is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24TXX:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, hour is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:XX:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, minutes are not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:XX.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, seconds are not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.XXX-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, milliseconds are not integer")
    n_tests   = n_tests + 34
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-XX:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_hour is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-05:XX"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_minutes is not integer")
    n_tests   = n_tests + 33
    n_failing = n_failing + 1
    
    timestamp = "1000-02-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, year is too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "3000-02-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, year is too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-00-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, month is too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-13-24T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, month is too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-00T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, day is too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-32T18:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, day is too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T-1:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, hour is too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T25:50:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, hour is too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:-1:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, minutes are too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:60:53.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, minutes are too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:-1.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, seconds are too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:60.194-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, seconds are too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.-01-05:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, milliseconds are too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    ! This is commented out as I guess I can't make milliseconds too high.
!    timestamp = "2024-02-24T18:50:53.1e4-05:00"
!    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, milliseconds are too high")
!    n_tests   = n_tests + 36
!    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194--1:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_hour is too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-25:00"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_hour is too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-05:-1"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_minutes are too low")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    timestamp = "2024-02-24T18:50:53.194-05:60"
    call validate_timestamp(failing_tests, timestamp, "test_validate_timestamp, timezone_minutes are too high")
    n_tests   = n_tests + 35
    n_failing = n_failing + 1
    
    call tests%integer_eq(failing_tests%n_tests, n_tests, "test_validate_timestamp, correct number of tests")
    call tests%integer_eq(failing_tests%n_failures, n_failing, "test_validate_timestamp, correct number of tests that fail")
end subroutine test_validate_timestamp

subroutine test_read_unittest_nml(tests)
    use prec, only: RP, CL
    use unittest, only: validate_timestamp
    use nmllog, only: TIMESTAMP_LEN, NML_RECL
    
    type(test_results_type), intent(inout) :: tests
    
    character(len=*), parameter :: TEST_FILENAME = "test.nml"
    type(test_results_type)     :: nml_tests
    type(log_type)              :: logger
    integer                     :: nml_unit
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=7)             :: variable_type
    character(len=2)             :: test_operator
    logical                      :: test_passes, returned_logical, compared_logical
    character(len=CL)            :: message, returned_string, compared_string
    real(kind=RP)                :: returned_real, compared_real, tolerance, difference
    integer                      :: returned_integer, compared_integer
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
                            returned_logical, compared_logical, &
                            returned_real, compared_real, tolerance, difference, &
                            returned_integer, compared_integer, &
                            returned_string, compared_string
    
    timestamp        = ""
    variable_type    = ""
    test_operator    = ""
    test_passes      = .false.
    message          = ""
    returned_logical = .false.
    compared_logical = .false.
    returned_real    = 0.0_RP
    compared_real    = 0.0_RP
    tolerance        = 0.0_RP
    difference       = 0.0_RP
    returned_integer = 0
    compared_integer = 0
    returned_string  = ""
    compared_string  = ""
    
    ! logical_true (pass)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    call nml_tests%logical_true(.true., "logical_true (pass)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, logical_true (pass)")
    call tests%character_eq(variable_type, "logical", "test_read_unittest_nml, logical_true (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, logical_true (pass), test_passes")
    call tests%character_eq(trim(message), "logical_true (pass)", "test_read_unittest_nml, logical_true (pass), message")
    call tests%logical_true(returned_logical, "test_read_unittest_nml, logical_true (pass), returned_logical")
    call tests%logical_true(compared_logical, "test_read_unittest_nml, logical_true (pass), compared_logical")
    
    ! logical_true (fail)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    nml_tests%logger%stdout_level=CRITICAL_LEVEL
    call nml_tests%logical_true(.false., "logical_true (fail)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, logical_true (fail)")
    call tests%character_eq(variable_type, "logical", "test_read_unittest_nml, logical_true (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, logical_true (fail), test_passes")
    call tests%character_eq(trim(message), "logical_true (fail)", "test_read_unittest_nml, logical_true (fail), message")
    call tests%logical_false(returned_logical, "test_read_unittest_nml, logical_true (fail), returned_logical")
    call tests%logical_true(compared_logical, "test_read_unittest_nml, logical_true (fail), compared_logical")
    
    ! logical_false (pass)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    call nml_tests%logical_true(.true., "logical_false (pass)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, logical_false (pass)")
    call tests%character_eq(variable_type, "logical", "test_read_unittest_nml, logical_false (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, logical_false (pass), test_passes")
    call tests%character_eq(trim(message), "logical_false (pass)", "test_read_unittest_nml, logical_false (pass), message")
    call tests%logical_true(returned_logical, "test_read_unittest_nml, logical_false (pass), returned_logical")
    call tests%logical_true(compared_logical, "test_read_unittest_nml, logical_false (pass), compared_logical")
    
    ! logical_false (fail)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    nml_tests%logger%stdout_level=CRITICAL_LEVEL
    call nml_tests%logical_true(.false., "logical_false (fail)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, logical_false (fail)")
    call tests%character_eq(variable_type, "logical", "test_read_unittest_nml, logical_false (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, logical_false (fail), test_passes")
    call tests%character_eq(trim(message), "logical_false (fail)", "test_read_unittest_nml, logical_false (fail), message")
    call tests%logical_false(returned_logical, "test_read_unittest_nml, logical_false (fail), returned_logical")
    call tests%logical_true(compared_logical, "test_read_unittest_nml, logical_false (fail), compared_logical")
    
    ! real_eq (pass)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    call nml_tests%real_eq(1.0_RP, 1.0_RP, "real_eq (pass)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_eq (pass)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_eq (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, real_eq (pass), test_passes")
    call tests%character_eq(trim(message), "real_eq (pass)", "test_read_unittest_nml, real_eq (pass), message")
    call tests%real_eq(returned_real, 1.0_RP, "test_read_unittest_nml, real_eq (pass), returned_real")
    call tests%real_eq(compared_real, 1.0_RP, "test_read_unittest_nml, real_eq (pass), compared_real")
    
    ! real_eq (fail)
    call logger%open(TEST_FILENAME)
    call nml_tests%start_tests(logger)
    nml_tests%logger%stdout_level=CRITICAL_LEVEL
    call nml_tests%real_eq(3.0_RP, 2.0_RP, "real_eq (fail)")
    call logger%close()
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_eq (fail)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_eq (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, real_eq (fail), test_passes")
    call tests%character_eq(trim(message), "real_eq (fail)", "test_read_unittest_nml, real_eq (fail), message")
    call tests%real_eq(returned_real, 3.0_RP, "test_read_unittest_nml, real_eq (fail), returned_real")
    call tests%real_eq(compared_real, 2.0_RP, "test_read_unittest_nml, real_eq (fail), compared_real")
    
    ! TODO: real_ne (pass)
    ! TODO: real_ne (fail)
    ! TODO: integer_eq (pass)
    ! TODO: integer_eq (fail)
    ! TODO: integer_ne (pass)
    ! TODO: integer_ne (fail)
    ! TODO: integer_ge (pass)
    ! TODO: integer_ge (fail)
    ! TODO: integer_le (pass)
    ! TODO: integer_le (fail)
    ! TODO: character_eq (pass)
    ! TODO: character_eq (fail)
    ! TODO: test log duration
end subroutine test_read_unittest_nml

end program test_unittest
