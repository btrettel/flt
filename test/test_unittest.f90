! tests for unittest
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_unittest

use checks, only: TOL_FACTOR
use prec, only: WP, I10
use unittest, only: test_results_type
implicit none

type(test_results_type) :: tests, failing_tests

integer, parameter :: N_FAILING = 19
character(len=*), parameter :: EXIT_CODE_FILE = "exit_code.txt"

logical :: exit_code_file_exists
integer :: exit_code_file_unit

call tests%start_tests("unittest.nml")
call failing_tests%start_tests("unittest_failing_1.nml") ! These are for tests which should fail.

call tests%integer_eq(tests%n_failures, 0, "tests%n_failures at start")
call tests%integer_eq(tests%n_tests, 1, "tests%n_tests at start")

call tests%integer_eq(failing_tests%n_failures, 0, "failing_tests%n_failures at start")
call tests%integer_eq(failing_tests%n_tests, 0, "failing_tests%n_tests at start")

call tests%logical_true(.true., "logical_true, .true.")

call tests%logical_false(.false., "logical_true, .false.")

call tests%real_eq(1.0_WP, 1.0_WP, "real_eq, identical numbers (1)")

call tests%real_eq(15.0_WP, 15.0_WP, "real_eq, identical numbers (2)")

call tests%real_eq(0.0001_WP, 0.0001_WP, "real_eq, identical numbers (3)")

call tests%real_ne(1.0_WP, 10.0_WP, "real_ne, different numbers (1)")

call tests%real_ne(5.0_WP, 1000.0_WP, "real_ne, different numbers (2)")

call tests%real_ne(0.1_WP, 1000.0_WP, "real_ne, different numbers (3)")

call tests%real_eq(1.0_WP, 1.0_WP + 0.5_WP * TOL_FACTOR * spacing(1.0_WP), "real_eq, different numbers within tolerance (1)")

call tests%real_eq(100.0_WP, 100.0_WP + 0.5_WP * TOL_FACTOR * spacing(100.0_WP), "real_eq, different numbers within tolerance (2)")

call tests%real_eq(0.1_WP, 0.1_WP + 0.5_WP * TOL_FACTOR * spacing(0.1_WP), "real_eq, different numbers within tolerance (3)")

call tests%real_ne(1.0_WP, 1.0_WP + 2.0_WP * TOL_FACTOR * spacing(1.0_WP), "real_ne, barely different numbers (1)")

call tests%real_ne(100.0_WP, 100.0_WP + 1000.0_WP * spacing(100.0_WP), "real_ne, barely different numbers (2)")

call tests%real_ne(0.1_WP, 0.1_WP + (TOL_FACTOR + 1.0_WP) * spacing(0.1_WP), "real_ne, barely different numbers (3)")

call tests%real_eq(0.0_WP, 0.0_WP, "real_eq, both zero")

call tests%real_ne(0.0_WP, 100.0_WP * spacing(0.0_WP), "real_ne, one zero, one different (1)")

call tests%real_ne(100.0_WP * spacing(0.0_WP), 0.0_WP, "real_ne, one zero, one different (2)")

call tests%real_gt(1.0_WP, 0.0_WP, "real_gt")

call tests%real_lt(0.0_WP, 1.0_WP, "real_lt")

call tests%real_ge(1.0_WP, 0.0_WP, "real_ge (1)")

call tests%real_ge(1.0_WP, 1.0_WP, "real_ge (2)")

call tests%real_le(0.0_WP, 1.0_WP, "real_le (1)")

call tests%real_le(1.0_WP, 1.0_WP, "real_le (2)")

call tests%integer_eq(1, 1, "integer_eq")

call tests%integer_eq(3_I10, 3_I10, "integer_eq, I10")

call tests%character_eq("a", "a", "character_eq")

call tests%real_eq(10.0_WP, 5.0_WP, "real_eq, large abs_tol set", abs_tol=5.1_WP)

call tests%integer_ge(2, 1, "integer_ge, greater")

call tests%integer_ge(2, 2, "integer_ge, equal")

call tests%integer_le(1, 2, "integer_le, greater")

call tests%integer_le(2, 2, "integer_le, equal")

call tests%integer_ge(2_I10, 1_I10, "integer_ge, greater, I10")

call tests%integer_ge(2_I10, 2_I10, "integer_ge, equal, I10")

call tests%integer_le(1_I10, 2_I10, "integer_le, greater, I10")

call tests%integer_le(2_I10, 2_I10, "integer_le, equal, I10")

call tests%integer_le(1, 2, "integer_ne, equal")

! IBM XLF comment start
! TODO: This assumes *nix.
call tests%exit_code_ne("false", 0, "exit_code_ne, success, no file", EXIT_CODE_FILE)

inquire(file=EXIT_CODE_FILE, exist=exit_code_file_exists)
call tests%logical_false(exit_code_file_exists, "exit_code_ne, success, no file, file does not exist")

! TODO: This assumes *nix.
call tests%exit_code_eq("true", 0, "exit_code_eq, success, with file", EXIT_CODE_FILE, keep_file=.true.)

call tests%exit_code_ne("false", 0, "exit_code_ne, success, with file", EXIT_CODE_FILE, keep_file=.true.)

inquire(file=EXIT_CODE_FILE, exist=exit_code_file_exists)
call tests%logical_true(exit_code_file_exists, "exit_code_ne, success, with file, file exists")

if (exit_code_file_exists) then
    open(newunit=exit_code_file_unit, file=EXIT_CODE_FILE, status="old", action="read")
    close(unit=exit_code_file_unit, status="delete")
end if
! IBM XLF comment end

! tests which should fail

! Don't print these to stdout.
failing_tests%stdout = .false.

call failing_tests%logical_true(.false., "logical_true, failure")

call failing_tests%logical_false(.true., "logical_false, failure")

call failing_tests%real_eq(1.0_WP, 0.0_WP, "real_eq, failure (greater)")

call failing_tests%real_eq(0.0_WP, 1.0_WP, "real_eq, failure (less)")

call failing_tests%real_ne(1.0_WP, 1.0_WP, "real_ne, failure")

call failing_tests%real_gt(-1.0_WP, 0.0_WP, "real_gt, failure")

call failing_tests%real_lt(0.0_WP, -1.0_WP, "real_lt, failure")

call failing_tests%real_ge(-1.0_WP, 0.0_WP, "real_ge, failure")

call failing_tests%real_le(0.0_WP, -1.0_WP, "real_le, failure")

call failing_tests%real_eq(10.0_WP, 5.0_WP, "real_eq, failure, abs_tol set", abs_tol=4.1_WP)

call failing_tests%integer_eq(1, 0, "integer_eq, failure (greater)")

call failing_tests%integer_eq(0, 1, "integer_eq, failure (less)")

call failing_tests%integer_ne(2, 2, "integer_ne, failure")

call failing_tests%integer_ge(1, 2, "integer_ge, failure")

call failing_tests%integer_le(2, 1, "integer_le, failure")

call failing_tests%character_eq("a", "b", "character_eq, failure (greater)")

call failing_tests%character_eq("b", "a", "character_eq, failure (less)")

! IBM XLF comment start
! TODO: This assumes *nix.
call failing_tests%exit_code_eq("true", 1, "exit_code_eq, failure, with file", EXIT_CODE_FILE)

call failing_tests%exit_code_ne("true", 0, "exit_code_ne, failure, with file", EXIT_CODE_FILE)

inquire(file=EXIT_CODE_FILE, exist=exit_code_file_exists)
call tests%logical_true(exit_code_file_exists, "exit_code_ne, failure, no file, file exists")

if (exit_code_file_exists) then
    open(newunit=exit_code_file_unit, file=EXIT_CODE_FILE, status="old", action="read")
    close(unit=exit_code_file_unit, status="delete")
end if

! Now check that the expected number of tests that should fail did in fact fail, and update the total number of tests appropriately.

call tests%integer_eq(failing_tests%n_tests, N_FAILING, "correct number of tests expected to fail")

call tests%integer_eq(failing_tests%n_failures, N_FAILING, &
                                "correct number of tests expected to fail that fail")
! IBM XLF comment end

tests%n_tests    = tests%n_tests + failing_tests%n_tests
tests%n_failures = tests%n_failures + (failing_tests%n_tests - failing_tests%n_failures)

call test_validate_timestamp(tests)
call test_read_unittest_nml(tests)

call tests%end_tests()

contains

subroutine test_validate_timestamp(tests)
    use unittest, only: validate_timestamp
    
    type(test_results_type), intent(in out) :: tests
    
    character(len=:), allocatable :: timestamp
    type(test_results_type)       :: failing_tests
    
    integer :: n_tests, n_failing
    
    call failing_tests%start_tests("unittest_failing_2.nml")
    failing_tests%stdout = .false.
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
    
    ! This is split to avoid a linter false positive.
    timestamp = "2024-02-24T18:50:53." // "XXX-05:00"
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
    use prec, only: WP, CL, NML_RECL
    use unittest, only: validate_timestamp
    use timer, only: TIMESTAMP_LEN
    
    type(test_results_type), intent(in out) :: tests
    
    character(len=*), parameter :: TEST_FILENAME = "test.nml"
    type(test_results_type)     :: nml_tests
    integer                     :: nml_unit
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=9)             :: variable_type
    character(len=2)             :: test_operator
    logical                      :: test_passes, returned_logical, compared_logical
    character(len=CL)            :: message, returned_character, compared_character
    real(WP)                     :: returned_real, compared_real, tolerance, difference
    integer                      :: returned_integer, compared_integer
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
                            returned_logical, compared_logical, &
                            returned_real, compared_real, tolerance, difference, &
                            returned_integer, compared_integer, &
                            returned_character, compared_character
    
    integer  :: n_tests, n_failures
    real(WP) :: duration ! in seconds
    
    namelist /tests_summary/ n_tests, n_failures, duration
    
    timestamp          = ""
    variable_type      = ""
    test_operator      = ""
    test_passes        = .false.
    message            = ""
    returned_logical   = .false.
    compared_logical   = .false.
    returned_real      = 0.0_WP
    compared_real      = 0.0_WP
    tolerance          = 0.0_WP
    difference         = 0.0_WP
    returned_integer   = 0
    compared_integer   = 0
    returned_character = ""
    compared_character = ""
    
    ! logical_true (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%logical_true(.true., "logical_true (pass)")
    close(unit=nml_tests%unit)
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
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%logical_true(.false., "logical_true (fail)")
    close(unit=nml_tests%unit)
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
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%logical_true(.true., "logical_false (pass)")
    close(unit=nml_tests%unit)
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
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%logical_true(.false., "logical_false (fail)")
    close(unit=nml_tests%unit)
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
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%real_eq(1.0_WP, 1.0_WP, "real_eq (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_eq (pass)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_eq (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, real_eq (pass), test_passes")
    call tests%character_eq(trim(message), "real_eq (pass)", "test_read_unittest_nml, real_eq (pass), message")
    call tests%real_eq(returned_real, 1.0_WP, "test_read_unittest_nml, real_eq (pass), returned_real")
    call tests%real_eq(compared_real, 1.0_WP, "test_read_unittest_nml, real_eq (pass), compared_real")
    
    ! real_eq (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%real_eq(3.0_WP, 2.0_WP, "real_eq (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_eq (fail)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_eq (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, real_eq (fail), test_passes")
    call tests%character_eq(trim(message), "real_eq (fail)", "test_read_unittest_nml, real_eq (fail), message")
    call tests%real_eq(returned_real, 3.0_WP, "test_read_unittest_nml, real_eq (fail), returned_real")
    call tests%real_eq(compared_real, 2.0_WP, "test_read_unittest_nml, real_eq (fail), compared_real")
    
    ! real_ne (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%real_ne(0.0_WP, 1.0_WP, "real_ne (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_ne (pass)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_ne (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, real_ne (pass), test_passes")
    call tests%character_eq(trim(message), "real_ne (pass)", "test_read_unittest_nml, real_ne (pass), message")
    call tests%real_eq(returned_real, 0.0_WP, "test_read_unittest_nml, real_ne (pass), returned_real")
    call tests%real_eq(compared_real, 1.0_WP, "test_read_unittest_nml, real_ne (pass), compared_real")
    
    ! real_ne (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%real_ne(3.0_WP, 3.0_WP, "real_ne (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, real_ne (fail)")
    call tests%character_eq(variable_type, "real", "test_read_unittest_nml, real_ne (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, real_ne (fail), test_passes")
    call tests%character_eq(trim(message), "real_ne (fail)", "test_read_unittest_nml, real_ne (fail), message")
    call tests%real_eq(returned_real, 3.0_WP, "test_read_unittest_nml, real_ne (fail), returned_real")
    call tests%real_eq(compared_real, 3.0_WP, "test_read_unittest_nml, real_ne (fail), compared_real")
    
    ! integer_eq (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%integer_eq(2, 2, "integer_eq (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_eq (pass)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_eq (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, integer_eq (pass), test_passes")
    call tests%character_eq(trim(message), "integer_eq (pass)", "test_read_unittest_nml, integer_eq (pass), message")
    call tests%integer_eq(returned_integer, 2, "test_read_unittest_nml, integer_eq (pass), returned_integer")
    call tests%integer_eq(compared_integer, 2, "test_read_unittest_nml, integer_eq (pass), compared_integer")
    
    ! integer_eq (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%integer_eq(-1, 4, "integer_eq (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_eq (fail)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_eq (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, integer_eq (fail), test_passes")
    call tests%character_eq(trim(message), "integer_eq (fail)", "test_read_unittest_nml, integer_eq (fail), message")
    call tests%integer_eq(returned_integer, -1, "test_read_unittest_nml, integer_eq (fail), returned_integer")
    call tests%integer_eq(compared_integer, 4, "test_read_unittest_nml, integer_eq (fail), compared_integer")
    
    ! integer_ne (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%integer_ne(2, 1, "integer_ne (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_ne (pass)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_ne (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, integer_ne (pass), test_passes")
    call tests%character_eq(trim(message), "integer_ne (pass)", "test_read_unittest_nml, integer_ne (pass), message")
    call tests%integer_eq(returned_integer, 2, "test_read_unittest_nml, integer_ne (pass), returned_integer")
    call tests%integer_eq(compared_integer, 1, "test_read_unittest_nml, integer_ne (pass), compared_integer")
    
    ! integer_ne (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%integer_ne(4, 4, "integer_ne (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_ne (fail)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_ne (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, integer_ne (fail), test_passes")
    call tests%character_eq(trim(message), "integer_ne (fail)", "test_read_unittest_nml, integer_ne (fail), message")
    call tests%integer_eq(returned_integer, 4, "test_read_unittest_nml, integer_ne (fail), returned_integer")
    call tests%integer_eq(compared_integer, 4, "test_read_unittest_nml, integer_ne (fail), compared_integer")
    
    ! integer_ge (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%integer_ge(2, 1, "integer_ge (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_ge (pass)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_ge (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, integer_ge (pass), test_passes")
    call tests%character_eq(trim(message), "integer_ge (pass)", "test_read_unittest_nml, integer_ge (pass), message")
    call tests%integer_eq(returned_integer, 2, "test_read_unittest_nml, integer_ge (pass), returned_integer")
    call tests%integer_eq(compared_integer, 1, "test_read_unittest_nml, integer_ge (pass), compared_integer")
    
    ! integer_ge (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%integer_ge(3, 4, "integer_ge (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_ge (fail)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_ge (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, integer_ge (fail), test_passes")
    call tests%character_eq(trim(message), "integer_ge (fail)", "test_read_unittest_nml, integer_ge (fail), message")
    call tests%integer_eq(returned_integer, 3, "test_read_unittest_nml, integer_ge (fail), returned_integer")
    call tests%integer_eq(compared_integer, 4, "test_read_unittest_nml, integer_ge (fail), compared_integer")
    
    ! integer_le (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%integer_le(0, 1, "integer_le (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_le (pass)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_le (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, integer_le (pass), test_passes")
    call tests%character_eq(trim(message), "integer_le (pass)", "test_read_unittest_nml, integer_le (pass), message")
    call tests%integer_eq(returned_integer, 0, "test_read_unittest_nml, integer_le (pass), returned_integer")
    call tests%integer_eq(compared_integer, 1, "test_read_unittest_nml, integer_le (pass), compared_integer")
    
    ! integer_le (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%integer_le(5, 2, "integer_le (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, integer_le (fail)")
    call tests%character_eq(variable_type, "integer", "test_read_unittest_nml, integer_le (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, integer_le (fail), test_passes")
    call tests%character_eq(trim(message), "integer_le (fail)", "test_read_unittest_nml, integer_le (fail), message")
    call tests%integer_eq(returned_integer, 5, "test_read_unittest_nml, integer_le (fail), returned_integer")
    call tests%integer_eq(compared_integer, 2, "test_read_unittest_nml, integer_le (fail), compared_integer")
    
    ! character_eq (pass)
    call nml_tests%start_tests(TEST_FILENAME)
    call nml_tests%character_eq("a", "a", "character_eq (pass)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, character_eq (pass)")
    call tests%character_eq(variable_type, "character", "test_read_unittest_nml, character_eq (pass), variable_type")
    call tests%logical_true(test_passes, "test_read_unittest_nml, character_eq (pass), test_passes")
    call tests%character_eq(trim(message), "character_eq (pass)", "test_read_unittest_nml, character_eq (pass), message")
    call tests%character_eq(returned_character, "a", "test_read_unittest_nml, character_eq (pass), returned_character")
    call tests%character_eq(compared_character, "a", "test_read_unittest_nml, character_eq (pass), compared_character")
    
    ! character_eq (fail)
    call nml_tests%start_tests(TEST_FILENAME)
    nml_tests%stdout = .false.
    call nml_tests%character_eq("a", "b", "character_eq (fail)")
    close(unit=nml_tests%unit)
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=test_result)
    close(unit=nml_unit, status="delete")
    call validate_timestamp(tests, timestamp, "test_read_unittest_nml, character_eq (fail)")
    call tests%character_eq(variable_type, "character", "test_read_unittest_nml, character_eq (fail), variable_type")
    call tests%logical_false(test_passes, "test_read_unittest_nml, character_eq (fail), test_passes")
    call tests%character_eq(trim(message), "character_eq (fail)", "test_read_unittest_nml, character_eq (fail), message")
    call tests%character_eq(returned_character, "a", "test_read_unittest_nml, character_eq (fail), returned_character")
    call tests%character_eq(compared_character, "b", "test_read_unittest_nml, character_eq (fail), compared_character")
    
    ! tests summary
    open(newunit=nml_unit, file="prec.nml", status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=tests_summary)
    close(unit=nml_unit)
    call tests%integer_eq(n_tests, 9, "test_read_unittest_nml, tests_summary, n_tests for prec.nml")
    call tests%integer_eq(n_failures, 0, "test_read_unittest_nml, tests_summary, n_failures for prec.nml")
    ! call tests%real_gt(duration, 0.0_WP, "test_read_unittest_nml, tests_summary, duration > 0")
    ! TODO: duration
    ! Unfortunately the duration for prec.nml is often zero due to the low precision of the clock.
    ! So I can't test whether it's greater than zero. And I don't have a `real_gt` test subroutine yet.
    ! I had the following in the Python tests:
    ! ```
    ! duration = datetime.fromisoformat(log_lines.last_time) - datetime.fromisoformat(log_lines.first_time)
    ! 
    ! # Once, this failed for flang-7 for unknown reasons.
    ! self.assertAlmostEqual(log_lines.duration, duration.total_seconds(), places=1)
    ! ```
end subroutine test_read_unittest_nml

end program test_unittest
