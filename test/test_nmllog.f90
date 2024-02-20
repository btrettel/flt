! # $File$
! 
! Summary: tests for the logging module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_nmllog

use nmllog, only: log_type, UNIT_CLOSED, WARNING_LEVEL
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger, test_logger
type(test_results_type) :: test_data
!logical                 :: unit_opened
integer                 :: test_logger_unit

character(len=*), parameter :: TEST_FILENAME = "test.nml"

call logger%open("nmllog.nml")
call test_data%start_tests(logger)

call test_data%integer_equality_test(test_logger%unit, UNIT_CLOSED, "logger, unit closed before opening")
call test_logger%open(TEST_FILENAME)

test_logger_unit = test_logger%unit

call test_data%character_equality_test(test_logger%filename, TEST_FILENAME, "logger, filename")
call test_data%integer_equality_test(test_logger%unit, logger%unit + 1, "logger, unit after opening")
call test_data%integer_equality_test(test_logger%level, WARNING_LEVEL, "logger, level before opening")

call test_logger%debug("Debug level test")
call test_logger%info("Info level test")
call test_logger%warning("Warning level test")
call test_logger%error("Error level test")
call test_logger%critical("Critical level test")
call test_logger%close()

! TODO: Test where `LOG_UNIT` is already open to see if `logger%unit` is set to another unit.
! TODO: Test where `MAX_TRIES` criteria is met to make sure that the unit number is negative as the `logger%open` operation failed.
! TODO: Read log file to make sure that all messages are written properly.
! TODO: Test to make sure that log is closed after `logger%close()`
! TODO: Test `now()`

! Delete `TEST_FILENAME`.
open(unit=test_logger_unit, file=TEST_FILENAME, status="old")
close(unit=test_logger_unit, status="delete")

call test_data%end_tests()
call logger%close()

end program test_nmllog
