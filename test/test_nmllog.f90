! tests for the logging module
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_nmllog

use nmllog, only: log_type, UNIT_CLOSED, WARNING_LEVEL
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger, test_logger
type(test_results_type) :: test_data
!integer                 :: rm_unit

character(len=*), parameter :: TEST_FILENAME = "test.nml"

call logger%open("nmllog.nml")
call test_data%start_tests(logger)

call test_data%integer_equality_test(test_logger%unit, UNIT_CLOSED, "logger, unit closed before opening")
call test_logger%open(TEST_FILENAME)

call test_data%character_equality_test(test_logger%filename, TEST_FILENAME, "logger, filename")
call test_data%integer_equality_test(test_logger%level, WARNING_LEVEL, "logger, level before opening")

call test_logger%debug("Debug level test")
call test_logger%info("Info level test")
call test_logger%warning("Warning level test")
call test_logger%error("Error level test")
call test_logger%critical("Critical level test")

call test_logger%debug_info()

call test_logger%close()

! TODO: Read log file to make sure that all messages are written properly.
! TODO: Test to make sure that log is closed after `logger%close()`
! TODO: Test `now()`
! TODO: Test `log_debug_info`

! Delete `TEST_FILENAME`.
!open(newunit=rm_unit, file=TEST_FILENAME, status="old")
!close(unit=rm_unit, status="delete")

call test_data%end_tests()
call logger%close()

end program test_nmllog
