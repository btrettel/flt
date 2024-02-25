! tests for the logging module
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_nmllog

use nmllog, only: log_type, UNIT_CLOSED, WARNING_LEVEL, TIMESTAMP_LEN
use unittest, only: test_results_type
implicit none

type(log_type)               :: logger, test_logger
type(test_results_type)      :: test_data
integer                      :: rm_unit

character(len=*), parameter :: TEST_FILENAME = "test.nml"

call logger%open("nmllog.nml")
call test_data%start_tests(logger)

call test_data%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed before opening")
call test_logger%open(TEST_FILENAME)

call test_data%character_eq(test_logger%filename, TEST_FILENAME, "logger, filename")
call test_data%integer_eq(test_logger%level, WARNING_LEVEL, "logger, level before opening")
call test_data%integer_ne(test_logger%unit, UNIT_CLOSED, "logger, unit open after opening")

call test_logger%debug("Debug level test")
call test_logger%info("Info level test")
call test_logger%warning("Warning level test")
call test_logger%error("Error level test")
call test_logger%critical("Critical level test")

call test_logger%debug_info()

call test_now(test_data)

call test_logger%close()

! TODO: Read log file to make sure that all messages are written properly.
! TODO: Test `log_debug_info`

! Delete `TEST_FILENAME`.
open(newunit=rm_unit, file=TEST_FILENAME, status="old")
close(unit=rm_unit, status="delete")

call test_data%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed after closing")

call test_data%end_tests()
call logger%close()

contains

subroutine test_now(test_data)
    use nmllog, only: now
    
    type(test_results_type), intent(inout) :: test_data
    
    character(len=TIMESTAMP_LEN) :: timestamp
    integer :: year, month, day, hour, minutes, seconds, milliseconds, timezone_hour, timezone_minutes
    
    ! 2024-02-24T18:50:53.194-05:00
    
    timestamp = now()
    
    read(timestamp(1:4), "(i4)") year
    call test_data%integer_ge(year, 2000, "now, year lower bound")
    call test_data%integer_le(year, 2050, "now, year upper bound")
    
    call test_data%character_eq(timestamp(5:5), "-", "now, timestamp(5)")
    
    read(timestamp(6:7), "(i2)") month
    call test_data%integer_ge(month, 1, "now, month lower bound")
    call test_data%integer_le(month, 12, "now, month upper bound")
    
    call test_data%character_eq(timestamp(8:8), "-", "now, timestamp(8)")
    
    read(timestamp(9:10), "(i2)") day
    call test_data%integer_ge(day, 1, "now, day lower bound")
    call test_data%integer_le(day, 31, "now, day upper bound")
    
    call test_data%character_eq(timestamp(11:11), "T", "now, timestamp(11)")
    
    read(timestamp(12:13), "(i2)") hour
    call test_data%integer_ge(hour, 0, "now, hour lower bound")
    call test_data%integer_le(hour, 23, "now, hour upper bound")
    
    call test_data%character_eq(timestamp(14:14), ":", "now, timestamp(14)")
    
    read(timestamp(15:16), "(i2)") minutes
    call test_data%integer_ge(minutes, 0, "now, minutes lower bound")
    call test_data%integer_le(minutes, 59, "now, minutes upper bound")
    
    call test_data%character_eq(timestamp(17:17), ":", "now, timestamp(17)")
    
    read(timestamp(18:19), "(i2)") seconds
    call test_data%integer_ge(seconds, 0, "now, seconds lower bound")
    call test_data%integer_le(seconds, 59, "now, seconds upper bound")
    
    call test_data%character_eq(timestamp(20:20), ".", "now, timestamp(20)")
    
    read(timestamp(21:23), "(i3)") milliseconds
    call test_data%integer_ge(milliseconds, 0, "now, milliseconds lower bound")
    call test_data%integer_le(milliseconds, 999, "now, milliseconds upper bound")
    
    call test_data%character_eq(timestamp(24:24), "-", "now, timestamp(24)")
    
    read(timestamp(25:26), "(i2)") timezone_hour
    call test_data%integer_ge(timezone_hour, 0, "now, timezone_hour lower bound")
    call test_data%integer_le(timezone_hour, 23, "now, timezone_hour upper bound")
    
    call test_data%character_eq(timestamp(27:27), ":", "now, timestamp(27)")
    
    read(timestamp(28:29), "(i2)") timezone_minutes
    call test_data%integer_ge(timezone_minutes, 0, "now, timezone_minutes lower bound")
    call test_data%integer_le(timezone_minutes, 59, "now, timezone_minutes upper bound")
end subroutine test_now

end program test_nmllog
