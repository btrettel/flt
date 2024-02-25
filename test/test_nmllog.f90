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

type(log_type)          :: logger
type(test_results_type) :: tests

call logger%open("nmllog.nml")
call tests%start_tests(logger)

call test_validate_timestamp(tests)
call test_now(tests)
call test_log_subroutines(tests)

! TODO: Read log file to make sure that all messages are written properly.
! TODO: Test `log_debug_info`
! TODO: Test `validate_timestamp`.

call tests%end_tests()
call logger%close()

contains

subroutine validate_timestamp(tests, timestamp, message)
    type(test_results_type), intent(inout) :: tests
    
    character(len=*), intent(in) :: timestamp, message
    integer :: year, month, day, hour, minutes, seconds, milliseconds, timezone_hour, timezone_minutes, rc_read
    
    ! TODO: If any of these fail, write the timestamp to the log.
    
    call tests%integer_eq(len(trim(timestamp)), TIMESTAMP_LEN, message // ", timestamp is correct length")
    
    ! There will be a run-time error if the timestamp is too short, so don't do these tests in that case.
    ! The "timestamp is correct length" test will fail, alerting you to that problem if it occurs.
    if (len(trim(timestamp)) >= TIMESTAMP_LEN) then
        read(timestamp(1:4), "(i4)", iostat=rc_read) year
        call tests%integer_eq(rc_read, 0, message // ", year is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(year, 2000, message // ", year lower bound")
            call tests%integer_le(year, 2050, message // ", year upper bound")
        end if
        
        call tests%character_eq(timestamp(5:5), "-", message // ", timestamp(5)")
        
        read(timestamp(6:7), "(i2)", iostat=rc_read) month
        call tests%integer_eq(rc_read, 0, message // ", month is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(month, 1, message // ", month lower bound")
            call tests%integer_le(month, 12, message // ", month upper bound")
        end if
        
        call tests%character_eq(timestamp(8:8), "-", message // ", timestamp(8)")
        
        read(timestamp(9:10), "(i2)", iostat=rc_read) day
        call tests%integer_eq(rc_read, 0, message // ", day is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(day, 1, message // ", day lower bound")
            call tests%integer_le(day, 31, message // ", day upper bound")
        end if
        
        call tests%character_eq(timestamp(11:11), "T", message // ", timestamp(11)")
        
        read(timestamp(12:13), "(i2)", iostat=rc_read) hour
        call tests%integer_eq(rc_read, 0, message // ", hour is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(hour, 0, message // ", hour lower bound")
            call tests%integer_le(hour, 23, message // ", hour upper bound")
        end if
        
        call tests%character_eq(timestamp(14:14), ":", message // ", timestamp(14)")
        
        read(timestamp(15:16), "(i2)", iostat=rc_read) minutes
        call tests%integer_eq(rc_read, 0, message // ", minutes are numeric")
        if (rc_read == 0) then
            call tests%integer_ge(minutes, 0, message // ", minutes lower bound")
            call tests%integer_le(minutes, 59, message // ", minutes upper bound")
        end if
        
        call tests%character_eq(timestamp(17:17), ":", message // ", timestamp(17)")
        
        read(timestamp(18:19), "(i2)", iostat=rc_read) seconds
        call tests%integer_eq(rc_read, 0, message // ", seconds are numeric")
        if (rc_read == 0) then
            call tests%integer_ge(seconds, 0, message // ", seconds lower bound")
            call tests%integer_le(seconds, 59, message // ", seconds upper bound")
        end if
        
        call tests%character_eq(timestamp(20:20), ".", message // ", timestamp(20)")
        
        read(timestamp(21:23), "(i3)", iostat=rc_read) milliseconds
        call tests%integer_eq(rc_read, 0, message // ", milliseconds are numeric")
        if (rc_read == 0) then
            call tests%integer_ge(milliseconds, 0, message // ", milliseconds lower bound")
            
            ! This is commented out as I guess I can't make milliseconds too high.
!            call tests%integer_le(milliseconds, 999, message // ", milliseconds upper bound")
        end if
        
        call tests%character_eq(timestamp(24:24), "-", message // ", timestamp(24)")
        
        read(timestamp(25:26), "(i2)", iostat=rc_read) timezone_hour
        call tests%integer_eq(rc_read, 0, message // ", timezone_hour is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(timezone_hour, 0, message // ", timezone_hour lower bound")
            call tests%integer_le(timezone_hour, 23, message // ", timezone_hour upper bound")
        end if
        
        call tests%character_eq(timestamp(27:27), ":", message // ", timestamp(27)")
        
        read(timestamp(28:29), "(i2)", iostat=rc_read) timezone_minutes
        call tests%integer_eq(rc_read, 0, message // ", timezone_minutes is numeric")
        if (rc_read == 0) then
            call tests%integer_ge(timezone_minutes, 0, message // ", timezone_minutes lower bound")
            call tests%integer_le(timezone_minutes, 59, message // ", timezone_minutes upper bound")
        end if
    end if
end subroutine validate_timestamp

subroutine test_validate_timestamp(tests)
    type(test_results_type), intent(inout) :: tests
    
    character(len=:), allocatable :: timestamp
    type(test_results_type)       :: failing_tests
    
    integer :: n_tests, n_failing
    
    call failing_tests%start_tests(tests%logger)
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

subroutine test_now(tests)
    use nmllog, only: now
    
    type(test_results_type), intent(inout) :: tests
    
    character(len=TIMESTAMP_LEN) :: timestamp
    
    timestamp = now()
    
    call validate_timestamp(tests, timestamp, "now")
end subroutine test_now

subroutine test_log_subroutines(tests)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use prec, only: CL
    use nmllog, only: DEBUG_STRING, INFO_STRING, WARNING_STRING, ERROR_STRING, CRITICAL_STRING
    
    type(test_results_type), intent(inout) :: tests
    
    type(log_type) :: test_logger
    integer        :: nml_unit, rc_nml, num_nml_groups, n_debug, n_info, n_warning, n_error, n_critical, n_not_set
    
    character(len=*), parameter :: TEST_FILENAME    = "test.nml"
    character(len=*), parameter :: DEBUG_MESSAGE    = "Debug level test"
    character(len=*), parameter :: INFO_MESSAGE     = "Info level test"
    character(len=*), parameter :: WARNING_MESSAGE  = "Warning level test"
    character(len=*), parameter :: ERROR_MESSAGE    = "Error level test"
    character(len=*), parameter :: CRITICAL_MESSAGE = "Critical level test"
    
    character(len=CL) :: nml_error_message
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=8)             :: level
    character(len=CL)            :: message
    character(len=1)             :: nml_group_str
    
    namelist /log/ timestamp, level, message
    
    call tests%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed before opening")
    call test_logger%open(TEST_FILENAME)

    call tests%character_eq(test_logger%filename, TEST_FILENAME, "logger, filename")
    call tests%integer_eq(test_logger%level, WARNING_LEVEL, "logger, level before opening")
    call tests%integer_ne(test_logger%unit, UNIT_CLOSED, "logger, unit open after opening")

    call test_logger%debug(DEBUG_MESSAGE)
    call test_logger%info(INFO_MESSAGE)
    call test_logger%warning(WARNING_MESSAGE)
    call test_logger%error(ERROR_MESSAGE)
    call test_logger%critical(CRITICAL_MESSAGE)

    call test_logger%debug_info()

    call test_logger%close()
    
    call tests%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed after closing")
    
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote")
    num_nml_groups = 0
    n_debug        = 0
    n_info         = 0
    n_warning      = 0
    n_error        = 0
    n_critical     = 0
    n_not_set      = 0
    do
        read(unit=nml_unit, nml=log, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call tests%integer_eq(rc_nml, 0, "reading nmllog, error")
            call tests%logger%error(trim(nml_error_message))
            exit
        end if
        
        num_nml_groups = num_nml_groups + 1
        
        write(unit=nml_group_str, fmt="(i1)") num_nml_groups
        
        call validate_timestamp(tests, timestamp, message)
        
        call tests%integer_ge(len(trim(message)), 1, "Namelist group number " // nml_group_str // " message absent")
        call tests%integer_ge(len(trim(level)), 1, "Namelist group number " // nml_group_str // " level absent")
        
        select case (trim(level))
            case (DEBUG_STRING)
                n_debug = n_debug + 1
                call tests%character_eq(message, DEBUG_MESSAGE, "nmllog, debug message")
            case (INFO_STRING)
                n_info = n_info + 1
                call tests%character_eq(message, INFO_MESSAGE, "nmllog, info message")
            case (WARNING_STRING)
                n_warning = n_warning + 1
                call tests%character_eq(message, WARNING_MESSAGE, "nmllog, warning message")
            case (ERROR_STRING)
                n_error = n_error + 1
                call tests%character_eq(message, ERROR_MESSAGE, "nmllog, error message")
            case (CRITICAL_STRING)
                n_critical = n_critical + 1
                call tests%character_eq(message, CRITICAL_MESSAGE, "nmllog, critical message")
            case default
                n_not_set = n_not_set + 1
        end select
        
        !write(unit=*, fmt=*) rc_nml, timestamp, trim(level), trim(message)
    end do
    
    call tests%integer_eq(rc_nml, IOSTAT_END, "nmllog, iostat at end of file")
    
    if (rc_nml == IOSTAT_END) then
        close(unit=nml_unit, status="delete")
    else
        ! Don't delete if there was an error.
        close(unit=nml_unit)
    end if
    
    call tests%integer_eq(n_debug, 1, "nmllog, number of debug levels")
    call tests%integer_eq(n_info, 1, "nmllog, number of info levels")
    call tests%integer_eq(n_warning, 1, "nmllog, number of warning levels")
    call tests%integer_eq(n_error, 1, "nmllog, number of error levels")
    call tests%integer_eq(n_critical, 1, "nmllog, number of critical levels")
    call tests%integer_eq(n_not_set, 0, "nmllog, no unknown levels")
end subroutine test_log_subroutines

end program test_nmllog
