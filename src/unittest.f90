! Module for unit testing procedures.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module unittest

use prec, only: RP, CL
use nmllog, only: log_type, now, TIMESTAMP_LEN, DEBUG_LEVEL
implicit none
private

public :: validate_timestamp

character(len=70), parameter :: LONG_LINE = "----------------------------------------------------------------------"

type, public :: test_results_type
    integer        :: n_failures = 0
    integer        :: n_tests    = 0
    real(kind=RP)  :: start_time
    real(kind=RP)  :: end_time
    type(log_type) :: logger
contains
    procedure :: logical_true => logical_true
    procedure :: logical_false => logical_false
    procedure :: real_eq => real_eq
    procedure :: real_ne => real_ne
    ! TODO: real_gt, real_ge, real_lt, real_le
    procedure :: integer_eq => integer_eq
    procedure :: integer_ne => integer_ne
    procedure :: integer_ge => integer_ge
    procedure :: integer_le => integer_le
    ! TODO: integer_gt, integer_lt
    procedure :: character_eq => character_eq
    ! TODO: character_ne
    procedure :: start_tests => start_tests
    procedure :: end_tests => end_tests
end type test_results_type

contains

subroutine logical_true(this, condition, message_in)
    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: message_in
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    logical                       :: test_passes, returned_logical, compared_logical
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_passes, message, returned_logical, compared_logical
    
    timestamp        = now()
    variable_type    = "logical"
    message          = message_in
    returned_logical = condition
    compared_logical = .true.
    
    test_passes = condition
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine logical_true

subroutine logical_false(this, condition, message_in)
    ! Check whether test `condition` is `.false.`, increase `number_of_failures` if `.true.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: message_in
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    logical                       :: test_passes, returned_logical, compared_logical
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_passes, message, returned_logical, compared_logical
    
    timestamp        = now()
    variable_type    = "logical"
    message          = message_in
    returned_logical = condition
    compared_logical = .false.
    
    test_passes = .not. condition
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine logical_false

subroutine real_eq(this, returned_real, compared_real, message_in, abs_tol, ne)
    ! Check whether two reals are close, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    use checks, only: TOL_FACTOR, check, is_close
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)    :: returned_real, compared_real
    character(len=*), intent(in) :: message_in
    
    real(kind=RP), intent(in), optional :: abs_tol
    logical, intent(in), optional       :: ne ! `.false.` for checking equality, `.true.` for checking inequality
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=4)              :: variable_type
    character(len=2)              :: test_operator
    logical                       :: test_passes, checking_equality
    character(len=:), allocatable :: message
    real(kind=RP)                 :: tolerance, difference
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
                            returned_real, compared_real, tolerance, difference
    
    if (present(abs_tol)) then
        tolerance = abs_tol
    else
        tolerance = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    if (present(ne)) then
        checking_equality = .not. ne
    else
        checking_equality = .true.
    end if
    
    difference = abs(returned_real - compared_real)
    test_passes = is_close(returned_real, compared_real, abs_tol=tolerance)
    
    if (checking_equality) then
        test_operator = "=="
    else
        test_operator = "/="
        test_passes = .not. test_passes
    end if
    
    timestamp     = now()
    variable_type = "real"
    message       = message_in
    
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, es15.8)") "real returned = ", returned_real
            
            if (checking_equality) then
                write(unit=ERROR_UNIT, fmt="(a, es15.8)") "real expected = ", compared_real
            else
                write(unit=ERROR_UNIT, fmt="(a, es15.8)") "      /= real = ", compared_real
            end if
            
            write(unit=ERROR_UNIT, fmt="(a, es15.8)") "    tolerance = ", tolerance
            if (is_close(abs(compared_real), 0.0_RP)) then
                write(unit=ERROR_UNIT, fmt="(a, es15.8)") "   difference = ", difference
            else
                write(unit=ERROR_UNIT, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", difference, &
                                                            " (", 100.0_RP * difference / abs(compared_real), "%)"
            end if
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    call check(difference >= 0.0_RP, this%logger, "real_eq, difference < 0", this%n_failures)
    
    this%n_tests = this%n_tests + 1
end subroutine real_eq

subroutine real_ne(this, returned_real, compared_real, message_in, abs_tol)
    ! Check whether two reals are not close, increase `number_of_failures` if `.true.`.
    
    use checks, only: TOL_FACTOR
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)    :: returned_real, compared_real
    character(len=*), intent(in) :: message_in
    
    real(kind=RP), intent(in), optional :: abs_tol
    
    real(kind=RP) :: tolerance
    
    if (present(abs_tol)) then
        tolerance = abs_tol
    else
        tolerance = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    call real_eq(this, returned_real, compared_real, message_in, abs_tol=tolerance, ne=.true.)
end subroutine real_ne

subroutine integer_eq(this, returned_integer, compared_integer, message_in)
    ! Check whether two integers are identical, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message_in
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    character(len=2)              :: test_operator
    logical                       :: test_passes
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    timestamp     = now()
    variable_type = "integer"
    message       = message_in
    test_operator = "=="
    
    test_passes = (returned_integer == compared_integer)
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, i7)") "integer returned = ", returned_integer
            write(unit=ERROR_UNIT, fmt="(a, i7)") "integer expected = ", compared_integer
            write(unit=ERROR_UNIT, fmt="(a, i7)") "      difference = ", abs(returned_integer - compared_integer)
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine integer_eq

subroutine integer_ne(this, returned_integer, compared_integer, message_in)
    ! Check whether `returned_integer` equal than `compared_integer`, increase `number_of_failures` if `.true.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message_in
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    character(len=2)              :: test_operator
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    timestamp     = now()
    variable_type = "integer"
    test_operator = "/="
    message       = message_in
    
    test_passes = (returned_integer /= compared_integer)
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, i7)") "          integer returned = ", returned_integer
            write(unit=ERROR_UNIT, fmt="(a, i7)") "which should be /= integer = ", compared_integer
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine integer_ne

subroutine integer_ge(this, returned_integer, compared_integer, message_in)
    ! Check whether `returned_integer` is greater or equal than `compared_integer`, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message_in
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    character(len=2)              :: test_operator
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    timestamp     = now()
    variable_type = "integer"
    test_operator = ">="
    message       = message_in
    
    test_passes = (returned_integer >= compared_integer)
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, i7)") "          integer returned = ", returned_integer
            write(unit=ERROR_UNIT, fmt="(a, i7)") "which should be >= integer = ", compared_integer
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine integer_ge

subroutine integer_le(this, returned_integer, compared_integer, message_in)
    ! Check whether `returned_integer` is less or equal than `compared_integer`, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message_in
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=7)              :: variable_type
    character(len=2)              :: test_operator
    character(len=:), allocatable :: message
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    timestamp     = now()
    variable_type = "integer"
    test_operator = "<="
    message       = message_in
    
    test_passes = (returned_integer <= compared_integer)
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a, i7)") "          integer returned = ", returned_integer
            write(unit=ERROR_UNIT, fmt="(a, i7)") "which should be <= integer = ", compared_integer
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine integer_le

subroutine character_eq(this, returned_character_in, compared_character_in, message_in)
    ! Check whether two character variables are identical, increase `number_of_failures` if `.false.`.
    
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    character(len=*), intent(in) :: returned_character_in, compared_character_in
    character(len=*), intent(in) :: message_in
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=9)              :: variable_type
    character(len=2)              :: test_operator
    character(len=:), allocatable :: message, returned_character, compared_character
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_character, compared_character
    
    timestamp          = now()
    variable_type      = "character"
    test_operator      = "=="
    message            = message_in
    returned_character = trim(returned_character_in)
    compared_character = trim(compared_character_in)
    
    ! It's not clear to me why `returned_character` needs to be trimmed.
    ! The namelist file will have a lot of spaces otherwise.
    
    test_passes = (trim(returned_character) == trim(compared_character))
    write(unit=this%logger%unit, nml=test_result)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        
        if (DEBUG_LEVEL >= this%logger%stdout_level) then
            write(unit=ERROR_UNIT, fmt="(a)") "character returned = " // returned_character
            write(unit=ERROR_UNIT, fmt="(a)") "character expected = " // compared_character
            write(unit=ERROR_UNIT, fmt="(a, a, a)") "fail: ", message, new_line("a")
        end if
    end if
    
    this%n_tests = this%n_tests + 1
end subroutine character_eq

function current_time()
    real(kind=RP) :: current_time ! in seconds
    
    integer :: clock_count, count_rate
    
    call system_clock(clock_count, count_rate)
    current_time = real(clock_count, RP) / real(count_rate, RP)
    
    return
end function current_time

subroutine start_tests(this, logger)
    use nmllog, only: DEBUG_LEVEL
    
    class(test_results_type), intent(out) :: this
    
    type(log_type), intent(in) :: logger
    
    this%start_time          = current_time()
    this%logger              = logger
    this%logger%stdout_level = DEBUG_LEVEL
    this%logger%file_level   = DEBUG_LEVEL
end subroutine start_tests

subroutine end_tests(this)
    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
    
    class(test_results_type), intent(inout) :: this
    
    integer       :: n_tests, n_failures
    real(kind=RP) :: duration ! in seconds
    
    namelist /tests_summary/ n_tests, n_failures, duration
    
    this%end_time = current_time()
    duration      = this%end_time - this%start_time
    n_tests       = this%n_tests
    n_failures    = this%n_failures
    write(unit=this%logger%unit, nml=tests_summary)
    
    write(unit=*, fmt="(a, i0, a, f0.3, a)") "Ran ", this%n_tests, " tests in ", duration, "s"
    
    if (this%n_failures /= 0) then
        write(unit=ERROR_UNIT, fmt="(a, i0, a)") "FAILED (failures=", this%n_failures, ")"
        write(unit=ERROR_UNIT, fmt="(a)") LONG_LINE
        call this%logger%close()
        stop 1
    else
        write(unit=*, fmt="(a, a)") "All tests passed.", new_line("a")
        write(unit=*, fmt="(a)") "OK"
        write(unit=*, fmt="(a)") LONG_LINE
    end if
end subroutine end_tests

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

end module unittest
