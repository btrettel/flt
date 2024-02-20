! # $File$
! 
! Summary: Module for structured logging in a namelist file.
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Based roughly on the Python logging module.
! <https://docs.python.org/3/howto/logging.html>

module nmllog

use prec, only: RP, CL
implicit none
private

public :: now

integer, parameter :: LOG_UNIT      = 10
integer, parameter :: MAX_TRIES     = 10
integer, parameter :: TIMESTAMP_LEN = 29

integer, public, parameter :: UNIT_CLOSED = -1

integer, public, parameter :: NOT_SET_LEVEL  = 0
integer, public, parameter :: DEBUG_LEVEL    = 10
integer, public, parameter :: INFO_LEVEL     = 20
integer, public, parameter :: WARNING_LEVEL  = 30
integer, public, parameter :: ERROR_LEVEL    = 40
integer, public, parameter :: CRITICAL_LEVEL = 50

type, public :: log_type
    character(len=:), allocatable :: filename
    integer :: unit  = UNIT_CLOSED
    integer :: level = WARNING_LEVEL
contains
    procedure :: open => log_open
    procedure :: close => log_close
    procedure :: debug => log_debug
    procedure :: info => log_info
    procedure :: warning => log_warning
    procedure :: error => log_error
    procedure :: critical => log_critical
end type log_type

contains

function now()
    character(len=5)             :: zone
    integer                      :: values(8)
    character(len=4)             :: year
    character(len=2)             :: month, day, hour, minutes, seconds
    character(len=3)             :: milliseconds
    character(len=TIMESTAMP_LEN) :: now
    
    ! ISO 8601 date-time format.
    ! <https://en.wikipedia.org/wiki/ISO_8601>
    
    call date_and_time(zone=zone, values=values)
    
    write(unit=year,         fmt="(i4.4)") values(1)
    write(unit=month,        fmt="(i2.2)") values(2)
    write(unit=day,          fmt="(i2.2)") values(3)
    write(unit=hour,         fmt="(i2.2)") values(5)
    write(unit=minutes,      fmt="(i2.2)") values(6)
    write(unit=seconds,      fmt="(i2.2)") values(7)
    write(unit=milliseconds, fmt="(i3.3)") values(8)
    
    now = year // "-" // month // "-" // day // "T" // hour // ":" // minutes // ":" // seconds // &
                "." // milliseconds // zone(1:3) // ":" // zone(4:5)
end function now

subroutine log_open(this, filename, level)
    class(log_type), intent(out) :: this
    
    character(len=*), intent(in)  :: filename
    integer, optional, intent(in) :: level
    
    integer :: unit_tries
    logical :: unit_opened
    
    if (present(level)) then
        this%level = level
    end if
    
    this%unit = LOG_UNIT
    do unit_tries = 1, MAX_TRIES
        inquire(unit=this%unit, opened=unit_opened)
        
        if (.not. unit_opened) then
            exit
        end if
        
        this%unit = this%unit + 1
    end do
    
    if (unit_opened) then
        this%unit = UNIT_CLOSED
    else
        this%filename = filename
        open(unit=this%unit, action="write", status="replace", position="rewind", file=trim(this%filename))
    end if
end subroutine log_open

subroutine log_close(this)
    class(log_type), intent(inout) :: this
    
    close(unit=this%unit)
    this%unit = UNIT_CLOSED
end subroutine log_close

subroutine log_writer(this, message, level_code)
    type(log_type) :: this
    
    character(len=*), intent(in) :: message
    integer, intent(in)          :: level_code
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=:), allocatable :: level
    
    namelist /log/ timestamp, message, level
    
    timestamp = now()
    
    select case (level_code)
        case (DEBUG_LEVEL)
            level = "debug"
        case (INFO_LEVEL)
            level = "info"
        case (WARNING_LEVEL)
            level = "warning"
        case (ERROR_LEVEL)
            level = "error"
        case (CRITICAL_LEVEL)
            level = "critical"
        case default
            level = "not set"
    end select
    
    write(unit=this%unit, nml=log)
    
    if (level_code >= this%level) then
        write(unit=*, fmt="(a, a, a, a, a)") timestamp, " [", level, "] ", message
    end if
end subroutine log_writer

subroutine log_debug(this, message)
    class(log_type) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, DEBUG_LEVEL)
end subroutine log_debug

subroutine log_info(this, message)
    class(log_type) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, INFO_LEVEL)
end subroutine log_info

subroutine log_warning(this, message)
    class(log_type) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, WARNING_LEVEL)
end subroutine log_warning

subroutine log_error(this, message)
    class(log_type) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, ERROR_LEVEL)
end subroutine log_error

subroutine log_critical(this, message)
    class(log_type) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, CRITICAL_LEVEL)
end subroutine log_critical

end module nmllog
