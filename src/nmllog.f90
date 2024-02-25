! Module for structured logging in a namelist file.
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Based roughly on the Python logging module.
! <https://docs.python.org/3/howto/logging.html>

module nmllog

use prec, only: RP, CL
implicit none
private

public :: now

integer, parameter :: LOG_UNIT  = 10
integer, parameter :: MAX_TRIES = 10

integer, public, parameter :: TIMESTAMP_LEN = 29
integer, public, parameter :: UNIT_CLOSED   = -1

! Needed or else ifort truncates namelist file lines and can't read them back correctly.
! 10000 is arbitrary. Just pick a number larger than anything you expect to use.
! gfortran had a strange run-time error with 1000.
integer, public, parameter :: NML_RECL = 10000

integer, public, parameter :: NOT_SET_LEVEL  = 0
integer, public, parameter :: DEBUG_LEVEL    = 10
integer, public, parameter :: INFO_LEVEL     = 20
integer, public, parameter :: WARNING_LEVEL  = 30
integer, public, parameter :: ERROR_LEVEL    = 40
integer, public, parameter :: CRITICAL_LEVEL = 50

character(len=*), public, parameter :: NOT_SET_STRING  = "not set"
character(len=*), public, parameter :: DEBUG_STRING    = "debug"
character(len=*), public, parameter :: INFO_STRING     = "info"
character(len=*), public, parameter :: WARNING_STRING  = "warning"
character(len=*), public, parameter :: ERROR_STRING    = "error"
character(len=*), public, parameter :: CRITICAL_STRING = "critical"

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
    procedure :: debug_info => log_debug_info
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
        open(unit=this%unit, &
                action="write", &
                status="replace", &
                position="rewind", &
                file=trim(this%filename), &
                delim="quote", &
                recl=NML_RECL)
    end if
end subroutine log_open

subroutine log_close(this)
    class(log_type), intent(inout) :: this
    
    close(unit=this%unit)
    this%unit = UNIT_CLOSED
end subroutine log_close

subroutine log_writer(this, message_in, level_code)
    use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
    
    type(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message_in
    integer, intent(in)          :: level_code
    
    integer :: print_unit
    
    character(len=TIMESTAMP_LEN)  :: timestamp
    character(len=:), allocatable :: level, message
    
    namelist /log/ timestamp, level, message
    
    timestamp = now()
    
    select case (level_code)
        case (DEBUG_LEVEL)
            level = DEBUG_STRING
        case (INFO_LEVEL)
            level = INFO_STRING
        case (WARNING_LEVEL)
            level = WARNING_STRING
        case (ERROR_LEVEL)
            level = ERROR_STRING
        case (CRITICAL_LEVEL)
            level = CRITICAL_STRING
        case default
            level = NOT_SET_STRING
    end select
    
    message = message_in
    write(unit=this%unit, nml=log)
    
    if (level_code >= WARNING_LEVEL) then
        print_unit = ERROR_UNIT
    else
        print_unit = OUTPUT_UNIT
    end if
    
    if (level_code >= this%level) then
        write(unit=print_unit, fmt="(a, a, a, a, a)") timestamp, " [", level, "] ", message
    end if
end subroutine log_writer

subroutine log_debug(this, message)
    class(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, DEBUG_LEVEL)
end subroutine log_debug

subroutine log_info(this, message)
    class(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, INFO_LEVEL)
end subroutine log_info

subroutine log_warning(this, message)
    class(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, WARNING_LEVEL)
end subroutine log_warning

subroutine log_error(this, message)
    class(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, ERROR_LEVEL)
end subroutine log_error

subroutine log_critical(this, message)
    class(log_type), intent(in) :: this
    
    character(len=*), intent(in) :: message
    
    call log_writer(this, message, CRITICAL_LEVEL)
end subroutine log_critical

subroutine log_debug_info(this)
    use, intrinsic :: iso_fortran_env, only: compiler_opt => compiler_options, compiler_ver => compiler_version
    use, intrinsic :: ieee_arithmetic, only: ieee_support_datatype, ieee_support_denormal, ieee_support_divide, &
                                                ieee_support_inf, ieee_support_nan, ieee_support_sqrt, ieee_support_standard
    use prec, only: RP
    
    ! Maybe:
    ! - `epsilon`
    ! - `spacing`
    ! - kind code, range, and huge for `I5`
    
    ! TODO: If `DEBUG_LEVEL >= this%level` then print this information to stdout.
    ! TODO: Print version number, Git revision, revision date, compile date, and whether there are local code modifications.
    ! Similar to <https://github.com/firemodels/fds/blob/master/Source/cons.f90#L278>.
    
    class(log_type), intent(in) :: this
    
    character(len=:), allocatable :: compiler_options, compiler_version, level
    character(len=TIMESTAMP_LEN)  :: timestamp
    
    ! Removed because ifort and ifx can't read `huge` from namelists? Presumably `real_huge` is being read as single-precision.
    !real(kind=RP) :: real_huge
    
    integer       :: real_kind_code, real_precision, real_range, real_radix, &
                        integer_kind_code, integer_range, integer_huge
    logical       :: real_support_datatype, real_support_denormal, real_support_divide, &
                        real_support_inf, real_support_nan, real_support_sqrt, real_support_standard
    
    namelist /debug_info/ timestamp, level, compiler_options, compiler_version, &
                            real_kind_code, real_precision, real_range, real_radix, & !real_huge, &
                            real_support_datatype, real_support_denormal, real_support_divide, &
                            real_support_inf, real_support_nan, real_support_sqrt, real_support_standard, &
                            integer_kind_code, integer_range, integer_huge
    
    timestamp        = now()
    level            = DEBUG_STRING
    compiler_options = compiler_opt()
    compiler_version = compiler_ver()
    
    real_kind_code        = RP
    real_precision        = precision(1.0_RP)
    real_range            = range(1.0_RP)
    real_radix            = radix(1.0_RP)
    !real_huge             = huge(1.0_RP)
    real_support_datatype = ieee_support_datatype(1.0_RP)
    real_support_denormal = ieee_support_denormal(1.0_RP)
    real_support_divide   = ieee_support_divide(1.0_RP)
    real_support_inf      = ieee_support_inf(1.0_RP)
    real_support_nan      = ieee_support_nan(1.0_RP)
    real_support_sqrt     = ieee_support_sqrt(1.0_RP)
    real_support_standard = ieee_support_standard(1.0_RP)
    
    integer_kind_code = kind(1)
    integer_range     = range(1)
    integer_huge      = huge(1)
    
    write(unit=this%unit, nml=debug_info)
end subroutine log_debug_info

end module nmllog
