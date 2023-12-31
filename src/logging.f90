! # $File$
! 
! Summary: Module for structured logging in a JSON lines file.
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module logging

use prec, only: RP, CL
implicit none
private

public :: start_log
public :: log_message
public :: log_error
public :: real_dict
public :: integer_dict
public :: string_dict
public :: upcase

private :: r2c

integer, public, parameter :: KL       = 31 ! dictionary key length
integer, public, parameter :: LOG_UNIT = 10

character(len=*), public, parameter :: CHAR_FMT     = "(a)"
character(len=*), public, parameter :: FULL_INT_FMT = "(i7)"
character(len=*), public, parameter :: ERROR_PREFIX = "ERROR: "

type, public :: dict
    character(len=KL) :: k ! key
    character(len=CL) :: v ! value
end type dict

contains

subroutine start_log(log_filename)
    character(len=*), intent(in) :: log_filename ! filename to write log to
    
    open(unit=LOG_UNIT, action="write", &
            status="replace", position="rewind", file=log_filename)
    
    close(unit=LOG_UNIT)
    
    return
end subroutine start_log

subroutine log_message(log_filename, message, rc, dict_log, stdout)
    ! Write to the log file.
    
    character(len=*), intent(in)           :: log_filename, message
    integer, optional, intent(in) :: rc
    type(dict), optional, intent(in)       :: dict_log(:)
    logical, optional, intent(in)          :: stdout
    
    integer          :: rc_set, i_dict
    character(len=7) :: rc_string
    
    type(dict), allocatable :: dict_set(:)
    logical                 :: stdout_set
    
    character(len=5)  :: zone
    integer           :: values(8)
    character(len=4)  :: year
    character(len=2)  :: month, day, hour, minutes, seconds
    character(len=3)  :: milliseconds
    character(len=29) :: datetime
    
    if (present(rc)) then
        rc_set = rc
    else
        rc_set = 0
    end if
    
    if (present(stdout)) then
        stdout_set = stdout ! NO COMMENT FMUTATE
    else ! NO COMMENT FMUTATE
        stdout_set = .false. ! NO COMMENT FMUTATE
    end if
    
    if (present(dict_log)) then
        allocate(dict_set(size(dict_log)))
        dict_set = dict_log
    else
        allocate(dict_set(0)) ! NO FMUTATE
    end if
    
    ! ISO 8601 date-time format.
    ! <https://en.wikipedia.org/wiki/ISO_8601>
    
    call date_and_time(zone=zone, values=values)
    
    write(unit=year, fmt="(i4.4)") values(1)
    write(unit=month, fmt="(i2.2)") values(2)
    write(unit=day, fmt="(i2.2)") values(3)
    write(unit=hour, fmt="(i2.2)") values(5)
    write(unit=minutes, fmt="(i2.2)") values(6)
    write(unit=seconds, fmt="(i2.2)") values(7)
    write(unit=milliseconds, fmt="(i3.3)") values(8)
    
    datetime = year // "-" // month // "-" // day // "T" // hour // ":" // minutes // ":" // seconds // &
                "." // milliseconds // zone(1:3) // ":" // zone(4:5)
    
    write(unit=rc_string, fmt="(i7)") rc_set
    
    open(unit=LOG_UNIT, action="write", &
            status="old", position="append", file=log_filename)
    
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '{"time": "'
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") datetime
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '", "rc": '
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") trim(adjustl(rc_string))
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") ', "message": "'
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") message
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '"'
    
    do i_dict = 1, size(dict_set)
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") ", "
        
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '"' // trim(adjustl(dict_set(i_dict)%k)) // '": '
        
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") trim(adjustl(dict_set(i_dict)%v))
    end do
    
    write(unit=LOG_UNIT, fmt=CHAR_FMT) "}"
    close(unit=LOG_UNIT)
    
    if (stdout_set) then
        write(unit=*, fmt=CHAR_FMT) message ! NO COMMENT FMUTATE
        
        do i_dict = 1, size(dict_set) ! NO FMUTATE
            write(unit=*, fmt=CHAR_FMT, advance="no") trim(adjustl(dict_set(i_dict)%k)) // " = " ! NO COMMENT FMUTATE
            write(unit=*, fmt=CHAR_FMT) trim(adjustl(dict_set(i_dict)%v)) ! NO COMMENT FMUTATE
        end do
    end if
    
    deallocate(dict_set) ! NO COMMENT FMUTATE
end subroutine log_message

subroutine log_error(log_filename, message, rc, dict_log)
    character(len=*), intent(in)     :: log_filename, message
    integer, optional, intent(in)    :: rc
    type(dict), optional, intent(in) :: dict_log(:)
    
    integer :: rc_set
    
    if (present(rc)) then
        rc_set = rc
    else
        rc_set = 1
    end if
    
    if (present(dict_log)) then
        call log_message(log_filename, ERROR_PREFIX // message, rc=rc_set, dict_log=dict_log, stdout=.true.)
    else
        call log_message(log_filename, ERROR_PREFIX // message, rc=rc_set)
    end if
end subroutine log_error

pure function r2c(x)
    real(kind=RP), intent(in) :: x
    
    character(len=41) :: r2c, r2c_before ! large enough to handle quad precision
    
    integer           :: prec
    character(len=2)  :: prec_string, len_string
    character(len=11) :: full_prec_fmt
    
    ! es15.8:
    ! 2.00000000E+00
    ! 12345678901234 (I guess an extra is needed for the possibility of a negative sign)
    !   12345678
    ! negative sign, first number, decimal point, digits of precision, e+, 2 digits for exponent
    ! 1 + 1 + 1 + prec + 2 + 2
    ! prec + 7
    
    ! Originally I used 3 digits for the exponent, but flang-7 would only write 2 digits for the exponent.
    
    prec = precision(1.0_RP) ! NO FMUTATE
    write(unit=prec_string, fmt="(i2)") prec
    write(unit=len_string, fmt="(i2)") prec + 7
    
    full_prec_fmt = "(es" // len_string // "." // prec_string // "e2)"
    
    write(unit=r2c_before, fmt=full_prec_fmt) x
    
    r2c = upcase(r2c_before)
end function r2c

pure function upcase(string)
    ! <https://www.star.le.ac.uk/%7ecgp/fortran.html>
    
    character(len=*), intent(in) :: string
    
    character(len=len(string)) :: upcase
    
    integer :: j
    
    do j = 1, len(string)
        if(string(j:j) >= "a" .and. string(j:j) <= "z") then
            upcase(j:j) = achar(iachar(string(j:j)) - 32)
        else
            upcase(j:j) = string(j:j)
        end if
    end do
end function upcase

pure subroutine real_dict(key, real_in, dict_out)
    character(len=*), intent(in) :: key
    real(kind=RP), intent(in)    :: real_in
    type(dict), intent(out)      :: dict_out
    
    dict_out%k = key
    dict_out%v = r2c(real_in)
end subroutine real_dict

pure subroutine integer_dict(key, integer_in, dict_out)
    character(len=*), intent(in) :: key
    integer, intent(in)          :: integer_in
    type(dict), intent(out)      :: dict_out
    
    character(len=CL) :: integer_char
    
    dict_out%k = key
    write(unit=integer_char, fmt=FULL_INT_FMT) integer_in
    dict_out%v = trim(adjustl(integer_char))
end subroutine integer_dict

pure subroutine string_dict(key, string_in, dict_out)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: string_in
    type(dict), intent(out)      :: dict_out
    
    dict_out%k = key
    dict_out%v = '"' // trim(adjustl(string_in)) // '"'
end subroutine string_dict

end module logging
