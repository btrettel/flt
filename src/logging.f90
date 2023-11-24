! # $File$
! 
! Summary: Module for structured logging in a JSON lines file.
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module logging

use prec, only: I5, RP, CL
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

integer(kind=I5), public, parameter :: KL       = 31_I5 ! dictionary key length
integer(kind=I5), public, parameter :: LOG_UNIT = 10_I5

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
    
    !write(unit=*, fmt=CHAR_FMT) " Writing to log file: " // LOG_FILENAME
    
    close(unit=LOG_UNIT)
    
    return
end subroutine start_log

subroutine log_message(log_filename, message, rc, dict_log, stdout)
    ! Write to the log file.
    
    character(len=*), intent(in)                   :: log_filename, message
    integer(kind=I5), optional, intent(in)         :: rc
    type(dict), dimension(:), optional, intent(in) :: dict_log
    logical, optional, intent(in)                  :: stdout
    
    integer(kind=I5)    :: rc_set, i_dict
    character(len=7_I5) :: rc_string
    
    type(dict), dimension(:), allocatable :: dict_set
    logical                               :: stdout_set
    
    character(len=5_I5)               :: zone
    integer(kind=I5), dimension(8_I5) :: values
    character(len=4)                  :: year
    character(len=2)                  :: month, day, hour, minutes, seconds
    character(len=3)                  :: milliseconds
    character(len=29)                 :: datetime
    
    if (present(rc)) then
        rc_set = rc
    else
        rc_set = 0_I5
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
        allocate(dict_set(0_I5)) ! NO FMUTATE
    end if
    
    ! ISO 8601 date-time format.
    ! <https://en.wikipedia.org/wiki/ISO_8601>
    
    call date_and_time(zone=zone, values=values)
    
    write(unit=year, fmt="(i4.4)") values(1_I5)
    write(unit=month, fmt="(i2.2)") values(2_I5)
    write(unit=day, fmt="(i2.2)") values(3_I5)
    write(unit=hour, fmt="(i2.2)") values(5_I5)
    write(unit=minutes, fmt="(i2.2)") values(6_I5)
    write(unit=seconds, fmt="(i2.2)") values(7_I5)
    write(unit=milliseconds, fmt="(i3.3)") values(8_I5)
    
    datetime = year // "-" // month // "-" // day // "T" // hour // ":" // minutes // ":" // seconds // &
                "." // milliseconds // zone(1_I5:3_I5) // ":" // zone(4_I5:5_I5)
    
    write(unit=rc_string, fmt="(i7)") rc_set
    
    open(unit=LOG_UNIT, action="write", &
            status="old", position="append", file=LOG_FILENAME)
    
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '{"time": "'
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") datetime
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '", "rc": '
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") trim(adjustl(rc_string))
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") ', "message": "'
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") message
    write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '"'
    
    do i_dict = 1_I5, size(dict_set)
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") ", "
        
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") '"' // trim(adjustl(dict_set(i_dict)%k)) // '": '
        
        write(unit=LOG_UNIT, fmt=CHAR_FMT, advance="no") trim(adjustl(dict_set(i_dict)%v))
    end do
    
    write(unit=LOG_UNIT, fmt=CHAR_FMT) "}"
    close(unit=LOG_UNIT)
    
    if (stdout_set) then
        write(unit=*, fmt=CHAR_FMT) message ! NO COMMENT FMUTATE
        
        do i_dict = 1_I5, size(dict_set) ! NO FMUTATE
            write(unit=*, fmt=CHAR_FMT, advance="no") trim(adjustl(dict_set(i_dict)%k)) // " = " ! NO COMMENT FMUTATE
            write(unit=*, fmt=CHAR_FMT) trim(adjustl(dict_set(i_dict)%v)) ! NO COMMENT FMUTATE
        end do
    end if
    
    deallocate(dict_set) ! NO COMMENT FMUTATE
    
    return
end subroutine log_message

subroutine log_error(log_filename, message, rc, dict_log)
    character(len=*), intent(in)                   :: log_filename, message
    integer(kind=I5), optional, intent(in)         :: rc
    type(dict), dimension(:), optional, intent(in) :: dict_log
    
    integer(kind=I5) :: rc_set
    
    if (present(rc)) then
        rc_set = rc
    else
        rc_set = 1_I5
    end if
    
    if (present(dict_log)) then
        call log_message(log_filename, ERROR_PREFIX // message, rc=rc_set, dict_log=dict_log, stdout=.true.)
    else
        call log_message(log_filename, ERROR_PREFIX // message, rc=rc_set)
    end if
    
    return
end subroutine log_error

function r2c(x)
    real(kind=RP), intent(in) :: x
    
    character(len=41) :: r2c, r2c_before ! large enough to handle quad precision
    
    integer(kind=I5)  :: prec
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
    write(unit=len_string, fmt="(i2)") prec + 7_I5
    
    full_prec_fmt = "(es" // len_string // "." // prec_string // "e2)"
    
    write(unit=r2c_before, fmt=full_prec_fmt) x
    
    r2c = upcase(r2c_before)
    
    return
end function r2c

function upcase(string)
    ! <https://www.star.le.ac.uk/%7ecgp/fortran.html>
    
    character(len=*), intent(in) :: string
    
    character(len=len(string)) :: upcase
    
    integer :: j
    
    do j = 1_I5, len(string)
        if(string(j:j) >= "a" .and. string(j:j) <= "z") then
            upcase(j:j) = achar(iachar(string(j:j)) - 32)
        else
            upcase(j:j) = string(j:j)
        end if
    end do
    
    return
end function upcase

subroutine real_dict(key, real_in, dict_out)
    character(len=*), intent(in) :: key
    real(kind=RP), intent(in)    :: real_in
    type(dict), intent(out)      :: dict_out
    
    dict_out%k = key
    dict_out%v = r2c(real_in)
    
    return
end subroutine real_dict

subroutine integer_dict(key, integer_in, dict_out)
    character(len=*), intent(in) :: key
    integer(kind=I5), intent(in) :: integer_in
    type(dict), intent(out)      :: dict_out
    
    character(len=CL) :: integer_char
    
    dict_out%k = key
    write(unit=integer_char, fmt=FULL_INT_FMT) integer_in
    dict_out%v = trim(adjustl(integer_char))
    
    return
end subroutine integer_dict

subroutine string_dict(key, string_in, dict_out)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: string_in
    type(dict), intent(out)      :: dict_out
    
    dict_out%k = key
    dict_out%v = '"' // trim(adjustl(string_in)) // '"'
    
    return
end subroutine string_dict

end module logging
