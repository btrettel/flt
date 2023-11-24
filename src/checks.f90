! # $File$
! 
! Summary: Module for procedures used for run-time checks.
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module asserts

use prec, only: I5, RP, CL
implicit none
private

real(kind=RP), public, parameter :: TOL_FACTOR = 10.0_RP

public :: is_close
public :: check

contains

function is_close(input_real_1, input_real_2, rel_tol, abs_tol)
    ! Determine whether two reals are close.
    
    real(kind=RP), intent(in)           :: input_real_1, input_real_2
    real(kind=RP), intent(in), optional :: rel_tol, abs_tol
    
    real(kind=RP)                       :: rel_tol_set, abs_tol_set, tol
    logical                             :: is_close
    
    if (present(rel_tol)) then
        rel_tol_set = rel_tol
    else
        rel_tol_set = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    if (present(abs_tol)) then
        abs_tol_set = abs_tol
    else
        abs_tol_set = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    tol = max(rel_tol_set * abs(input_real_1), rel_tol_set * abs(input_real_2), abs_tol_set) ! NO OPERATOR 1 FMUTATE NO ARG FMUTATE
    
    if (abs(input_real_1 - input_real_2) < tol) then
        is_close = .true.
    else
        is_close = .false.
    end if
    
    return
end function is_close

subroutine check(condition, log_filename, message, rc, dict_log)
    ! If `condition` is `.false.`, then print a message and increment `rc`.
    ! If `(rc /= RC_SUCCESS)` later, computation will stop.
    ! This is used for assertions and input validation.
    ! Making `rc` increment is useful to string up multiple `check`s
    ! without adding too much logic. The details of the `check` are
    ! logged, so there's `rc` does not need to be meaningful beyond
    ! pass/fail.
    
    use logging, only: dict, log_error
    
    logical, intent(in)              :: condition ! condition to check
    character(len=*), intent(in)     :: log_filename
    character(len=*), intent(in)     :: message   ! error message to print if `condition` is `.false.`
    integer(kind=I5), intent(in out) :: rc        ! number of errors encountered
    
    type(dict), dimension(:), optional, intent(in) :: dict_log
    
    if (.not. condition) then
        if (present(dict_log)) then
            call log_error(log_filename, message, dict_log=dict_log)
        else
            call log_error(log_filename, message)
        end if
        
        rc = rc + 1_I5
    end if
    
    return
end subroutine check

end module asserts
