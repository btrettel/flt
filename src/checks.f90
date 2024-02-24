! Module for procedures used for run-time checks.
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module checks

use prec, only: RP
implicit none
private

real(kind=RP), public, parameter :: TOL_FACTOR = 10.0_RP

public :: is_close
public :: check

contains

pure function is_close(input_real_1, input_real_2, rel_tol, abs_tol)
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
end function is_close

subroutine check(condition, logger, message, rc)
    ! If `condition` is `.false.`, then print a message and increment `rc`.
    ! If `(rc /= RC_SUCCESS)` later, computation will stop.
    ! This is used for assertions and input validation.
    ! Making `rc` increment is useful to string up multiple `check`s
    ! without adding too much logic. The details of the `check` are
    ! logged, so there's `rc` does not need to be meaningful beyond
    ! pass/fail.
    
    use nmllog, only: log_type
    
    logical, intent(in)          :: condition ! condition to check
    type(log_type), intent(in)   :: logger
    character(len=*), intent(in) :: message   ! error message to print if `condition` is `.false.`
    integer, intent(inout)       :: rc        ! number of errors encountered
    
    if (.not. condition) then
        call logger%error(message)
        
        rc = rc + 1
    end if
end subroutine check

! TODO: function to check whether arrays have identical sizes
! <https://fortran-lang.discourse.group/t/add-a-conform-function-to-check-that-argument-dimensions-match/1018>

end module checks
