! Module for procedures used for run-time checks.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module checks

use prec, only: RP
implicit none
private

real(kind=RP), public, parameter :: TOL_FACTOR = 10.0_RP

logical :: debug = .true.

public :: is_close ! TODO: make rel_tol zero by default, abs_tol based on local spacing
public :: assert ! TODO: test
public :: check
public :: same_shape_as ! TODO: test

interface same_shape_as
    module procedure same_shape_as_real_rank_1
    module procedure same_shape_as_real_rank_2
    module procedure same_shape_as_real_rank_3
end interface same_shape_as

contains

pure function is_close(input_real_1, input_real_2, rel_tol, abs_tol)
    ! Determine whether two reals are close.
    
    real(kind=RP), intent(in)           :: input_real_1, input_real_2
    real(kind=RP), intent(in), optional :: rel_tol, abs_tol
    
    real(kind=RP) :: rel_tol_set, abs_tol_set, tol
    logical       :: is_close
    
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
    
    tol = max(rel_tol_set * abs(input_real_1), rel_tol_set * abs(input_real_2), abs_tol_set)
    
    if (abs(input_real_1 - input_real_2) < tol) then
        is_close = .true.
    else
        is_close = .false.
    end if
end function is_close

pure subroutine assert(condition, message)
    logical, intent(in) :: condition
    
    character(len=*), intent(in), optional :: message
    
    character(len=:), allocatable :: message_, full_message
    
    if (present(message)) then
        message_ = new_line("a") // message
    else
        message_ = ""
    end if
    
    if (debug) then
        if (.not. condition) then
            ! Why not concatenate the strings on the `error stop` line?
            ! That leads to ifx garbling the error message as of version `ifx (IFX) 2024.0.2 20231213`.
            full_message = "***" // new_line("a") // "Assertion failed." // message_
            
            error stop full_message
        end if
    end if
end subroutine assert

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

pure function same_shape_as_real_rank_1(a, b)
    real(kind=RP), intent(in) :: a(:), b(:)
    
    logical :: same_shape_as_real_rank_1
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_1 = .false.
    else
        same_shape_as_real_rank_1 = .true.
    end if
end function same_shape_as_real_rank_1

pure function same_shape_as_real_rank_2(a, b)
    real(kind=RP), intent(in) :: a(:, :), b(:, :)
    
    logical :: same_shape_as_real_rank_2
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_2 = .false.
    else
        same_shape_as_real_rank_2 = .true.
    end if
end function same_shape_as_real_rank_2

pure function same_shape_as_real_rank_3(a, b)
    real(kind=RP), intent(in) :: a(:, :, :), b(:, :, :)
    
    logical :: same_shape_as_real_rank_3
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_3 = .false.
    else
        same_shape_as_real_rank_3 = .true.
    end if
end function same_shape_as_real_rank_3

end module checks
