! Module for procedures used for run-time checks.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module checks

use prec, only: WP
implicit none
private

real(kind=WP), public, parameter :: TOL_FACTOR = 5.0_WP

logical :: debug = .true.

public :: abs_tolerance
public :: is_close
public :: assert
public :: same_shape_as

! TODO: `all_close` like <https://numpy.org/doc/stable/reference/generated/numpy.allclose.html>

interface same_shape_as
    module procedure same_shape_as_real_rank_1
    module procedure same_shape_as_real_rank_2
    module procedure same_shape_as_real_rank_3
end interface same_shape_as

contains

pure function abs_tolerance(input_real_1, input_real_2)
    ! <https://community.intel.com/t5/Intel-Fortran-Compiler/Real-number-tolerance-and-if/m-p/824459#M49399>
    ! <https://fortran-lang.discourse.group/t/suggestion-findloc-tolerance/5131/5>
    ! <https://community.intel.com/t5/Intel-Fortran-Compiler/Floating-point-equivalence-check/m-p/1087149>
    
    real(kind=WP), intent(in) :: input_real_1, input_real_2
    
    real(kind=WP) :: abs_tolerance
    
    abs_tolerance = TOL_FACTOR * spacing(max(abs(input_real_1), abs(input_real_2)))
end function abs_tolerance

pure function is_close(input_real_1, input_real_2, rel_tol, abs_tol)
    ! Determine whether two reals are close.
    
    ! Interface based on stdlib, though implementation is different.
    ! <https://stdlib.fortran-lang.org/page/specs/stdlib_math.html#is_close-function>
    
    ! Also see:
    ! <https://numpy.org/doc/stable/reference/generated/numpy.isclose.html>
    ! <https://docs.python.org/3/library/math.html#math.isclose>
    
    ! TODO: Make array versions like in numpy.
    
    real(kind=WP), intent(in)           :: input_real_1, input_real_2
    real(kind=WP), intent(in), optional :: rel_tol, abs_tol
    
    real(kind=WP) :: rel_tol_, abs_tol_, tol
    
    logical :: is_close
    
    if (present(rel_tol)) then
        rel_tol_ = rel_tol
    else
        rel_tol_ = 0.0_WP
    end if
    
    if (present(abs_tol)) then
        abs_tol_ = abs_tol
    else
        abs_tol_ = abs_tolerance(input_real_1, input_real_2)
    end if
    
    tol = max(rel_tol_ * abs(input_real_1), rel_tol_ * abs(input_real_2), abs_tol_)
    
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
        message_ = " " // message
    else
        message_ = ""
    end if
    
    if (debug) then
        if (.not. condition) then
            ! Why not concatenate the strings on the `error stop` line?
            ! That leads to ifx garbling the error message as of version `ifx (IFX) 2024.0.2 20231213`.
            full_message = "***" // new_line("a") // "ASSERTION FAILED." // message_
            
            ! Why is the message in all caps? To make it more noticeable.
            
            error stop full_message
        end if
    end if
end subroutine assert

pure function same_shape_as_real_rank_1(a, b)
    real(kind=WP), intent(in) :: a(:), b(:)
    
    logical :: same_shape_as_real_rank_1
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_1 = .false.
    else
        same_shape_as_real_rank_1 = .true.
    end if
end function same_shape_as_real_rank_1

pure function same_shape_as_real_rank_2(a, b)
    real(kind=WP), intent(in) :: a(:, :), b(:, :)
    
    logical :: same_shape_as_real_rank_2
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_2 = .false.
    else
        same_shape_as_real_rank_2 = .true.
    end if
end function same_shape_as_real_rank_2

pure function same_shape_as_real_rank_3(a, b)
    real(kind=WP), intent(in) :: a(:, :, :), b(:, :, :)
    
    logical :: same_shape_as_real_rank_3
    
    if (any(shape(a) /= shape(b))) then
        same_shape_as_real_rank_3 = .false.
    else
        same_shape_as_real_rank_3 = .true.
    end if
end function same_shape_as_real_rank_3

end module checks
