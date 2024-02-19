! # $File$
! 
! Summary: Module for unit testing procedures.
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module unittest

use prec, only: RP, CL
use nmllog, only: log_type
implicit none
private

character(len=70), parameter :: LONG_LINE = "----------------------------------------------------------------------"

type, public :: test_results_type
    integer        :: n_failures = 0
    integer        :: n_tests    = 0
    real(kind=RP)  :: start_time
    real(kind=RP)  :: end_time
    type(log_type) :: logger
contains
    procedure :: logical_test => logical_test
    procedure :: real_equality_test => real_equality_test
    procedure :: real_inequality_test => real_inequality_test
    procedure :: integer_equality_test => integer_equality_test
    procedure :: integer_greater_equal_test => integer_greater_equal_test
    procedure :: string_equality_test => string_equality_test
    procedure :: start_tests => start_tests
    procedure :: end_tests => end_tests
end type test_results_type

contains

subroutine logical_test(this, condition, message)
    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
    class(test_results_type), intent(inout) :: this
    
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: message
    
    character(len=*), parameter :: variable_type = "logical"
    
    namelist /test_result/ variable_type, test_passes, message, returned_logical
    
    this%n_tests = this%n_tests + 1
    
    if (.not. condition) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    test_passes      = condition
    returned_logical = condition
    write(unit=this%logger%unit, nml=test_result)
end subroutine logical_test

subroutine real_equality_test(this, returned_real, expected_real, message, abs_tol)
    ! Check whether two reals are close, increase `number_of_failures` if `.false.`.
    
    use asserts, only: TOL_FACTOR, check, is_close
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)           :: returned_real, expected_real
    character(len=*), intent(in)        :: message
    real(kind=RP), intent(in), optional :: abs_tol
    
    character(len=*), parameter :: variable_type = "real"
    logical       :: test_passes
    real(kind=RP) :: tolerance, difference
    
    namelist /test_result/ variable_type, test_operator, test_passes, message, returned_real, expected_real, tolerance, difference
    
    if (present(abs_tol)) then
        tolerance = abs_tol
    else
        tolerance = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    test_passes = is_close(returned_real, expected_real, abs_tol=tolerance)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, es15.8)") "real returned = ", returned_real
        write(unit=*, fmt="(a, es15.8)") "real expected = ", expected_real
        write(unit=*, fmt="(a, es15.8)") "    tolerance = ", tolerance
        if (is_close(abs(expected_real), 0.0_RP)) then
            write(unit=*, fmt="(a, es15.8)") "   difference = ", difference
        else
            write(unit=*, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", difference, &
                                                        " (", 100.0_RP * difference / abs(expected_real), "%)"
        end if
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    call check(abs_diff >= 0.0_RP, this%logger, "real_equality_test, abs_diff < 0", this%n_failures)
    
    test_operator = "=="
    write(unit=this%logger%unit, nml=test_result)
end subroutine real_equality_test

subroutine real_inequality_test(this, returned_real, expected_real, message)
    ! Check whether two reals are not close, increase `number_of_failures` if `.true.`.
    
    ! TODO: START HERE
    
    use asserts, only: check, is_close
    
    real(kind=RP), intent(in)    :: returned_real, expected_real
    character(len=*), intent(in) :: message
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP) :: abs_diff
    logical       :: test_passes
    
    call real_dict("real returned", returned_real, dict_log(1))
    call real_dict("real expected", expected_real, dict_log(2))
    abs_diff = abs(returned_real - expected_real)
    call real_dict("difference", abs_diff, dict_log(3))
    !call real_dict("tolerance", abs_tol_set, dict_log(4))
    
    test_passes = .not. is_close(returned_real, expected_real)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, es15.8)") "real returned = ", returned_real
        write(unit=*, fmt="(a, es15.8)") "real expected = ", expected_real
        if (is_close(abs(expected_real), 0.0_RP)) then ! NO ARG FMUTATE
            write(unit=*, fmt="(a, es15.8)") "   difference = ", abs_diff
        else
            write(unit=*, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", abs_diff, &
                                                        " (", 100.0_RP * abs_diff / abs(expected_real), "%)"
        end if
        !write(unit=*, fmt="(a, es15.8)") "    tolerance = ", abs_tol_set
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    call check(abs_diff >= 0.0_RP, this%log_filename, "real_inequality_test, abs_diff < 0", this%n_failures)
    
    write(unit=this%logger%unit, nml=test_result)
end subroutine real_inequality_test

subroutine integer_equality_test(this, returned_integer, expected_integer, message)
    ! Check whether two integers are identical, increase `number_of_failures` if `.false.`.
    
    integer, intent(in)          :: returned_integer, expected_integer
    character(len=*), intent(in) :: message
    
    class(test_results_type), intent(inout) :: this
    
    logical :: test_passes
    
    call integer_dict("integer returned", returned_integer, dict_log(1))
    call integer_dict("integer expected", expected_integer, dict_log(2))
    call integer_dict("difference", abs(returned_integer - expected_integer), dict_log(3))
    
    test_passes = (returned_integer == expected_integer)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, i7)") "integer returned = ", returned_integer
        write(unit=*, fmt="(a, i7)") "integer expected = ", expected_integer
        write(unit=*, fmt="(a, i7)") "      difference = ", abs(returned_integer - expected_integer)
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    write(unit=this%logger%unit, nml=test_result)
end subroutine integer_equality_test

subroutine integer_greater_equal_test(this, test_integer, lower_integer, message)
    ! Check whether one integer is greater than the other, increase `number_of_failures` if `.false.`.
    
    integer, intent(in)          :: test_integer, lower_integer
    character(len=*), intent(in) :: message
    
    class(test_results_type), intent(inout) :: this
    
    logical :: test_passes
    
    call integer_dict("integer returned", test_integer, dict_log(1))
    call integer_dict("lower integer", lower_integer, dict_log(2))
    
    test_passes = (test_integer >= lower_integer)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, i7)") "integer returned = ", test_integer
        write(unit=*, fmt="(a, i7)") "  not >= integer = ", lower_integer
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    write(unit=this%logger%unit, nml=test_result)
end subroutine integer_greater_equal_test

subroutine string_equality_test(this, returned_string, expected_string, message)
    ! Check whether two strings are identical, increase `number_of_failures` if `.false.`.
    
    use logging, only: "(a)", dict, log_message, string_dict
    
    class(test_results_type), intent(inout) :: this
    
    character(len=*), intent(in) :: returned_string, expected_string
    character(len=*), intent(in) :: message
    
    logical    :: test_passes
    type(dict) :: dict_log(2)
    
    call string_dict("string returned", returned_string, dict_log(1))
    call string_dict("string expected", expected_string, dict_log(2))
    
    test_passes = (trim(adjustl(returned_string)) == trim(adjustl(expected_string)))
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a)") "string returned = " // trim(adjustl(returned_string))
        write(unit=*, fmt="(a)") "string expected = " // trim(adjustl(expected_string))
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    write(unit=this%logger%unit, nml=test_result)
end subroutine string_equality_test

function current_time()
    real(kind=RP) :: current_time ! in seconds
    
    integer :: clock_count, count_rate
    
    call system_clock(clock_count, count_rate)
    current_time = real(clock_count, RP) / real(count_rate, RP)
    
    return
end function current_time

subroutine start_tests(this, logger)
    class(test_results_type), intent(out) :: this
    
    type(log_type), intent(in) :: logger
    
    this%start_time = current_time()
    this%logger     = logger
end subroutine start_tests

subroutine end_tests(this)
    class(test_results_type), intent(inout) :: this
    
    integer       :: n_tests, n_failures
    real(kind=RP) :: duration ! in seconds
    
    namelist /tests_end/ n_tests, n_failures, duration
    
    this%end_time = current_time()
    duration      = this%end_time - this%start_time
    n_tests       = this%n_tests
    n_failures    = this%n_failures
    write(unit=this%unit, nml=tests_end)
    
    write(unit=*, fmt="(a, i0, a, f5.3, a)") "Ran ", this%n_tests, " tests in ", test_duration, "s"
    
    if (this%n_failures /= 0) then
        write(unit=*, fmt="(a, i0, a)") "FAILED (failures=", this%n_failures, ")"
        write(unit=*, fmt="(a)") LONG_LINE
        stop 1
    else
        write(unit=*, fmt="(a)") "All tests passed."
        write(unit=*, fmt="(a)")
        write(unit=*, fmt="(a)") "OK"
        write(unit=*, fmt="(a)") LONG_LINE
    end if
end subroutine end_tests

!subroutine result_reader(filename)
!    namelist /test_result/ variable_type, test_operator, test_passes, message, &
!                            returned_logical, &
!                            returned_real, expected_real, tolerance. &
!                            returned_integer, expected_integer, &
!                            returned_string, expected_string
!end subroutine result_reader

end module unittest
