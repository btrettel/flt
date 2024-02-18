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
use logging, only: dict
implicit none
private

public :: logical_test
public :: real_equality_test
public :: real_inequality_test
public :: integer_equality_test
public :: integer_greater_equal_test
public :: string_equality_test
public :: start_tests
public :: end_tests

private :: current_time

character(len=70), private, parameter :: LONG_LINE = "----------------------------------------------------------------------"

type, public :: test_results_type
    integer           :: number_of_failures
    integer           :: number_of_tests
    real(kind=RP)     :: start_time
    real(kind=RP)     :: end_time
    character(len=CL) :: log_filename
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

subroutine logical_test(this, condition, message, dict_log)
    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
    use logging, only: CHAR_FMT, dict, log_message, integer_dict, string_dict
    
    class(test_results_type), intent(inout) :: this
    
    logical, intent(in)              :: condition
    character(len=*), intent(in)     :: message
    type(dict), intent(in), optional :: dict_log(:)
    
    type(dict), allocatable :: dict_set(:)
    integer        :: i_dict
    
    if (present(dict_log)) then
        allocate(dict_set(size(dict_log) + 3))
        
        do i_dict = 1, size(dict_log)
            dict_set(i_dict + 3) = dict_log(i_dict)
        end do
    else
        allocate(dict_set(3))
    end if
    
    this%number_of_tests = this%number_of_tests + 1
    
    call string_dict("type", "test", dict_set(1))
    call integer_dict("cumulative tests", this%number_of_tests, dict_set(2))
    
    if (condition) then
        call integer_dict("cumulative failures", this%number_of_failures, dict_set(3))
        call log_message(this%log_filename, "pass: " // message, dict_log=dict_set)
    else
        this%number_of_failures = this%number_of_failures + 1
        call integer_dict("cumulative failures", this%number_of_failures, dict_set(3))
        call log_message(this%log_filename, "fail: " // message, rc=1, dict_log=dict_set)
        
        write(unit=*, fmt=CHAR_FMT) "fail: " // message ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) ! NO COMMENT FMUTATE
    end if
end subroutine logical_test

subroutine real_equality_test(this, returned_real, expected_real, message, abs_tol)
    ! Check whether two reals are close, increase `number_of_failures` if `.false.`.
    
    use asserts, only: TOL_FACTOR, check, is_close
    use logging, only: dict, log_message, real_dict
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)           :: returned_real, expected_real
    character(len=*), intent(in)        :: message
    real(kind=RP), intent(in), optional :: abs_tol
    
    real(kind=RP) :: abs_tol_set, abs_diff
    logical       :: test_passes
    type(dict)    :: dict_log(4)
    
    if (present(abs_tol)) then
        abs_tol_set = abs_tol
    else
        abs_tol_set = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    call real_dict("real returned", returned_real, dict_log(1))
    call real_dict("real expected", expected_real, dict_log(2))
    abs_diff = abs(returned_real - expected_real)
    call real_dict("difference", abs_diff, dict_log(3))
    call real_dict("tolerance", abs_tol_set, dict_log(4))
    
    test_passes = is_close(returned_real, expected_real, abs_tol=abs_tol_set)
    
    if (.not. test_passes) then ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, es15.8)") "real returned = ", returned_real ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, es15.8)") "real expected = ", expected_real ! NO COMMENT FMUTATE
        if (is_close(abs(expected_real), 0.0_RP)) then ! NO ARG FMUTATE
            write(unit=*, fmt="(a, es15.8)") "   difference = ", abs_diff ! NO COMMENT FMUTATE
        else ! NO COMMENT FMUTATE
            write(unit=*, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", abs_diff, & ! NO COMMENT FMUTATE
                                                        " (", 100.0_RP * abs_diff / abs(expected_real), "%)" ! NO FMUTATE
        end if ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, es15.8)") "    tolerance = ", abs_tol_set ! NO COMMENT FMUTATE
    end if ! NO COMMENT FMUTATE
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    call check(abs_diff >= 0.0_RP, this%log_filename, & ! NO FMUTATE
                "real_equality_test, abs_diff < 0", this%number_of_failures) ! NO FMUTATE
end subroutine real_equality_test

subroutine real_inequality_test(this, returned_real, expected_real, message)
    ! Check whether two reals are not close, increase `number_of_failures` if `.true.`.
    
    use asserts, only: check, is_close
    use logging, only: dict, log_message, real_dict
    
    real(kind=RP), intent(in)       :: returned_real, expected_real
    character(len=*), intent(in)    :: message
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP) :: abs_diff
    logical       :: test_passes
    type(dict)    :: dict_log(3)
    
    call real_dict("real returned", returned_real, dict_log(1))
    call real_dict("real expected", expected_real, dict_log(2))
    abs_diff = abs(returned_real - expected_real)
    call real_dict("difference", abs_diff, dict_log(3))
    !call real_dict("tolerance", abs_tol_set, dict_log(4))
    
    test_passes = .not. is_close(returned_real, expected_real)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, es15.8)") "real returned = ", returned_real ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, es15.8)") "real expected = ", expected_real ! NO COMMENT FMUTATE
        if (is_close(abs(expected_real), 0.0_RP)) then ! NO ARG FMUTATE
            write(unit=*, fmt="(a, es15.8)") "   difference = ", abs_diff ! NO COMMENT FMUTATE
        else ! NO COMMENT FMUTATE
            write(unit=*, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", abs_diff, & ! NO COMMENT FMUTATE
                                                        " (", 100.0_RP * abs_diff / abs(expected_real), "%)" ! NO FMUTATE
        end if
        !write(unit=*, fmt="(a, es15.8)") "    tolerance = ", abs_tol_set
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
    
    call check(abs_diff >= 0.0_RP, this%log_filename, & ! NO FMUTATE
                "real_inequality_test, abs_diff < 0", this%number_of_failures) ! NO FMUTATE
end subroutine real_inequality_test

subroutine integer_equality_test(this, actual_integer, expected_integer, message)
    ! Check whether two integers are identical, increase `number_of_failures` if `.false.`.
    
    use logging, only: dict, log_message, integer_dict
    
    integer, intent(in)             :: actual_integer, expected_integer
    character(len=*), intent(in)    :: message
    
    class(test_results_type), intent(inout) :: this
    
    logical    :: test_passes
    type(dict) :: dict_log(3)
    
    call integer_dict("integer returned", actual_integer, dict_log(1))
    call integer_dict("integer expected", expected_integer, dict_log(2))
    call integer_dict("difference", abs(actual_integer - expected_integer), dict_log(3))
    
    test_passes = (actual_integer == expected_integer)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, i7)") "integer returned = ", actual_integer ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, i7)") "integer expected = ", expected_integer ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, i7)") "      difference = ", abs(actual_integer - expected_integer) ! NO FMUTATE
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
end subroutine integer_equality_test

subroutine integer_greater_equal_test(this, test_integer, lower_integer, message)
    ! Check whether one integer is greater than the other, increase `number_of_failures` if `.false.`.
    
    use logging, only: dict, log_message, integer_dict
    
    integer, intent(in)             :: test_integer, lower_integer
    character(len=*), intent(in)    :: message
    
    class(test_results_type), intent(inout) :: this
    
    logical    :: test_passes
    type(dict) :: dict_log(2)
    
    call integer_dict("integer returned", test_integer, dict_log(1))
    call integer_dict("lower integer", lower_integer, dict_log(2))
    
    test_passes = (test_integer >= lower_integer)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, i7)") "integer returned = ", test_integer ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, i7)") "  not >= integer = ", lower_integer ! NO COMMENT FMUTATE
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
end subroutine integer_greater_equal_test

subroutine string_equality_test(this, actual_string, expected_string, message)
    ! Check whether two strings are identical, increase `number_of_failures` if `.false.`.
    
    use logging, only: CHAR_FMT, dict, log_message, string_dict
    
    class(test_results_type), intent(inout) :: this
    
    character(len=*), intent(in) :: actual_string, expected_string
    character(len=*), intent(in) :: message
    
    logical    :: test_passes
    type(dict) :: dict_log(2)
    
    call string_dict("string returned", actual_string, dict_log(1))
    call string_dict("string expected", expected_string, dict_log(2))
    
    test_passes = (trim(adjustl(actual_string)) == trim(adjustl(expected_string)))
    
    if (.not. test_passes) then
        write(unit=*, fmt=CHAR_FMT) "string returned = " // trim(adjustl(actual_string)) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) "string expected = " // trim(adjustl(expected_string)) ! NO COMMENT FMUTATE
    end if
    
    call this%logical_test(test_passes, message, dict_log=dict_log)
end subroutine string_equality_test

function current_time()
    real(kind=RP) :: current_time ! in seconds
    
    integer :: clock_count, count_rate
    
    call system_clock(clock_count, count_rate)
    current_time = real(clock_count, RP) / real(count_rate, RP) ! NO OPERATOR 1 FMUTATE NO ARG FMUTATE
    
    return
end function current_time

subroutine start_tests(this, log_filename)
    class(test_results_type), intent(out) :: this
    
    character(len=*), intent(in) :: log_filename
    
    this%number_of_failures = 0
    this%number_of_tests    = 0
    this%start_time         = current_time()
    this%log_filename       = log_filename
end subroutine start_tests

subroutine end_tests(this)
    use prec, only: CL
    use logging, only: CHAR_FMT, dict, log_message, log_error, real_dict, integer_dict
    
    class(test_results_type), intent(inout) :: this
    
    character(len=CL) :: out_string
    real(kind=RP)     :: test_duration ! in seconds
    type(dict)        :: dict_log(3)
    
    this%end_time = current_time()
    test_duration      = this%end_time - this%start_time
    
    call integer_dict("total tests", this%number_of_tests, dict_log(1))
    call integer_dict("total failures", this%number_of_failures, dict_log(2))
    call real_dict("duration (s)", test_duration, dict_log(3))
    
    write(unit=*, fmt="(a, i3, a, f5.3, a)") "Ran ", this%number_of_tests, & ! NO COMMENT FMUTATE
                                                " tests in ", test_duration, "s" ! NO COMMENT FMUTATE
    
    if (this%number_of_failures /= 0) then ! NO OPERATOR 1 FMUTATE
        write(unit=out_string, fmt="(a, i3, a)") "FAILED (failures=", this%number_of_failures, ")" ! NO COMMENT FMUTATE
        call log_error(this%log_filename, trim(adjustl(out_string)), rc=1, dict_log=dict_log) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) LONG_LINE ! NO COMMENT FMUTATE
        stop 1 ! NO COMMENT FMUTATE
    else
        call log_message(this%log_filename, "All tests passed.", dict_log=dict_log)
        write(unit=*, fmt=CHAR_FMT) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) "OK" ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) LONG_LINE ! NO COMMENT FMUTATE
    end if
end subroutine end_tests

end module unittest
