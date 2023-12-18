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

use prec, only: I5, RP, CL
use logging, only: dict
implicit none
private

public :: logical_test
public :: real_equality_test
public :: real_inequality_test
public :: integer_equality_test
public :: string_equality_test
public :: start_tests
public :: end_tests

private :: current_time

character(len=70_I5), public, parameter :: LONG_LINE = "----------------------------------------------------------------------"

type, public :: test_type
    integer(kind=I5)  :: number_of_failures
    integer(kind=I5)  :: number_of_tests
    real(kind=RP)     :: start_time
    real(kind=RP)     :: end_time
    character(len=CL) :: log_filename ! filename to write log to
end type test_type

contains

subroutine logical_test(condition, message, test_data, dict_log)
    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
    use logging, only: CHAR_FMT, dict, log_message, integer_dict, string_dict
    
    logical, intent(in)              :: condition
    character(len=*), intent(in)     :: message
    type(test_type), intent(in out)  :: test_data
    type(dict), intent(in), optional :: dict_log(:)
    
    type(dict), allocatable :: dict_set(:)
    integer(kind=I5)        :: i_dict
    
    if (present(dict_log)) then
        allocate(dict_set(size(dict_log) + 3_I5))
        
        do i_dict = 1_I5, size(dict_log)
            dict_set(i_dict + 3_I5) = dict_log(i_dict)
        end do
    else
        allocate(dict_set(3_I5))
    end if
    
    test_data%number_of_tests = test_data%number_of_tests + 1_I5
    
    call string_dict("type", "test", dict_set(1_I5))
    call integer_dict("cumulative tests", test_data%number_of_tests, dict_set(2_I5))
    
    if (condition) then
        call integer_dict("cumulative failures", test_data%number_of_failures, dict_set(3_I5))
        call log_message(test_data%log_filename, "pass: " // message, dict_log=dict_set)
    else
        test_data%number_of_failures = test_data%number_of_failures + 1_I5
        call integer_dict("cumulative failures", test_data%number_of_failures, dict_set(3_I5))
        call log_message(test_data%log_filename, "fail: " // message, rc=1_I5, dict_log=dict_set)
        
        write(unit=*, fmt=CHAR_FMT) "fail: " // message ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) ! NO COMMENT FMUTATE
    end if
end subroutine logical_test

subroutine real_equality_test(returned_real, expected_real, message, test_data, abs_tol)
    ! Check whether two reals are close, increase `number_of_failures` if `.false.`.
    
    use asserts, only: TOL_FACTOR, check, is_close
    use logging, only: dict, log_message, real_dict
    
    real(kind=RP), intent(in)           :: returned_real, expected_real
    character(len=*), intent(in)        :: message
    type(test_type), intent(in out)     :: test_data
    real(kind=RP), intent(in), optional :: abs_tol
    
    real(kind=RP) :: abs_tol_set, abs_diff
    logical       :: test_passes
    type(dict)    :: dict_log(4_I5)
    
    if (present(abs_tol)) then
        abs_tol_set = abs_tol
    else
        abs_tol_set = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    call real_dict("real returned", returned_real, dict_log(1_I5))
    call real_dict("real expected", expected_real, dict_log(2_I5))
    abs_diff = abs(returned_real - expected_real)
    call real_dict("difference", abs_diff, dict_log(3_I5))
    call real_dict("tolerance", abs_tol_set, dict_log(4_I5))
    
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
    
    call logical_test(test_passes, message, test_data, dict_log=dict_log)
    
    call check(abs_diff >= 0.0_RP, test_data%log_filename, & ! NO FMUTATE
                "real_equality_test, abs_diff < 0", test_data%number_of_failures) ! NO FMUTATE
end subroutine real_equality_test

subroutine real_inequality_test(returned_real, expected_real, message, test_data)
    ! Check whether two reals are not close, increase `number_of_failures` if `.true.`.
    
    use asserts, only: check, is_close
    use logging, only: dict, log_message, real_dict
    
    real(kind=RP), intent(in)       :: returned_real, expected_real
    character(len=*), intent(in)    :: message
    type(test_type), intent(in out) :: test_data
    
    real(kind=RP) :: abs_diff
    logical       :: test_passes
    type(dict)    :: dict_log(3_I5)
    
    call real_dict("real returned", returned_real, dict_log(1_I5))
    call real_dict("real expected", expected_real, dict_log(2_I5))
    abs_diff = abs(returned_real - expected_real)
    call real_dict("difference", abs_diff, dict_log(3_I5))
    !call real_dict("tolerance", abs_tol_set, dict_log(4_I5))
    
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
    
    call logical_test(test_passes, message, test_data, dict_log=dict_log)
    
    call check(abs_diff >= 0.0_RP, test_data%log_filename, & ! NO FMUTATE
                "real_inequality_test, abs_diff < 0", test_data%number_of_failures) ! NO FMUTATE
end subroutine real_inequality_test

subroutine integer_equality_test(actual_integer, expected_integer, message, test_data)
    ! Check whether two integers are identical, increase `number_of_failures` if `.false.`.
    
    use logging, only: dict, log_message, integer_dict
    
    integer(kind=I5), intent(in)    :: actual_integer, expected_integer
    character(len=*), intent(in)    :: message
    type(test_type), intent(in out) :: test_data
    
    logical    :: test_passes
    type(dict) :: dict_log(3_I5)
    
    call integer_dict("integer returned", actual_integer, dict_log(1_I5))
    call integer_dict("integer expected", expected_integer, dict_log(2_I5))
    call integer_dict("difference", abs(actual_integer - expected_integer), dict_log(3_I5))
    
    test_passes = (actual_integer == expected_integer)
    
    if (.not. test_passes) then
        write(unit=*, fmt="(a, i7)") "integer returned = ", actual_integer ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, i7)") "integer expected = ", expected_integer ! NO COMMENT FMUTATE
        write(unit=*, fmt="(a, i7)") "      difference = ", abs(actual_integer - expected_integer) ! NO FMUTATE
    end if
    
    call logical_test(test_passes, message, test_data, dict_log=dict_log)
end subroutine integer_equality_test

subroutine string_equality_test(actual_string, expected_string, message, test_data)
    ! Check whether two strings are identical, increase `number_of_failures` if `.false.`.
    
    use logging, only: CHAR_FMT, dict, log_message, string_dict
    
    character(len=*), intent(in)    :: actual_string, expected_string
    character(len=*), intent(in)    :: message
    type(test_type), intent(in out) :: test_data
    
    logical    :: test_passes
    type(dict) :: dict_log(2_I5)
    
    call string_dict("string returned", actual_string, dict_log(1_I5))
    call string_dict("string expected", expected_string, dict_log(2_I5))
    
    test_passes = (trim(adjustl(actual_string)) == trim(adjustl(expected_string)))
    
    if (.not. test_passes) then
        write(unit=*, fmt=CHAR_FMT) "string returned = " // trim(adjustl(actual_string)) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) "string expected = " // trim(adjustl(expected_string)) ! NO COMMENT FMUTATE
    end if
    
    call logical_test(test_passes, message, test_data, dict_log=dict_log)
end subroutine string_equality_test

function current_time()
    real(kind=RP) :: current_time ! in seconds
    
    integer(kind=I5) :: clock_count, count_rate
    
    call system_clock(clock_count, count_rate)
    current_time = real(clock_count, RP) / real(count_rate, RP) ! NO OPERATOR 1 FMUTATE NO ARG FMUTATE
    
    return
end function current_time

subroutine start_tests(log_filename, test_data)
    character(len=*), intent(in) :: log_filename
    type(test_type), intent(out) :: test_data
    
    test_data%number_of_failures = 0_I5
    test_data%number_of_tests    = 0_I5
    test_data%start_time         = current_time()
    test_data%log_filename       = log_filename
end subroutine start_tests

subroutine end_tests(test_data)
    use prec, only: CL
    use logging, only: CHAR_FMT, dict, log_message, log_error, real_dict, integer_dict
    
    type(test_type), intent(in out) :: test_data
    
    character(len=CL) :: out_string
    real(kind=RP)     :: test_duration ! in seconds
    type(dict)        :: dict_log(3_I5)
    
    test_data%end_time = current_time()
    test_duration      = test_data%end_time - test_data%start_time
    
    call integer_dict("total tests", test_data%number_of_tests, dict_log(1_I5))
    call integer_dict("total failures", test_data%number_of_failures, dict_log(2_I5))
    call real_dict("duration (s)", test_duration, dict_log(3_I5))
    
    write(unit=*, fmt="(a, i3, a, f5.3, a)") "Ran ", test_data%number_of_tests, & ! NO COMMENT FMUTATE
                                                " tests in ", test_duration, "s" ! NO COMMENT FMUTATE
    
    if (test_data%number_of_failures /= 0_I5) then ! NO OPERATOR 1 FMUTATE
        write(unit=out_string, fmt="(a, i3, a)") "FAILED (failures=", test_data%number_of_failures, ")" ! NO COMMENT FMUTATE
        call log_error(test_data%log_filename, trim(adjustl(out_string)), rc=1_I5, dict_log=dict_log) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) LONG_LINE ! NO COMMENT FMUTATE
        stop 1 ! NO COMMENT FMUTATE
    else
        call log_message(test_data%log_filename, "All tests passed.", dict_log=dict_log)
        write(unit=*, fmt=CHAR_FMT) ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) "OK" ! NO COMMENT FMUTATE
        write(unit=*, fmt=CHAR_FMT) LONG_LINE ! NO COMMENT FMUTATE
    end if
end subroutine end_tests

end module unittest
