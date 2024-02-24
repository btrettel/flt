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
use nmllog, only: log_type, now, TIMESTAMP_LEN
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
    procedure :: character_equality_test => character_equality_test
    procedure :: start_tests => start_tests
    procedure :: end_tests => end_tests
end type test_results_type

contains

subroutine logical_test(this, condition, message)
    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
    class(test_results_type), intent(inout) :: this
    
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: message
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=7)             :: variable_type
    logical                      :: test_passes, returned_logical
    
    namelist /test_result/ timestamp, variable_type, test_passes, message, returned_logical
    
    if (.not. condition) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    timestamp        = now()
    variable_type    = "logical"
    test_passes      = condition
    returned_logical = condition
    write(unit=this%logger%unit, nml=test_result)
    
    this%n_tests = this%n_tests + 1
end subroutine logical_test

! Not used yet. Will use later after I figure out a good way to make `returned_logical` not equal to `condition` for this special 
! case in `logical_test`.
!subroutine logical_not_test(this, condition, message)
!    ! Check whether test `condition` is `.true.`, increase `number_of_failures` if `.false.`.
    
!    class(test_results_type), intent(inout) :: this
    
!    logical, intent(in)          :: condition
!    character(len=*), intent(in) :: message
    
!    call logical_test(this, .not. condition, message)
!end subroutine logical_not_test

subroutine real_equality_test(this, returned_real, compared_real, message, abs_tol, ne)
    ! Check whether two reals are close, increase `number_of_failures` if `.false.`.
    
    use checks, only: TOL_FACTOR, check, is_close
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)    :: returned_real, compared_real
    character(len=*), intent(in) :: message
    
    real(kind=RP), intent(in), optional :: abs_tol
    logical, intent(in), optional       :: ne ! `.false.` for checking equality, `.true.` for checking inequality
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=4)             :: variable_type
    character(len=2)             :: test_operator
    logical                      :: test_passes, checking_equality
    real(kind=RP)                :: tolerance, difference
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
                            returned_real, compared_real, tolerance, difference
    
    if (present(abs_tol)) then
        tolerance = abs_tol
    else
        tolerance = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    if (present(ne)) then
        checking_equality = .not. ne
    else
        checking_equality = .true.
    end if
    
    difference = abs(returned_real - compared_real)
    test_passes = is_close(returned_real, compared_real, abs_tol=tolerance)
    
    if (checking_equality) then
        test_operator = "=="
    else
        test_operator = "/="
        test_passes = .not. test_passes
    end if
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, es15.8)") "real returned = ", returned_real
        
        if (checking_equality) then
            write(unit=*, fmt="(a, es15.8)") "real expected = ", compared_real
        else
            write(unit=*, fmt="(a, es15.8)") "      /= real = ", compared_real
        end if
        
        write(unit=*, fmt="(a, es15.8)") "    tolerance = ", tolerance
        if (is_close(abs(compared_real), 0.0_RP)) then
            write(unit=*, fmt="(a, es15.8)") "   difference = ", difference
        else
            write(unit=*, fmt="(a, es15.8, a, f6.3, a)") "   difference = ", difference, &
                                                        " (", 100.0_RP * difference / abs(compared_real), "%)"
        end if
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    call check(difference >= 0.0_RP, this%logger, "real_equality_test, difference < 0", this%n_failures)
    
    timestamp     = now()
    variable_type = "real"
    write(unit=this%logger%unit, nml=test_result)
    
    this%n_tests = this%n_tests + 1
end subroutine real_equality_test

subroutine real_inequality_test(this, returned_real, compared_real, message, abs_tol)
    ! Check whether two reals are not close, increase `number_of_failures` if `.true.`.
    
    use checks, only: TOL_FACTOR
    
    class(test_results_type), intent(inout) :: this
    
    real(kind=RP), intent(in)    :: returned_real, compared_real
    character(len=*), intent(in) :: message
    
    real(kind=RP), intent(in), optional :: abs_tol
    
    real(kind=RP) :: tolerance
    
    if (present(abs_tol)) then
        tolerance = abs_tol
    else
        tolerance = TOL_FACTOR * epsilon(1.0_RP)
    end if
    
    call real_equality_test(this, returned_real, compared_real, message, abs_tol=tolerance, ne=.true.)
end subroutine real_inequality_test

subroutine integer_equality_test(this, returned_integer, compared_integer, message)
    ! Check whether two integers are identical, increase `number_of_failures` if `.false.`.
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=7)             :: variable_type
    character(len=2)             :: test_operator
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    test_passes = (returned_integer == compared_integer)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, i7)") "integer returned = ", returned_integer
        write(unit=*, fmt="(a, i7)") "integer expected = ", compared_integer
        write(unit=*, fmt="(a, i7)") "      difference = ", abs(returned_integer - compared_integer)
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    
    timestamp     = now()
    variable_type = "integer"
    test_operator = "=="
    write(unit=this%logger%unit, nml=test_result)
    
    this%n_tests = this%n_tests + 1
end subroutine integer_equality_test

subroutine integer_greater_equal_test(this, returned_integer, compared_integer, message)
    ! Check whether `returned_integer` is greater than `compared_integer`, increase `number_of_failures` if `.false.`.
    
    class(test_results_type), intent(inout) :: this
    
    integer, intent(in)          :: returned_integer, compared_integer
    character(len=*), intent(in) :: message
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=7)             :: variable_type
    character(len=2)             :: test_operator
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_integer, compared_integer
    
    test_passes = (returned_integer >= compared_integer)
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a, i7)") "integer returned = ", returned_integer
        write(unit=*, fmt="(a, i7)") "  not >= integer = ", compared_integer
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    
    timestamp     = now()
    variable_type = "integer"
    test_operator = ">="
    write(unit=this%logger%unit, nml=test_result)
    
    this%n_tests = this%n_tests + 1
end subroutine integer_greater_equal_test

subroutine character_equality_test(this, returned_string, compared_string, message)
    ! Check whether two character variables are identical, increase `number_of_failures` if `.false.`.
    
    class(test_results_type), intent(inout) :: this
    
    character(len=*), intent(in) :: returned_string, compared_string
    character(len=*), intent(in) :: message
    
    logical :: test_passes
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=9)             :: variable_type
    character(len=2)             :: test_operator
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, returned_string, compared_string
    
    test_passes = (trim(adjustl(returned_string)) == trim(adjustl(compared_string)))
    
    if (.not. test_passes) then
        this%n_failures = this%n_failures + 1
        write(unit=*, fmt="(a)") "string returned = " // trim(adjustl(returned_string))
        write(unit=*, fmt="(a)") "string expected = " // trim(adjustl(compared_string))
        write(unit=*, fmt="(a, a)") "fail: ", message
        write(unit=*, fmt="(a)")
    end if
    
    timestamp     = now()
    variable_type = "character"
    test_operator = "=="
    write(unit=this%logger%unit, nml=test_result)
    
    this%n_tests = this%n_tests + 1
end subroutine character_equality_test

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
    
    namelist /tests_summary/ n_tests, n_failures, duration
    
    this%end_time = current_time()
    duration      = this%end_time - this%start_time
    n_tests       = this%n_tests
    n_failures    = this%n_failures
    write(unit=this%logger%unit, nml=tests_summary)
    
    write(unit=*, fmt="(a, i0, a, f5.3, a)") "Ran ", this%n_tests, " tests in ", duration, "s"
    
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
!    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
!                            returned_logical,
!                            returned_real, compared_real, tolerance, difference, &
!                            returned_integer, compared_integer, &
!                            returned_string, compared_string
!end subroutine result_reader

end module unittest
