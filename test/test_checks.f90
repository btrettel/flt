! tests for the checks module
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_checks

use checks, only: is_close, check
use nmllog, only: log_type
use prec, only: RP
use unittest, only: test_results_type

implicit none

type(log_type)          :: logger
type(test_results_type) :: test_data
integer                 :: rc_check

character(len=*), parameter :: LOG_FILENAME = "checks.nml"

call logger%open(LOG_FILENAME)
call test_data%start_tests(logger)

call test_data%logical_test(is_close(1.0_RP, 1.0_RP), "is_close, identical numbers (1)")

call test_data%logical_test(is_close(15.0_RP, 15.0_RP), "is_close, identical numbers (2)")

call test_data%logical_test(is_close(0.0001_RP, 0.0001_RP), "is_close, identical numbers (3)")

call test_data%logical_test(.not. is_close(1.0_RP, 10.0_RP), "is_close, different numbers (1)")

call test_data%logical_test(.not. is_close(5.0_RP, 1000.0_RP), "is_close, different numbers (2)")

call test_data%logical_test(.not. is_close(0.1_RP, 1000.0_RP), "is_close, different numbers (3)")

call test_data%logical_test(is_close(1.0_RP, 1.0_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (1)")

call test_data%logical_test(is_close(100.0_RP, 100.0_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (2)")

call test_data%logical_test(is_close(0.1_RP, 0.1_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (3)")

call test_data%logical_test(.not. is_close(1.0_RP, 1.0_RP + 20.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (1)")

call test_data%logical_test(.not. is_close(100.0_RP, 100.0_RP + 1000.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (2)")

call test_data%logical_test(.not. is_close(0.1_RP, 0.1_RP + 11.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (3)")

call test_data%logical_test(is_close(0.0_RP, 0.0_RP), "is_close, both zero")

call test_data%logical_test(.not. is_close(0.0_RP, 100.0_RP * epsilon(1.0_RP)), &
    "is_close, one zero, one different (1)")

call test_data%logical_test(.not. is_close(100.0_RP * epsilon(1.0_RP), 0.0_RP), &
    "is_close, one zero, one different (2)")

call test_data%logical_test(is_close(1.0_RP, 1.05_RP, abs_tol=0.1_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (1)")

call test_data%logical_test(is_close(10.0_RP, 10.1_RP, abs_tol=0.2_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (2)")

call test_data%logical_test(is_close(0.1_RP, 0.11_RP, abs_tol=0.02_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (3)")

call test_data%logical_test(.not. is_close(1.0_RP, 1.15_RP, abs_tol=0.1_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (1)")

call test_data%logical_test(.not. is_close(20.0_RP, 21.0_RP, abs_tol=0.5_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (2)")

call test_data%logical_test(.not. is_close(0.01_RP, 0.02_RP, abs_tol=0.005_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (3)")

call test_data%logical_test(is_close(1.0_RP, 1.05_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, inside rel_tol")

call test_data%logical_test(.not. is_close(1.0_RP, 1.15_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (1)")

call test_data%logical_test(.not. is_close(20.0_RP, 19.7_RP, abs_tol=0.0_RP, rel_tol=0.01_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (2)")

call test_data%logical_test(.not. is_close(0.0001_RP, 0.0003_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (3)")

call test_data%logical_test(.not. is_close(1.0_RP, 0.0_RP, abs_tol=1.0_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, just outside")

call test_data%logical_test(.not. is_close(1.0_RP, 0.0_RP, abs_tol=0.0_RP, rel_tol=1.0_RP), &
        "is_close, close numbers with set rel_tol, just outside")

rc_check = 0
call check(.true., logger, "check, .true.", rc_check)
call test_data%integer_equality_test(rc_check, 0, "check, .true.")

rc_check = 0
call check(.false., logger, "check, .false.", rc_check)
call test_data%integer_equality_test(rc_check, 1, "check, .false.")

call test_data%end_tests()
call logger%close()

end program test_checks
