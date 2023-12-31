! # $File$
! 
! Summary: tests for the checks module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_asserts

use asserts, only: is_close, check
use logging, only: start_log, dict, integer_dict
use prec, only: RP
use unittest, only: start_tests, end_tests, test_type, &
                        logical_test, integer_equality_test
                    
implicit none

type(test_type) :: test_data
integer         :: rc_check
type(dict), dimension(:), allocatable :: dict_log

character(len=*), parameter :: LOG_FILENAME = "checks.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

call logical_test(is_close(1.0_RP, 1.0_RP), "is_close, identical numbers (1)", test_data)

call logical_test(is_close(15.0_RP, 15.0_RP), "is_close, identical numbers (2)", test_data)

call logical_test(is_close(0.0001_RP, 0.0001_RP), "is_close, identical numbers (3)", test_data)

call logical_test(.not. is_close(1.0_RP, 10.0_RP), "is_close, different numbers (1)", test_data)

call logical_test(.not. is_close(5.0_RP, 1000.0_RP), "is_close, different numbers (2)", test_data)

call logical_test(.not. is_close(0.1_RP, 1000.0_RP), "is_close, different numbers (3)", test_data)

call logical_test(is_close(1.0_RP, 1.0_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (1)", test_data)

call logical_test(is_close(100.0_RP, 100.0_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (2)", test_data)

call logical_test(is_close(0.1_RP, 0.1_RP + 5.0_RP * epsilon(1.0_RP)), &
    "is_close, different numbers within tolerance (3)", test_data)

call logical_test(.not. is_close(1.0_RP, 1.0_RP + 20.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (1)", test_data)

call logical_test(.not. is_close(100.0_RP, 100.0_RP + 1000.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (2)", test_data)

call logical_test(.not. is_close(0.1_RP, 0.1_RP + 11.0_RP * epsilon(1.0_RP)), &
    "is_close, barely different numbers (3)", test_data)

call logical_test(is_close(0.0_RP, 0.0_RP), "is_close, both zero", test_data)

call logical_test(.not. is_close(0.0_RP, 100.0_RP * epsilon(1.0_RP)), &
    "is_close, one zero, one different (1)", test_data)

call logical_test(.not. is_close(100.0_RP * epsilon(1.0_RP), 0.0_RP), &
    "is_close, one zero, one different (2)", test_data)

call logical_test(is_close(1.0_RP, 1.05_RP, abs_tol=0.1_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (1)", test_data)

call logical_test(is_close(10.0_RP, 10.1_RP, abs_tol=0.2_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (2)", test_data)

call logical_test(is_close(0.1_RP, 0.11_RP, abs_tol=0.02_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, inside abs_tol (3)", test_data)

call logical_test(.not. is_close(1.0_RP, 1.15_RP, abs_tol=0.1_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (1)", test_data)

call logical_test(.not. is_close(20.0_RP, 21.0_RP, abs_tol=0.5_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (2)", test_data)

call logical_test(.not. is_close(0.01_RP, 0.02_RP, abs_tol=0.005_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, outside abs_tol (3)", test_data)

call logical_test(is_close(1.0_RP, 1.05_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, inside rel_tol", test_data)

call logical_test(.not. is_close(1.0_RP, 1.15_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (1)", test_data)

call logical_test(.not. is_close(20.0_RP, 19.7_RP, abs_tol=0.0_RP, rel_tol=0.01_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (2)", test_data)

call logical_test(.not. is_close(0.0001_RP, 0.0003_RP, abs_tol=0.0_RP, rel_tol=0.1_RP), &
        "is_close, close numbers with set rel_tol, outside rel_tol (3)", test_data)

call logical_test(.not. is_close(1.0_RP, 0.0_RP, abs_tol=1.0_RP, rel_tol=0.0_RP), &
        "is_close, close numbers with set abs_tol, just outside", test_data)

call logical_test(.not. is_close(1.0_RP, 0.0_RP, abs_tol=0.0_RP, rel_tol=1.0_RP), &
        "is_close, close numbers with set rel_tol, just outside", test_data)

rc_check = 0
call check(.true., LOG_FILENAME, "check, .true.", rc_check)
call integer_equality_test(rc_check, 0, "check, .true.", test_data)

rc_check = 0
call check(.false., LOG_FILENAME, "check, .false.", rc_check)
call integer_equality_test(rc_check, 1, "check, .false.", test_data)

rc_check = 0
allocate(dict_log(1))
call integer_dict("check_integer", 67890, dict_log(1))
call check(.false., LOG_FILENAME, "check, .false., dict_log", rc_check, dict_log=dict_log)
deallocate(dict_log)

call end_tests(test_data)

end program test_asserts
