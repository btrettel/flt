! # $File$
! 
! Summary: tests for the logging module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_asserts

use prec, only: RP
use logging, only: start_log, dict, LOG_UNIT, log_message, log_error, integer_dict, real_dict, string_dict
use unittest, only: test_type, start_tests, end_tests, logical_test, string_equality_test
implicit none

type(test_type)                       :: test_data
logical                               :: unit_opened, test_passes
type(dict), dimension(:), allocatable :: dict_log
type(dict)                            :: dict_test

character(len=*), parameter :: LOG_FILENAME = "logging.jsonl"

call start_tests(LOG_FILENAME, test_data)
call start_log(LOG_FILENAME)

! This needs to be put here as the `log_message` below will close the `LOG_UNIT` if this is not here.
inquire(unit=LOG_UNIT, opened=unit_opened)
if (unit_opened) then
    close(unit=LOG_UNIT)
end if

call logical_test(.not. unit_opened, "start_log, unit closed after return", test_data)

call log_message(LOG_FILENAME, "log_message test")
call log_error(LOG_FILENAME, "log_error test")

allocate(dict_log(0))

call log_message(LOG_FILENAME, "dict_log(0) test", dict_log=dict_log)

call log_error(LOG_FILENAME, "dict_log(0) test (error)", dict_log=dict_log)

deallocate(dict_log)

allocate(dict_log(1))

call integer_dict("test_integer", 1234, dict_log(1))
call log_message(LOG_FILENAME, "dict_log(1) test", dict_log=dict_log)

call integer_dict("test_integer", 9876, dict_log(1))
call log_error(LOG_FILENAME, "dict_log(1) test (error)", dict_log=dict_log)

deallocate(dict_log)

allocate(dict_log(2))

call integer_dict("test_integer_1", 3456, dict_log(1))
call integer_dict("test_integer_2", 890, dict_log(2))
call log_message(LOG_FILENAME, "dict_log(2) test", dict_log=dict_log)

call integer_dict("test_integer_1", 987, dict_log(1))
call integer_dict("test_integer_2", 5432, dict_log(2))
call log_error(LOG_FILENAME, "dict_log(2) test (error)", dict_log=dict_log)

deallocate(dict_log)

call log_message(LOG_FILENAME, "custom rc test", rc=1234)
call log_error(LOG_FILENAME, "custom rc test (error)", rc=5678)

! dictionaries

call real_dict("test_real", 1.23e-4_RP, dict_test)
call string_equality_test(dict_test%k, "test_real", "dict, real, key", test_data)
call string_equality_test(dict_test%v, "1.230000000000000E-04", "dict, real, value", test_data)

call integer_dict("test_integer", 50, dict_test)
call string_equality_test(dict_test%k, "test_integer", "dict, integer, key", test_data)
call string_equality_test(dict_test%v, "50", "dict, integer, value", test_data)

call string_dict("test_string", "dictionary string", dict_test)
call string_equality_test(dict_test%k, "test_string", "dict, string, key", test_data)

! `string_equality_test` can't be used because the dictionaries automatically quote string, so that would lead to double quoting
! here in the output.
test_passes = (trim(adjustl(dict_test%v)) == '"dictionary string"')
call logical_test(test_passes, "dict, string, value", test_data)

call end_tests(test_data)

end program test_asserts
