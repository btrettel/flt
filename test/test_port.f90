! tests for the port module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_port

use port, only: platform, PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

integer :: min_platform, max_platform

call logger%open("port.nml")
call tests%start_tests(logger)

min_platform = min(PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS)
max_platform = max(PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS)

call tests%integer_ne(platform(), PLATFORM_UNKNOWN, "platform not unknown")
call tests%integer_ge(platform(), PLATFORM_UNKNOWN, "platform lower bound")
call tests%integer_le(platform(), max_platform, "platform upper bound")
call tests%integer_le(min_platform, PLATFORM_UNKNOWN, "unknown platform is the lowest")

call tests%end_tests()
call logger%close()

end program test_port
