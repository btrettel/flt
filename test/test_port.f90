! tests for the port module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_port

use port, only: PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS, DIR_SEPS, platform, path_join, convert_path_unix_to_win
use prec, only: CL
use unittest, only: test_results_type
implicit none

type(test_results_type) :: tests
character(len=CL)       :: path, path_array(2)

integer :: min_platform, max_platform

call tests%start_tests("port.nml")

min_platform = min(PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS)
max_platform = max(PLATFORM_UNKNOWN, PLATFORM_UNIXLIKE, PLATFORM_WINDOWS)

call tests%integer_ne(platform(), PLATFORM_UNKNOWN, "platform not unknown")
call tests%integer_ge(platform(), PLATFORM_UNKNOWN, "platform lower bound")
call tests%integer_le(platform(), max_platform, "platform upper bound")
call tests%integer_le(min_platform, PLATFORM_UNKNOWN, "unknown platform is the lowest")

path_array(1) = "test"
path_array(2) = "file.txt"
path = path_join(path_array)

call tests%character_eq(path, "test" // DIR_SEPS(platform()) // "file.txt", "path_join")

path = "src/units.f90"
call convert_path_unix_to_win(path, test=.true.)
call tests%character_eq(path, "src\units.f90", "convert_path_unix_to_win")

call tests%end_tests()

end program test_port
