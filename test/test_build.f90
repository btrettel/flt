! tests for the build module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_build

use build, only: REVISION, REVISION_DATE
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

call logger%open("build.nml")
call tests%start_tests(logger)

call tests%integer_eq(len(REVISION), 7, "build, REVISION length")
call tests%integer_eq(len(REVISION_DATE), 25, "build, REVISION_DATE length")

call tests%end_tests()
call logger%close()

end program test_build
