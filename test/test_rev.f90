! tests for the build module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_rev

use rev, only: REVISION, REVISION_DATE
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(test_results_type) :: tests

call tests%start_tests("rev.nml")

call tests%integer_eq(len(REVISION), 7, "build, REVISION length")
call tests%integer_eq(len(REVISION_DATE), 25, "build, REVISION_DATE length")

call tests%end_tests()

end program test_rev
