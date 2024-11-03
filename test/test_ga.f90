! tests for the ga module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_ga

use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

call logger%open("ga.nml")
call tests%start_tests(logger)

call test_clip(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_clip(tests)
    use prec, only: WP
    use ga, only: clip
    
    type(test_results_type), intent(in out) :: tests
    
    real(WP) :: lower, upper, rr

    lower = 0.0_WP
    upper = 1.0_WP

    rr = -5.0_WP
    call clip(lower, upper, rr)
    call tests%real_eq(rr, 0.0_WP, "clip (below)")

    rr = 0.5_WP
    call clip(lower, upper, rr)
    call tests%real_eq(rr, 0.5_WP, "clip (no change)")

    rr = 5.0_WP
    call clip(lower, upper, rr)
    call tests%real_eq(rr, 1.0_WP, "clip (above)")
end subroutine test_clip

end program test_ga
