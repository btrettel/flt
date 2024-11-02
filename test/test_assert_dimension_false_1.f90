! test that `call assert_dimension` fails when expected, real, rank 1
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_assert_dimension_false_1

use checks, only: assert_dimension
use prec, only: WP
implicit none

real(WP) :: a1(5), c1(4)

call assert_dimension(a1, c1)

end program test_assert_dimension_false_1
