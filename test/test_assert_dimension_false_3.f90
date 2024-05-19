! test that `call assert_dimension` fails when expected, rank 3
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_assert_dimension_false_3

use checks, only: assert_dimension
use prec, only: WP
implicit none

real(kind=WP) :: a3(5, 5), c3(4, 4)

call assert_dimension(a3, c3)

end program test_assert_dimension_false_3
