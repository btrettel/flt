! test that `call assert_dimension` fails when expected, ad, rank 2
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_assert_dimension_false_5

use autodiff, only: ad, assert_dimension
implicit none

type(ad) :: a2(5, 5), c2(4, 4)

call assert_dimension(a2, c2)

end program test_assert_dimension_false_5
