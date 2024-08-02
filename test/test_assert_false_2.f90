! test that `call assert(.false.)` fails
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_assert_false_2

use checks, only: assert
implicit none

call assert([.false., .true.], "Custom message.")

end program test_assert_false_2
