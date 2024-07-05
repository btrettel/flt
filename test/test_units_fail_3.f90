! test that an invalid units operation fails
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units_fail_3

use prec, only: WP
use units, only: length   => unit_p10000_p00000_p00000, &
                 time     => unit_p00000_p00000_p10000
implicit none

type(length) :: x
type(time)   :: t

x%v = 0.0_WP
t%v = 1.0_WP

! This should fail to compile.
print *, x < t

end program test_units_fail_3
