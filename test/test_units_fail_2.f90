! test that an invalid units operation fails
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units_fail_2

use prec, only: WP
use units, only: si_length => unit_p10_p00_p00, sqrt
implicit none

type(si_length) :: x, z

x%v = 1.0_WP

! This should fail to compile.
z = sqrt(x)

end program test_units_fail_2
