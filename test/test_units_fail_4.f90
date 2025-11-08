! test that an invalid units operation fails
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_units_fail_4

use prec, only: WP
use units
implicit none

type(unitless)  :: x
type(si_length) :: y
type(si_time)   :: z

x%v = 1.0_WP
y%v = 1.0_WP

! This should fail to compile.
z = x * y + x

end program test_units_fail_4
