! test that `call assert_precision_loss` can fail
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_assert_precision_loss_1

use prec, only: WP
use checks, only: assert_precision_loss
implicit none

real(kind=WP) :: x, y, z

x = 100000.0_WP
y = 100000.0_WP - spacing(x)
z = x - y
call assert_precision_loss(x, y, z)

end program test_assert_precision_loss_1
