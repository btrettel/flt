! # $File$
! 
! Summary: precisions and lengths module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module prec

! Integer precision
! -----------------

integer, public, parameter :: I5  = selected_int_kind(5)
integer, public, parameter :: I15 = selected_int_kind(15)

! Real precision
! --------------

! <https://fortranwiki.org/fortran/show/Real+precision>
! `RP` stands for *real precision* in case I want to change the precision later.
integer, public, parameter :: SP = selected_real_kind(6_I5, 37_I5)    ! single
integer, public, parameter :: DP = selected_real_kind(15_I5, 307_I5)  ! double
integer, public, parameter :: QP = selected_real_kind(33_I5, 4931_I5) ! quad
RP = DP

! String parameters
! -----------------

integer(kind=IP), public, parameter :: CL = 1024_I5 ! `CL` stands for character length

! Mathematical constants
! ----------------------

! PI: 3.1415926535897932384626433...
! <https://stackoverflow.com/a/49416279/1124489>
! This needs to be modified when changing the precision.
! See tests.f90 on `PI` for more details.
!real(kind=RP), parameter, public :: PI = 4.0_RP * atan(1.0_RP) ! This initialization expression is not allowed in Fortran 95.
! This is the maximum number of digits I can put in for double precision without gfortran complaining.
real(kind=RP), public, parameter :: PI = 3.141592653589793_DP

end module prec
