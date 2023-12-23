! # $File$
! 
! Summary: Module to set precisions, lengths, and mathematical constants.
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module prec

implicit none
private

! Integer precision
! -----------------

integer, public, parameter :: I5 = selected_int_kind(5)
integer, public, parameter :: I9 = selected_int_kind(9) ! This is the most ELF90 allows.

! Real precision
! --------------

! <https://fortranwiki.org/fortran/show/Real+precision>
! `RP` stands for *real precision* in case I want to change the precision later.
!integer, public, parameter :: SP = selected_real_kind(6, 37)    ! single
integer, public, parameter :: DP = selected_real_kind(15, 307)  ! double
!integer, public, parameter :: QP = selected_real_kind(33, 4931) ! quad
integer, public, parameter :: RP = DP

! String parameters
! -----------------

integer, public, parameter :: CL = 1024 ! `CL` stands for character length

! Mathematical constants
! ----------------------

! PI: 3.1415926535897932384626433...
! <https://stackoverflow.com/a/49416279/1124489>
real(kind=RP), public, parameter :: PI = 4.0_RP * atan(1.0_RP)

end module prec
