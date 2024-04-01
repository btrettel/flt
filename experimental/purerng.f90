! Module for pure random number generators.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module purerng

use prec, only: WP
implicit none
private

integer, parameter :: I10 = selected_int_kind(10)

type, public :: rng_type
    integer(kind=I10), allocatable :: seed(:)
end type rng_type

contains

elemental function l88f3(rng)
    ! Random number generator from lecuyer_efficient_1988, fig. 3.
    
    type(rng_type), intent(in out) :: rng
    
    real(kind=WP) :: l88f3
    
    ! TODO: For speed, I might be able to reduce the precision of some of these `integer`s.
    
    integer, parameter :: L = 2
    
    ! from lecuyer_efficient_1988, table III
    integer(kind=I10), parameter :: M(L) = [2147483563, 2147483399]
    integer(kind=I10), parameter :: A(L) = [40014, 40692]
    integer(kind=I10), parameter :: Q(L) = [53668, 52774]
    integer(kind=I10), parameter :: R(L) = [12211, 3791]
    
    integer(kind=I10) :: k(L), z
    
    k        = rng%seed / Q
    rng%seed = A * (rng%seed - k * Q) - k * R
    where (rng%seed < 0) then
        rng%seed = rng%seed + M
    end if
    
    z = rng%seed(1) - rng%seed(2)
    ! TODO: Consider rewriting as a `min`? Though that would require computing both
    if (z < 1) then
        z = z + M(1) - 1
    end if
    
    l88f3 = real(z, kind=WP) / real(M(1), kind=WP)
end function l88f3

end module purerng
