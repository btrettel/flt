! Module for pure random number generators.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module purerng

use prec, only: I10, WP
use checks, only: assert
implicit none
private

! TODO: `random_seed` sets based on current time.

public :: lecuyer

type, public :: rng_type
    integer(kind=I10), allocatable :: seed(:)
    !procedure(rng_procedure) :: rng ! TODO: Does `rng = lecuyer` set the default?
end type rng_type

contains

subroutine lecuyer(rng, harvest)
    ! Random number generator from lecuyer_efficient_1988, fig. 3.
    
    type(rng_type), intent(in out) :: rng
    
    real(kind=WP) :: harvest
    
    ! TODO: For speed, I might be able to reduce the precision of some of these `integer`s.
    
    integer, parameter :: L = 2
    
    ! from lecuyer_efficient_1988, table III
    integer(kind=I10), parameter :: M(L) = [2147483563_I10, 2147483399_I10]
    integer(kind=I10), parameter :: A(L) = [40014_I10, 40692_I10]
    integer(kind=I10), parameter :: Q(L) = [53668_I10, 52774_I10]
    integer(kind=I10), parameter :: R(L) = [12211_I10, 3791_I10]
    
    integer(kind=I10) :: k(L), z
    
    call assert(allocated(rng%seed))
    call assert(size(rng%seed) == 2)
    
    k        = rng%seed / Q
    rng%seed = A * (rng%seed - k * Q) - k * R
    where (rng%seed < 0)
        rng%seed = rng%seed + M
    end where
    
    z = rng%seed(1) - rng%seed(2)
    ! TODO: Consider rewriting as a `min` to improve branch prediction? Though that would require computing both.
    if (z < 1) then
        z = z + M(1) - 1
    end if
    
    harvest = real(z, kind=WP) / real(M(1), kind=WP)
    
    ! TODO: Assert something else. Are the seeds always positive?
    
    ! lecuyer_efficient_1988 p. 747R:
    ! > Notice that the function will never return 0.0 or 1.0, as long as `REAL` variables have at least 23-bit mantissa (this is
    ! > the case for most 32-bit machines).
    call assert(harvest > 0.0_WP)
    call assert(harvest < 1.0_WP)
    call assert(z > 0_I10)
    call assert(z < M(1))
end subroutine lecuyer

end module purerng
