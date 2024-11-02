! Module for pure random number generators.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module purerng

use prec, only: I10
use checks, only: assert
implicit none
private

! For arrays: One `rng_type` per `harvest`.

! TODO: `random_seed` sets based on current time. Uses spacing in lecuyer_efficient_1988 to set for arrays.

public :: lecuyer

type, public :: rng_type
    integer(kind=I10), allocatable     :: seed(:)
    procedure(rng_subroutine), pointer :: rng ! TODO: Does `rng = lecuyer` set the default?
contains
    procedure :: random_number => pure_random_number
end type rng_type

abstract interface
    ! Making this `elemental` makes nvfortran complain.
    ! NVFORTRAN-S-1010-Illegal use of an elemental interface with procedure pointer rng (src/purerng.f90: 23)
    elemental subroutine rng_subroutine(rng, harvest)
        use prec, only: WP
        import :: rng_type
        
        class(rng_type), intent(in out) :: rng
        real(WP), intent(out)           :: harvest
    end subroutine
end interface

contains

elemental subroutine pure_random_number(rng, harvest)
    use prec, only: WP
    
    class(rng_type), intent(in out) :: rng
    real(WP), intent(out)           :: harvest
    
    ! TODO: Why do I not have to pass rng in the argument list?
    call rng%rng(harvest)
end subroutine pure_random_number

elemental subroutine lecuyer(rng, harvest)
    ! Random number generator from lecuyer_efficient_1988, fig. 3.
    
    use prec, only: WP
    
    type(rng_type), intent(in out) :: rng
    real(WP), intent(out)          :: harvest
    
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
    
    ! lecuyer_efficient_1988 p. 747R
    call assert(rng%seed(1) >= 1_I10)
    call assert(rng%seed(1) <= M(1) - 1_I10)
    call assert(rng%seed(2) >= 1_I10)
    call assert(rng%seed(2) <= M(2) - 1_I10)
    
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
    
    ! lecuyer_efficient_1988 p. 747R:
    ! > Notice that the function will never return 0.0 or 1.0, as long as `REAL` variables have at least 23-bit mantissa (this is
    ! > the case for most 32-bit machines).
    call assert(harvest > 0.0_WP)
    call assert(harvest < 1.0_WP)
    call assert(z > 0_I10)
    call assert(z < M(1))
    
    ! Same `seed` bounds as before.
    call assert(rng%seed(1) >= 1_I10)
    call assert(rng%seed(1) <= M(1) - 1_I10)
    call assert(rng%seed(2) >= 1_I10)
    call assert(rng%seed(2) <= M(2) - 1_I10)
end subroutine lecuyer

end module purerng
