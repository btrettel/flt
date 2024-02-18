! # $File$
! 
! Summary: Module for random number generators, including a deterministic "random number" generator for testing.
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module rngmod

use prec, only: RP
implicit none
private
public :: set_determ
public :: determ, pseudo

type, private :: rng_type
    real(kind=RP), allocatable :: seed(:)
    !procedure(rng_procedure) :: rng
end type rng_type

type(rng_type), private :: determ_rng

contains

subroutine set_determ(values)
    real(kind=RP), intent(in) :: values(:)
    
    if (allocated(determ_rng%seed)) then
        deallocate(determ_rng%seed)
    end if
    allocate(determ_rng%seed(1 + size(values)))
    
    ! The first index picks which part of the seed to pick as the next random number.
    determ_rng%seed(1) = real(2, RP)
    
    ! The remainder of the seed is collection of the random numbers.
    determ_rng%seed(2:) = values(:)
end subroutine set_determ

subroutine next_determ(rng)
    type(rng_type), intent(inout) :: rng
    
    determ_rng%seed(1) = determ_rng%seed(1) + 1.0_RP
    
    if (nint(determ_rng%seed(1)) > size(rng%seed)) then
        determ_rng%seed(1) = real(2, RP)
    end if
end subroutine next_determ

subroutine determ(r)
    ! deterministic "random" number
    
    real(kind=RP), intent(out) :: r
    
    r = determ_rng%seed(nint(determ_rng%seed(1)))
    call next_determ(determ_rng)
end subroutine determ

! I can't make this `elemental` to make it generic to the rank of `r` because `random_number` is not pure.
! I could simply make different versions of each for each rank. There aren't too many ranks, fortunately.
subroutine pseudo(r)
    ! pseudo-random number
    
    real(kind=RP), intent(out) :: r
    
    call random_number(r)
end subroutine pseudo

end module rngmod
