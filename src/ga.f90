! # $File$
! 
! Summary: Module for derivative-free optimization of `real`s with a genetic algorithm.
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Notation follows luke_essentials_2013 in most instances.

module ga

use prec, only: I5, RP
implicit none
private

! maximum population size
integer, public, parameter :: MAX_N_POP = 64_I5

! maximum number of components
integer, public, parameter :: MAX_N_COMP = 16_I5

! number of quality function values
integer, public, parameter :: N_QUALITY = 8_I5

type, public :: ga_config
    real(kind=RP)    :: p_select, p_elite, p_cross, p_mutate, stop_time
    integer(kind=I5) :: n_gener, n_stall
end type ga_config

type, public :: bounds_type
    ! lower and upper bounds
    real(kind=RP) :: lower, upper
end type bounds_type

type, public :: candidate_type
    real(kind=RP) :: components(MAX_N_COMP)
    
    ! number of components actually used
    integer(kind=I5) :: n_comp
    
    ! whether the quality function(s) has/have been set
    logical :: set
    
    ! quality function values
    real(kind=RP) :: quality(N_QUALITY)
end type candidate_type

type, public :: pop_type
    type(candidate_type) :: candidates(MAX_N_POP)
    type(bounds_type)    :: bounds(MAX_N_COMP)
    
    ! number of candidates in population
    integer(kind=I5)  :: n_pop
    
    ! number of components actually used
    integer(kind=I5)  :: n_comp
    
    type(candidate_type) :: best_pop_candidate, best_ever_candidate
end type pop_type

public :: rand_int, rand_cauchy
public :: clip

contains

function rand_int(lower_bound, upper_bound, r)
    integer(kind=I5), intent(in) :: lower_bound, upper_bound
    real(kind=RP), intent(in)    :: r
    
    integer(kind=I5) :: rand_int
    
    ! The `min` function makes this not return `upper_bound + 1_I5` when `r = 1.0_RP`.
    rand_int = min(lower_bound + floor(real(upper_bound + 1_I5 - lower_bound, RP) * r), upper_bound)
    
    return
end function rand_int

function rand_cauchy(m, b, r)
    ! Returns a Cauchy random variable with inverse transform sampling.
    ! Notation follows <https://mathworld.wolfram.com/CauchyDistribution.html>.
    
    use prec, only: PI
    
    ! median
    real(kind=RP), intent(in) :: m
    
    ! half width
    real(kind=RP), intent(in) :: b
    
    ! random CDF value for inverse sampling
    real(kind=RP), intent(in) :: r
    
    real(kind=RP) :: rand_cauchy
    
    rand_cauchy = m + b * tan(PI * (r - 0.5_RP))
    
    return
end function rand_cauchy

subroutine clip(bounds, x)
    ! Clip variable `x` within upper and lower bounds.
    
    type(bounds_type), intent(in) :: bounds
    real(kind=RP), intent(in out) :: x
    
    x = min(x, bounds%upper)
    x = max(x, bounds%lower)
    
    return
end subroutine clip

!subroutine optimize(bounds, f, best_ever_candidate, rc)
!    type(bounds_type), intent(in) :: bounds(:)
!    type(candidate_type), intent(out) :: best_ever_candidate
!    integer(kind=I5), intent(out) :: rc
    
!    interface
!        function f(x)
!            type(individual_type), intent(in) :: x
!            real(kind=RP)                     :: f(:)
!        end function fun
!    end interface
!end subroutine optimize

end module ga
