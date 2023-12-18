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

! Notation follows luke_essentials_2013 p. 31 in most instances.

module ga

use prec, only: I5, RP
implicit none
private

! maximum population size
integer, public, parameter :: MAX_N_POP = 64_I5

! maximum number of genes
integer, public, parameter :: MAX_N_GENES = 16_I5

! maximum number of fitness function values
integer, public, parameter :: MAX_N_FITNESS = 8_I5

type, public :: bounds_type
    ! lower and upper bounds
    real(kind=RP) :: lower, upper
end type bounds_type

type, public :: ga_config
    real(kind=RP)    :: p_select, p_elite, p_cross, p_mutate, stop_time
    integer(kind=I5) :: n_gener, n_stall
    
    ! number of individuals in population
    integer(kind=I5) :: n_pop
    
    ! number of genes actually used
    integer(kind=I5) :: n_genes
    
    ! number of fitness functions actually used
    integer(kind=I5) :: n_fitness
    
    type(bounds_type) :: bounds(MAX_N_GENES)
end type ga_config

type, public :: individual_type
    real(kind=RP) :: chromo(MAX_N_GENES)
    
    ! whether the quality function(s) has/have been set
    logical :: set
    
    ! quality function values
    real(kind=RP) :: fitness(MAX_N_FITNESS)
end type individual_type

type, public :: pop_type
    type(individual_type) :: individuals(MAX_N_POP)
    
    type(individual_type) :: best_pop_individual, best_ever_individual
end type pop_type

public :: rand_int, rand_uniform, rand_cauchy
public :: clip
public :: initialize

contains

function rand_int(lower_bound, upper_bound, r)
    integer(kind=I5), intent(in) :: lower_bound, upper_bound
    real(kind=RP), intent(in)    :: r
    
    integer(kind=I5) :: rand_int
    
    ! The `min` function makes this not return `upper_bound + 1_I5` when `r = 1.0_RP`.
    rand_int = min(lower_bound + floor(real(upper_bound + 1_I5 - lower_bound, RP) * r), upper_bound)
end function rand_int

function rand_uniform(lower_bound, upper_bound, r)
    ! Returns a uniform random variable.
    
    real(kind=RP), intent(in) :: lower_bound, upper_bound, r
    
    real(kind=RP) :: rand_uniform
    
    rand_uniform = lower_bound + (upper_bound - lower_bound) * r
end function rand_uniform

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
end function rand_cauchy

subroutine clip(bounds, x)
    ! Clip variable `x` within upper and lower bounds.
    
    type(bounds_type), intent(in) :: bounds
    real(kind=RP), intent(in out) :: x
    
    x = min(x, bounds%upper)
    x = max(x, bounds%lower)
end subroutine clip

subroutine initialize(config, pop)
    type(ga_config), intent(in) :: config
    type(pop_type), intent(out) :: pop
    
    integer(kind=I5) :: i_pop, i_gene
    real(kind=RP)    :: r
    
    do i_pop = 1_I5, config%n_pop
        do i_gene = 1_I5, config%n_genes
            call random_number(r)
            pop%individuals(i_pop)%chromo(i_gene) = rand_uniform(config%bounds(i_pop)%lower, config%bounds(i_pop)%lower, r)
        end do
    end do
end subroutine initialize

!subroutine optimize(config, f, best_ever_individual, rc)
!    type(ga_config), intent(in)        :: config
!    type(individual_type), intent(out) :: best_ever_individual
!    integer(kind=I5), intent(out)      :: rc
    
!    interface
!        function f(x)
!            ! TODO: Make ga_types.f90 as the `interface` block needs to use `individual_type`
!            type(individual_type), intent(in) :: x
!            real(kind=RP)                     :: f(:)
!        end function fun
!    end interface
!    ! TODO: Check that `config%n_pop < MAX_N_POP`.
!    ! TODO: Check that `config%n_genes < MAX_N_GENES`.
!    ! TODO: Check that `config%n_fitness < MAX_N_GENES`.
!end subroutine optimize

end module ga
