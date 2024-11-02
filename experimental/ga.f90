! Module for derivative-free optimization of `real`s with a genetic algorithm.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Notation follows luke_essentials_2013 p. 31 in most instances.

module ga

use prec, only: WP
use rngmod, only: determ, pseudo
implicit none
private

type, public :: bounds_type
    ! lower and upper bounds
    real(WP) :: lower, upper
end type bounds_type

type, public :: ga_config
    real(WP) :: p_select, p_cross, p_mutate, stop_time
    integer  :: n_gener, n_stall
    
    ! number of individuals in population
    integer :: n_pop
    
    ! number of genes actually used
    integer :: n_genes
    
    ! number of fitness functions actually used
    integer :: n_fitness
    
    type(bounds_type), allocatable :: bounds(:)
end type ga_config

type, public :: individual_type
    real(WP), allocatable :: chromo(:)
    
    ! whether the quality function(s) has/have been set
    logical :: set
    
    ! quality function values
    real(WP), allocatable :: fitness(:)
end type individual_type

type, public :: pop_type
    type(individual_type), allocatable :: individuals(:)
    
    type(individual_type) :: best_pop_individual, best_ever_individual
end type pop_type

public :: rand_int, rand_uniform, rand_cauchy
public :: clip
public :: initialize

contains

function rand_int(lower_bound, upper_bound, nu)
    integer, intent(in)  :: lower_bound, upper_bound
    real(WP), intent(in) :: nu
    
    integer :: rand_int
    
    ! The `min` function makes this not return `upper_bound + 1` when `r = 1.0_WP`.
    rand_int = min(lower_bound + floor(real(upper_bound + 1 - lower_bound, WP) * nu), upper_bound)
end function rand_int

function rand_uniform(lower_bound, upper_bound, nu)
    ! Returns a uniform random variable.
    
    real(WP), intent(in) :: lower_bound, upper_bound, nu
    
    real(WP) :: rand_uniform
    
    rand_uniform = lower_bound + (upper_bound - lower_bound) * nu
end function rand_uniform

function rand_cauchy(m, b, nu)
    ! Returns a Cauchy random variable with inverse transform sampling.
    ! Notation follows <https://mathworld.wolfram.com/CauchyDistribution.html>.
    
    use prec, only: PI
    
    real(WP), intent(in) :: m ! median
    real(WP), intent(in) :: b ! half width
    real(WP), intent(in) :: nu ! random CDF value for inverse sampling
    
    real(WP) :: rand_cauchy
    
    rand_cauchy = m + b * tan(PI * (nu - 0.5_WP))
end function rand_cauchy

pure subroutine clip(bounds, x)
    ! Clip variable `x` within upper and lower bounds.
    
    type(bounds_type), intent(in) :: bounds
    real(WP), intent(in out)      :: x
    
    x = min(x, bounds%upper)
    x = max(x, bounds%lower)
end subroutine clip

subroutine initialize(config, pop)
    type(ga_config), intent(in) :: config
    type(pop_type), intent(out) :: pop
    
    integer  :: i_pop, i_gene
    real(WP) :: nu
    
    pop_loop: do i_pop = 1, config%n_pop
        gene_loop: do i_gene = 1, config%n_genes
            call random_number(nu)
            pop%individuals(i_pop)%chromo(i_gene) = rand_uniform(config%bounds(i_pop)%lower, config%bounds(i_pop)%lower, nu)
        end do gene_loop
    end do pop_loop
end subroutine initialize

!subroutine optimize(config, f, best_ever_individual, rc)
!    type(ga_config), intent(in)        :: config
!    type(individual_type), intent(out) :: best_ever_individual
!    integer, intent(out)               :: rc
    
!    interface
!        function f(x)
!            ! TODO: Make ga_types.f90 as the `interface` block needs to use `individual_type`
!            type(individual_type), intent(in) :: x
!            real(WP)                          :: f(:)
!        end function fun
!    end interface
!end subroutine optimize

end module ga
