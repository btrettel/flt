! Module for derivative-free optimization of `real`s with a genetic algorithm.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Notation follows luke_essentials_2013 p. 31 in most instances.

module ga

use prec, only: WP
implicit none
private

type, public :: ga_config
    real(WP) :: p_select, p_cross, p_mutate, stop_time
    integer  :: n_gener, n_stall
    
    ! number of individuals in population
    integer :: n_pop
    
    ! number of genes actually used
    integer :: n_genes
    
    ! number of fitness functions actually used
    integer :: n_fitness
    
    ! lower and upper bounds for each gene
    real(WP), allocatable :: lb(:), ub(:)
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

public :: clip
public :: initialize

contains

pure subroutine clip(lower_bound, upper_bound, x)
    ! Clip variable `x` within upper and lower bounds.
    
    use checks, only: assert
    
    real(WP), intent(in)     :: lower_bound, upper_bound
    real(WP), intent(in out) :: x
    
    x = max(x, lower_bound)
    x = min(x, upper_bound)
    
    call assert(x >= lower_bound, "purerng (rand_uniform): x >= lower_bound violated")
    call assert(x <= upper_bound, "purerng (rand_uniform): x <= upper_bound violated")
end subroutine clip

subroutine initialize(config, rng, pop)
    use purerng, only: rng_type
    use checks, only: assert
    
    type(ga_config), intent(in)    :: config
    type(rng_type), intent(in out) :: rng
    type(pop_type), intent(out)    :: pop
    
    integer :: i_pop, i_gene
    
    pop_loop: do i_pop = 1, config%n_pop ! SERIAL
        gene_loop: do i_gene = 1, config%n_genes ! SERIAL
            call rng%uniform(config%lb(i_pop), config%ub(i_pop), pop%individuals(i_pop)%chromo(i_gene))
            
            call assert(pop%individuals(i_pop)%chromo(i_gene) >= config%lb(i_pop), &
                            "ga (initialize): lower bound violated")
            call assert(pop%individuals(i_pop)%chromo(i_gene) <= config%ub(i_pop), &
                            "ga (initialize): upper bound violated")
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
