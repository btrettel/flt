! Module for derivative-free optimization of `real`s with a genetic algorithm.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

! Notation follows luke_essentials_2013 p. 31 in most instances.
! Instead of "fitness" or "quality", I am using the term objective function.
! In contrast to luke_essentials_2013, lower objective function values are better.

module ga

use prec, only: WP
implicit none
private

integer, parameter :: MAX_SAMPLES = 10000

type, public :: ga_config
    ! `n_pop`, `p_cross`, and `p_mutate` defaults based on:
    ! de_jong_analysis_1975 pp. 68, 70 (pdf pp. 83, 85): for n_pop = 50, p_mutate = 0.02 is about optimal
    ! de_jong_analysis_1975 pp. 75 (pdf pp. 90): p_cross_indiv = 0.6 is about optimal
    ! It's not exactly clear to me how large a population is ideal, but I guess `n_pop = 50` is a decent starting point.
    integer  :: n_pop = 50 ! number of indivs in population
    real(WP) :: p_cross_indiv = 0.5_WP, p_mutate = 0.02_WP
    
    ! `n_select = 2` is most popular according to luke_essentials_2013 p. 45.
    integer :: n_select = 2
    
    ! `p_cross_gene = 0.5_WP` was the first proposed suggestion according to luke_essentials_2013 p. 39.
    real(WP) :: p_cross_gene = 0.5_WP
    
    real(WP) :: stop_time = huge(1.0_WP)
    integer  :: n_gener = 1000, n_stall = 200
    real(WP) :: rel_b = 0.2_WP ! `b` parameter for Cauchy dist., relative to range of variable determined from `lb` and `ub`
    integer  :: n_genes = 0 ! number of genes (default set to zero to catch when not set)
    
    real(WP), allocatable :: lb(:), ub(:) ! lower and upper bounds for each gene
end type ga_config

type, public :: indiv_type
    real(WP), allocatable :: chromo(:)
    
    ! whether the objective function has been set (`.false.` by default)
    logical :: f_set = .false.
    
    ! objective function value
    real(WP) :: f
end type indiv_type

type, public :: pop_type
    type(indiv_type), allocatable :: indivs(:)
    
    type(indiv_type) :: best_pop_indiv, best_ever_indiv
end type pop_type

public :: init_pop, mutate_indiv, cross_two_indivs, select_indiv

contains

subroutine init_pop(config, rng, pop)
    use purerng, only: rng_type
    use checks, only: assert
    
    type(ga_config), intent(in)    :: config
    type(rng_type), intent(in out) :: rng
    type(pop_type), intent(out)    :: pop
    
    integer :: i_pop, i_gene
    
    call assert(.not. allocated(pop%indivs), "ga (init_pop): .not. allocated(pop%indivs)")
    allocate(pop%indivs(config%n_pop))
    
    pop_loop: do concurrent (i_pop = 1:config%n_pop)
        call assert(.not. allocated(pop%indivs(i_pop)%chromo), "ga (init_pop): .not. allocated(pop%indivs(i_pop)%chromo)")
        allocate(pop%indivs(i_pop)%chromo(config%n_genes))
        
        gene_loop: do concurrent (i_gene = 1:config%n_genes)
            call assert(config%lb(i_gene) <= config%ub(i_gene), "ga (init_pop): lb > ub?")
            
            call rng%uniform(config%lb(i_gene), config%ub(i_gene), pop%indivs(i_pop)%chromo(i_gene))
            
            call assert(pop%indivs(i_pop)%chromo(i_gene) >= config%lb(i_gene), "ga (init_pop): lower bound violated")
            call assert(pop%indivs(i_pop)%chromo(i_gene) <= config%ub(i_gene), "ga (init_pop): upper bound violated")
        end do gene_loop
    end do pop_loop
end subroutine init_pop

pure subroutine mutate_indiv(config, rng, indiv)
    ! Follows luke_essentials_2013 Algorithm 11, p. 23.
    
    use purerng, only: rng_type
    use checks, only: assert
    
    type(ga_config), intent(in)      :: config
    type(rng_type), intent(in out)   :: rng
    type(indiv_type), intent(in out) :: indiv
    
    integer  :: i_gene, i_sample
    real(WP) :: nu, b
    
    call assert(config%n_genes > 0, "ga (mutate_indiv): config%n_genes > 0 violated")
    
    do concurrent (i_gene = 1:config%n_genes)
        call rng%random_number(nu)
        if (config%p_mutate >= nu) then
            call assert(config%lb(i_gene) <= config%ub(i_gene), "ga (mutation): lb > ub?")
            b = config%rel_b * (config%ub(i_gene) - config%lb(i_gene))
            
            i_sample = 0
            do ! SERIAL
                i_sample = i_sample + 1
                if (i_sample > MAX_SAMPLES) then
                    error stop "ga (mutate_indiv): MAX_SAMPLES exceeded"
                end if
                
                call rng%cauchy(0.0_WP, b, nu)
                
                if (((indiv%chromo(i_gene) + nu) >= config%lb(i_gene)) &
                        .and. ((indiv%chromo(i_gene) + nu) <= config%ub(i_gene))) then
                    exit
                end if
            end do
            
            indiv%chromo(i_gene) = indiv%chromo(i_gene) + nu
            
            call assert(indiv%chromo(i_gene) >= config%lb(i_gene), "ga (mutate_indiv): lower bound violated")
            call assert(indiv%chromo(i_gene) <= config%ub(i_gene), "ga (mutate_indiv): upper bound violated")
        end if
    end do
end subroutine mutate_indiv

pure subroutine cross_two_indivs(config, rng, indiv_1, indiv_2)
    ! Uniform crossover
    ! Follows luke_essentials_2013 Algorithm 25, p. 39.
    
    use purerng, only: rng_type
    use checks, only: assert, assert_dimension
    
    type(ga_config), intent(in)      :: config
    type(rng_type), intent(in out)   :: rng
    type(indiv_type), intent(in out) :: indiv_1, indiv_2
    
    integer  :: i_gene
    real(WP) :: nu, gene_temp
    
    call assert(config%n_genes > 0, "ga (cross_two_indivs): config%n_genes > 0 violated")
    call assert_dimension(indiv_1%chromo, indiv_2%chromo)
    
    do concurrent (i_gene = 1:config%n_genes)
        call rng%random_number(nu)
        if (config%p_cross_indiv >= nu) then
            call assert(indiv_1%chromo(i_gene) >= config%lb(i_gene), "ga (cross_two_indivs): lower bound violated (1)")
            call assert(indiv_1%chromo(i_gene) <= config%ub(i_gene), "ga (cross_two_indivs): upper bound violated (1)")
            call assert(indiv_2%chromo(i_gene) >= config%lb(i_gene), "ga (cross_two_indivs): lower bound violated (2)")
            call assert(indiv_2%chromo(i_gene) <= config%ub(i_gene), "ga (cross_two_indivs): upper bound violated (2)")
            
            gene_temp              = indiv_1%chromo(i_gene)
            indiv_1%chromo(i_gene) = indiv_2%chromo(i_gene)
            indiv_2%chromo(i_gene) = gene_temp
        end if
    end do
end subroutine cross_two_indivs

pure subroutine select_indiv(config, rng, pop, indiv)
    ! Tournament selection
    ! Follows luke_essentials_2013 Algorithm 32, p. 45.
    
    use purerng, only: rng_type
    use checks, only: assert, assert_dimension
    
    type(ga_config), intent(in)    :: config
    type(rng_type), intent(in out) :: rng
    type(pop_type), intent(in out) :: pop
    type(indiv_type), intent(out)  :: indiv
    
    integer :: nu, i_pop
    
    call assert(config%n_pop == size(pop%indivs), "ga (select_indiv): config%n_pop == size(pop%indivs) violated")
    
    call rng%int(1, config%n_pop, nu)
    indiv = pop%indivs(nu)
    call assert(indiv%f_set, "ga (select_indiv): objective function not set (1)")
    do i_pop = 2, config%n_select ! SERIAL
        call rng%int(1, config%n_pop, nu)
        
        call assert(pop%indivs(nu)%f_set, "ga (select_indiv): objective function not set (2)")
        
        if (pop%indivs(nu)%f < indiv%f) then
            indiv = pop%indivs(nu)
        end if
    end do
end subroutine select_indiv

!subroutine optimize(config, objfun, best_ever_indiv, rc)
!    type(ga_config), intent(in)   :: config
!    type(indiv_type), intent(out) :: best_ever_indiv
!    integer, intent(out)          :: rc
    
!    interface
!        subroutine objfun(indiv, f, violations, out)
!            ! TODO: Make ga_types.f90 as the `interface` block needs to use `indiv_type`? Might be okay in later standards.
!            ! TODO: sum of constraint violations
!            type(indiv_type), intent(in) :: indiv
!            real(WP)                     :: f, violations
!        end function objfun
!    end interface
!end subroutine optimize

end module ga
