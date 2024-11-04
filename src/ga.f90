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

integer, parameter          :: MAX_SAMPLES = 10000
character(len=*), parameter :: GENER_FMT = "(i8)"

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
    
    integer  :: n_gener = 20
    real(WP) :: rel_b   = 0.2_WP ! `b` parameter for Cauchy dist., relative to range of variable determined from `lb` and `ub`
    integer  :: n_genes = 0 ! number of genes (default set to zero to catch when not set)
    
    real(WP), allocatable :: lb(:), ub(:) ! lower and upper bounds for each gene
    
    logical          :: progress = .true.
    character(len=8) :: f_fmt = "f12.2"
end type ga_config

type, public :: indiv_type
    real(WP), allocatable :: chromo(:)
    
    ! whether the objective function has been set (`.false.` by default)
    logical :: f_set = .false.
    
    ! objective function value
    real(WP) :: f
    
    ! sum of constraint violations
    real(WP) :: sum_g = 0.0_WP
end type indiv_type

type, public :: pop_type
    type(indiv_type), allocatable :: indivs(:)
    
    type(indiv_type) :: best_pop_indiv, best_ever_indiv
end type pop_type

public :: init_pop, mutate_indiv, cross_two_indivs, select_indiv, evaluate, optimize

contains

subroutine init_pop(config, rng, pop)
    use purerng, only: rng_type
    use checks, only: assert
    
    type(ga_config), intent(in)    :: config
    type(rng_type), intent(in out) :: rng
    type(pop_type), intent(out)    :: pop
    
    integer :: i_pop, i_gene
    
    call assert(allocated(config%lb), "ga (init_pop): allocated(config%lb) violated")
    call assert(allocated(config%ub), "ga (init_pop): allocated(config%ub) violated")
    
    call assert(.not. allocated(pop%indivs), "ga (init_pop): .not. allocated(pop%indivs) violated")
    allocate(pop%indivs(config%n_pop))
    
    call assert(config%n_genes > 0, "ga (init_pop): config%n_genes > 0 violated")
    
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
    
    pop%best_pop_indiv%f      = huge(1.0_WP)
    pop%best_pop_indiv%f_set  = .true.
    pop%best_ever_indiv%f     = huge(1.0_WP)
    pop%best_ever_indiv%f_set = .true.
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
    call assert(allocated(config%lb), "ga (mutate_indiv): allocated(config%lb) violated")
    call assert(allocated(config%ub), "ga (mutate_indiv): allocated(config%ub) violated")
    
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
                    ! TODO: Alternatively, clip to the bounds.
                end if
                
                call rng%cauchy(0.0_WP, b, nu)
                
                if (((indiv%chromo(i_gene) + nu) >= config%lb(i_gene)) &
                        .and. ((indiv%chromo(i_gene) + nu) <= config%ub(i_gene))) then
                    exit
                end if
            end do
            
            indiv%chromo(i_gene) = indiv%chromo(i_gene) + nu
            indiv%f_set = .false.
            
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
    call assert(allocated(config%lb), "ga (cross_two_indivs): allocated(config%lb) violated")
    call assert(allocated(config%ub), "ga (cross_two_indivs): allocated(config%ub) violated")
    
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
            
            indiv_1%f_set = .false.
            indiv_2%f_set = .false.
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

subroutine evaluate(config, objfun, pop)
    use prec, only: WP
    use checks, only: assert, is_close
    
    type(ga_config), intent(in)    :: config
    type(pop_type), intent(in out) :: pop
    
    integer  :: i_pop, i_pop_best
    real(WP) :: f_max
    
    interface
        subroutine objfun(chromo, f, sum_g)
            use prec, only: WP
            
            ! Passing in all `real`s means that the objective function does not need any of this module's derived types.
            real(WP), intent(in)  :: chromo(:)
            real(WP), intent(out) :: f     ! objective function value
            real(WP), intent(out) :: sum_g ! sum of constraint violations
        end subroutine objfun
    end interface
    
    call assert(config%n_pop == size(pop%indivs), "ga (evaluate): config%n_pop == size(pop%indivs) violated")
    call assert(pop%best_ever_indiv%f_set, "ga (evaluate): pop%best_ever_indiv%f_set violated")
    
    f_max      = -huge(1.0_WP)
    i_pop_best = 0
    do i_pop = 1, config%n_pop ! SERIAL
        call objfun(pop%indivs(i_pop)%chromo, pop%indivs(i_pop)%f, pop%indivs(i_pop)%sum_g)
        
        call assert(pop%indivs(i_pop)%sum_g >= 0.0_WP, "ga (evaluate): pop%indivs(i_pop)%sum_g >= 0 violated")
        
        if (is_close(pop%indivs(i_pop)%sum_g, 0.0_WP)) then
            f_max      = max(f_max, pop%indivs(i_pop)%f)
            i_pop_best = i_pop
            pop%indivs(i_pop)%f_set = .true.
        end if
    end do
    
    call assert(i_pop_best > 0, "ga (evaluate): i_pop_best > 0 violated")
    
    ! set `f` for indivs that had constraint violations
    ! See deb_efficient_2000 eq. 4.
    do concurrent (i_pop = 1:config%n_pop)
        if (pop%indivs(i_pop)%sum_g > 0.0_WP) then
            pop%indivs(i_pop)%f     = f_max + pop%indivs(i_pop)%sum_g
            pop%indivs(i_pop)%f_set = .true.
        end if
    end do
    
    ! pick pop best individual
    pop%best_pop_indiv = pop%indivs(i_pop_best)
    
    ! set best ever individual
    if (pop%best_ever_indiv%f > pop%best_pop_indiv%f) then
        pop%best_ever_indiv = pop%best_pop_indiv
    end if
end subroutine evaluate

subroutine optimize(config, rng, objfun, pop, rc)
    ! Tournament selection
    ! Follows luke_essentials_2013 Algorithm 20, p. 37.
    
    use purerng, only: rng_type
    use checks, only: assert
    
    type(ga_config), intent(in)    :: config
    type(rng_type), intent(in out) :: rng
    type(pop_type), intent(in out) :: pop
    integer, intent(out)           :: rc ! TODO: return codes
    
    integer :: i_gener, i_pop
    
    type(pop_type) :: next_pop
    
    interface
        subroutine objfun(chromo, f, sum_g)
            use prec, only: WP
            
            ! Passing in all `real`s means that the objective function does not need any of this module's derived types.
            real(WP), intent(in)  :: chromo(:)
            real(WP), intent(out) :: f     ! objective function value
            real(WP), intent(out) :: sum_g ! sum of constraint violations
        end subroutine objfun
    end interface
    
    call assert(config%n_pop == size(pop%indivs), "ga (optimize): config%n_pop == size(pop%indivs) violated")
    call assert(config%n_genes > 0, "ga (optimize): config%n_genes > 0 violated")
    call assert(config%n_gener > 0, "ga (optimize): config%n_gener > 0 violated")
    call assert(allocated(config%lb), "ga (optimize): allocated(config%lb) violated")
    call assert(allocated(config%ub), "ga (optimize): allocated(config%ub) violated")
    
    allocate(next_pop%indivs(config%n_pop))
    next_pop%best_ever_indiv      = pop%best_ever_indiv
    next_pop%best_pop_indiv%f_set = .false.
    
    if (config%progress) then
        write(unit=*, fmt="(a)") "   gener    pop best   best ever"
        write(unit=*, fmt=GENER_FMT, advance="no") 0
    end if
    call evaluate(config, objfun, pop)
    if (config%progress) then
        write(unit=*, fmt="(2" // trim(config%f_fmt) // ")") pop%best_pop_indiv%f, pop%best_ever_indiv%f
    end if
    
    rc = 0
    do i_gener = 1, config%n_gener ! SERIAL
        do i_pop = 1, config%n_pop, 2 ! SERIAL
            ! I don't have a parallel RNG right now, so this can't be parallelized at the moment.
            ! Not that parallelizing this would matter much anyway as it's quick.
            
            call select_indiv(config, rng, pop, next_pop%indivs(i_pop))
            call select_indiv(config, rng, pop, next_pop%indivs(i_pop + 1))
            call cross_two_indivs(config, rng, next_pop%indivs(i_pop), next_pop%indivs(i_pop + 1))
            call mutate_indiv(config, rng, next_pop%indivs(i_pop))
            call mutate_indiv(config, rng, next_pop%indivs(i_pop + 1))
        end do
        
        pop%indivs = next_pop%indivs
        
        if (config%progress) then
            write(unit=*, fmt=GENER_FMT, advance="no") i_gener
        end if
        call evaluate(config, objfun, pop)
        if (config%progress) then
            write(unit=*, fmt="(2" // trim(config%f_fmt) // ")") pop%best_pop_indiv%f, pop%best_ever_indiv%f
        end if
    end do
end subroutine optimize

end module ga
