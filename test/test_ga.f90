! tests for the ga module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_ga

use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

call logger%open("ga.nml")
call tests%start_tests(logger)

call test_init_pop(tests)
call test_mutate_indiv(tests)
call test_cross_two_indivs(tests)
call test_select_indiv(tests)
call test_evaluate_1(tests)
call test_optimize_ga(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_init_pop(tests)
    use prec, only: WP
    use ga, only: ga_config, pop_type, init_pop
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config) :: config
    type(rng_type)  :: rng
    type(pop_type)  :: pop
    
    config%n_pop   = 2
    config%n_genes = 3
    allocate(config%lb(config%n_genes))
    allocate(config%ub(config%n_genes))
    config%lb(1) = 0.0_WP
    config%ub(1) = 1.0_WP
    config%lb(2) = 1.0_WP
    config%ub(2) = 3.0_WP
    config%lb(3) = 4.0_WP
    config%ub(3) = 4.0_WP
    
    call rng%set_determ([0.5_WP])
    
    call init_pop(config, rng, pop)
    
    call tests%integer_eq(size(pop%indivs), config%n_pop, "size(pop%indivs)")
    call tests%integer_eq(size(pop%indivs(1)%chromo), config%n_genes, "size(pop%indivs(1)%chromo)")
    call tests%integer_eq(size(pop%indivs(2)%chromo), config%n_genes, "size(pop%indivs(2)%chromo)")
    call tests%logical_false(pop%indivs(1)%set, "pop%indivs(1)%set")
    call tests%logical_false(pop%indivs(2)%set, "pop%indivs(2)%set")
    
    call tests%real_eq(pop%indivs(1)%chromo(1), 0.5_WP, "pop%indivs(1)%chromo(1)")
    call tests%real_eq(pop%indivs(1)%chromo(2), 2.0_WP, "pop%indivs(1)%chromo(2)")
    call tests%real_eq(pop%indivs(1)%chromo(3), 4.0_WP, "pop%indivs(1)%chromo(3)")
    
    call tests%real_eq(pop%indivs(2)%chromo(1), 0.5_WP, "pop%indivs(2)%chromo(1)")
    call tests%real_eq(pop%indivs(2)%chromo(2), 2.0_WP, "pop%indivs(2)%chromo(2)")
    call tests%real_eq(pop%indivs(2)%chromo(3), 4.0_WP, "pop%indivs(2)%chromo(3)")
end subroutine test_init_pop

subroutine test_mutate_indiv(tests)
    use prec, only: WP
    use ga, only: ga_config, indiv_type, mutate_indiv
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config)  :: config
    type(rng_type)   :: rng
    type(indiv_type) :: indiv
    
    real(WP) :: b, nu
    
    config%n_pop   = 1
    config%n_genes = 2
    allocate(config%lb(config%n_genes))
    allocate(config%ub(config%n_genes))
    config%lb(1)    = 0.0_WP
    config%ub(1)    = 2.0_WP
    config%lb(2)    = 5.0_WP
    config%ub(2)    = 15.0_WP
    config%rel_b    = 0.5_WP ! changed from default to test it
    config%p_mutate = 1.0_WP ! always mutate for the test
    
    ! Cauchy RV will always return -1 in this case.
    call rng%set_determ([0.25_WP])
    
    b = (config%ub(1) - config%lb(1)) * config%rel_b
    call rng%cauchy(0.0_WP, b, nu)
    call tests%real_eq(nu, -1.0_WP, "Cauchy RV check")
    
    allocate(indiv%chromo(config%n_genes))
    
    ! mutation test
    indiv%chromo(1) = 2.0_WP
    indiv%chromo(2) = 15.0_WP
    indiv%set       = .true.
    call mutate_indiv(config, rng, indiv)
    call tests%real_eq(indiv%chromo(1), 1.0_WP, "mutate_indiv, mutation, indiv%chromo(1)")
    call tests%real_eq(indiv%chromo(2), 10.0_WP, "mutate_indiv, mutation, indiv%chromo(2)")
    call tests%logical_false(indiv%set, "mutate_indiv, mutation, set")
    
    ! no mutation test
    indiv%chromo(1) = 2.0_WP
    indiv%chromo(2) = 15.0_WP
    indiv%set       = .true.
    config%p_mutate = 0.2_WP
    call mutate_indiv(config, rng, indiv)
    call tests%real_eq(indiv%chromo(1), 2.0_WP, "mutate_indiv, no mutation, indiv%chromo(1)")
    call tests%real_eq(indiv%chromo(2), 15.0_WP, "mutate_indiv, no mutation, indiv%chromo(2)")
    call tests%logical_true(indiv%set, "mutate_indiv, no mutation, set")
end subroutine test_mutate_indiv

subroutine test_cross_two_indivs(tests)
    use prec, only: WP
    use ga, only: ga_config, indiv_type, cross_two_indivs
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config)  :: config
    type(rng_type)   :: rng
    type(indiv_type) :: indiv_1, indiv_2
    
    config%n_pop   = 1
    config%n_genes = 2
    allocate(config%lb(config%n_genes))
    allocate(config%ub(config%n_genes))
    config%lb(1)    = 0.0_WP
    config%ub(1)    = 2.0_WP
    config%lb(2)    = 5.0_WP
    config%ub(2)    = 15.0_WP
    
    allocate(indiv_1%chromo(config%n_genes))
    allocate(indiv_2%chromo(config%n_genes))
    
    ! crossover
    indiv_1%chromo(1) = 2.0_WP
    indiv_1%chromo(2) = 15.0_WP
    indiv_1%set       = .true.
    indiv_2%chromo(1) = 0.0_WP
    indiv_2%chromo(2) = 5.0_WP
    indiv_2%set       = .true.
    call rng%set_determ([0.25_WP])
    call cross_two_indivs(config, rng, indiv_1, indiv_2)
    call tests%real_eq(indiv_1%chromo(1), 0.0_WP, "cross_two_indivs, crossover, indiv_1%chromo(1)")
    call tests%real_eq(indiv_1%chromo(2), 5.0_WP, "cross_two_indivs, crossover, indiv_1%chromo(2)")
    call tests%real_eq(indiv_2%chromo(1), 2.0_WP, "cross_two_indivs, crossover, indiv_2%chromo(1)")
    call tests%real_eq(indiv_2%chromo(2), 15.0_WP, "cross_two_indivs, crossover, indiv_2%chromo(2)")
    call tests%logical_false(indiv_1%set, "cross_two_indivs, crossover, set 1")
    call tests%logical_false(indiv_2%set, "cross_two_indivs, crossover, set 2")
    
    ! no crossover
    indiv_1%chromo(1) = 2.0_WP
    indiv_1%chromo(2) = 15.0_WP
    indiv_1%set       = .true.
    indiv_2%chromo(1) = 0.0_WP
    indiv_2%chromo(2) = 5.0_WP
    indiv_2%set       = .true.
    call rng%set_determ([0.75_WP])
    call cross_two_indivs(config, rng, indiv_1, indiv_2)
    call tests%real_eq(indiv_1%chromo(1), 2.0_WP, "cross_two_indivs, crossover, indiv_1%chromo(1)")
    call tests%real_eq(indiv_1%chromo(2), 15.0_WP, "cross_two_indivs, crossover, indiv_1%chromo(2)")
    call tests%real_eq(indiv_2%chromo(1), 0.0_WP, "cross_two_indivs, crossover, indiv_2%chromo(1)")
    call tests%real_eq(indiv_2%chromo(2), 5.0_WP, "cross_two_indivs, crossover, indiv_2%chromo(2)")
    call tests%logical_true(indiv_1%set, "cross_two_indivs, no crossover, set 1")
    call tests%logical_true(indiv_2%set, "cross_two_indivs, no crossover, set 2")
end subroutine test_cross_two_indivs

subroutine test_select_indiv(tests)
    use prec, only: WP
    use ga, only: ga_config, pop_type, indiv_type, select_indiv
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config)  :: config
    type(rng_type)   :: rng
    type(pop_type)   :: pop
    type(indiv_type) :: indiv
    
    config%n_pop = 2
    allocate(pop%indivs(config%n_pop))
    
    pop%indivs(1)%set = .true.
    pop%indivs(1)%f   = 0.0_WP
    pop%indivs(2)%set = .true.
    pop%indivs(2)%f   = -1.0_WP
    call rng%set_determ([0.0_WP, 1.0_WP])
    call select_indiv(config, rng, pop, indiv)
    call tests%real_eq(indiv%f, -1.0_WP, "select_indiv, indiv%f (1)")
    
    pop%indivs(1)%set = .true.
    pop%indivs(1)%f   = 0.0_WP
    pop%indivs(2)%set = .true.
    pop%indivs(2)%f   = -1.0_WP
    call rng%set_determ([1.0_WP, 0.0_WP])
    call select_indiv(config, rng, pop, indiv)
    call tests%real_eq(indiv%f, -1.0_WP, "select_indiv, indiv%f (2)")
    
    pop%indivs(1)%set = .true.
    pop%indivs(1)%f   = 0.0_WP
    pop%indivs(2)%set = .true.
    pop%indivs(2)%f   = -1.0_WP
    call rng%set_determ([0.0_WP, 0.0_WP])
    call select_indiv(config, rng, pop, indiv)
    call tests%real_eq(indiv%f, 0.0_WP, "select_indiv, indiv%f (3)")
end subroutine test_select_indiv

subroutine rosenbrock(chromo, f, sum_g)
    ! <https://en.wikipedia.org/wiki/Rosenbrock_function>
    
    use prec, only: WP
    use checks, only: assert
    
    real(WP), intent(in)  :: chromo(:)
    real(WP), intent(out) :: f
    real(WP), intent(out) :: sum_g
    
    real(WP) :: a, b, x, y
    
    call assert(size(chromo) == 2, "test_ga (rosenbrock): wrong size chromo")
    
    a = 1.0_WP
    b = 100.0_WP
    x = chromo(1)
    y = chromo(2)
    
    f     = (a - x)**2 + b*(y - x**2)**2
    sum_g = 0.0_WP
end subroutine rosenbrock

subroutine test_evaluate_1(tests)
    use prec, only: WP
    use ga, only: ga_config, pop_type, evaluate
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config) :: config
    type(pop_type)  :: pop
    
    config%n_pop   = 2
    config%n_genes = 2
    allocate(pop%indivs(config%n_pop))
    allocate(pop%indivs(1)%chromo(config%n_genes))
    allocate(pop%indivs(2)%chromo(config%n_genes))
    
    pop%best_ever_indiv%set = .true.
    pop%best_ever_indiv%f   = huge(1.0_WP)
    pop%best_pop_indiv%set  = .true.
    pop%best_pop_indiv%f    = huge(1.0_WP)
    pop%indivs(1)%set = .false.
    pop%indivs(2)%set = .false.
    pop%indivs(1)%chromo(1) = 0.0_WP
    pop%indivs(1)%chromo(2) = 0.0_WP
    pop%indivs(2)%chromo(1) = 1.0_WP
    pop%indivs(2)%chromo(2) = 1.0_WP
    
    call evaluate(config, rosenbrock, pop)
    
    call tests%real_eq(pop%indivs(1)%f, 1.0_WP, "evaluate, pop%indivs(1)%f")
    call tests%real_eq(pop%indivs(2)%f, 0.0_WP, "evaluate, pop%indivs(2)%f")
end subroutine test_evaluate_1

!subroutine test_evaluate_2(tests)
!    ! Check that `f` and `g_sum` are only set if `set = .false.`.
    
!    use prec, only: WP
!    use ga, only: ga_config, pop_type, evaluate
    
!    type(test_results_type), intent(in out) :: tests
    
!    ! TODO
!end subroutine test_evaluate_2

subroutine test_optimize_ga(tests)
    use prec, only: WP, I10
    use ga, only: ga_config, pop_type, init_pop, optimize_ga
    use purerng, only: rng_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(ga_config) :: config
    type(rng_type)  :: rng
    type(pop_type)  :: pop
    
    integer :: rc
    
    config%n_genes = 2
    allocate(pop%indivs(config%n_pop))
    allocate(pop%indivs(1)%chromo(config%n_genes))
    allocate(pop%indivs(2)%chromo(config%n_genes))
    allocate(config%lb(config%n_genes))
    allocate(config%ub(config%n_genes))
    config%lb(1) = -20.0_WP
    config%ub(1) = 20.0_WP
    config%lb(2) = -20.0_WP
    config%ub(2) = 20.0_WP
    
    call rng%random_seed(put=[2147483562_I10, 2147483398_I10])
    call init_pop(config, rng, pop)
    call optimize_ga(config, rng, rosenbrock, pop, rc)
    
    call tests%integer_eq(rc, 0, "optimize, rc")
    
    ! characterization test
    call tests%real_eq(pop%best_ever_indiv%f, 2.8404480517590853_WP, "optimize, pop%best_ever_indiv%f", abs_tol=1.0e-12_WP)
end subroutine test_optimize_ga

end program test_ga
