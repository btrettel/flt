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
    call tests%logical_false(pop%indivs(1)%f_set, "pop%indivs(1)%fset")
    call tests%logical_false(pop%indivs(2)%f_set, "pop%indivs(2)%fset")
    
    call tests%real_eq(pop%indivs(1)%chromo(1), 0.5_WP, "pop%indivs(1)%chromo(1)")
    call tests%real_eq(pop%indivs(1)%chromo(2), 2.0_WP, "pop%indivs(1)%chromo(2)")
    call tests%real_eq(pop%indivs(1)%chromo(3), 4.0_WP, "pop%indivs(1)%chromo(3)")
    
    call tests%real_eq(pop%indivs(2)%chromo(1), 0.5_WP, "pop%indivs(2)%chromo(1)")
    call tests%real_eq(pop%indivs(2)%chromo(2), 2.0_WP, "pop%indivs(2)%chromo(2)")
    call tests%real_eq(pop%indivs(2)%chromo(3), 4.0_WP, "pop%indivs(2)%chromo(3)")
end subroutine test_init_pop

end program test_ga
