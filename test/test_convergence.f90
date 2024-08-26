! tests for the convergence module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_convergence

use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
use fmad, only: ad
use convergence, only: convergence_test
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

call logger%open("convergence.nml")
call tests%start_tests(logger)

call convergence_test([1, 10, 100], fake_de, [1.0_WP], "fake_de test", tests)

call tests%end_tests()
call logger%close()

contains

subroutine fake_de(n, tests, de, de_dv)
    integer, intent(in)                     :: n
    type(test_results_type), intent(in out) :: tests
    type(ad), intent(out), allocatable      :: de(:)
    real(kind=WP), intent(out), allocatable :: de_dv(:, :)
    
    integer, parameter :: N_VAR = 1, N_DV = 1
    
    allocate(de(N_VAR))
    allocate(de_dv(N_VAR, N_DV))
    
    call de%init_const(1.0_WP / real(n, WP), 1)
    de_dv(1, 1) = 2.0_WP / real(n, WP)
    
    call tests%real_eq(1.0_WP, 1.0_WP, "fake_de, fake test")
end subroutine fake_de

end program test_convergence
