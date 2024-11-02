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
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests

call logger%open("convergence.nml")
call tests%start_tests(logger)

call test_convergence_test(tests)
call test_logspace(tests)
call test_dnorm(tests)

call tests%end_tests()
call logger%close()

contains

subroutine fake_de(n, tests, de, de_dv)
    integer, intent(in)                     :: n
    type(test_results_type), intent(in out) :: tests
    type(ad), intent(out), allocatable      :: de(:)
    real(WP), intent(out), allocatable      :: de_dv(:, :)
    
    integer, parameter :: N_VAR = 1, N_DV = 1
    
    allocate(de(N_VAR))
    allocate(de_dv(N_VAR, N_DV))
    
    call de%init_const(1.0_WP / real(n, WP), 1)
    de_dv(1, 1) = 2.0_WP / real(n, WP)
    
    call tests%real_eq(1.0_WP, 1.0_WP, "fake_de, fake test")
end subroutine fake_de

subroutine fake_de2(n, tests, de, de_dv)
    integer, intent(in)                     :: n
    type(test_results_type), intent(in out) :: tests
    type(ad), intent(out), allocatable      :: de(:)
    real(WP), intent(out), allocatable      :: de_dv(:, :)
    
    integer, parameter :: N_VAR = 2, N_DV = 1
    
    integer :: i_var
    
    allocate(de(N_VAR))
    allocate(de_dv(N_VAR, N_DV))
    
    do i_var = 1, N_VAR
        call de(i_var)%init_const(1.0_WP / real(n, WP), 1)
        de_dv(i_var, 1) = 2.0_WP / real(n, WP)
    end do
    
    call tests%real_eq(1.0_WP, 1.0_WP, "fake_de2, fake test")
end subroutine fake_de2

subroutine test_convergence_test(tests)
    use convergence, only: convergence_test
    use nmllog, only: CRITICAL_LEVEL
    
    type(test_results_type), intent(in out) :: tests
    
    type(test_results_type) :: failing_tests
    
    integer, parameter :: N_TESTS = 6, N_FAILING = 2
    integer :: stdout_level
    
    call convergence_test([1, 10, 100], fake_de, [1.0_WP], "fake_de, passing", tests)
    
    ! `convergence_test` that fails
    call failing_tests%start_tests(logger)
    stdout_level = failing_tests%logger%stdout_level
    failing_tests%logger%stdout_level = CRITICAL_LEVEL + 1! Don't print these to stdout.
    call convergence_test([1, 10, 100], fake_de, [2.0_WP], "fake_de, failing", failing_tests)
    failing_tests%logger%stdout_level = stdout_level
    
    call tests%integer_eq(failing_tests%n_tests, N_TESTS, "correct number of tests expected to fail")

    call tests%integer_eq(failing_tests%n_failures, N_FAILING, &
                                    "correct number of tests expected to fail that fail")
    
    ! To check that the output looks correct.
    ! I want `n` to be printed once per `solver_de` subroutine call.
    call convergence_test([1, 10, 100], fake_de2, [1.0_WP, 1.0_WP], "fake_de2, passing", tests)
end subroutine test_convergence_test

subroutine test_logspace(tests)
    use convergence, only: logspace
    
    type(test_results_type), intent(in out) :: tests
    
    integer, allocatable :: n(:)
    
    n = logspace(0.0_WP, 1.0_WP, 3)
    call tests%integer_eq(size(n), 3, "logspace, size")
    call tests%integer_eq(minval(n), 1, "logspace, minval")
    call tests%integer_eq(maxval(n), 10, "logspace, maxval")
    call tests%integer_eq(n(1), 1, "logspace, index 1")
    call tests%integer_eq(n(2), 3, "logspace, index 2")
    call tests%integer_eq(n(3), 10, "logspace, index 3")
end subroutine test_logspace

subroutine test_dnorm(tests)
    use convergence, only: dnorm
    
    type(test_results_type), intent(in out) :: tests
    
    real(WP), allocatable :: x(:)
    type(ad), allocatable :: y(:)
    
    integer, parameter :: N_DV = 1
    
    real(WP) :: dnorm_x, dnorm_exact
    type(ad) :: dnorm_y
    integer  :: i
    
    dnorm_exact = 1.5_WP
    allocate(x(3))
    x = dnorm_exact
    allocate(y(size(x)))
    do i = 1, size(x)
        call y(i)%init_const(x(i), N_DV)
    end do
    dnorm_x = dnorm(x)
    dnorm_y = dnorm(y)
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=2, uniform)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=2, uniform)")
    deallocate(x, y)
    
    dnorm_exact = sqrt((1.0_WP + 2.0_WP**2) / 3.0_WP)
    x = [-1.0_WP, 0.0_WP, 2.0_WP]
    allocate(y(size(x)))
    do i = 1, size(x)
        call y(i)%init_const(x(i), N_DV)
    end do
    dnorm_x = dnorm(x)
    dnorm_y = dnorm(y)
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=2, non-uniform, 1)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=2, non-uniform, 1)")
    dnorm_exact = 1.0_WP
    dnorm_x = dnorm(x, ord=1)
    dnorm_y = dnorm(y, ord=1)
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=1, non-uniform, 1)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=1, non-uniform, 1)")
    dnorm_exact = 2.0_WP
    dnorm_x = dnorm(x, ord=huge(1))
    dnorm_y = dnorm(y, ord=huge(1))
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=huge(1), non-uniform, 1)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=huge(1), non-uniform, 1)")
    deallocate(x, y)
    
    dnorm_exact = sqrt((1.0_WP + 2.0_WP**2) / 3.0_WP)
    x = [-2.0_WP, 1.0_WP, 0.0_WP]
    allocate(y(size(x)))
    do i = 1, size(x)
        call y(i)%init_const(x(i), N_DV)
    end do
    dnorm_x = dnorm(x)
    dnorm_y = dnorm(y)
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=2, non-uniform, 2)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=2, non-uniform, 2)")
    dnorm_exact = 1.0_WP
    dnorm_x = dnorm(x, ord=1)
    dnorm_y = dnorm(y, ord=1)
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=1, non-uniform, 2)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=1, non-uniform, 2)")
    dnorm_exact = 2.0_WP
    dnorm_x = dnorm(x, ord=huge(1))
    dnorm_y = dnorm(y, ord=huge(1))
    call tests%real_eq(dnorm_x, dnorm_exact, "dnorm (real, ord=huge(1), non-uniform, 2)")
    call tests%real_eq(dnorm_y%v, dnorm_exact, "dnorm (ad, ord=huge(1), non-uniform, 2)")
    deallocate(x, y)
end subroutine test_dnorm

end program test_convergence
