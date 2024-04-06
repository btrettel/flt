! tests for the timer module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_timer

use nmllog, only: log_type
use prec, only: WP
use unittest, only: test_results_type
use timer, only: timer_type, timeit
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(timer_type)        :: wtime
real(kind=WP)           :: duration_seconds, timeit_duration_seconds

integer :: i, j
integer, parameter :: N = 2000
real(kind=WP) :: a(N, N)

call logger%open("timer.nml")
call tests%start_tests(logger)

call tests%integer_eq(int(wtime%sum_count, kind=kind(1)), 0, "timer_type, default sum_count")
call tests%logical_false(wtime%active, "timer_type, active before start")

call wtime%start()
call random_number(a)
do i = 1, N
    do j = 1, N
        a(i, j) = a(i, j)**2 + a(1, 1)
    end do
end do
call tests%logical_true(wtime%active, "timer_type, active after start")
call wtime%stop()
call tests%logical_false(wtime%active, "timer_type, active after stop")

duration_seconds = wtime%read()
!write(*, "(a,f0.3,a)") "duration=", duration_seconds, "s"
call tests%real_gt(duration_seconds, 0.0_WP, "timer_type, duration")

call tests%integer_ge(int(wtime%sum_count, kind=kind(1)), 1, "timer_type, sum_count after timing")

call wtime%reset()
call tests%integer_eq(int(wtime%sum_count, kind=kind(1)), 0, "timer_type, sum_count after resetting")

timeit_duration_seconds = timeit(timeit_test, number=5)
call tests%real_gt(timeit_duration_seconds, 0.0_WP, "timeit")

call tests%end_tests()
call logger%close()

contains

subroutine timeit_test()
    print *, 5
end subroutine timeit_test

end program test_timer
