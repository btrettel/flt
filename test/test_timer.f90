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
use timer, only: timer_type, timeit, sleep
implicit none

type(log_type), target  :: logger
type(test_results_type) :: tests
type(timer_type)        :: wtime
real(kind=WP)           :: duration_seconds, timeit_duration_seconds

call logger%open("timer.nml")
call tests%start_tests(logger)

call tests%integer_eq(int(wtime%sum_count, kind=kind(1)), 0, "timer_type, default sum_count")
call tests%logical_false(wtime%active, "timer_type, active before start")

call wtime%start()
call sleep(0.1_WP)
call tests%logical_true(wtime%active, "timer_type, active after start")
call wtime%stop()
call tests%logical_false(wtime%active, "timer_type, active after stop")

duration_seconds = wtime%read()
!write(*, "(a, f0.3, a)") "duration=", duration_seconds, "s"
call tests%real_gt(duration_seconds, 0.1_WP, "timer_type, duration, minimum")
call tests%real_eq(duration_seconds, 0.1_WP, "timer_type, duration", abs_tol=0.01_WP)

call tests%integer_ge(int(wtime%sum_count, kind=kind(1)), 1, "timer_type, sum_count after timing")

call wtime%reset()
call tests%integer_eq(int(wtime%sum_count, kind=kind(1)), 0, "timer_type, sum_count after resetting")
call tests%logical_false(wtime%active, "timer_type, active after restart")

timeit_duration_seconds = timeit(timeit_test, number=10)
call tests%real_gt(timeit_duration_seconds, 0.1_WP, "timeit, minimum")
call tests%real_eq(timeit_duration_seconds, 0.1_WP, "timeit (absolute)", abs_tol=0.01_WP)

call tests%end_tests()
call logger%close()

contains

subroutine timeit_test()
    call sleep(0.01_WP)
end subroutine timeit_test

end program test_timer
