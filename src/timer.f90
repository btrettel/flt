! Module for wall-clock timers.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module timer

use prec, only: WP
use checks, only: assert
implicit none
private

! <https://j3-fortran.org/doc/year/21/21-117r3.txt>:
! > Many systems have a clock that is too fast to be effectively supported with default integers. Therefore, it is recommended that
! > all invocations of SYSTEM_CLOCK use integer arguments with a decimal exponent range of at least 18.
! > 
! > Using SYSTEM_CLOCK in this manner lets the processor select the most accurate clock available and minimize how often the COUNT
! > value resets to zero.
! <https://fortran-lang.discourse.group/t/proper-usage-of-system-clock/3245/3>
integer, public, parameter :: TIMER_KIND = selected_int_kind(18)

public :: timeit

type, public :: timer_type
    logical                  :: active = .false. ! whether the timer is currently timing
    integer(kind=TIMER_KIND) :: sum_count = 0_TIMER_KIND, & ! time before current timer start
                                start_count, count_max
    real(kind=WP)            :: count_rate
    
    ! nagfor complains if `count_rate` is different type than `count` or `count_max`:
    ! <https://fortran-lang.discourse.group/t/proper-usage-of-system-clock/3245>
    
    ! However, the timer is more accurate with a `real` `count_rate`:
    ! <https://stackoverflow.com/a/54275654/1124489>
contains
    procedure :: start => start_timer
    procedure :: stop  => stop_timer
    procedure :: reset => reset_timer
    procedure :: read  => read_timer
end type timer_type

abstract interface
    subroutine timeit_subroutine()
    end subroutine
end interface

contains

subroutine start_timer(timer)
    use checks, only: assert, is_close
    
    class(timer_type), intent(in out) :: timer
    
    integer(kind=TIMER_KIND) :: start_count, count_max
    real(kind=WP)            :: count_rate
    
    call assert(.not. timer%active, "timer (start_timer): An active timer can not be started.")
    
    timer%active = .true.
    
    call system_clock(start_count, count_rate, count_max)
    
    call assert(count_rate > 0.0_WP)
    call assert(count_max > 0)
    
    ! If the timer was previously used, make sure that the `count_rate` and `count_max` match that previously used.
    if (timer%sum_count /= 0_TIMER_KIND) then
        call assert(timer%count_max == count_max)
        call assert(is_close(timer%count_rate, count_rate))
    end if
    
    timer%start_count = start_count
    timer%count_rate  = count_rate
    timer%count_max   = count_max
end subroutine start_timer

subroutine stop_timer(timer)
    use checks, only: assert, is_close
    
    class(timer_type), intent(in out) :: timer
    
    integer(kind=TIMER_KIND) :: stop_count, count_max, before_count_max
    real(kind=WP)            :: count_rate
    
    call assert(timer%active, "timer (stop_timer): An inactive timer has already stopped.")
    
    timer%active = .false.
    
    call system_clock(stop_count, count_rate, count_max)
    
    call assert(count_rate > 0.0_WP)
    call assert(count_max > 0)
    
    call assert(is_close(timer%count_rate, count_rate))
    call assert(timer%count_max == count_max)
    
    ! Detect if the timer wraps.
    if (stop_count < timer%start_count) then
        ! Then the count wrapped, so adjust count appropriately. This is similar to the following Intel Fortran forum post.
        ! <https://community.intel.com/t5/Intel-Fortran-Compiler/how-to-stop-resetting-the-system-clock/m-p/756868#M12355>
        ! It's not clear to me if this is off by one, but I guess that doesn't matter much.
        
        before_count_max = count_max - timer%start_count
        timer%sum_count = timer%sum_count + stop_count + before_count_max
    else
        ! Then the count did not wrap.
        
        timer%sum_count = timer%sum_count + stop_count - timer%start_count
    end if
end subroutine stop_timer

pure subroutine reset_timer(timer)
    class(timer_type), intent(in out) :: timer
    
    call assert(.not. timer%active, "timer (reset_timer): Timer must be inactive to be reset.")
    
    timer%sum_count = 0_TIMER_KIND
end subroutine reset_timer

pure function read_timer(timer)
    class(timer_type), intent(in) :: timer
    
    real(kind=WP) :: read_timer
    
    call assert(.not. timer%active, "timer (read_timer): Timer must be inactive to be read.")
    
    read_timer = real(timer%sum_count, WP) / timer%count_rate
end function read_timer

function timeit(f, number)
    ! Simplified version of <https://docs.python.org/3/library/timeit.html>.
    
    use prec, only: I9
    
    procedure(timeit_subroutine) :: f
    
    integer(kind=I9), optional :: number
    
    real(kind=WP) :: timeit
    
    type(timer_type) :: wtime
    integer(kind=I9) :: i, number_
    
    if (present(number)) then
        number_ = number
    else
        number_ = 1000000_I9
    end if
    
    call wtime%start()
    do i = 1, number_
        ! Since the subroutine doesn't return anything, won't it be optimized out?
        call f()
    end do
    call wtime%stop()
    
    timeit = wtime%read()
end function timeit

end module timer
