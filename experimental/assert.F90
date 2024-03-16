#include "assert.h"

! `gfortran -std=f2018 -cpp -g assert.F90`
! `ifx -stand:f18 -traceback -fpp assert.F90`
! `nvfortran -Mpreprocess -g assert.F90`

program test

implicit none

integer :: x, y

x = 1
y = 2

assert(x == y)
!assert_info(x == y, "Test message.")

! TODO: can pass in array of values to print as namelists don't work in nvfortran.
! TODO: return code with filename and line number

contains

pure subroutine assert_(condition, filename, line_number, info)
    ! This doesn't print the condition as that can be looked up with the filename and line number.
    ! If you want a custom message, add it as a code comment and look it up from the filename and line number.
    
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: line_number
    
    character(len=*), intent(in), optional :: info
    
    character(len=8)              :: line_number_string
    character(len=:), allocatable :: info_, full_message
    
    if (present(info)) then
        info_ = new_line("a") // info
    else
        info_ = ""
    end if
    
    if (.not. condition) then
        write(unit=line_number_string, fmt="(i0)") line_number
        
        ! Why not concatenate the strings on the `error stop` line?
        ! That leads to ifx garbling the error message as of version `ifx (IFX) 2024.0.2 20231213`.
        full_message = "***" // new_line("a") &
                    // "Assertion failed at " // filename // ":" // trim(line_number_string) // "." &
                    // info_
        
        error stop full_message
    end if
end subroutine assert_

end program test
