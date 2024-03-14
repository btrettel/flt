#include "assert_header.F90"

! `gfortran -cpp -g assert.F90`
! `ifort -fpp assert.F90`
! `flang-7 assert.F90`

program test

assert(.false., "Test message.")

contains

pure subroutine assert_(condition, message, filename, line_number)
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: message, filename
    integer, intent(in)          :: line_number
    
    character(len=8)              :: line_number_string
    character(len=:), allocatable :: full_message
    
    if (.not. condition) then
        write(unit=line_number_string, fmt="(i0)") line_number
        
        full_message = "Assertion failed" // new_line("a") &
                        // "at " // filename // ":" // trim(line_number_string) // new_line("a") &
                        // message
        
        error stop full_message
    end if
end subroutine assert_

end program test
