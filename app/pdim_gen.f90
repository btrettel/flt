! Program to generate a module `pdim_types` which allows for compile-time physical dimension consistency checking.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program pdim_gen

use pdim_mod, only: pdim_config_type, write_module, read_config
implicit none

character(len=:), allocatable :: input_file
type(pdim_config_type)        :: config
integer                       :: arg_len, out_unit, rc

call get_command_argument(1, length=arg_len)
allocate(character(len=arg_len) :: input_file)
call get_command_argument(1, value=input_file)

if (len(input_file) == 0) then
    write(unit=*, fmt="(a)") "Usage: pdim_gen FILENAME"
    stop
end if

call read_config(input_file, config, rc)

if (rc /= 0) then
    error stop
end if

open(newunit=out_unit, action="write", status="replace", position="rewind", file=config%output_file)
call write_module(config, out_unit, rc)
close(unit=out_unit)

if (rc /= 0) then
    error stop
end if

call config%logger%close()

! Required to prevent Valgrind from complaining, though I'm not sure this is actually needed.
deallocate(input_file)

end program pdim_gen
