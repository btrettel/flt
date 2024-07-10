! Program to generate a module `units` which allows for compile-time physical dimension consistency checking.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program genunits

use cli, only: get_input_file_name_from_cli
use genunits_data, only: unit_system_type
use genunits_io, only: config_type
implicit none

character(len=:), allocatable :: input_file_name
type(config_type)             :: config
integer                       :: out_unit, rc
type(unit_system_type)        :: unit_system

call get_input_file_name_from_cli("genunits", input_file_name)

call config%read_config_namelist(input_file_name, rc)
if (rc /= 0) then
    error stop
end if

call config%read_seed_unit_namelists(input_file_name, rc)
if (rc /= 0) then
    error stop
end if

call config%generate_system(unit_system)

open(newunit=out_unit, action="write", status="replace", position="rewind", file=config%output_file)
call config%write_module(unit_system, out_unit, rc)
close(unit=out_unit)
if (rc /= 0) then
    error stop
end if

call config%logger%close()

! Required to prevent Valgrind from complaining, though I'm not sure this is actually needed.
deallocate(input_file_name)
deallocate(unit_system%units)

end program genunits
