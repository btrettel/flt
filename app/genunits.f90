! Program to generate a module `units` which allows for compile-time physical dimension consistency checking.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program genunits

use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
use build, only: DEBUG, REVISION, REVISION_DATE, MODIFIED
use genunits_data, only: unit_system_type
use genunits_io, only: config_type
implicit none

character(len=:), allocatable :: input_file, modified_string
type(config_type)             :: config
integer                       :: arg_len, out_unit, rc
type(unit_system_type)        :: unit_system

call get_command_argument(1, length=arg_len)
allocate(character(len=arg_len) :: input_file)
call get_command_argument(1, input_file)

if (len(input_file) == 0) then
    write(unit=*, fmt="(a)") "Usage: genunits FILENAME"
    
    if (MODIFIED) then
       modified_string = ", modified"
    else
       modified_string = "" 
    end if
    
    write(unit=*, fmt="(a)") "Git revision: " // REVISION // " (" // REVISION_DATE // modified_string // ")"
    
    if (DEBUG) then
        write(unit=*, fmt="(a)") "Build: debug"
    else
        write(unit=*, fmt="(a)") "Build: release"
    end if
    
    write(unit=*, fmt="(a, a)") "Compiler: ", compiler_version()
    write(unit=*, fmt="(a, a)") "Compiler flags: ", compiler_options()
    
    stop
end if

call config%read_config_namelist(input_file, rc)
if (rc /= 0) then
    error stop
end if

call config%read_seed_unit_namelists(input_file, rc)
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
deallocate(input_file)
deallocate(unit_system%units)

end program genunits
