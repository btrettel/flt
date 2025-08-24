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
integer                       :: out_unit, rc_config, rc_seed, rc_module
type(unit_system_type)        :: unit_system

call get_input_file_name_from_cli("genunits", input_file_name)

! Read all namelists and exit if any have issues.
call config%read_config_namelist(input_file_name, rc_config)
call config%read_seed_unit_namelists(input_file_name, rc_seed)
if ((rc_config /= 0) .or. (rc_seed /= 0)) then
    error stop
end if

call config%generate_system(unit_system)

open(newunit=out_unit, action="write", status="replace", position="rewind", file=config%output_file)
call config%write_module(unit_system, out_unit, rc_module)
close(unit=out_unit)
if (rc_module /= 0) then
    error stop
end if

end program genunits
