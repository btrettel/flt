! Program to generate consistent Fortran code to read namelist input, input validation, and documentation for inputs.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program geninput

use prec, only: CL
use geninput_io
use cli, only: get_input_file_name_from_cli
implicit none

character(len=CL)                      :: input_file
integer                                :: rc_config, rc_input_variables
type(config_type)                      :: config
type(input_variable_type), allocatable :: input_variables(:)

call get_input_file_name_from_cli("geninput", input_file)

! Read all namelists and exit if any have issues.
call read_config_namelist(input_file, config, rc_config)
if (rc_config /= 0) then
    error stop
end if

call read_input_variable_namelists(input_file, input_variables, rc_input_variables)
if (rc_input_variables /= 0) then
    error stop
end if

call sort_input_variables(input_variables)

if (config%use_type) call write_type(config, input_variables)
call write_subroutine(config, input_variables)

if (config%write_tex) call write_tex(config, input_variables)
! TODO if (config%write_md) call write_md(config, input_variables)

end program geninput
