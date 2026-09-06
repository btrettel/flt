! Program to fuzz test Fortran code that uses the geninput namelist code generation module.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program nmlfuzz

use prec, only: CL, WP
use cli, only: get_input_file_name_from_cli
use geninput_io, only: config_type, input_variable_type, read_config_namelist, read_input_variable_namelists, &
                        sort_input_variables
use purerng, only: rng_type
use checks, only: is_close
implicit none

character(len=CL) :: input_file, nml_file
type(config_type) :: config
integer           :: rc_config, rc_input_variables, i_fuzz, out_unit, i_var, x_integer
type(rng_type)    :: rng
character(4)      :: type4
real(WP)          :: x, x_real
type(input_variable_type), allocatable :: input_variables(:)

! Read all namelists and exit if any have issues.
call get_input_file_name_from_cli("nmlfuzz", input_file)

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

call rng%random_seed()

i_fuzz = 0
fuzzer_loop: do
    i_fuzz = i_fuzz + 1
    
    write(unit=nml_file, fmt="(i32.32, a)") i_fuzz, ".nml"
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(nml_file))
    write(unit=out_unit, fmt="(2a)") "&", trim(config%namelist_group)
    var_loop: do i_var = 1, size(input_variables)
        if (.not. is_close(input_variables(i_var)%fuzz_range(1), input_variables(i_var)%fuzz_range(2))) then
            call rng%random_number(x)
            
            type4 = input_variables(i_var)%type_definition(1:4)
            select case (type4)
                case ("inte")
                    x_integer = nint(input_variables(i_var)%fuzz_range(1) &
                                + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1)))
                    
                    write(unit=out_unit, fmt="(2a, i0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                                " ! ", trim(input_variables(i_var)%txt_unit)
                case ("real", "type")
                    x_real = input_variables(i_var)%fuzz_range(1) &
                                + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1))
                    
                    write(unit=out_unit, fmt="(2a, g0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                                " ! ", trim(input_variables(i_var)%txt_unit)
                case ("char")
                    error stop "nmlfuzz can't handle char variables at present: " !&
                                    !// trim(input_variables(i)%variable_name)
                case ("logi")
                    x_integer = nint(input_variables(i_var)%fuzz_range(1) &
                                + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1)))
                    
                    if (x_integer == 0) then
                        write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .false.", &
                                                                " ! ", trim(input_variables(i_var)%txt_unit)
                    else if (x_integer == 1) then
                        write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .true.", &
                                                                " ! ", trim(input_variables(i_var)%txt_unit)
                    else
                        stop "Any element of fuzz_range for logicals must be 0.0 for false or 1.0 for true: " &
                                        // trim(input_variables(i_var)%variable_name)
                    end if
                case default
                    error stop "Invalid type definition: " // trim(input_variables(i_var)%type_definition)
            end select
        end if
    end do var_loop
    write(unit=out_unit, fmt="(a)") "/"
    close(unit=out_unit)
    
    stop
    !call execute_command_line(config%executable // " " // nml_file, exitstat=exit_code)
    
    ! TODO: Check exit code to see if there's a problem.
    
    ! TODO: detect `stop_now` file and quit
end do fuzzer_loop

end program nmlfuzz
