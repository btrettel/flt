! Program to fuzz test Fortran code that uses the geninput namelist code generation module.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program nmlfuzz

use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
use prec, only: CL, WP
use cli, only: get_input_file_name_from_cli
use geninput_io, only: config_type, input_variable_type, read_config_namelist, read_input_variable_namelists, &
                        sort_input_variables
use purerng, only: rng_type
use checks, only: is_close
use stopcodes, only: EX_OK
implicit none

character(len=CL) :: input_file, nml_file
type(config_type) :: config
integer           :: rc_config, rc_input_variables, i_fuzz, out_unit, i_var, x_integer, exit_code, i_exit_code
type(rng_type)    :: rng
character(4)      :: type4
real(WP)          :: x, x_real
logical           :: exit_code_is_acceptable, stop_now_detected
type(input_variable_type), allocatable :: input_variables(:)
character(len=*), parameter            :: STOP_NOW_FILE = "stop_now"

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
        if (input_variables(i_var)%fuzz) then
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
    
    call execute_command_line(config%executable // " " // nml_file, exitstat=exit_code)
    
    ! Check exit code to see if it's acceptable.
    exit_code_is_acceptable = .false.
    exit_code_loop: do i_exit_code = 1, size(config%acceptable_exit_codes)
        if (exit_code == config%acceptable_exit_codes(i_exit_code)) then
            exit_code_is_acceptable = .true.
            exit exit_code_loop
        end if
    end do exit_code_loop
    
    if (exit_code_is_acceptable) then
        open(newunit=out_unit, status="old", file=trim(nml_file))
        close(unit=out_unit, status="delete")
    else
        print "(2a)", "Failure detected, file kept: ", trim(nml_file)
    end if
    
    ! detect `stop_now` file and quit if found
    inquire(file=STOP_NOW_FILE, exist=stop_now_detected)
    if (stop_now_detected) then
        open(newunit=out_unit, status="old", file=STOP_NOW_FILE)
        close(unit=out_unit, status="delete")
        write(unit=OUTPUT_UNIT, fmt="(a)") STOP_NOW_FILE // " detected, terminating."
        stop EX_OK, quiet=.true.
    end if
end do fuzzer_loop

end program nmlfuzz
