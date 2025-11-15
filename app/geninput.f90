! Program to generate consistent Fortran code to read namelist input, input validation, and documentation for inputs.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program geninput

use cli, only: get_input_file_name_from_cli
use, intrinsic :: iso_fortran_env, only: IOSTAT_END, ERROR_UNIT
use prec, only: CL, WP
use checks, only: assert, check
implicit none

type :: input_parameter_type
    character(len=CL) :: parameter_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value
    logical           :: add_to_type
    logical           :: lower_bound_active
    logical           :: lower_bound_or_equal
    real(WP)          :: lower_bound
    character(len=CL) :: lower_bound_error_message
    logical           :: upper_bound_active
    logical           :: upper_bound_or_equal
    real(WP)          :: upper_bound
    character(len=CL) :: upper_bound_error_message
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
end type input_parameter_type

character(len=:), allocatable           :: input_file, output_file
integer                                 :: rc_config, rc_input_parameters!, out_unit, rc_code
type(input_parameter_type), allocatable :: input_parameters(:)

call get_input_file_name_from_cli("geninput", input_file)

print *, "Input file: ", trim(input_file)

! Read all namelists and exit if any have issues.
call read_config_namelist(input_file, output_file, rc_config)
if (rc_config /= 0) then
    error stop
end if

call read_input_parameter_namelists(input_file, input_parameters, rc_input_parameters)
if (rc_input_parameters /= 0) then
    error stop
end if

print *, size(input_parameters)

!open(newunit=out_unit, action="write", status="replace", position="rewind", file=trim(output_file))
!call write_input_code(input_parameters, out_unit, rc_code)
!close(unit=out_unit)
!if (rc_module /= 0) then
!    error stop
!end if

contains

subroutine read_config_namelist(input_file, output_file_, rc)
    character(len=*), intent(in)               :: input_file
    character(len=:), allocatable, intent(out) :: output_file_
    integer, intent(out)                       :: rc
    
    integer           :: nml_unit, rc_nml
    character(len=CL) :: output_file, nml_error_message
    
    namelist /config/ output_file
    
    output_file = ""
    
    open(newunit=nml_unit, file=trim(input_file), status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
        rc = rc_nml
        return
    end if
    
    rc = 0
    call check(len(trim(output_file)) > 0, "output_file must be defined", rc)
    
    ! Convert to allocatable.
    output_file_ = trim(output_file)
end subroutine read_config_namelist

subroutine read_input_parameter_namelists(input_file, input_parameters, rc)
    character(len=*), intent(in)                         :: input_file
    type(input_parameter_type), allocatable, intent(out) :: input_parameters(:)
    integer, intent(out)                                 :: rc
    
    integer           :: nml_unit, rc_nml, n_input_parameters, i_input_parameter, n_failures
    character(len=CL) :: nml_error_message
    character(len=3)  :: i_input_parameter_string
    
    ! `seed_unit` namelist group
    character(len=CL) :: parameter_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value
    logical           :: add_to_type
    logical           :: lower_bound_active
    logical           :: lower_bound_or_equal
    real(WP)          :: lower_bound
    character(len=CL) :: lower_bound_error_message
    logical           :: upper_bound_active
    logical           :: upper_bound_or_equal
    real(WP)          :: upper_bound
    character(len=CL) :: upper_bound_error_message
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
    
    namelist /input_parameter/ parameter_name, type_definition, default_value, add_to_type, &
                                lower_bound_active, lower_bound_or_equal, lower_bound, lower_bound_error_message, &
                                upper_bound_active, upper_bound_or_equal, upper_bound, upper_bound_error_message, &
                                tex_unit, tex_description
    
    open(newunit=nml_unit, file=input_file, status="old", action="read", delim="quote")
    
    ! First get `n_input_parameters`, allocate `input_parameters`, and read everything in.
    n_input_parameters = 0
    n_failures         = 0
    do ! SERIAL
        read(unit=nml_unit, nml=input_parameter, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        n_input_parameters = n_input_parameters + 1
    end do
    
    call assert(n_input_parameters >= 0, "geninput (read_input_parameter_namelists): n_input_parameters >= 0 violated")
    call check(n_input_parameters > 0, "At least one seed unit is required.", n_failures)
    
    ! Once the arrays are sized properly, go back and read all of the seed units.
    rewind nml_unit
    allocate(input_parameters(n_input_parameters))
    i_input_parameter = 0
    do ! SERIAL
        parameter_name            = ""
        type_definition           = ""
        default_value             = ""
        add_to_type               = .true.
        lower_bound_active        = .false.
        lower_bound_or_equal      = .false.
        lower_bound               = 0.0_WP
        lower_bound_error_message = ""
        upper_bound_active        = .false.
        upper_bound_or_equal      = .false.
        upper_bound               = 0.0_WP
        upper_bound_error_message = ""
        tex_unit                  = ""
        tex_description           = ""
        read(unit=nml_unit, nml=input_parameter, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        i_input_parameter = i_input_parameter + 1
        
        input_parameters(i_input_parameter)%parameter_name            = trim(parameter_name)
        input_parameters(i_input_parameter)%type_definition           = trim(type_definition)
        input_parameters(i_input_parameter)%default_value             = trim(default_value)
        input_parameters(i_input_parameter)%add_to_type               = add_to_type
        input_parameters(i_input_parameter)%lower_bound_active        = lower_bound_active
        input_parameters(i_input_parameter)%lower_bound_or_equal      = lower_bound_or_equal
        input_parameters(i_input_parameter)%lower_bound               = lower_bound
        input_parameters(i_input_parameter)%lower_bound_error_message = trim(lower_bound_error_message)
        input_parameters(i_input_parameter)%upper_bound_active        = upper_bound_active
        input_parameters(i_input_parameter)%upper_bound_or_equal      = upper_bound_or_equal
        input_parameters(i_input_parameter)%upper_bound               = upper_bound
        input_parameters(i_input_parameter)%upper_bound_error_message = trim(upper_bound_error_message)
        input_parameters(i_input_parameter)%tex_unit                  = trim(tex_unit)
        input_parameters(i_input_parameter)%tex_description           = trim(tex_description)
        
        write(unit=i_input_parameter_string, fmt="(i0)") i_input_parameter
        
        call check(len(trim(parameter_name)) > 0, "input_parameter #" // trim(i_input_parameter_string) &
                                                // "has an empty parameter_name.", n_failures)
        call check(len(trim(type_definition)) > 0, "input_parameter #" // trim(i_input_parameter_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' has an empty type_definition.", n_failures)
        call check(len(trim(tex_unit)) > 0, "input_parameter #" // trim(i_input_parameter_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' has an empty tex_unit.", n_failures)
        call check(len(trim(tex_description)) > 0, "input_parameter #" // trim(i_input_parameter_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' has an empty tex_description.", n_failures)
        
        call check(.not. ((.not. lower_bound_active) .and. lower_bound_or_equal), &
                    "input_parameter #" // trim(i_input_parameter_string) &
                    // " with parameter_name '" // trim(parameter_name) &
                    // "': lower_bound_or_equal can not be .true. unless lower_bound_active=.true.", n_failures)
        
        call check(.not. ((.not. upper_bound_active) .and. upper_bound_or_equal), &
                    "input_parameter #" // trim(i_input_parameter_string) &
                    // " with parameter_name '" // trim(parameter_name) &
                    // "': upper_bound_or_equal can not be .true. unless upper_bound_active=.true.", n_failures)
        
        if (lower_bound_active .and. upper_bound_active) then
            call check(lower_bound < upper_bound, "input_parameter #" // trim(i_input_parameter_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "': lower_bound < upper_bound violated.", n_failures)
        end if
    end do
    close(unit=nml_unit)
    
    call assert(n_failures >= 0, "geninput (read_input_parameter_namelists): n_failures is negative")
    if (n_failures > 0) then
        write(unit=ERROR_UNIT, fmt="(a)") "input_parameter namelist input validation error(s)"
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_input_parameter_namelists

end program geninput
