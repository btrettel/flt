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
use checks, only: assert, check, is_close
implicit none

type :: config_type
    character(len=CL) :: output_file_prefix
    character(len=CL) :: namelist_group
    character(len=CL) :: type_name
    character(len=CL) :: config_variable
    character(len=CL) :: kind_parameter
    logical           :: write_tex, write_md ! whether to enable writing TeX or Markdown documentation
    logical           :: uq, ga ! whether to enable uncertainty quantification or the genetic algorithm respectively
    
    ! Write code for multiple namelist groups of the same name, like in `read_input_parameter_namelists` here
    ! TODO: `logical :: multiple_namelist_groups`
end type config_type

type :: input_parameter_type
    character(len=CL) :: parameter_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value ! do not include the kind parameter as this will be added automatically
    logical           :: required
    logical           :: add_to_type
    logical           :: lower_bound_active
    logical           :: lower_bound_not_equal
    real(WP)          :: lower_bound
    character(len=CL) :: lower_bound_error_message
    logical           :: upper_bound_active
    logical           :: upper_bound_not_equal
    real(WP)          :: upper_bound
    character(len=CL) :: upper_bound_error_message
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
end type input_parameter_type

integer, parameter :: MAX_LINE_LENGTH = 132

character(len=:), allocatable           :: input_file
integer                                 :: rc_config, rc_input_parameters
type(config_type)                       :: config
type(input_parameter_type), allocatable :: input_parameters(:)

call get_input_file_name_from_cli("geninput", input_file)

! Read all namelists and exit if any have issues.
call read_config_namelist(input_file, config, rc_config)
if (rc_config /= 0) then
    error stop
end if

call read_input_parameter_namelists(input_file, input_parameters, rc_input_parameters)
if (rc_input_parameters /= 0) then
    error stop
end if

call sort_input_parameters(input_parameters)

call write_type(config, input_parameters)
call write_subroutine(config, input_parameters)

! TODO if (config%write_tex) call write_tex(config, input_parameters)
! TODO if (config%write_md) call write_md(config, input_parameters)

contains

subroutine read_config_namelist(input_file, config, rc)
    character(len=*), intent(in)   :: input_file
    type(config_type), intent(out) :: config
    integer, intent(out)           :: rc
    
    integer           :: nml_unit, rc_nml
    character(len=CL) :: nml_error_message
    
    ! `config` namelist group
    ! See type definition above for some comments on these.
    character(len=CL) :: output_file_prefix
    character(len=CL) :: namelist_group
    character(len=CL) :: type_name
    character(len=CL) :: config_variable
    character(len=CL) :: kind_parameter
    logical           :: write_tex, write_md
    logical           :: uq, ga
    
    namelist /geninput_config/ output_file_prefix, namelist_group, type_name, config_variable, kind_parameter
    
    output_file_prefix = ""
    namelist_group     = ""
    type_name          = ""
    config_variable    = ""
    kind_parameter     = ""
    write_tex          = .false.
    write_md           = .false.
    uq                 = .false.
    ga                 = .false.
    
    open(newunit=nml_unit, file=trim(input_file), status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=geninput_config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
        rc = rc_nml
        return
    end if
    
    rc = 0
    call check(len(trim(output_file_prefix)) > 0, "output_file_prefix must be defined", rc)
    call check(len(trim(namelist_group)) > 0, "namelist_group must be defined", rc)
    call check(len(trim(type_name)) > 0, "type_name must be defined", rc)
    call check(len(trim(config_variable)) > 0, "config_variable must be defined", rc)
    
    config%output_file_prefix = trim(output_file_prefix)
    config%namelist_group     = trim(namelist_group)
    config%type_name          = trim(type_name)
    config%config_variable    = trim(config_variable)
    config%kind_parameter     = trim(kind_parameter)
    config%write_tex          = write_tex
    config%write_md           = write_md
    config%uq                 = uq
    config%ga                 = ga
end subroutine read_config_namelist

subroutine read_input_parameter_namelists(input_file, input_parameters, rc)
    character(len=*), intent(in)                         :: input_file
    type(input_parameter_type), allocatable, intent(out) :: input_parameters(:)
    integer, intent(out)                                 :: rc
    
    integer           :: nml_unit, rc_nml, n_input_parameters, i, j, n_failures, default_value_integer
    character(len=CL) :: nml_error_message
    character(len=3)  :: i_string, j_string
    logical           :: outside_of_lower_bound, outside_of_upper_bound
    real(WP)          :: default_value_real
    
    ! `input_parameter` namelist group
    character(len=CL) :: parameter_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value
    logical           :: required
    logical           :: add_to_type
    logical           :: lower_bound_active
    logical           :: lower_bound_not_equal
    real(WP)          :: lower_bound
    character(len=CL) :: lower_bound_error_message
    logical           :: upper_bound_active
    logical           :: upper_bound_not_equal
    real(WP)          :: upper_bound
    character(len=CL) :: upper_bound_error_message
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
    
    namelist /input_parameter/ parameter_name, type_definition, default_value, required, add_to_type, &
                                lower_bound_active, lower_bound_not_equal, lower_bound, lower_bound_error_message, &
                                upper_bound_active, upper_bound_not_equal, upper_bound, upper_bound_error_message, &
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
    i = 0
    do ! SERIAL
        parameter_name            = ""
        type_definition           = ""
        default_value             = ""
        required                  = .false.
        add_to_type               = .true.
        lower_bound_active        = .false.
        lower_bound_not_equal     = .false.
        lower_bound               = 0.0_WP
        lower_bound_error_message = ""
        upper_bound_active        = .false.
        upper_bound_not_equal     = .false.
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
        
        i = i + 1
        
        input_parameters(i)%parameter_name            = trim(parameter_name)
        input_parameters(i)%type_definition           = trim(type_definition)
        input_parameters(i)%default_value             = trim(default_value)
        input_parameters(i)%required                  = required
        input_parameters(i)%add_to_type               = add_to_type
        input_parameters(i)%lower_bound_active        = lower_bound_active
        input_parameters(i)%lower_bound_not_equal     = lower_bound_not_equal
        input_parameters(i)%lower_bound               = lower_bound
        input_parameters(i)%lower_bound_error_message = trim(lower_bound_error_message)
        input_parameters(i)%upper_bound_active        = upper_bound_active
        input_parameters(i)%upper_bound_not_equal     = upper_bound_not_equal
        input_parameters(i)%upper_bound               = upper_bound
        input_parameters(i)%upper_bound_error_message = trim(upper_bound_error_message)
        input_parameters(i)%tex_unit                  = trim(tex_unit)
        input_parameters(i)%tex_description           = trim(tex_description)
        
        write(unit=i_string, fmt="(i0)") i
        
        call check(len(trim(parameter_name)) > 0, "input_parameter #" // trim(i_string) &
                                                // "has an empty parameter_name.", n_failures)
        call check(len(trim(type_definition)) > 0, "input_parameter #" // trim(i_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' has an empty type_definition.", n_failures)
        call check(len(trim(tex_description)) > 0, "input_parameter #" // trim(i_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' has an empty tex_description.", n_failures)
        
        call check(.not. ((.not. lower_bound_active) .and. lower_bound_not_equal), &
                    "input_parameter #" // trim(i_string) &
                    // " with parameter_name '" // trim(parameter_name) &
                    // "': lower_bound_not_equal can not be .true. unless lower_bound_active=.true.", n_failures)
        
        call check(.not. ((.not. upper_bound_active) .and. upper_bound_not_equal), &
                    "input_parameter #" // trim(i_string) &
                    // " with parameter_name '" // trim(parameter_name) &
                    // "': upper_bound_not_equal can not be .true. unless upper_bound_active=.true.", n_failures)
        
        if (lower_bound_active .and. upper_bound_active) then
            call check(lower_bound < upper_bound, "input_parameter #" // trim(i_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "': lower_bound < upper_bound violated.", n_failures)
        end if
        
        if (((input_parameters(i)%type_definition(1:4) == "real") &
                .or. (input_parameters(i)%type_definition(1:4) == "type") &
                .or. (input_parameters(i)%type_definition(1:4) == "inte")) &
                    .and. input_parameters(i)%required) then
            call check(len(trim(tex_unit)) > 0, "input_parameter #" // trim(i_string) &
                                                    // " with parameter_name '" // trim(parameter_name) &
                                                    // "' is numeric and has an empty tex_unit.", n_failures)
            call check(input_parameters(i)%lower_bound_active .or. input_parameters(i)%upper_bound_active, &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': one or more bounds must be set if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            call check(len(trim(input_parameters(i)%default_value)) > 0, &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': default_value must be set if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            if ((input_parameters(i)%lower_bound_active .or. input_parameters(i)%upper_bound_active) &
                    .and. (len(trim(input_parameters(i)%default_value)) > 0)) then
                if (input_parameters(i)%type_definition(1:4) == "inte") then
                    read(unit=input_parameters(i)%default_value, fmt="(i8)") default_value_integer
                    outside_of_lower_bound = default_value_integer < nint(input_parameters(i)%lower_bound)
                    outside_of_upper_bound = default_value_integer > nint(input_parameters(i)%upper_bound)
                else
                    read(unit=input_parameters(i)%default_value, fmt="(f16.8)") default_value_real
                    outside_of_lower_bound = default_value_real < input_parameters(i)%lower_bound
                    outside_of_upper_bound = default_value_real > input_parameters(i)%upper_bound
                end if
                
                call check(outside_of_lower_bound .or. outside_of_upper_bound, &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': default_value must be outside bounds if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            end if
        end if
        
        if (input_parameters(i)%type_definition(1:4) == "logi") then
            call check(len(trim(input_parameters(i)%default_value)) > 0, &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': default_value must be set for logical input parameters.", n_failures)
            call check(.not. input_parameters(i)%required, &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': logical parameters can not be required as there is no way to check in a Fortran namelist.", &
                        n_failures)
        end if
        
        if ((input_parameters(i)%type_definition(1:4) == "inte") .and. input_parameters(i)%lower_bound_active) then
            call check(is_close(input_parameters(i)%lower_bound, real(nint(input_parameters(i)%lower_bound), WP)), &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': integer input parameter must have integer lower bound.", n_failures)
        end if
        
        if ((input_parameters(i)%type_definition(1:4) == "inte") .and. input_parameters(i)%upper_bound_active) then
            call check(is_close(input_parameters(i)%upper_bound, real(nint(input_parameters(i)%upper_bound), WP)), &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "': integer input parameter must have integer upper bound.", n_failures)
        end if
        
        ! I could check that the `default_value` for an integer is an integer.
        ! But apparently `-1` converted to `(f16.8)` format is 0.0!
        ! I'd need to add a decimal point to get the right number.
        
        do j = 1, i - 1
            write(unit=j_string, fmt="(i0)") j
            call check(trim(input_parameters(j)%parameter_name) /= trim(input_parameters(i)%parameter_name), &
                        "input_parameter #" // trim(i_string) &
                        // " with parameter_name '" // trim(parameter_name) &
                        // "' has the same parameter_name as " &
                        // "input_parameter #" // trim(j_string) // ".", &
                        n_failures)
        end do
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

subroutine write_type(config, input_parameters)
    type(config_type), intent(in)                       :: config
    type(input_parameter_type), allocatable, intent(in) :: input_parameters(:)
    
    integer :: out_unit, n, i
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(config%output_file_prefix) // "_type.f90")
    
    n = size(input_parameters)
    
    write(unit=out_unit, fmt="(a)") "type :: " // trim(config%type_name)
    do i = 1, n
        write(unit=out_unit, fmt="(a)") "    " // trim(input_parameters(i)%type_definition) &
                                            // " :: " // trim(input_parameters(i)%parameter_name)
    end do
    write(unit=out_unit, fmt="(a)") "end type " // trim(config%type_name)
    
    close(unit=out_unit)
end subroutine write_type

subroutine write_subroutine(config, input_parameters)
    type(config_type), intent(in)                       :: config
    type(input_parameter_type), allocatable, intent(in) :: input_parameters(:)
    
    integer       :: out_unit, n, i, line_length
    character(CL) :: type_definition, line, default_value, underscore_kind_parameter, bound_string
    character(4)  :: type4
    character(2)  :: op
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(config%output_file_prefix) // "_subroutine.f90")
    
    n = size(input_parameters)
    
    write(unit=out_unit, fmt="(a)") "integer :: nml_unit, rc_nml"
    write(unit=out_unit, fmt="(a)") "character(len=CL) :: nml_error_message"
    write(unit=out_unit, fmt="(a)") ""
    
    write(unit=out_unit, fmt="(a)") "! `" // trim(config%namelist_group) // "` namelist group"
    do i = 1, n
        ! genunits types should be converted to `real` for the namelists
        if (input_parameters(i)%type_definition(1:4) == "type") then
            type_definition = "real(WP)" ! TODO: Assumes `WP`
            if (len(trim(config%kind_parameter)) == 0) then
                type_definition = "real(" // trim(config%kind_parameter) // ")"
            else
                type_definition = "real"
            end if
        else
            type_definition = input_parameters(i)%type_definition
        end if
        
        write(unit=out_unit, fmt="(a)") trim(type_definition) &
                                            // " :: " // trim(input_parameters(i)%parameter_name)
    end do
    write(unit=out_unit, fmt="(a)") ""
    
    line = "namelist /" // trim(config%namelist_group) // "/"
    write(unit=out_unit, fmt="(a)", advance="no") trim(line) // " "
    line_length = len(trim(line)) + 1
    do i = 1, n
        line_length = line_length + len(trim(input_parameters(i)%parameter_name))
        if (line_length > (MAX_LINE_LENGTH - 3)) then ! The 3 accounts for `, &`
            write(unit=out_unit, fmt="(a)") "&"
            line_length = 4 + len(trim(input_parameters(i)%parameter_name))
            write(unit=out_unit, fmt="(a)", advance="no") "    "
        end if
        
        write(unit=out_unit, fmt="(a)", advance="no") trim(input_parameters(i)%parameter_name)
        
        if (i /= n) then
            write(unit=out_unit, fmt="(a)", advance="no") ", "
            line_length = line_length + 2
        else
            write(unit=out_unit, fmt="(a)") ""
        end if
    end do
    
    write(unit=out_unit, fmt="(a)") ""
    write(unit=out_unit, fmt="(a)") "! defaults"
    do i = 1, n
        if (len(trim(input_parameters(i)%default_value)) == 0) then
            type4 = input_parameters(i)%type_definition(1:4)
            select case (type4)
                case ("inte")
                    default_value = "0"
                case ("real", "type")
                    if (len(trim(config%kind_parameter)) == 0) then
                        default_value = "0.0_" // trim(config%kind_parameter)
                    else
                        default_value = "0.0"
                    end if
                case ("char")
                    default_value = '""'
                case ("logi")
                    error stop "logical variables must have default_value set"
                case default
                    error stop "Invalid type definition: " // trim(input_parameters(i)%type_definition)
            end select
        else
            default_value = input_parameters(i)%default_value
        end if
        
        write(unit=out_unit, fmt="(a)") trim(input_parameters(i)%parameter_name) // " = " // trim(default_value)
    end do
    write(unit=out_unit, fmt="(a)") ""
    
    write(unit=out_unit, fmt="(a)") 'open(newunit=nml_unit, file=trim(input_file), status="old", action="read", delim="quote")'
    write(unit=out_unit, fmt="(a)") "read(unit=nml_unit, nml=" // trim(config%namelist_group) &
                                        // ", iostat=rc_nml, iomsg=nml_error_message)"
    write(unit=out_unit, fmt="(a)") "close(unit=nml_unit)"
    write(unit=out_unit, fmt="(a)") ""
    write(unit=out_unit, fmt="(a)") "if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then"
    write(unit=out_unit, fmt="(a)") '    write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)'
    write(unit=out_unit, fmt="(a)") "    rc = rc_nml"
    write(unit=out_unit, fmt="(a)") "    return"
    write(unit=out_unit, fmt="(a)") "end if"
    write(unit=out_unit, fmt="(a)") ""
    
    write(unit=out_unit, fmt="(a)") "rc = 0"
    write(unit=out_unit, fmt="(a)") ""
    
    ! Check if required input parameters are defined.
    do i = 1, n
        ! Check that required strings have greater than zero length.
        if ((input_parameters(i)%type_definition(1:4) == "char") .and. input_parameters(i)%required) then
            write(unit=out_unit, fmt="(a)") "call check(len(trim(" // trim(input_parameters(i)%parameter_name) &
                    // ")) > 0, " // '"' // trim(input_parameters(i)%parameter_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
        
        ! How can I make numeric variables required?
        ! Check with `==` for integers and `is_close` otherwise that the variable equals its default.
        if ((input_parameters(i)%type_definition(1:4) == "inte") .and. input_parameters(i)%required) then
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_parameters(i)%parameter_name) &
                    // " /= " // trim(input_parameters(i)%default_value) // ", " // '"' &
                    // trim(input_parameters(i)%parameter_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
        
        if (((input_parameters(i)%type_definition(1:4) == "real") .or. (input_parameters(i)%type_definition(1:4) == "type")) &
                .and. input_parameters(i)%required) then
            if (trim(input_parameters(i)%type_definition) == "real") then
                underscore_kind_parameter = ""
            else
                underscore_kind_parameter = "_" // trim(config%kind_parameter)
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(is_close(" // trim(input_parameters(i)%parameter_name) &
                    // ", " // trim(input_parameters(i)%default_value) // trim(underscore_kind_parameter) // "), " // '"' &
                    // trim(input_parameters(i)%parameter_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
    end do
    
    write(unit=out_unit, fmt="(a)") ""
    ! Check if input parameters are within the bounds.
    do i = 1, n
        if (input_parameters(i)%lower_bound_active) then
            select case (input_parameters(i)%type_definition(1:4))
                case ("real", "type")
                    if (trim(input_parameters(i)%type_definition) == "real") then
                        write(unit=bound_string, fmt="(g0)") input_parameters(i)%lower_bound
                    else
                        write(unit=bound_string, fmt="(g0, a, a)") input_parameters(i)%lower_bound, &
                                                                    "_", trim(config%kind_parameter)
                    end if
                case ("inte")
                    write(unit=bound_string, fmt="(i0)") nint(input_parameters(i)%lower_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") "This type of input parameter can't have a bound."
                    error stop
            end select
            
            if (input_parameters(i)%lower_bound_not_equal) then
                op = ">"
            else
                op = ">="
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_parameters(i)%parameter_name) &
                     // " " // trim(op) // " " // trim(bound_string) // "), " // '"' &
                    // trim(input_parameters(i)%parameter_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group violates the lower bound.", rc)'
            ! TODO: Add numbers to the error message
        end if
        
        if (input_parameters(i)%upper_bound_active) then
            select case (input_parameters(i)%type_definition(1:4))
                case ("real", "type")
                    if (trim(input_parameters(i)%type_definition) == "real") then
                        write(unit=bound_string, fmt="(g0)") input_parameters(i)%upper_bound
                    else
                        write(unit=bound_string, fmt="(g0, a, a)") input_parameters(i)%upper_bound, &
                                                                    "_", trim(config%kind_parameter)
                    end if
                case ("inte")
                    write(unit=bound_string, fmt="(i0)") nint(input_parameters(i)%upper_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") "This type of input parameter can't have a bound."
                    error stop
            end select
            
            if (input_parameters(i)%upper_bound_not_equal) then
                op = "<"
            else
                op = "<="
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_parameters(i)%parameter_name) &
                     // " " // trim(op) // " " // trim(bound_string) // "), " // '"' &
                    // trim(input_parameters(i)%parameter_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group violates the upper bound.", rc)'
            ! TODO: Add numbers to the error message
        end if
    end do
    
    ! TODO: write to config variable
    ! TODO: trim strings
    
    close(unit=out_unit)
end subroutine write_subroutine

subroutine sort_input_parameters(input_parameters)
    ! <https://en.wikipedia.org/wiki/Selection_sort>
    
    type(input_parameter_type), intent(in out) :: input_parameters(:)
    
    integer :: i, n, j, i_switch, j_min
    type(input_parameter_type) :: temp_input_parameter
    
    n = size(input_parameters)
    
    ! Separate input_parameters into first the required input parameters and then the optional input_parameters.
    outer_separate: do i = 1, n
        if (input_parameters(i)%required) cycle outer_separate
        
        do j = i + 1, n
            if (input_parameters(j)%required) then
                temp_input_parameter = input_parameters(i)
                input_parameters(i)  = input_parameters(j)
                input_parameters(j)  = temp_input_parameter
                cycle outer_separate
            end if
        end do
    end do outer_separate
    
    ! Then alphabetically sort the two within each section.
    
    ! Determine where the switch from required to optional is.
    do i = 1, n
        if (.not. input_parameters(i)%required) then
            i_switch = i
            exit
        end if
    end do
    
    do i = 1, i_switch - 1
        j_min = i
        do j = i + 1, i_switch - 1
            if (input_parameters(j)%parameter_name < input_parameters(j_min)%parameter_name) then
                j_min = j
            end if
        end do
        
        if (j_min /= i) then
            temp_input_parameter    = input_parameters(i)
            input_parameters(i)     = input_parameters(j_min)
            input_parameters(j_min) = temp_input_parameter
        end if
    end do
    
    do i = i_switch, n
        j_min = i
        do j = i + 1, n
            if (input_parameters(j)%parameter_name < input_parameters(j_min)%parameter_name) then
                j_min = j
            end if
        end do
        
        if (j_min /= i) then
            temp_input_parameter    = input_parameters(i)
            input_parameters(i)     = input_parameters(j_min)
            input_parameters(j_min) = temp_input_parameter
        end if
    end do
end subroutine sort_input_parameters

end program geninput
