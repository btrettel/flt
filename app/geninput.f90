! Program to generate consistent Fortran code to read namelist input, input validation, and documentation for inputs.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program geninput

use cli, only: get_input_file_name_from_cli
use, intrinsic :: iso_fortran_env, only: IOSTAT_END, ERROR_UNIT, OUTPUT_UNIT
use prec, only: CL, WP
use checks, only: assert, check, is_close
implicit none

type :: config_type
    character(len=CL) :: output_file_prefix
    character(len=CL) :: namelist_group
    character(len=CL) :: type_name
    character(len=CL) :: config_variable
    character(len=CL) :: kind_parameter
    logical           :: use_type
    logical           :: write_tex, write_md ! whether to enable writing TeX or Markdown documentation
    logical           :: uq, ga ! whether to enable uncertainty quantification or the genetic algorithm respectively
    
    ! Write code for multiple namelist groups of the same name, like in `read_input_parameter_namelists` here
    ! TODO: `logical :: multiple_namelist_groups`
end type config_type

type :: input_parameter_type
    character(len=CL) :: variable_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value ! do not include the kind parameter as this will be added automatically
    logical           :: no_kind_default_value
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
    character(len=CL) :: bound_fmt
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
    character(len=CL) :: tex_description_2
    character(len=CL) :: tex_variable_name
    character(len=CL) :: txt_unit
end type input_parameter_type

integer, parameter :: MAX_LINE_LENGTH = 132

character(len=CL)                       :: input_file
integer                                 :: rc_config, rc_input_parameters
type(config_type)                       :: config
type(input_parameter_type), allocatable :: input_variables(:)

call get_input_file_name_from_cli("geninput", input_file)

! Read all namelists and exit if any have issues.
call read_config_namelist(input_file, config, rc_config)
if (rc_config /= 0) then
    error stop
end if

call read_input_parameter_namelists(input_file, input_variables, rc_input_parameters)
if (rc_input_parameters /= 0) then
    error stop
end if

call sort_input_parameters(input_variables)

if (config%use_type) call write_type(config, input_variables)
call write_subroutine(config, input_variables)

if (config%write_tex) call write_tex(config, input_variables)
! TODO if (config%write_md) call write_md(config, input_variables)

contains

subroutine read_config_namelist(input_file, config, rc)
    use port, only: platform, PLATFORM_WINDOWS, convert_path_unix_to_win
    
    character(len=*), intent(in)   :: input_file
    type(config_type), intent(out) :: config
    integer, intent(out)           :: rc
    
    integer           :: nml_unit, rc_nml
    character(len=CL) :: nml_error_message
    logical           :: use_type
    
    ! `config` namelist group
    ! See type definition above for some comments on these.
    character(len=CL) :: output_file_prefix
    character(len=CL) :: namelist_group
    character(len=CL) :: type_name
    character(len=CL) :: config_variable
    character(len=CL) :: kind_parameter
    logical           :: write_tex, write_md
    logical           :: uq, ga
    
    namelist /geninput_config/ output_file_prefix, namelist_group, type_name, config_variable, kind_parameter, &
                                write_tex, write_md, uq, ga
    
    output_file_prefix = ""
    namelist_group     = ""
    type_name          = ""
    config_variable    = "config"
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
    
    use_type = len(trim(type_name)) > 0
    
    config%output_file_prefix = trim(output_file_prefix)
    config%namelist_group     = trim(namelist_group)
    config%type_name          = trim(type_name)
    config%config_variable    = trim(config_variable)
    config%kind_parameter     = trim(kind_parameter)
    config%use_type           = use_type
    config%write_tex          = write_tex
    config%write_md           = write_md
    config%uq                 = uq
    config%ga                 = ga
    
    if (platform() == PLATFORM_WINDOWS) then
        call convert_path_unix_to_win(config%output_file_prefix)
    end if
end subroutine read_config_namelist

subroutine sort_input_parameters(input_variables)
    ! <https://en.wikipedia.org/wiki/Selection_sort>
    
    type(input_parameter_type), intent(in out) :: input_variables(:)
    
    integer :: i, n, j, i_switch, j_min
    type(input_parameter_type) :: temp_input_parameter
    
    n = size(input_variables)
    
    ! Separate input_variables into first the required input parameters and then the optional input_variables.
    outer_separate: do i = 1, n
        if (input_variables(i)%required) cycle outer_separate
        
        do j = i + 1, n
            if (input_variables(j)%required) then
                temp_input_parameter = input_variables(i)
                input_variables(i)   = input_variables(j)
                input_variables(j)   = temp_input_parameter
                cycle outer_separate
            end if
        end do
    end do outer_separate
    
    ! Then alphabetically sort the two within each section.
    
    ! Determine where the switch from required to optional is.
    do i = 1, n
        if (.not. input_variables(i)%required) then
            i_switch = i
            exit
        end if
    end do
    
    do i = 1, i_switch - 1
        j_min = i
        do j = i + 1, i_switch - 1
            if (input_variables(j)%variable_name < input_variables(j_min)%variable_name) then
                j_min = j
            end if
        end do
        
        if (j_min /= i) then
            temp_input_parameter   = input_variables(i)
            input_variables(i)     = input_variables(j_min)
            input_variables(j_min) = temp_input_parameter
        end if
    end do
    
    do i = i_switch, n
        j_min = i
        do j = i + 1, n
            if (input_variables(j)%variable_name < input_variables(j_min)%variable_name) then
                j_min = j
            end if
        end do
        
        if (j_min /= i) then
            temp_input_parameter   = input_variables(i)
            input_variables(i)     = input_variables(j_min)
            input_variables(j_min) = temp_input_parameter
        end if
    end do
end subroutine sort_input_parameters

subroutine read_input_parameter_namelists(input_file, input_variables, rc)
    character(len=*), intent(in)                         :: input_file
    type(input_parameter_type), allocatable, intent(out) :: input_variables(:)
    integer, intent(out)                                 :: rc
    
    integer           :: nml_unit, rc_nml, n_input_parameters, i, j, n_failures, default_value_integer
    character(len=CL) :: nml_error_message
    character(len=3)  :: i_string, j_string
    logical           :: outside_of_lower_bound, outside_of_upper_bound
    real(WP)          :: default_value_real
    
    ! `input_variable` namelist group
    character(len=CL) :: variable_name
    character(len=CL) :: type_definition
    character(len=CL) :: default_value
    logical           :: no_kind_default_value
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
    character(len=CL) :: bound_fmt
    character(len=CL) :: tex_unit
    character(len=CL) :: tex_description
    character(len=CL) :: tex_description_2
    character(len=CL) :: tex_variable_name
    character(len=CL) :: txt_unit
    
    namelist /input_variable/ variable_name, type_definition, default_value, no_kind_default_value, required, add_to_type, &
                                lower_bound_active, lower_bound_not_equal, lower_bound, lower_bound_error_message, &
                                upper_bound_active, upper_bound_not_equal, upper_bound, upper_bound_error_message, &
                                bound_fmt, tex_unit, tex_description, tex_description_2, tex_variable_name, txt_unit
    
    open(newunit=nml_unit, file=input_file, status="old", action="read", delim="quote")
    
    ! First get `n_input_parameters`, allocate `input_variables`, and read everything in.
    n_input_parameters = 0
    n_failures         = 0
    do ! SERIAL
        read(unit=nml_unit, nml=input_variable, iostat=rc_nml, iomsg=nml_error_message)
        
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
    allocate(input_variables(n_input_parameters))
    i = 0
    do ! SERIAL
        variable_name             = ""
        type_definition           = ""
        default_value             = ""
        no_kind_default_value     = .false.
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
        bound_fmt                 = ""
        tex_unit                  = ""
        tex_description           = ""
        tex_description_2         = ""
        tex_variable_name         = ""
        txt_unit                  = ""
        
        read(unit=nml_unit, nml=input_variable, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        i = i + 1
        
        input_variables(i)%variable_name             = trim(variable_name)
        input_variables(i)%type_definition           = trim(type_definition)
        input_variables(i)%default_value             = trim(default_value)
        input_variables(i)%no_kind_default_value     = no_kind_default_value
        input_variables(i)%required                  = required
        input_variables(i)%add_to_type               = add_to_type
        input_variables(i)%lower_bound_active        = lower_bound_active
        input_variables(i)%lower_bound_not_equal     = lower_bound_not_equal
        input_variables(i)%lower_bound               = lower_bound
        input_variables(i)%lower_bound_error_message = trim(lower_bound_error_message)
        input_variables(i)%upper_bound_active        = upper_bound_active
        input_variables(i)%upper_bound_not_equal     = upper_bound_not_equal
        input_variables(i)%upper_bound               = upper_bound
        input_variables(i)%upper_bound_error_message = trim(upper_bound_error_message)
        input_variables(i)%bound_fmt                 = trim(bound_fmt)
        input_variables(i)%tex_unit                  = trim(tex_unit)
        input_variables(i)%tex_description           = trim(tex_description)
        input_variables(i)%tex_description_2         = trim(tex_description_2)
        input_variables(i)%tex_variable_name         = trim(tex_variable_name)
        
        ! By default, make `txt_unit` copy `tex_unit`, unless `txt_unit` is defined separately.
        if ((trim(txt_unit) == "") .and. (trim(tex_unit) /= "")) then
            input_variables(i)%txt_unit = trim(tex_unit)
        else
            input_variables(i)%txt_unit = trim(txt_unit)
        end if
        
        ! Conditional defaults.
        if ((len(trim(input_variables(i)%bound_fmt)) == 0) .and. (input_variables(i)%type_definition(1:4) == "inte")) then
            input_variables(i)%bound_fmt = "i0"
        end if
        
        if ((len(trim(input_variables(i)%bound_fmt)) == 0) &
                .and. ((input_variables(i)%type_definition(1:4) == "real") &
                .or. (input_variables(i)%type_definition(1:4) == "type"))) then
            input_variables(i)%bound_fmt = "g0"
        end if
        
        write(unit=i_string, fmt="(i0)") i
        
        call check(len(trim(variable_name)) > 0, "input_variable #" // trim(i_string) &
                                                // "has an empty variable_name.", n_failures)
        call check(len(trim(type_definition)) > 0, "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "' has an empty type_definition.", n_failures)
        call check(len(trim(tex_description)) > 0, "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "' has an empty tex_description.", n_failures)
        
        call check(.not. ((.not. lower_bound_active) .and. lower_bound_not_equal), &
                    "input_variable #" // trim(i_string) &
                    // " with variable_name '" // trim(variable_name) &
                    // "': lower_bound_not_equal can not be .true. unless lower_bound_active=.true.", n_failures)
        
        call check(.not. ((.not. upper_bound_active) .and. upper_bound_not_equal), &
                    "input_variable #" // trim(i_string) &
                    // " with variable_name '" // trim(variable_name) &
                    // "': upper_bound_not_equal can not be .true. unless upper_bound_active=.true.", n_failures)
        
        if (lower_bound_active .and. upper_bound_active) then
            call check(lower_bound < upper_bound, "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "': lower_bound < upper_bound violated.", n_failures)
        end if
        
        if (((input_variables(i)%type_definition(1:4) == "real") &
                .or. (input_variables(i)%type_definition(1:4) == "type") &
                .or. (input_variables(i)%type_definition(1:4) == "inte")) &
                    .and. input_variables(i)%required) then
            call check(len(trim(input_variables(i)%tex_unit)) > 0, "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "' is numeric and has an empty tex_unit.", n_failures)
            call check(len(trim(input_variables(i)%txt_unit)) > 0, "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "' is numeric and has an empty txt_unit.", n_failures)
            call check((index(input_variables(i)%txt_unit, "$") == 0) &
                            .and. (index(input_variables(i)%txt_unit, "^") == 0), "input_variable #" // trim(i_string) &
                                                    // " with variable_name '" // trim(variable_name) &
                                                    // "' has a txt_unit value which seems to include LaTeX code.", n_failures)
            call check(input_variables(i)%lower_bound_active .or. input_variables(i)%upper_bound_active, &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': one or more bounds must be set if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            call check(len(trim(input_variables(i)%default_value)) > 0, &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': default_value must be set if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            if ((input_variables(i)%lower_bound_active .or. input_variables(i)%upper_bound_active) &
                    .and. (len(trim(input_variables(i)%default_value)) > 0)) then
                if (input_variables(i)%type_definition(1:4) == "inte") then
                    read(unit=input_variables(i)%default_value, fmt="(i8)") default_value_integer
                    outside_of_lower_bound = default_value_integer < nint(input_variables(i)%lower_bound)
                    outside_of_upper_bound = default_value_integer > nint(input_variables(i)%upper_bound)
                else
                    read(unit=input_variables(i)%default_value, fmt="(f16.8)") default_value_real
                    outside_of_lower_bound = default_value_real < input_variables(i)%lower_bound
                    outside_of_upper_bound = default_value_real > input_variables(i)%upper_bound
                end if
                
                call check(outside_of_lower_bound .or. outside_of_upper_bound, &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': default_value must be outside bounds if numeric and required, " &
                        // "otherwise Fortran can not detect if a variable is not set.", n_failures)
            end if
        end if
        
        if (input_variables(i)%type_definition(1:4) == "logi") then
            call check(len(trim(input_variables(i)%default_value)) > 0, &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': default_value must be set for logical input parameters.", n_failures)
            call check(.not. input_variables(i)%required, &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': logical parameters can not be required as there is no way to check in a Fortran namelist.", &
                        n_failures)
        end if
        
        if ((input_variables(i)%type_definition(1:4) == "inte") .and. input_variables(i)%lower_bound_active) then
            call check(is_close(input_variables(i)%lower_bound, real(nint(input_variables(i)%lower_bound), WP)), &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': integer input parameter must have integer lower bound.", n_failures)
        end if
        
        if ((input_variables(i)%type_definition(1:4) == "inte") .and. input_variables(i)%upper_bound_active) then
            call check(is_close(input_variables(i)%upper_bound, real(nint(input_variables(i)%upper_bound), WP)), &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "': integer input parameter must have integer upper bound.", n_failures)
        end if
        
        ! I could check that the `default_value` for an integer is an integer.
        ! But apparently `-1` converted to `(f16.8)` format is 0.0!
        ! I'd need to add a decimal point to get the right number.
        
        do j = 1, i - 1
            write(unit=j_string, fmt="(i0)") j
            call check(trim(input_variables(j)%variable_name) /= trim(input_variables(i)%variable_name), &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "' has the same variable_name as " &
                        // "input_variable #" // trim(j_string) // ".", &
                        n_failures)
            call check(trim(input_variables(j)%tex_variable_name) /= trim(input_variables(i)%tex_variable_name), &
                        "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "' has the same tex_variable_name as " &
                        // "input_variable #" // trim(j_string) // ".", &
                        n_failures)
            
            ! I've had issues before with the `tex_unit` being wrong.
            ! genunits types (`type_definition`) are checked by the compiler, so I can assume those are correct.
            ! This checks that only one `tex_unit` and `txt_unit` are used for each genunits `type_definition`.
            ! The units could still be wrong, but at least they are consistent.
            ! It's unlikely they'll be wrong if there are multiple that are consistent.
            if ((input_variables(i)%type_definition(1:4) == "type") &
                    .and. (trim(input_variables(i)%type_definition) == trim(input_variables(j)%type_definition))) then
                call check(trim(input_variables(i)%tex_unit) == trim(input_variables(j)%tex_unit), &
                         "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "' has the same type_definition as " &
                        // "input_variable #" // trim(j_string) // " but a different tex_unit.", &
                        n_failures)
                call check(trim(input_variables(i)%txt_unit) == trim(input_variables(j)%txt_unit), &
                         "input_variable #" // trim(i_string) &
                        // " with variable_name '" // trim(variable_name) &
                        // "' has the same type_definition as " &
                        // "input_variable #" // trim(j_string) // " but a different txt_unit.", &
                        n_failures)
            end if
        end do
    end do
    close(unit=nml_unit)
    
    call assert(n_failures >= 0, "geninput (read_input_parameter_namelists): n_failures is negative")
    if (n_failures > 0) then
        write(unit=ERROR_UNIT, fmt="(a)") "input_variable namelist input validation error(s)"
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_input_parameter_namelists

subroutine write_type(config, input_variables)
    type(config_type), intent(in)                       :: config
    type(input_parameter_type), allocatable, intent(in) :: input_variables(:)
    
    integer :: out_unit, n, i
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(config%output_file_prefix) // "_type.f90")
    
    n = size(input_variables)
    
    write(unit=out_unit, fmt="(a)") "type :: " // trim(config%type_name)
    do i = 1, n
        write(unit=out_unit, fmt="(a)") "    " // trim(input_variables(i)%type_definition) &
                                            // " :: " // trim(input_variables(i)%variable_name)
    end do
    write(unit=out_unit, fmt="(a)") "end type " // trim(config%type_name)
    
    close(unit=out_unit)
    
    write(unit=OUTPUT_UNIT, fmt="(a)") "Wrote " // trim(config%output_file_prefix) // "_type.f90."
end subroutine write_type

subroutine write_subroutine(config, input_variables)
    type(config_type), intent(in)                       :: config
    type(input_parameter_type), allocatable, intent(in) :: input_variables(:)
    
    integer       :: out_unit, n, i, line_length
    character(CL) :: type_definition, line, default_value, underscore_kind_parameter, bound_value_string_1, bound_value_string_2
    character(4)  :: type4
    character(2)  :: op
    logical       :: write_new_line
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(config%output_file_prefix) // ".f90")
    
    n = size(input_variables)
    
    write(unit=out_unit, fmt="(a)") "! auto-generated"
    write(unit=out_unit, fmt="(a)") ""
    write(unit=out_unit, fmt="(a)") "integer :: nml_unit, rc_nml"
    write(unit=out_unit, fmt="(a)") "character(len=CL) :: nml_error_message, value_string"
    write(unit=out_unit, fmt="(a)") ""
    
    ! genunits types to auto-convert `real`s to genunits types
    write_new_line = .false.
    do i = 1, n
        if (input_variables(i)%type_definition(1:4) == "type") then
            write(unit=out_unit, fmt="(a)") trim(input_variables(i)%type_definition) &
                                            // " :: " // trim(input_variables(i)%variable_name) // "_u"
            write_new_line = .true.
        end if
    end do
    if (write_new_line) write(unit=out_unit, fmt="(a)") ""
    
    write(unit=out_unit, fmt="(a)") "! `" // trim(config%namelist_group) // "` namelist group"
    do i = 1, n
        ! genunits types should be converted to `real` for the namelists
        if (input_variables(i)%type_definition(1:4) == "type") then
            if (len(trim(config%kind_parameter)) == 0) then
                type_definition = "real"
            else
                type_definition = "real(" // trim(config%kind_parameter) // ")"
            end if
        else
            type_definition = input_variables(i)%type_definition
        end if
        
        write(unit=out_unit, fmt="(a)") trim(type_definition) &
                                            // " :: " // trim(input_variables(i)%variable_name)
    end do
    write(unit=out_unit, fmt="(a)") ""
    
    line = "namelist /" // trim(config%namelist_group) // "/"
    write(unit=out_unit, fmt="(a)", advance="no") trim(line) // " "
    line_length = len(trim(line)) + 1
    do i = 1, n
        line_length = line_length + len(trim(input_variables(i)%variable_name))
        if (line_length > (MAX_LINE_LENGTH - 3)) then ! The 3 accounts for `, &`
            write(unit=out_unit, fmt="(a)") "&"
            line_length = 4 + len(trim(input_variables(i)%variable_name))
            write(unit=out_unit, fmt="(a)", advance="no") "    "
        end if
        
        write(unit=out_unit, fmt="(a)", advance="no") trim(input_variables(i)%variable_name)
        
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
        type4 = input_variables(i)%type_definition(1:4)
        if (len(trim(input_variables(i)%default_value)) == 0) then
            select case (type4)
                case ("inte")
                    default_value = "0"
                case ("real", "type")
                    if ((len(trim(config%kind_parameter)) == 0) &
                            .or. input_variables(i)%no_kind_default_value) then
                        default_value = "0.0"
                    else
                        default_value = "0.0_" // trim(config%kind_parameter)
                    end if
                case ("char")
                    default_value = '""'
                case ("logi")
                    error stop "logical variables must have default_value set"
                case default
                    error stop "Invalid type definition: " // trim(input_variables(i)%type_definition)
            end select
        else
            if ((type4 == "real") .or. (type4 == "type")) then
                if ((len(trim(config%kind_parameter)) == 0) &
                    .or. input_variables(i)%no_kind_default_value) then
                    default_value = trim(input_variables(i)%default_value)
                else
                    default_value = trim(input_variables(i)%default_value) // "_" // trim(config%kind_parameter)
                end if
            elseif (type4 == "char") then
                default_value = '"' // trim(input_variables(i)%default_value) // '"'
            else
                default_value = trim(input_variables(i)%default_value)
            end if
        end if
        
        write(unit=out_unit, fmt="(a)") trim(input_variables(i)%variable_name) // " = " // trim(default_value)
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
        if ((input_variables(i)%type_definition(1:4) == "char") .and. input_variables(i)%required) then
            write(unit=out_unit, fmt="(a)") "call check(len(trim(" // trim(input_variables(i)%variable_name) &
                    // ")) > 0, " // '"' // trim(input_variables(i)%variable_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
        
        ! How can I make numeric variables required?
        ! Check with `==` for integers and `is_close` otherwise that the variable equals its default.
        if ((input_variables(i)%type_definition(1:4) == "inte") .and. input_variables(i)%required) then
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_variables(i)%variable_name) &
                    // " /= " // trim(input_variables(i)%default_value) // ", " // '"' &
                    // trim(input_variables(i)%variable_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
        
        if (((input_variables(i)%type_definition(1:4) == "real") .or. (input_variables(i)%type_definition(1:4) == "type")) &
                .and. input_variables(i)%required) then
            if (trim(input_variables(i)%type_definition) == "real") then
                underscore_kind_parameter = ""
            else
                underscore_kind_parameter = "_" // trim(config%kind_parameter)
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(.not. is_close(" // trim(input_variables(i)%variable_name) &
                    // ", " // trim(input_variables(i)%default_value) // trim(underscore_kind_parameter) // "), " // '"' &
                    // trim(input_variables(i)%variable_name) // " in the " // trim(config%namelist_group) &
                    // ' namelist group is required.", rc)'
        end if
    end do
    
    write(unit=out_unit, fmt="(a)") ""
    ! Check if input parameters are within the bounds.
    do i = 1, n
        if (input_variables(i)%lower_bound_active) then
            select case (input_variables(i)%type_definition(1:4))
                case ("real", "type")
                    if (trim(input_variables(i)%type_definition) == "real") then
                        write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                input_variables(i)%lower_bound
                    else
                        write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ", a, a)") &
                                input_variables(i)%lower_bound, "_", trim(config%kind_parameter)
                    end if
                    write(unit=out_unit, fmt="(a)") 'write(unit=value_string, fmt="(' &
                            ! old: // trim(input_variables(i)%bound_fmt) // ')") ' &
                            // "g0" // ')") ' &
                            // trim(input_variables(i)%variable_name)
                case ("inte")
                    write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                nint(input_variables(i)%lower_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") trim(input_variables(i)%variable_name) &
                            // ": This type of input parameter can't have a bound."
                    error stop
            end select
            
            select case (input_variables(i)%type_definition(1:4))
                case ("real", "type")
                    write(unit=bound_value_string_2, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                input_variables(i)%lower_bound
                case ("inte")
                    write(unit=bound_value_string_2, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                nint(input_variables(i)%lower_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") trim(input_variables(i)%variable_name) &
                            // ": This type of input parameter can't have a bound."
                    error stop
            end select
            
            if (input_variables(i)%lower_bound_not_equal) then
                op = ">"
            else
                op = ">="
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_variables(i)%variable_name) &
                     // " " // trim(op) // " " // trim(bound_value_string_1) // ", " // '"' &
                    // trim(input_variables(i)%variable_name) // " in the " &
                    // trim(config%namelist_group) &
                    // ' namelist group equals " // trim(value_string) &'
            write(unit=out_unit, fmt="(a)", advance="no") '            // " but must be ' // trim(op) // " " &
                    // trim(bound_value_string_2)
            
            if (trim(input_variables(i)%txt_unit) /= "1") then
                write(unit=out_unit, fmt="(a)", advance="no") " " // trim(input_variables(i)%txt_unit)
            end if
            
            write(unit=out_unit, fmt="(a)") ". " // trim(input_variables(i)%lower_bound_error_message) // '", rc)'
        end if
        
        if (input_variables(i)%upper_bound_active) then
            select case (input_variables(i)%type_definition(1:4))
                case ("real", "type")
                    if (trim(input_variables(i)%type_definition) == "real") then
                        write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                input_variables(i)%upper_bound
                    else
                        write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ", a, a)") &
                                input_variables(i)%upper_bound, "_", trim(config%kind_parameter)
                    end if
                case ("inte")
                    write(unit=bound_value_string_1, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                nint(input_variables(i)%upper_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") trim(input_variables(i)%variable_name) &
                            // ": This type of input parameter can't have a bound."
                    error stop
            end select
            
            select case (input_variables(i)%type_definition(1:4))
                case ("real", "type")
                    write(unit=bound_value_string_2, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                input_variables(i)%upper_bound
                case ("inte")
                    write(unit=bound_value_string_2, fmt="(" // trim(input_variables(i)%bound_fmt) // ")") &
                                nint(input_variables(i)%upper_bound)
                case default
                    write(unit=ERROR_UNIT, fmt="(a)") trim(input_variables(i)%variable_name) &
                            // ": This type of input parameter can't have a bound."
                    error stop
            end select
            
            if (input_variables(i)%upper_bound_not_equal) then
                op = "<"
            else
                op = "<="
            end if
            
            write(unit=out_unit, fmt="(a)") "call check(" // trim(input_variables(i)%variable_name) &
                     // " " // trim(op) // " " // trim(bound_value_string_1) // ", " // '"' &
                    // trim(input_variables(i)%variable_name) // " in the " &
                    // trim(config%namelist_group) &
                    // ' namelist group equals " // trim(value_string) &'
            write(unit=out_unit, fmt="(a)") '            // " but must be ' // trim(op) // " " &
                    // trim(bound_value_string_2) // ". " // trim(input_variables(i)%lower_bound_error_message) &
                    // '", rc)'
        end if
    end do
    
    if (config%use_type) then
        write(unit=out_unit, fmt="(a)") ""
        ! Write to config variable.
        do i = 1, n
            if (.not. (input_variables(i)%type_definition(1:4) == "char")) then
                write(unit=out_unit, fmt="(a)") "config%" // trim(input_variables(i)%variable_name) // " = " &
                                                    // trim(input_variables(i)%variable_name)
            else
                ! Trim strings.
                write(unit=out_unit, fmt="(a)") "config%" // trim(input_variables(i)%variable_name) // " = trim(" &
                                                    // trim(input_variables(i)%variable_name) // ")"
            end if
        end do
    end if
    
    write(unit=out_unit, fmt="(a)") ""
    write(unit=out_unit, fmt="(a)") "if (rc /= 0) then"
    write(unit=out_unit, fmt="(a)") "    return"
    write(unit=out_unit, fmt="(a)") "end if"
    
    ! genunits types to auto-convert `real`s to genunits types
    write_new_line = .true.
    do i = 1, n
        if (input_variables(i)%type_definition(1:4) == "type") then
            if (write_new_line) then
                write(unit=out_unit, fmt="(a)") ""
                write_new_line = .false.
            end if
            write(unit=out_unit, fmt="(a)") "call " // trim(input_variables(i)%variable_name) &
                                                // "_u%v%init_const(" // trim(input_variables(i)%variable_name) // ", 0)"
        end if
    end do
    
    close(unit=out_unit)
    
    write(unit=OUTPUT_UNIT, fmt="(a)") "Wrote " // trim(config%output_file_prefix) // ".f90."
end subroutine write_subroutine

pure function texttt_escape(string)
    character(len=*), intent(in) :: string
    
    character(len=:), allocatable :: texttt_escape
    
    character(len=:), allocatable :: trim_string
    
    integer :: i_prev, i_next, i
    
    trim_string = trim(string)
    
    i_prev = 1
    i = 0
    do
        i = i + 1
        i_next = index(trim_string(i_prev:len(trim_string)), "_")
        !print *, trim_string(i_prev:len(trim_string)), i_prev, i_next
        
        if (i_next == 0) then
            texttt_escape = texttt_escape // trim_string(i_prev:len(trim_string))
            exit
        else
            texttt_escape = texttt_escape // trim_string(i_prev:i_prev+i_next-2) // "\_"
        end if
        
        i_prev = i_next + i_prev
        
        call assert(i < 100, "geninput (texttt_escape): doesn't seem to want to finish")
    end do
end function texttt_escape

subroutine write_tex(config, input_variables)
    type(config_type), intent(in)                       :: config
    type(input_parameter_type), allocatable, intent(in) :: input_variables(:)
    
    integer :: out_unit, n, i
    logical :: optional_printed
    character(4)  :: type4
    character(CL) :: default_value
    
    n = size(input_variables)
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(config%output_file_prefix) // ".tex")
    
    write(unit=out_unit, fmt="(a)") "% auto-generated"
    write(unit=out_unit, fmt="(a)") ""
    write(unit=out_unit, fmt="(a)") "Required input variables:"
    write(unit=out_unit, fmt="(a)") "\begin{itemize}"
    
    optional_printed = .false.
    do i = 1, n
        if ((.not. optional_printed) .and. (.not. input_variables(i)%required)) then
            optional_printed = .true.
            write(unit=out_unit, fmt="(a)") "\end{itemize}"
            write(unit=out_unit, fmt="(a)") ""
            write(unit=out_unit, fmt="(a)") "Optional input variables:"
            write(unit=out_unit, fmt="(a)") "\begin{itemize}"
        end if
        
        write(unit=out_unit, fmt="(a)", advance="no") "\item \texttt{"
        write(unit=out_unit, fmt="(a)", advance="no") texttt_escape(input_variables(i)%variable_name)
        
        if (len(trim(input_variables(i)%tex_variable_name)) > 0) then
            write(unit=out_unit, fmt="(3a)", advance="no") "} (", trim(input_variables(i)%tex_variable_name), "): "
        else
            write(unit=out_unit, fmt="(a)", advance="no") "}: "
        end if
        
        write(unit=out_unit, fmt="(a)", advance="no") trim(input_variables(i)%tex_description)
        
        select case (input_variables(i)%type_definition(1:4))
            case ("real", "type")
                write(unit=out_unit, fmt="(3a)", advance="no") " Floating-point number."
            case ("inte")
                write(unit=out_unit, fmt="(3a)", advance="no") " Integer."
            case ("char")
                write(unit=out_unit, fmt="(3a)", advance="no") " String."
            case default
                write(unit=ERROR_UNIT, fmt="(a)") trim(input_variables(i)%variable_name) &
                        // ": Invalid type_definition."
                error stop
        end select
        
        if (len(trim(input_variables(i)%tex_unit)) > 0) then
            if (trim(input_variables(i)%tex_unit) == "1") then
                write(unit=out_unit, fmt="(3a)", advance="no") " Unitless."
            else
                write(unit=out_unit, fmt="(3a)", advance="no") " Units of ", trim(input_variables(i)%tex_unit), "."
            end if
        end if
        
        ! Why `.not. input_variables(i)%no_kind_default_value`?
        ! Then parameters like `P_ATM` which are pulled from somewhere else won't print simply `P_ATM`, which is not helpful.
        ! TODO: Figure out a way to print the actual parameter value.
        if ((.not. input_variables(i)%required) .and. (.not. input_variables(i)%no_kind_default_value)) then
            type4 = input_variables(i)%type_definition(1:4)
            if ((type4 == "real") .or. (type4 == "type")) then
                default_value = "$" // trim(input_variables(i)%default_value) // "$"
            elseif (type4 == "char") then
                default_value = "\texttt{" // trim(input_variables(i)%default_value) // "}"
            else
                default_value = trim(input_variables(i)%default_value)
            end if
            
            if ((len(trim(input_variables(i)%tex_unit)) > 0) &
                    .and. (trim(input_variables(i)%tex_unit) /= "1")) then
                write(unit=out_unit, fmt="(5a)", advance="no") " Default value is ", trim(default_value), &
                                                                    "~", trim(input_variables(i)%tex_unit), "."
            else
                write(unit=out_unit, fmt="(3a)", advance="no") " Default value is ", trim(default_value), "."
            end if
        end if
        
        if (len(trim(input_variables(i)%tex_description_2)) > 0) then
            write(unit=out_unit, fmt="(3a)", advance="no") " "
        end if
        write(unit=out_unit, fmt="(a)") trim(input_variables(i)%tex_description_2)
    end do
    
    write(unit=out_unit, fmt="(a)") "\end{itemize}"
    
    close(unit=out_unit)
    
    write(unit=OUTPUT_UNIT, fmt="(a)") "Wrote " // trim(config%output_file_prefix) // ".tex."
    
    ! TODO: add lower and upper bounds to TeX output
end subroutine write_tex

end program geninput
