! module containing I/0 procedures to generate a units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module genunits_io

use prec, only: WP
use genunits_data, only: MAX_LABEL_LEN, BASE_UNIT_LEN, unit_type
implicit none
private

public :: in_exponent_bounds, denominator_matches, &
            write_as_operators, write_comparison_operators, write_md_operators, write_binary_operator, &
            write_unary_operator, &
            write_unit_function, write_unit_wf, write_unit_rf, &
            write_exponentiation_interfaces, write_exponentiation_function, &
            write_intrinsic_interfaces, write_intrinsic_function, &
            write_module

integer, public, parameter :: DEFAULT_MAX_N_UNITS = 28, & ! This is about the most the ifx will compile as of 2024-05-12.
                              DEFAULT_MAX_ITER    = 50, &
                              DEFAULT_DENOMINATOR = 1,  &
                              MAX_USE_LINES       = 10

character(len=4), parameter :: INTRINSIC_1ARG_UNITLESS(5)  = ["sin", "cos", "tan", "exp", "log"]
character(len=4), parameter :: INTRINSIC_1ARG_WITHUNITS(1) = ["abs"]
character(len=4), parameter :: INTRINSIC_2ARG_WITHUNITS(2) = ["min", "max"]

type, public :: config_type
    character(len=:), allocatable :: output_file, type_definition, use_line, kind_parameter, module_name
    real(WP), allocatable         :: min_exponents(:), max_exponents(:)
    integer, allocatable          :: denominators(:)
    
    integer :: max_n_units, max_iter !, max_n_interfaces
    logical :: tests, comparison, unary, sqrt, cbrt, square, intrinsics, dtio
    
    character(len=BASE_UNIT_LEN), allocatable :: base_units(:)
    
    type(unit_type), allocatable              :: seed_units(:)
    character(len=MAX_LABEL_LEN), allocatable :: seed_labels(:)
    ! TODO: type(unit_type), allocatable              :: reject_units(:)
    
    character(len=MAX_LABEL_LEN), allocatable :: custom(:)
contains
    procedure :: read_config_namelist
    procedure :: read_seed_unit_namelists
    procedure :: generate_system
    procedure :: write_type
    procedure :: write_module
end type config_type

contains

subroutine read_config_namelist(config_out, filename, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END, ERROR_UNIT
    use genunits_data, only: MAX_BASE_UNITS, BASE_UNIT_LEN, EXPONENT_LEN
    
    use prec, only: CL
    use checks, only: assert, is_close, check
    
    class(config_type), intent(out) :: config_out
    character(len=*), intent(in)    :: filename
    integer, intent(out)            :: rc
    
    integer           :: i_base_unit, nml_unit, rc_nml, n_failures, n_base_units, n_use_lines
    character(len=CL) :: nml_error_message
    
    ! `config` namelist group
    character(len=CL)            :: output_file, type_definition, use_line, kind_parameter, module_name
    character(len=BASE_UNIT_LEN) :: base_units(MAX_BASE_UNITS)
    real(WP)                     :: min_exponents(MAX_BASE_UNITS), max_exponents(MAX_BASE_UNITS)
    integer                      :: denominators(MAX_BASE_UNITS), max_n_units, max_iter
    logical                      :: tests, comparison, unary, sqrt, cbrt, square, intrinsics, dtio
    
    namelist /config/ output_file, base_units, type_definition, use_line, kind_parameter, module_name, &
                        min_exponents, max_exponents, denominators, &
                        max_n_units, tests, comparison, unary, sqrt, cbrt, square, intrinsics, dtio
    
    n_failures = 0
    
    ! Set the defaults.
    
    do concurrent (i_base_unit = 1:MAX_BASE_UNITS)
        base_units(i_base_unit) = ""
    end do
    
    output_file     = ""
    type_definition = "real(WP)"
    use_line        = "use prec, only: WP"
    kind_parameter  = "_WP"
    module_name     = "units"
    min_exponents   = -huge(1.0_WP)
    max_exponents   = huge(1.0_WP)
    denominators    = DEFAULT_DENOMINATOR
    max_n_units     = DEFAULT_MAX_N_UNITS
    max_iter        = DEFAULT_MAX_ITER
    tests           = .false.
    comparison      = .true.
    unary           = .true.
    sqrt            = .true.
    cbrt            = .true.
    square          = .true.
    intrinsics      = .true.
    dtio            = .false.
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
        rc = rc_nml
        return
    end if
    
    n_base_units = 0
    do i_base_unit = 1, MAX_BASE_UNITS ! SERIAL
        if (trim(base_units(i_base_unit)) /= "") then
            n_base_units = n_base_units + 1
        end if
    end do
    ! A check that `n_base_units > 0` is done later.
    call assert(n_base_units <= MAX_BASE_UNITS, "genunits_io (read_config_namelist): n_base_units is impossibly high?")
    ! Maybe later: enforce maximum number of units to prevent labels from getting too long.
    
    ! Replace semicolons with new lines in the `use_line` variable so that multiple `use` lines can be written.
    n_use_lines = 0
    do ! SERIAL
        n_use_lines = n_use_lines + 1
        
        if (index(use_line, ";") > 0) then
            use_line(index(use_line, ";"):index(use_line, ";")) = new_line("a")
        end if
        
        if (index(use_line, ";") == 0) then
            exit
        end if
        
        if (n_use_lines > MAX_USE_LINES) then
            exit
        end if
    end do
    call assert(n_use_lines >= 0, "genunits_io (read_config_namelist): n_use_lines is negative")
    call check(index(use_line, ";") == 0, "use_line contains too many semicolons.", n_failures)
    
    ! Copy the data to the configuration type.
    config_out%output_file     = trim(output_file)
    config_out%type_definition = trim(type_definition)
    config_out%use_line        = trim(use_line)
    
    if (len(trim(kind_parameter)) == 0) then
        config_out%kind_parameter = ""
    else
        config_out%kind_parameter = "_" // trim(kind_parameter)
    end if
    
    config_out%module_name     = trim(module_name)
    
    config_out%min_exponents   = min_exponents(1:n_base_units) ! The allowable lower limits for the unit exponents.
    config_out%max_exponents   = max_exponents(1:n_base_units) ! The allowable upper limits for the unit exponents.
    config_out%denominators    = denominators(1:n_base_units) ! The allowable denominators for unit exponents.
    config_out%max_n_units     = max_n_units ! The maximum number of units in the unit system.
    config_out%max_iter        = max_iter ! The maximum number of iterations when generating the unit system.
    config_out%base_units      = base_units(1:n_base_units) ! The base units.
    config_out%tests           = tests ! Whether tests will be written. (Not used at the moment.)
    config_out%comparison      = comparison ! Whether comparison operators will be written.
    config_out%unary           = unary ! Whether unary operators will be written.
    config_out%sqrt            = sqrt ! Whether the `sqrt` function will be written.
    config_out%cbrt            = cbrt ! Whether the `cbrt` function will be written.
    config_out%square          = square ! Whether the `square` function will be written.
    config_out%intrinsics      = intrinsics ! Whether one argument intrinsics will be written.
    config_out%dtio            = dtio ! Whether derived type I/O will be written. Experimental, doesn't work right in all compilers.
    
    call check(len(config_out%output_file) > 0, "output_file must be defined", n_failures)
    call check(n_base_units > 0, "base_units must have 1 or more members", n_failures)
    call check(len(type_definition) >= 1, "type_definition must have 1 or more characters", n_failures)
    call check(all(denominators >= 1), "all denominators must be 1 or more", n_failures)
    call check(index(config_out%module_name, " ") == 0, "module_name can not contain spaces", n_failures)
    call check(len(config_out%module_name) >= 1, "module_name must not be empty", n_failures)
    call check(len(config_out%module_name) <= 31, &
                                    "module_name must be 31 characters or less to meet the Fortran standard", n_failures)
    
    ! Check that all denominators can be distinguished given `EXPONENT_LEN`.
    call check(log(real(maxval(config_out%denominators), WP)) / log(10.0_WP) < real(EXPONENT_LEN - 1, WP), &
                    "largest denominator is too high and can not be represented given EXPONENT_LEN. " &
                    // "genunits could be modified to increase EXPONENT_LEN to handle the denominators.", &
                    n_failures)
    
    ! Check that maximum exponent absolute value is below 10 to avoid overflow in the label.
    call check(max(maxval(abs(config_out%max_exponents)), maxval(abs(config_out%min_exponents))) < 10.0_WP, &
                "largest exponent absolute value requires more than 1 digit, which would overflow the label. " &
                // "Reduce the largest exponent absolute value or modify genunits to change the labels.", &
                n_failures)
    
    do i_base_unit = 1, n_base_units ! SERIAL
        call check(.not. is_close(min_exponents(i_base_unit), -huge(1.0_WP)), &
                                        "min_exponents is not set properly?", n_failures)
        call check(.not. is_close(max_exponents(i_base_unit), huge(1.0_WP)), &
                                        "max_exponents is not set properly?", n_failures)
        call check(min_exponents(i_base_unit) <= max_exponents(i_base_unit), &
                                        "a minimum exponent is larger than a maximum exponent", n_failures)
    end do
    
    call check(config_out%max_n_units > 0, "max_n_units must be 1 or greater.", n_failures)
    
    call assert(n_failures >= 0, "genunits_io (read_config_namelist): n_failures is negative")
    if (n_failures > 0) then
        write(unit=ERROR_UNIT, fmt="(a)") "config namelist input validation error(s)"
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_config_namelist

subroutine read_seed_unit_namelists(config, filename, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END, ERROR_UNIT
    use checks, only: assert, is_close, all_close, check
    use prec, only: CL
    use genunits_data, only: MAX_BASE_UNITS, MAX_LABEL_LEN
    
    class(config_type), intent(in out) :: config
    character(len=*), intent(in)       :: filename
    integer, intent(out)               :: rc
    
    integer           :: nml_unit, n_seed_units, i_seed_unit, j_seed_unit, rc_nml, n_failures, i_base_unit
    character(len=CL) :: nml_error_message
    character(len=2)  :: i_seed_unit_string, j_seed_unit_string, i_base_unit_string
    character(len=5)  :: n_seed_units_string, max_n_units_string
    
    ! `seed_unit` namelist group
    character(len=MAX_LABEL_LEN) :: label
    real(WP)                     :: e(MAX_BASE_UNITS)
    
    namelist /seed_unit/ label, e
    
    call assert(allocated(config%base_units), "genunits_io (read_seed_unit_namelists): base units must be allocated")
    call assert(.not. allocated(config%seed_units), "genunits_io (read_seed_unit_namelists): seed units must not be allocated")
    call assert(.not. allocated(config%seed_labels), "genunits_io (read_seed_unit_namelists): seed labels must not be allocated")
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    
    ! First get `n_seed_units`, allocate `config`, and read everything in.
    n_seed_units = 0
    n_failures   = 0
    do ! SERIAL
        label = ""
        e = 0.0_WP
        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        n_seed_units = n_seed_units + 1
    end do
    
    call assert(n_seed_units >= 0, "genunits_io (read_seed_unit_namelists): n_seed_units is negative")
    call check(n_seed_units > 0, "At least one seed unit is required.", n_failures)
    write(unit=n_seed_units_string, fmt="(i0)") n_seed_units
    write(unit=max_n_units_string, fmt="(i0)") config%max_n_units
    call check(n_seed_units <= config%max_n_units, &
                                    "The number of seed units (" // trim(n_seed_units_string) &
                                    // ") exceed max_n_units, the maximum number of units allowed (" // trim(max_n_units_string) &
                                    // "). Either increase max_n_units or reduce the number of seed units.", &
                                    n_failures)
    
    ! Once the arrays are sized properly, go back and read all of the seed units.
    rewind nml_unit
    allocate(config%seed_labels(n_seed_units))
    allocate(config%seed_units(n_seed_units))
    i_seed_unit = 0
    do ! SERIAL
        label = ""
        e     = huge(1.0_WP)
        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        i_seed_unit = i_seed_unit + 1
        
        !write(unit=*, fmt=*) i_seed_unit, trim(label), e(1:size(config%base_units))
        config%seed_labels(i_seed_unit)  = trim(label)
        config%seed_units(i_seed_unit)%e = e(1:size(config%base_units))
        
        write(unit=i_seed_unit_string, fmt="(i0)") i_seed_unit
        
        call check(len(trim(label)) /= 0, &
                                        "seed_unit #" // trim(i_seed_unit_string) // " has an empty label.", n_failures)
        
        call check(index(trim(label), " ") == 0, &
                                        "seed_unit #" // trim(i_seed_unit_string) // " with label '" // trim(label) // &
                                        "' has spaces in its label. " // &
                                        "Replace spaces with underscores or something else that can be in a Fortran type name", &
                                        n_failures)

        do j_seed_unit = 1, i_seed_unit - 1 ! SERIAL
            write(unit=j_seed_unit_string, fmt="(i0)") j_seed_unit
            call check(trim(label) /= config%seed_labels(j_seed_unit), &
                                            "seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same label as seed_unit #' // trim(j_seed_unit_string) // ".", &
                                                n_failures)
            call check(.not. all_close(config%seed_units(i_seed_unit)%e, config%seed_units(j_seed_unit)%e), &
                                            "seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same exponents as seed_unit #' // trim(j_seed_unit_string) &
                                                // ' labeled "' // trim(config%seed_labels(j_seed_unit)) // '".', n_failures)
        end do
        
        do i_base_unit = 1, size(config%base_units) ! SERIAL
            write(unit=i_base_unit_string, fmt="(i0)") i_base_unit
            call check(.not. is_close(config%seed_units(i_seed_unit)%e(i_base_unit), huge(1.0_WP)), &
                                            "In seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                            // trim(config%seed_labels(i_seed_unit)) &
                                            // '", exponent #' // trim(i_base_unit_string) // " has not been set.", n_failures)
            call check(config%seed_units(i_seed_unit)%e(i_base_unit) >= config%min_exponents(i_base_unit), &
                                            "In seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                            // trim(config%seed_labels(i_seed_unit)) &
                                            // '", exponent #' // trim(i_base_unit_string) // " is below the minimum exponent.", &
                                            n_failures)
            call check(config%seed_units(i_seed_unit)%e(i_base_unit) <= config%max_exponents(i_base_unit), &
                                            "In seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                            // trim(config%seed_labels(i_seed_unit)) &
                                            // '", exponent #' // trim(i_base_unit_string) // " is above the maximum exponent.", &
                                            n_failures)
        end do
    end do
    close(unit=nml_unit)
    
    call assert(n_failures >= 0, "genunits_io (read_seed_unit_namelists): n_failures is negative")
    if (n_failures > 0) then
        write(unit=ERROR_UNIT, fmt="(a)") "seed namelist input validation error(s)"
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_seed_unit_namelists

pure function in_exponent_bounds(config, unit)
    use checks, only: assert
    use genunits_data, only: unit_type
    
    type(config_type), intent(in) :: config
    type(unit_type), intent(in)   :: unit
    
    logical :: in_exponent_bounds
    
    integer :: i_base_unit
    
    call assert(size(config%min_exponents) == size(config%max_exponents), &
                    "genunits_io (in_exponent_bounds): min and max exponents have inconsistent sizes")
    call assert(all(config%min_exponents <= config%max_exponents), &
                    "genunits_io (in_exponent_bounds): min exponents must be <= max exponents")
    
    in_exponent_bounds = .true.
    do i_base_unit = 1, size(config%min_exponents) ! SERIAL
        if ((unit%e(i_base_unit) > config%max_exponents(i_base_unit)) &
                .or. (unit%e(i_base_unit) < config%min_exponents(i_base_unit))) then
            in_exponent_bounds = .false.
            exit
        end if
    end do
end function in_exponent_bounds

pure function denominator_matches(e, d)
    ! Returns `.true.` if `e` can be represented as a rational number with a denominator of `d`.
    
    use checks, only: is_close
    
    real(WP), intent(in) :: e
    integer, intent(in)  :: d
    
    logical :: denominator_matches
    
    real(WP) :: ed
    
    ed = e * real(d, WP)
    
    denominator_matches = is_close(real(nint(ed), WP), ed)
end function denominator_matches

pure subroutine process_trial_unit(config, trial_unit, units, n_units, rc)
    ! If trial unit is within bounds and not in the previous array of units, add it.
    
    use genunits_data, only: unit_type
    
    type(config_type), intent(in)   :: config
    type(unit_type), intent(in)     :: trial_unit
    type(unit_type), intent(in out) :: units(:)
    integer, intent(in out)         :: n_units
    integer, intent(out)            :: rc
    
    logical :: within_bounds, unseen, denominator_valid
    integer :: i_base_unit
    
    if (n_units >= config%max_n_units) then
        rc = 1
        return
    end if
    
    rc = 0
    
    within_bounds = in_exponent_bounds(config, trial_unit)
    unseen        = .not. trial_unit%is_in(units(1:n_units))
    
    denominator_valid = .true.
    do i_base_unit = 1, size(config%base_units) ! SERIAL
        if (.not. denominator_matches(trial_unit%e(i_base_unit), config%denominators(i_base_unit))) then
            denominator_valid = .false.
            exit
        end if
    end do
    
    if (within_bounds .and. unseen .and. denominator_valid) then
        n_units = n_units + 1
        units(n_units) = trial_unit
    end if
end subroutine process_trial_unit

subroutine generate_system(config, unit_system)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit, sqrt_unit, cbrt_unit, square_unit
    
    class(config_type), intent(in)      :: config
    type(unit_system_type), intent(out) :: unit_system
    
    type(unit_type) :: units(config%max_n_units), trial_unit
    integer         :: n_units, n_units_prev, i_units, j_units, iter, rc
    
    ! Add all `seed_units` to `units`
    units(1:size(config%seed_units)) = config%seed_units
    
    ! Create new units that appear from application of operators.
    n_units = size(config%seed_units)
    iter = 0
    genunit_loop: do ! SERIAL
        write(unit=*, fmt="(a, i0, a, i0)") "iter=", iter, " n_units=", n_units
        
        iter = iter + 1
        n_units_prev = n_units
        
        do i_units = 1, n_units_prev ! SERIAL
            ! binary operators
            do j_units = 1, n_units_prev ! SERIAL
                ! multiplication
                trial_unit = m_unit(units(i_units), units(j_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
                
                ! division
                trial_unit = d_unit(units(i_units), units(j_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
            end do
            
            ! unary operators: These don't generate new units.
            
            if (config%sqrt) then
                ! square root
                trial_unit = sqrt_unit(units(i_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
            end if
            
            if (config%cbrt) then
                ! cube root
                trial_unit = cbrt_unit(units(i_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
            end if
            
            if (config%square) then
                ! square
                trial_unit = square_unit(units(i_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
            end if
        end do
        
        if ((iter > (config%max_iter - 1)) .or. (n_units == n_units_prev)) then
            exit genunit_loop
        end if
    end do genunit_loop
    
    call assert(iter > 0, "genunits_io (generate_system): no iterations?")
    call assert(iter <= config%max_iter, "genunits_io (generate_system): too many iterations")
    call assert(n_units > 0, "genunits_io (generate_system): no units?")
    call assert(n_units <= config%max_n_units, "genunits_io (generate_system): too many units")
    
    write(unit=*, fmt="(a, i0, a, i0)") "FINAL iter=", iter, " n_units=", n_units
    
    unit_system%units        = units(1:n_units)
    unit_system%base_units   = config%base_units
    unit_system%n_base_units = size(config%base_units)
end subroutine generate_system

! output

subroutine write_type(config, file_unit, i_unit, unit_system)
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit
    
    class(config_type), intent(in)     :: config
    integer, intent(in)                :: file_unit, i_unit
    type(unit_system_type), intent(in) :: unit_system
    
    integer         :: j_unit
    type(unit_type) :: trial_unit
    logical         :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_type): file_unit must be open")
    
    write(unit=file_unit, fmt="(2a)") "type, public :: ", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(2a)") "    ! unit: ", trim(unit_system%units(i_unit)%readable(unit_system))
    write(unit=file_unit, fmt="(3a)") "    ", config%type_definition, " :: v"
    write(unit=file_unit, fmt="(a)") "contains"
    
    ! addition operators
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: a_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(+) => a_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    
    ! Allow using `real`s as unitless in some situations.
    if (all_close(unit_system%units(i_unit)%e, 0.0_WP)) then
        write(unit=file_unit, fmt="(3a)") "    procedure, private, pass(left) :: a_", &
            trim(unit_system%units(i_unit)%label()), "_real"
        write(unit=file_unit, fmt="(3a)") "    generic, public :: operator(+) => a_", &
            trim(unit_system%units(i_unit)%label()), "_real"
        
        ! The operations where the left is `real` won't have a corresponding type, so I put them here.
        write(unit=file_unit, fmt="(2a)") "    procedure, private, pass(right) :: a_real_", &
            trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(+) => a_real_", &
            trim(unit_system%units(i_unit)%label())
    end if
    
    ! subtraction operators
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: s_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(-) => s_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    
    ! Allow using `real`s as unitless in some situations.
    if (all_close(unit_system%units(i_unit)%e, 0.0_WP)) then
        write(unit=file_unit, fmt="(3a)") "    procedure, private, pass(left) :: s_", &
            trim(unit_system%units(i_unit)%label()), "_real"
        write(unit=file_unit, fmt="(3a)") "    generic, public :: operator(-) => s_", &
            trim(unit_system%units(i_unit)%label()), "_real"
        
        ! The operations where the left is `real` won't have a corresponding type, so I put them here.
        write(unit=file_unit, fmt="(2a)") "    procedure, private, pass(right) :: s_real_", &
            trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(-) => s_real_", &
            trim(unit_system%units(i_unit)%label())
    end if
    
    ! comparison operators
    if (config%comparison) then
        write(unit=file_unit, fmt="(4a)") "    procedure, private :: lt_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(<) => lt_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(4a)") "    procedure, private :: le_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(<=) => le_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(4a)") "    procedure, private :: gt_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(>) => gt_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(4a)") "    procedure, private :: ge_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(>=) => ge_", &
            trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    end if
    
    ! unary operators
    if (config%unary) then
        write(unit=file_unit, fmt="(2a)") "    procedure, private :: a_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(+) => a_", trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(2a)") "    procedure, private :: s_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(-) => s_", trim(unit_system%units(i_unit)%label())
    end if
    
    do j_unit = 1, size(unit_system%units) ! SERIAL
        ! multiply
        trial_unit = m_unit(unit_system%units(i_unit), unit_system%units(j_unit))
        if (trial_unit%is_in(unit_system%units)) then
            write(unit=file_unit, fmt="(4a)") "    procedure, private :: m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
            write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(*) => m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        else
            write(unit=file_unit, fmt="(4a)") "    ! excluded: m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        end if
        
        ! Allow using `real`s as unitless in some situations.
        if (all_close(unit_system%units(j_unit)%e, 0.0_WP)) then
            write(unit=file_unit, fmt="(3a)") "    procedure, private, pass(left) :: m_", &
                trim(unit_system%units(i_unit)%label()), "_real"
            write(unit=file_unit, fmt="(3a)") "    generic, public :: operator(*) => m_", &
                trim(unit_system%units(i_unit)%label()), "_real"
            
            ! The operations where the left is `real` won't have a corresponding type, so I put them here.
            trial_unit = m_unit(unit_system%units(j_unit), unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    procedure, private, pass(right) :: m_real_", &
                    trim(unit_system%units(i_unit)%label())
                write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(*) => m_real_", &
                    trim(unit_system%units(i_unit)%label())
            end if
        end if
        
        ! divide
        trial_unit = d_unit(unit_system%units(i_unit), unit_system%units(j_unit))
        if (trial_unit%is_in(unit_system%units)) then
            write(unit=file_unit, fmt="(4a)") "    procedure, private :: d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
            write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(/) => d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        else
            write(unit=file_unit, fmt="(4a)") "    ! excluded: d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        end if
        
        ! Allow using `real`s as unitless in some situations.
        if (all_close(unit_system%units(j_unit)%e, 0.0_WP)) then
            write(unit=file_unit, fmt="(3a)") "    procedure, private, pass(left) :: d_", &
                trim(unit_system%units(i_unit)%label()), "_real"
            write(unit=file_unit, fmt="(3a)") "    generic, public :: operator(/) => d_", &
                trim(unit_system%units(i_unit)%label()), "_real"
            
            ! The operations where the left is `real` won't have a corresponding type, so I put them here.
            trial_unit = d_unit(unit_system%units(j_unit), unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    procedure, private, pass(right) :: d_real_", &
                    trim(unit_system%units(i_unit)%label())
                write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(/) => d_real_", &
                    trim(unit_system%units(i_unit)%label())
            end if
        end if
    end do
    
    ! derived-type I/O
    if (config%dtio) then
        write(unit=file_unit, fmt="(2a)") "    procedure :: wf_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic   :: write(formatted) => wf_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    procedure :: rf_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic   :: read(formatted) => rf_", trim(unit_system%units(i_unit)%label())
    end if
    
    write(unit=file_unit, fmt="(3a)") "end type ", trim(unit_system%units(i_unit)%label()), new_line("a")
end subroutine write_type

subroutine write_as_operators(config, unit_system, file_unit, unit)
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type
    
    class(config_type), intent(in)     :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    
    logical :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_as_operators): file_unit must be open")
    
    ! add
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "+")
    
    if (all_close(unit%e, 0.0_WP)) then
        call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "+", unitless_is_real_left=.true.)
        call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "+", unitless_is_real_right=.true.)
    end if
    
    ! subtract
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "-")
    
    if (all_close(unit%e, 0.0_WP)) then
        call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "-", unitless_is_real_left=.true.)
        call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "-", unitless_is_real_right=.true.)
    end if
end subroutine write_as_operators

subroutine write_comparison_operators(config, unit_system, file_unit, unit)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    class(config_type), intent(in)     :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    
    logical :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_comparison_operators): file_unit must be open")
    
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "<")
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, "<=")
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, ">")
    call write_binary_operator(config, unit_system, file_unit, unit, unit, unit, ">=")
end subroutine write_comparison_operators

subroutine write_md_operators(config, unit_system, file_unit, unit_left, unit_right, n_interfaces)
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit
    
    class(config_type), intent(in)     :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit_left, unit_right
    integer, intent(in out)            :: n_interfaces
    
    type(unit_type) :: unit_m, unit_d
    logical         :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_md_operators): file_unit must be open")
    call assert(n_interfaces >= 0, "genunits_io (write_md_operators): n_interfaces is negative")
    
    ! multiply
    unit_m = m_unit(unit_left, unit_right)
    if (unit_m%is_in(unit_system%units)) then
        call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_m, "*")
        n_interfaces = n_interfaces + 1
        
        if (all_close(unit_left%e, 0.0_WP)) then
            call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_m, "*", &
                                        unitless_is_real_left=.true.)
            n_interfaces = n_interfaces + 1
        end if
        
        if (all_close(unit_right%e, 0.0_WP)) then
            call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_m, "*", &
                                        unitless_is_real_right=.true.)
            n_interfaces = n_interfaces + 1
        end if
    end if
    
    ! divide
    unit_d = d_unit(unit_left, unit_right)
    if (unit_d%is_in(unit_system%units)) then
        call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_d, "/")
        n_interfaces = n_interfaces + 1
        
        if (all_close(unit_left%e, 0.0_WP)) then
            call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_d, "/", &
                                        unitless_is_real_left=.true.)
            n_interfaces = n_interfaces + 1
        end if
        
        if (all_close(unit_right%e, 0.0_WP)) then
            call write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_d, "/", &
                                        unitless_is_real_right=.true.)
            n_interfaces = n_interfaces + 1
        end if
    end if
end subroutine write_md_operators

subroutine write_binary_operator(config, unit_system, file_unit, unit_left, unit_right, unit_result, op, &
                                    unitless_is_real_left, unitless_is_real_right)
    ! This subroutine writes binary operator functions, for example: `a + b`.
    
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type
    
    type(config_type), intent(in)      :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit_left, unit_right, unit_result
    character(len=*), intent(in)       :: op
    
    logical, intent(in), optional :: unitless_is_real_left, unitless_is_real_right
    
    character(len=2)              :: op_label
    character(len=:), allocatable :: binary_operator_procedure, real_type
    logical                       :: file_unit_open, conditional, unitless_is_real_left_, unitless_is_real_right_
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_binary_operator): file_unit must be open")
    
    if (present(unitless_is_real_left)) then
        unitless_is_real_left_ = unitless_is_real_left
    else
        unitless_is_real_left_ = .false.
    end if
    
    if (present(unitless_is_real_right)) then
        unitless_is_real_right_ = unitless_is_real_right
    else
        unitless_is_real_right_ = .false.
    end if
    
    select case (op)
        case ("+")
            op_label = "a"
            conditional = .false.
        case ("-")
            op_label = "s"
            conditional = .false.
        case ("*")
            op_label = "m"
            conditional = .false.
        case ("/")
            op_label = "d"
            conditional = .false.
!        case ("**")
!            op_label = "e"
!            conditional = .false.
        case ("<")
            op_label = "lt"
            conditional = .true.
        case ("<=")
            op_label = "le"
            conditional = .true.
        case (">")
            op_label = "gt"
            conditional = .true.
        case (">=")
            op_label = "ge"
            conditional = .true.
        case default
            error stop "write_binary_operator: invalid op"
    end select
    
    call assert(.not. (unitless_is_real_left_ .and. unitless_is_real_right_), "genunits_io (write_binary_operator): " // &
                    "can't have both sides be unitless reals")
    call assert(.not. (unitless_is_real_left_ .and. (.not. all_close(unit_left%e, 0.0_WP))), &
                    "genunits_io (write_binary_operator): unitless_is_real_left=.true., but left is not unitless")
    call assert(.not. (unitless_is_real_right_ .and. (.not. all_close(unit_right%e, 0.0_WP))), &
                    "genunits_io (write_binary_operator): unitless_is_real_right=.true., but right is not unitless")
    
    if (all_close(unit_left%e, 0.0_WP) .and. unitless_is_real_left_) then
        binary_operator_procedure = trim(op_label) // "_real_" // trim(unit_right%label())
    else if (all_close(unit_right%e, 0.0_WP) .and. unitless_is_real_right_) then
        binary_operator_procedure = trim(op_label) // "_" // trim(unit_left%label()) // "_real"
    else
        binary_operator_procedure = trim(op_label) // "_" // trim(unit_left%label()) // "_" // trim(unit_right%label())
    end if
    
    call assert(len(binary_operator_procedure) <= MAX_LABEL_LEN, "genunits_io (write_binary_operator): " &
                        // "binary_operator_procedure name is too long and won't meet the Fortran 2003 standard: " &
                        // binary_operator_procedure)
    
    if (len(config%kind_parameter) == 0) then
        real_type = "real"
    else
        call assert(len(config%kind_parameter) > 1, "genunits_io (write_binary_operator): kind parameter is too short: " &
                                                        // config%kind_parameter)
        real_type = "real(" // config%kind_parameter(2:) // ")"
    end if
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", binary_operator_procedure, "(left, right)"
    
    write(unit=file_unit, fmt="(2a)") "    ! left unit: ", trim(unit_left%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! right unit: ", trim(unit_right%readable(unit_system))
    if (.not. conditional) then
        write(unit=file_unit, fmt="(2a)") "    ! result unit: ", trim(unit_result%readable(unit_system))
    end if
    
    if (all_close(unit_left%e, 0.0_WP) .and. unitless_is_real_left_) then
        write(unit=file_unit, fmt="(3a)") "    ", real_type, ", intent(in) :: left"
    else
        write(unit=file_unit, fmt="(3a)") "    class(", trim(unit_left%label()), "), intent(in) :: left"
    end if
    
    if (all_close(unit_right%e, 0.0_WP) .and. unitless_is_real_right_) then
        write(unit=file_unit, fmt="(3a)") "    ", real_type, ", intent(in) :: right"
    else
        write(unit=file_unit, fmt="(3a)") "    class(", trim(unit_right%label()), "), intent(in) :: right"
    end if
    
    if (conditional) then
        write(unit=file_unit, fmt="(2a)") "    logical :: ", binary_operator_procedure
        write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, " = left%v ", op, " right%v"
    else
        write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_result%label()), ") :: ", binary_operator_procedure
        
        if (all_close(unit_left%e, 0.0_WP) .and. unitless_is_real_left_) then
            write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%v = left ", op, " right%v"
        else if (all_close(unit_right%e, 0.0_WP) .and. unitless_is_real_right_) then
            write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%v = left%v ", op, " right"
        else
            write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%v = left%v ", op, " right%v"
        end if
    end if
    
    write(unit=file_unit, fmt="(3a)") "end function ", binary_operator_procedure, new_line("a")
end subroutine write_binary_operator

subroutine write_unary_operator(unit_system, file_unit, unit, op)
    ! This subroutine writes unary operator functions, for example: `-b`.
    
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    character(len=*), intent(in)       :: op
    
    character(len=1)              :: op_label
    character(len=:), allocatable :: unary_operator_procedure
    logical                       :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_unary_operator): file_unit must be open")
    
    select case (op)
        case ("+")
            op_label = "a"
        case ("-")
            op_label = "s"
        case default
            error stop "write_unary_operator: invalid op"
    end select
    
    unary_operator_procedure = op_label // "_" // trim(unit%label())
    
    call assert(len(unary_operator_procedure) <= MAX_LABEL_LEN, "genunits_io (write_unary_operator): " // &
                        "unary_operator_procedure name is too long and won't meet the Fortran 2003 standard")
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", unary_operator_procedure, "(arg)"
    
    write(unit=file_unit, fmt="(2a)") "    ! argument unit: ", trim(unit%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! result unit: ", trim(unit%readable(unit_system))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(unit%label()), "), intent(in) :: arg"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit%label()), ") :: ", unary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", unary_operator_procedure, "%v = ", op, "arg%v"
    
    write(unit=file_unit, fmt="(3a)") "end function ", unary_operator_procedure, new_line("a")
end subroutine write_unary_operator

subroutine write_unit_function(config, unit_system, file_unit)
    ! Writes a function to return a human readable unit name for a particular unit.
    
    use checks, only: assert
    use genunits_data, only: unit_system_type
    
    class(config_type), intent(in)     :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    
    integer :: i_unit
    logical :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_unit_function): file_unit must be open")
    
    write(unit=file_unit, fmt="(a)") "pure function unit(arg)"
    
    write(unit=file_unit, fmt="(a)") "    class(*), intent(in) :: arg"
    
    write(unit=file_unit, fmt="(a)") "    character(len=32) :: unit"
    
    write(unit=file_unit, fmt="(a)") "    select type (arg)"
    
    do i_unit = 1, size(unit_system%units) ! SERIAL
        write(unit=file_unit, fmt="(3a)") "        type is (", trim(unit_system%units(i_unit)%label()), ")"
        write(unit=file_unit, fmt="(3a)") '            unit = "', trim(unit_system%units(i_unit)%readable(unit_system)), '"'
    end do
    
    write(unit=file_unit, fmt="(a)") "        class default"
    write(unit=file_unit, fmt="(3a)") '            error stop "', config%module_name, ' (unit): invalid type"'
    
    write(unit=file_unit, fmt="(a)") "    end select"
    
    write(unit=file_unit, fmt="(2a)") "end function unit", new_line("a")
end subroutine write_unit_function

subroutine write_unit_wf(unit_system, file_unit, unit)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    
    character(len=:), allocatable :: wf_procedure
    logical                       :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_unit_wf): file_unit must be open")
    
    wf_procedure = "wf_" // trim(unit%label())
    
    write(unit=file_unit, fmt="(3a)") "subroutine ", wf_procedure, "(dtv, unit, iotype, vlist, iostat, iomsg)"
    
    write(unit=file_unit, fmt="(2a)") "    ! unit: ", trim(unit%readable(unit_system))
    
    write(unit=file_unit, fmt="(a)") "    use, intrinsic :: iso_fortran_env, only: compiler_version"
    write(unit=file_unit, fmt="(3a)") "    class(", trim(unit%label()), "), intent(in) :: dtv"
    write(unit=file_unit, fmt="(a)") "    integer, intent(in) :: unit"
    write(unit=file_unit, fmt="(a)") "    character(len=*), intent(in) :: iotype"
    write(unit=file_unit, fmt="(a)") "    integer, intent(in) :: vlist(:)"
    write(unit=file_unit, fmt="(a)") "    integer, intent(out) :: iostat"
    write(unit=file_unit, fmt="(a)") "    character(len=*), intent(in out) :: iomsg"
    write(unit=file_unit, fmt="(a)") "    character(len=16) :: pfmt"
    write(unit=file_unit, fmt="(a)") "    character(len=1)  :: space"
    write(unit=file_unit, fmt="(a)") '    if (iotype == "LISTDIRECTED" .or. iotype == "DTg0" .or. iotype == "NAMELIST") then'
    write(unit=file_unit, fmt="(a)") '        if (index(compiler_version(), "XL Fortran") == 0) then'
    write(unit=file_unit, fmt="(a)") '            pfmt = "(g0, a, a)"'
    write(unit=file_unit, fmt="(a)") "        else"
    write(unit=file_unit, fmt="(a)") '            pfmt = "(f0.6, a, a)"'
    write(unit=file_unit, fmt="(a)") "        end if"
    write(unit=file_unit, fmt="(a)") "    else"
    write(unit=file_unit, fmt="(a, a)") '        write(pfmt, "(2a, i0, a, i0, a)") ', &
                                            '"(", iotype(3:), vlist(1), ".", vlist(2), ", a, a)"'
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") '    if (iotype == "NAMELIST") then'
    write(unit=file_unit, fmt="(a)") '        space = "_"'
    write(unit=file_unit, fmt="(a)") "    else"
    write(unit=file_unit, fmt="(a)") '        space = " "'
    write(unit=file_unit, fmt="(a)") "    end if"
    
    write(unit=file_unit, fmt="(3a)") '    write(unit, fmt=trim(pfmt), iostat=iostat, iomsg=iomsg) dtv%v, space, "', &
                                        trim(unit%readable(unit_system)), '"'
    
    write(unit=file_unit, fmt="(3a)") "end subroutine ", wf_procedure, new_line("a")
end subroutine write_unit_wf

subroutine write_unit_rf(unit_system, file_unit, unit)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    
    character(len=:), allocatable :: rf_procedure
    logical                       :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_unit_rf): file_unit must be open")
    
    rf_procedure = "rf_" // trim(unit%label())
    
    write(unit=file_unit, fmt="(3a)") "subroutine ", rf_procedure, "(dtv, unit, iotype, vlist, iostat, iomsg)"
    
    write(unit=file_unit, fmt="(a)") "    use, intrinsic :: iso_fortran_env, only: IOSTAT_END, IOSTAT_EOR"
    write(unit=file_unit, fmt="(2a)") "    ! unit: ", trim(unit%readable(unit_system))
    
    write(unit=file_unit, fmt="(3a)") "    class(", trim(unit%label()), "), intent(in out) :: dtv"
    write(unit=file_unit, fmt="(a)") "    integer, intent(in) :: unit"
    write(unit=file_unit, fmt="(a)") "    character(len=*), intent(in) :: iotype"
    write(unit=file_unit, fmt="(a)") "    integer, intent(in) :: vlist(:)"
    write(unit=file_unit, fmt="(a)") "    integer, intent(out) :: iostat"
    write(unit=file_unit, fmt="(a)") "    character(len=*), intent(in out) :: iomsg"
    write(unit=file_unit, fmt="(a)") "    character(len=2048) :: full_input, value_char, msg"
    write(unit=file_unit, fmt="(a)") "    character(len=64) :: unit_char"
    write(unit=file_unit, fmt="(a)") "    integer :: underscore_index, end_index"
    write(unit=file_unit, fmt="(a)") '    read(unit, fmt="(a)", iostat=iostat, iomsg=msg) full_input'
    write(unit=file_unit, fmt="(a)") "    if ((iostat == IOSTAT_END) .or. (iostat == IOSTAT_EOR)) then"
    write(unit=file_unit, fmt="(a)") "        iostat = 0"
    write(unit=file_unit, fmt="(a)") '        msg = ""'
    write(unit=file_unit, fmt="(a)") "    end if"
    ! Needed for ifx namelist input.
    write(unit=file_unit, fmt="(a)") "    full_input = adjustl(full_input)"
    ! Spaces are namelist "value separators" in the standard, so the separators should be something else.
    write(unit=file_unit, fmt="(a)") '    underscore_index = index(full_input, "_")'
    ! The next line will handle buggy compilers that include the `/` in `full_input`.
    write(unit=file_unit, fmt="(a)") '    if (index(full_input, "/") > 0) then'
    write(unit=file_unit, fmt="(a)") '        end_index = index(full_input, "/") - 1'
    write(unit=file_unit, fmt="(a)") '    else'
    write(unit=file_unit, fmt="(a)") '        end_index = len(trim(full_input))'
    write(unit=file_unit, fmt="(a)") '    end if'
    write(unit=file_unit, fmt="(a)") "    if (underscore_index == 0) then"
    ! If the unit isn't present, read the number anyway.
    write(unit=file_unit, fmt="(a)") "        underscore_index = end_index + 1"
    write(unit=file_unit, fmt="(a)") '        unit_char = ""'
    write(unit=file_unit, fmt="(a)") "    else"
    write(unit=file_unit, fmt="(a)") "        unit_char = full_input(underscore_index+1:end_index)"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") "    value_char = full_input(1:underscore_index-1)"
    write(unit=file_unit, fmt="(a)") "    read(unit=value_char, fmt=*, iostat=iostat, iomsg=msg) dtv%v"
    write(unit=file_unit, fmt="(a)") "    if (iostat /= 0) then"
    write(unit=file_unit, fmt="(a)") "        iomsg = trim(msg)"
    write(unit=file_unit, fmt="(a)") "        return"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") "    if (len(trim(unit_char)) == 0) then"
    write(unit=file_unit, fmt="(a)") "        iostat = 1"
    write(unit=file_unit, fmt="(a)") '        iomsg = "Unit expected, none appeared: " // trim(full_input)'
    write(unit=file_unit, fmt="(a)") "        return"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") '    if (trim(unit_char) /= "' // trim(unit%readable(unit_system)) // '") then'
    write(unit=file_unit, fmt="(a)") "        iostat = 2"
    write(unit=file_unit, fmt="(a)") '        iomsg = "Unit mismatch: ' // trim(unit%readable(unit_system)) // &
                                        ' expected, " // trim(unit_char) // " appeared."'
    write(unit=file_unit, fmt="(a)") "        return"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") '    if ((iotype /= "DT") .and. (iotype /= "NAMELIST")) then'
    write(unit=file_unit, fmt="(a)") "        iostat = 3"
    write(unit=file_unit, fmt="(a)") "        iomsg = 'Only iotype=" // '"' // "DT" // '"' // " and iotype=" // '"' // &
                                        "NAMELIST" // '"' // " are implemented for read. iotype=' // iotype"
    write(unit=file_unit, fmt="(a)") "        return"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(a)") "    if (size(vlist) > 0) then"
    write(unit=file_unit, fmt="(a)") "        iostat = 4"
    write(unit=file_unit, fmt="(a)") '        iomsg = "vlist must be not specified for read."'
    write(unit=file_unit, fmt="(a)") "        return"
    write(unit=file_unit, fmt="(a)") "    end if"
    write(unit=file_unit, fmt="(3a)") "end subroutine ", rf_procedure, new_line("a")
end subroutine write_unit_rf

subroutine write_exponentiation_interfaces(use_sqrt, use_cbrt, use_square, unit_system, file_unit)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    logical, intent(in)                :: use_sqrt, use_cbrt, use_square
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    
    integer         :: i_unit
    type(unit_type) :: trial_unit
    logical         :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_exponentiation_interfaces): file_unit must be open")
    
    if (use_sqrt) then
        write(unit=file_unit, fmt="(a)") "interface sqrt"
        do i_unit = 1, size(unit_system%units) ! SERIAL
            trial_unit = sqrt_unit(unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    module procedure sqrt_", trim(unit_system%units(i_unit)%label())
            else
                write(unit=file_unit, fmt="(2a)") "    ! excluded: sqrt_", trim(unit_system%units(i_unit)%label())
            end if
        end do
        write(unit=file_unit, fmt="(2a)") "end interface sqrt", new_line("a")
    end if
    
    if (use_cbrt) then
        write(unit=file_unit, fmt="(a)") "interface cbrt"
        do i_unit = 1, size(unit_system%units) ! SERIAL
            trial_unit = cbrt_unit(unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    module procedure cbrt_", trim(unit_system%units(i_unit)%label())
            else
                write(unit=file_unit, fmt="(2a)") "    ! excluded: cbrt_", trim(unit_system%units(i_unit)%label())
            end if
        end do
        write(unit=file_unit, fmt="(2a)") "end interface cbrt", new_line("a")
    end if
    
    if (use_square) then
        write(unit=file_unit, fmt="(a)") "interface square"
        do i_unit = 1, size(unit_system%units) ! SERIAL
            trial_unit = square_unit(unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    module procedure square_", trim(unit_system%units(i_unit)%label())
            else
                write(unit=file_unit, fmt="(2a)") "    ! excluded: square_", trim(unit_system%units(i_unit)%label())
            end if
        end do
        write(unit=file_unit, fmt="(2a)") "end interface square", new_line("a")
    end if
end subroutine write_exponentiation_interfaces

subroutine write_exponentiation_function(config, unit_system, file_unit, unit, op)
    ! Writes exponentiation functions. For compile-time unit checking, this is limited to special cases. The compiler can't know
    ! the units of `a**2` at present, but it can know the units of `square(a)`.
    
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    type(config_type), intent(in)      :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    character(len=*), intent(in)       :: op
    
    type(unit_type)               :: unit_result
    character(len=:), allocatable :: op_pre, op_post, exponentiation_function
    logical                       :: file_unit_open
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_exponentiation_function): file_unit must be open")
    
    select case (op)
        case ("sqrt")
            op_pre = "sqrt("
            op_post = ")"
            
            unit_result = sqrt_unit(unit)
        case ("cbrt")
            ! <https://community.intel.com/t5/Intel-Fortran-Compiler/Fast-cube-root/m-p/1171728>
            ! <https://www.reddit.com/r/fortran/comments/t9qkqd/cuberoot_and_my_dissent_into_madness/>
            ! <https://github.com/fortran-lang/stdlib/issues/214>
            op_pre = "("
            op_post = ")**(1.0" // config%kind_parameter // "/3.0" // config%kind_parameter // ")"
            
            unit_result = cbrt_unit(unit)
        case ("square")
            op_pre = "("
            op_post = ")**2"
            
            unit_result = square_unit(unit)
        case default
            error stop "write_exponentiation_function: invalid op"
    end select
    
    exponentiation_function = op // "_" // trim(unit%label())
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", exponentiation_function, "(arg)"
    
    write(unit=file_unit, fmt="(2a)") "    ! arg: ", trim(unit%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! result: ", trim(unit_result%readable(unit_system))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(unit%label()), "), intent(in) :: arg"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_result%label()), ") :: ", exponentiation_function
    
    write(unit=file_unit, fmt="(6a)") "    ", exponentiation_function, "%v = ", op_pre, "arg%v", op_post
    
    write(unit=file_unit, fmt="(3a)") "end function ", exponentiation_function, new_line("a")
end subroutine write_exponentiation_function

subroutine write_intrinsic_interfaces(unit_system, file_unit)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    
    logical :: file_unit_open
    integer :: i_intrinsic
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_intrinsic_interfaces): file_unit must be open")
    
    do i_intrinsic = 1, size(INTRINSIC_1ARG_UNITLESS) ! SERIAL
        call write_intrinsic_interface(unit_system, file_unit, trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic)), .true.)
    end do
    
    do i_intrinsic = 1, size(INTRINSIC_1ARG_WITHUNITS) ! SERIAL
        call write_intrinsic_interface(unit_system, file_unit, trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic)), .false.)
    end do
    
    do i_intrinsic = 1, size(INTRINSIC_2ARG_WITHUNITS) ! SERIAL
        call write_intrinsic_interface(unit_system, file_unit, trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic)), .false.)
    end do
end subroutine write_intrinsic_interfaces

subroutine write_intrinsic_interface(unit_system, file_unit, fun, unitless)
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    character(len=*), intent(in)       :: fun
    logical, intent(in)                :: unitless
    
    type(unit_type) :: unit
    logical         :: file_unit_open
    integer         :: i_unit
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_intrinsic_interface): file_unit must be open")
    
    if (unitless) then
        allocate(unit%e(unit_system%n_base_units))
        unit%e = 0.0_WP
        call assert(unit%is_in(unit_system%units), &
                    "genunits_io (write_intrinsic_interface): unitless=.true. requires that unitless be in the unit_system")
    end if
    
    call assert(index(fun, " ") == 0, &
                    "genunits_io (write_intrinsic_interface): spaces should not be in the function name: '" // fun // "'")
    
    write(unit=file_unit, fmt="(2a)") "interface ", fun
    do i_unit = 1, size(unit_system%units) ! SERIAL
        if (unitless .and. (.not. all_close(unit_system%units(i_unit)%e, 0.0_WP))) then
            cycle
        end if
        
        write(unit=file_unit, fmt="(4a)") "    module procedure ", fun, "_", trim(unit_system%units(i_unit)%label())
    end do
    write(unit=file_unit, fmt="(3a)") "end interface ", fun, new_line("a")
end subroutine write_intrinsic_interface

subroutine write_intrinsic_function(unit_system, file_unit, fun, unitless, n_args)
    ! Writes intrinsic functions. Input units are same as output units.
    ! This can handle an arbitary number of arguments, but only up to two arguments are used at present.
    
    use checks, only: assert, all_close
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    character(len=*), intent(in)       :: fun ! function name
    logical, intent(in)                :: unitless
    integer, intent(in)                :: n_args
    
    character(len=:), allocatable :: intrinsic_function
    logical                       :: file_unit_open
    type(unit_type)               :: unit
    integer                       :: i_unit, i_arg
    
    inquire(unit=file_unit, opened=file_unit_open)
    call assert(file_unit_open, "genunits_io (write_intrinsic_function): file_unit must be open")
    
    call assert(n_args >= 1, "genunits_io (write_intrinsic_function): n_args must be greater than or equal to 1")
    call assert(n_args < 20, "genunits_io (write_intrinsic_function): huge number of arguments?")
    
    call assert(index(fun, " ") == 0, &
                    "genunits_io (write_intrinsic_function): spaces should not be in the function name '" // fun // "'")
    if (unitless) then
        allocate(unit%e(unit_system%n_base_units))
        unit%e = 0.0_WP
        call assert(unit%is_in(unit_system%units), &
                    "genunits_io (write_intrinsic_function): unitless=.true. requires that unitless be in the unit_system")
    end if
    
    do i_unit = 1, size(unit_system%units) ! SERIAL
        if (unitless .and. (.not. all_close(unit_system%units(i_unit)%e, 0.0_WP))) then
            cycle
        end if
        
        intrinsic_function = fun // "_" // trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(3a)", advance="no") "elemental function ", intrinsic_function, "("
        
        do i_arg = 1, n_args ! SERIAL
            write(unit=file_unit, fmt="(a, i0)", advance="no") "arg", i_arg
            
            if (i_arg /= n_args) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end do
        write(unit=file_unit, fmt="(a)") ")"
        
        write(unit=file_unit, fmt="(2a)") "    ! args: ", trim(unit_system%units(i_unit)%readable(unit_system))
        write(unit=file_unit, fmt="(2a)") "    ! result: ", trim(unit_system%units(i_unit)%readable(unit_system))
        
        do i_arg = 1, n_args ! SERIAL
            write(unit=file_unit, fmt="(3a, i0)") "    class(", trim(unit_system%units(i_unit)%label()), &
                                                    "), intent(in) :: arg", i_arg
        end do
        
        write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_system%units(i_unit)%label()), ") :: ", intrinsic_function
        
        write(unit=file_unit, fmt="(5a)", advance="no") "    ", intrinsic_function, "%v = ", fun, "("
        do i_arg = 1, n_args ! SERIAL
            write(unit=file_unit, fmt="(a, i0, a)", advance="no") "arg", i_arg, "%v"
            
            if (i_arg /= n_args) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end do
        write(unit=file_unit, fmt="(a)") ")"
        
        write(unit=file_unit, fmt="(3a)") "end function ", intrinsic_function, new_line("a")
    end do
end subroutine write_intrinsic_function

subroutine write_module(config, unit_system, file_unit, rc)
    use checks, only: all_close, assert
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    class(config_type), intent(in)      :: config
    class(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                 :: file_unit
    integer, intent(out)                :: rc
    
    integer           :: i_seed_unit, i_unit, j_unit, max_label_len, n_interfaces, i_space, i_intrinsic, j_intrinsic
    character(len=10) :: n_char
    character(len=20) :: use_format
    type(unit_type)   :: trial_unit
    logical           :: use_sqrt, use_cbrt, use_square
    
    write(unit=n_char, fmt="(i0)") size(unit_system%units)
    print "(a)", "Generated " // trim(n_char) // " physical dimensions. Writing " // config%output_file // "..."
    
    ! Now actually write the module.
    
    write(unit=file_unit, fmt="(2a)") "! Automatically generated by genunits.", new_line("a")
    
    ! Most programmers don't know much about copyright. My understanding is that the output of this program can't be copyrighted
    ! in the US because the output doesn't contain any "creative expression". It's basically just an implementation of well-known
    ! mathematical rules for quantity calculus.
    write(unit=file_unit, fmt="(2a)") "! genunits output Fortran code is in the public domain.", new_line("a")
    write(unit=file_unit, fmt="(3a)") "module ", config%module_name, new_line("a")
    if (len(trim(config%use_line)) > 0) then
        write(unit=file_unit, fmt="(2a)") config%use_line, new_line("a")
    end if
    write(unit=file_unit, fmt="(2a)") "implicit none"
    write(unit=file_unit, fmt="(2a)") "private", new_line("a")
    
    use_sqrt   = .false.
    use_cbrt   = .false.
    use_square = .false.
    if (config%sqrt .or. config%cbrt .or. config%square) then
        do i_unit = 1, size(unit_system%units) ! SERIAL
            if (config%sqrt) then
                trial_unit = sqrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    use_sqrt = .true.
                    exit
                end if
            end if
        end do
        
        do i_unit = 1, size(unit_system%units) ! SERIAL
            if (config%cbrt) then
                trial_unit = cbrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    use_cbrt = .true.
                    exit
                end if
            end if
        end do
        
        do i_unit = 1, size(unit_system%units) ! SERIAL
            if (config%cbrt) then
                trial_unit = square_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    use_square = .true.
                    exit
                end if
            end if
        end do
    end if
    
    write(unit=file_unit, fmt="(a)") "public :: unit"
    
    if (config%intrinsics) then
        if (size(INTRINSIC_1ARG_UNITLESS) > 0) then
            write(unit=file_unit, fmt="(a)", advance="no") "public :: "
            do i_intrinsic = 1, size(INTRINSIC_1ARG_UNITLESS) ! SERIAL
                if (i_intrinsic /= size(INTRINSIC_1ARG_UNITLESS)) then
                    write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic)), ", "
                else
                    write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic))
                end if
            end do
            write(unit=file_unit, fmt="(a)") ""
        end if
        
        if (size(INTRINSIC_1ARG_WITHUNITS) > 0) then
            write(unit=file_unit, fmt="(a)", advance="no") "public :: "
            do i_intrinsic = 1, size(INTRINSIC_1ARG_WITHUNITS) ! SERIAL
                if (i_intrinsic /= size(INTRINSIC_1ARG_WITHUNITS)) then
                    write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic)), ", "
                else
                    write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic))
                end if
            end do
            write(unit=file_unit, fmt="(a)") ""
        end if
        
        if (size(INTRINSIC_2ARG_WITHUNITS) > 0) then
            write(unit=file_unit, fmt="(a)", advance="no") "public :: "
            do i_intrinsic = 1, size(INTRINSIC_2ARG_WITHUNITS) ! SERIAL
                if (i_intrinsic /= size(INTRINSIC_2ARG_WITHUNITS)) then
                    write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic)), ", "
                else
                    write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic))
                end if
            end do
            write(unit=file_unit, fmt="(a)") ""
        end if
    end if
    
    if (use_sqrt) then
        write(unit=file_unit, fmt="(a)") "public :: sqrt"
    end if
    
    if (use_cbrt) then
        write(unit=file_unit, fmt="(a)") "public :: cbrt"
    end if
    
    if (use_square) then
        write(unit=file_unit, fmt="(a)") "public :: square"
    end if
    
    if (use_sqrt .or. use_cbrt .or. use_square .or. config%intrinsics) then
        write(unit=file_unit, fmt="(a)") ""
    end if
    
    ! Write `use` lines as comments.
    if (size(config%seed_labels) > 0) then
        write(unit=file_unit, fmt="(3a)", advance="no") "!use ", config%module_name, ", only: "
        
        max_label_len = 0
        do i_seed_unit = 1, size(config%seed_labels) ! SERIAL
            max_label_len = max(max_label_len, len(trim(config%seed_labels(i_seed_unit))))
        end do
        
        do i_seed_unit = 1, size(config%seed_labels) ! SERIAL
            if (i_seed_unit /= 1) then
                write(unit=file_unit, fmt="(a)", advance="no") "!"
                do i_space = 1, 12 + len(config%module_name) ! SERIAL
                    write(unit=file_unit, fmt="(a)", advance="no") " "
                end do
            end if
            
            ! Align the `=>` and left-justify the labels.
            ! <https://fortran-lang.discourse.group/t/left-justification-of-strings/345>
            write(unit=use_format, fmt="(a, i0, a)") "(a, tr", max_label_len &
                                                                    - len(trim(config%seed_labels(i_seed_unit))) + 1, ", 2a)"
            write(unit=file_unit, fmt=trim(use_format), advance="no") trim(config%seed_labels(i_seed_unit)), &
                                                                        "=> ", &
                                                                        trim(config%seed_units(i_seed_unit)%label())
            if (config%intrinsics .or. use_sqrt .or. use_cbrt .or. use_square) then
                write(unit=file_unit, fmt="(a)") ", &"
            else
                if (i_seed_unit /= size(config%seed_labels)) then
                    write(unit=file_unit, fmt="(a)") ", &"
                else
                    write(unit=file_unit, fmt="(a)") ""
                end if
            end if
        end do
        
        write(unit=file_unit, fmt="(a)", advance="no") "!"
        do i_space = 1, 12 + len(config%module_name) ! SERIAL
            write(unit=file_unit, fmt="(a)", advance="no") " "
        end do
        write(unit=file_unit, fmt="(a)", advance="no") "unit, "
        
        if (config%intrinsics) then
            if (size(INTRINSIC_1ARG_UNITLESS) > 0) then
                do i_intrinsic = 1, size(INTRINSIC_1ARG_UNITLESS) ! SERIAL
                    if (i_intrinsic /= size(INTRINSIC_1ARG_UNITLESS)) then
                        write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic)), ", "
                    else
                        write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic))
                    end if
                end do
            end if
            
            if (size(INTRINSIC_1ARG_WITHUNITS) > 0) then
                if (size(INTRINSIC_1ARG_WITHUNITS) > 0) then
                    write(unit=file_unit, fmt="(a)", advance="no") ", "
                end if
                
                do i_intrinsic = 1, size(INTRINSIC_1ARG_WITHUNITS) ! SERIAL
                    if (i_intrinsic /= size(INTRINSIC_1ARG_WITHUNITS)) then
                        write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic)), ", "
                    else
                        write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic))
                    end if
                end do
            end if
            
            if (size(INTRINSIC_2ARG_WITHUNITS) > 0) then
                if (size(INTRINSIC_2ARG_WITHUNITS) > 0) then
                    write(unit=file_unit, fmt="(a)", advance="no") ", "
                end if
                
                do i_intrinsic = 1, size(INTRINSIC_2ARG_WITHUNITS) ! SERIAL
                    if (i_intrinsic /= size(INTRINSIC_2ARG_WITHUNITS)) then
                        write(unit=file_unit, fmt="(2a)", advance="no") trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic)), ", "
                    else
                        write(unit=file_unit, fmt="(a)", advance="no") trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic))
                    end if
                end do
            end if
            
            if ((size(INTRINSIC_1ARG_UNITLESS) > 0) &
                    .or. (size(INTRINSIC_1ARG_WITHUNITS) > 0) &
                    .or. (size(INTRINSIC_2ARG_WITHUNITS) > 0)) then
                if (use_sqrt .or. use_cbrt .or. use_square) then
                    write(unit=file_unit, fmt="(a)", advance="no") ", "
                end if
            end if
        end if
        
        if (use_sqrt) then
            write(unit=file_unit, fmt="(a)", advance="no") "sqrt"
            
            if (use_cbrt .or. use_square) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end if
        
        if (use_cbrt) then
            write(unit=file_unit, fmt="(a)", advance="no") "cbrt"
            
            if (use_square) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end if
        
        if (use_square) then
            write(unit=file_unit, fmt="(a)", advance="no") "square"
        end if
        
        if (use_sqrt .or. use_cbrt .or. use_square .or. config%intrinsics) then
            write(unit=file_unit, fmt="(a)") ""
        end if
        
        write(unit=file_unit, fmt="(a)") ""
    end if
    
    do i_unit = 1, size(unit_system%units) ! SERIAL
        call config%write_type(file_unit, i_unit, unit_system)
    end do
    
    call write_exponentiation_interfaces(use_sqrt, use_cbrt, use_square, unit_system, file_unit)
    if (config%intrinsics) then
        call write_intrinsic_interfaces(unit_system, file_unit)
    end if
    
    write(unit=file_unit, fmt="(2a)") "contains", new_line("a")
    
    n_interfaces = 0
    
    call write_unit_function(config, unit_system, file_unit)
    n_interfaces = n_interfaces + 1
    
    do i_unit = 1, size(unit_system%units) ! SERIAL
        if (config%dtio) then
            call write_unit_wf(unit_system, file_unit, unit_system%units(i_unit))
            call write_unit_rf(unit_system, file_unit, unit_system%units(i_unit))
            n_interfaces = n_interfaces + 2
        end if
        
        call write_as_operators(config, unit_system, file_unit, unit_system%units(i_unit))
        n_interfaces = n_interfaces + 2
        
        if (all_close(unit_system%units(i_unit)%e, 0.0_WP)) then
            n_interfaces = n_interfaces + 4
        end if
        
        if (config%comparison) then
            call write_comparison_operators(config, unit_system, file_unit, unit_system%units(i_unit))
            n_interfaces = n_interfaces + 4
        end if
        
        if (config%unary) then
            call write_unary_operator(unit_system, file_unit, unit_system%units(i_unit), "+")
            call write_unary_operator(unit_system, file_unit, unit_system%units(i_unit), "-")
            n_interfaces = n_interfaces + 2
        end if
    end do
    
    do i_unit = 1, size(unit_system%units) ! SERIAL
        do j_unit = 1, size(unit_system%units) ! SERIAL
            call write_md_operators(config, unit_system, file_unit, unit_system%units(i_unit), unit_system%units(j_unit), &
                                    n_interfaces)
        end do
    end do
    
    if (use_sqrt .or. use_cbrt .or. use_square) then
        do i_unit = 1, size(unit_system%units) ! SERIAL
            if (use_sqrt) then
                trial_unit = sqrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(config, unit_system, file_unit, unit_system%units(i_unit), "sqrt")
                    n_interfaces = n_interfaces + 1
                end if
            end if
            
            if (use_cbrt) then
                trial_unit = cbrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(config, unit_system, file_unit, unit_system%units(i_unit), "cbrt")
                    n_interfaces = n_interfaces + 1
                end if
            end if
            
            if (use_square) then
                trial_unit = square_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(config, unit_system, file_unit, unit_system%units(i_unit), "square")
                    n_interfaces = n_interfaces + 1
                end if
            end if
        end do
    end if
    
    if (config%intrinsics) then
        do i_intrinsic = 1, size(INTRINSIC_1ARG_UNITLESS) ! SERIAL
            do j_intrinsic = 1, size(INTRINSIC_1ARG_WITHUNITS) ! SERIAL
                call assert(trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic)) /= trim(INTRINSIC_1ARG_WITHUNITS(j_intrinsic)), &
                                "genunits_io (write_module): can't have an intrinsic in both " &
                                // "INTRINSIC_1ARG_UNITLESS and INTRINSIC_1ARG_WITHUNITS.")
            end do
        end do
        
        do i_intrinsic = 1, size(INTRINSIC_1ARG_UNITLESS) ! SERIAL
            call write_intrinsic_function(unit_system, file_unit, trim(INTRINSIC_1ARG_UNITLESS(i_intrinsic)), .true., 1)
            n_interfaces = n_interfaces + 1
        end do
        
        do i_intrinsic = 1, size(INTRINSIC_1ARG_WITHUNITS) ! SERIAL
            call write_intrinsic_function(unit_system, file_unit, trim(INTRINSIC_1ARG_WITHUNITS(i_intrinsic)), .false., 1)
            n_interfaces = n_interfaces + size(unit_system%units)
        end do
        
        do i_intrinsic = 1, size(INTRINSIC_2ARG_WITHUNITS) ! SERIAL
            call write_intrinsic_function(unit_system, file_unit, trim(INTRINSIC_2ARG_WITHUNITS(i_intrinsic)), .false., 2)
            n_interfaces = n_interfaces + size(unit_system%units)
        end do
    end if
    
    write(unit=file_unit, fmt="(2a)") "end module ", config%module_name
    
    write(unit=n_char, fmt="(i0)") n_interfaces
    print "(a)", "Generated " // trim(n_char) // " interfaces."
    
    rc = 0
    
    call assert(n_interfaces >= 0, "genunits_io (write_module): n_interfaces is negative")
end subroutine write_module

end module genunits_io
