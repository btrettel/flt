! module containing I/0 procedures to generate a units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module genunits_io

use prec, only: WP
use nmllog, only: log_type
use genunits_data, only: MAX_LABEL_LEN, BASE_UNIT_LEN, unit_type
implicit none
private

public :: in_exponent_bounds, denominator_matches, &
            write_as_operators, write_md_operators, write_binary_operator, &
            write_unary_operator, &
            write_exponentiation_interfaces, write_exponentiation_function, &
            write_module

character(len=*), parameter :: GENUNITS_LOG = "genunits.nml"

integer, public, parameter :: DEFAULT_MAX_N_UNITS = 28, & ! This is about the most the ifx will compile as of 2024-05-12.
                              DEFAULT_MAX_ITER    = 50, &
                              DEFAULT_DENOMINATOR = 1

type, public :: config_type
    character(len=:), allocatable :: output_file, type_definition, use_line
    real(kind=WP), allocatable    :: min_exponents(:), &
                                     max_exponents(:)
    integer, allocatable          :: denominators(:)
    
    integer :: max_n_units, max_iter !, max_n_interfaces
    logical :: tests, comparison, unary, sqrt, cbrt, square, intrinsics
    
    type(log_type) :: logger
    
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
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use genunits_data, only: MAX_BASE_UNITS, BASE_UNIT_LEN
    
    use prec, only: CL
    use checks, only: is_close
    use nmllog, only: INFO_LEVEL
    
    class(config_type), intent(out) :: config_out
    character(len=*), intent(in)    :: filename
    integer, intent(out)            :: rc
    
    integer           :: i_base_unit, nml_unit, rc_nml, n_failures, n_base_units
    character(len=CL) :: nml_error_message
    
    ! `config` namelist group
    character(len=CL)            :: output_file, type_definition, use_line
    character(len=BASE_UNIT_LEN) :: base_units(MAX_BASE_UNITS)
    real(kind=WP)                :: min_exponents(MAX_BASE_UNITS), max_exponents(MAX_BASE_UNITS)
    integer                      :: denominators(MAX_BASE_UNITS), max_n_units, max_iter
    logical                      :: tests, comparison, unary, sqrt, cbrt, square, intrinsics
    
    namelist /config/ output_file, base_units, type_definition, use_line, min_exponents, max_exponents, denominators, &
                        max_n_units, tests, comparison, unary, sqrt, cbrt, square, intrinsics
    
    call config_out%logger%open(GENUNITS_LOG, level=INFO_LEVEL)
    
    do i_base_unit = 1, MAX_BASE_UNITS
        base_units(i_base_unit) = ""
    end do
    
    output_file     = ""
    type_definition = "real(kind=WP)"
    use_line        = "use prec, only: WP"
    min_exponents   = -huge(1.0_WP)
    max_exponents   = huge(1.0_WP)
    denominators    = DEFAULT_DENOMINATOR
    max_n_units     = DEFAULT_MAX_N_UNITS
    max_iter        = DEFAULT_MAX_ITER
    tests           = .true.
    comparison      = .true.
    unary           = .true.
    sqrt            = .true.
    cbrt            = .true.
    square          = .true.
    intrinsics      = .true.
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        call config_out%logger%error(trim(nml_error_message))
        rc = rc_nml
        close(unit=nml_unit)
        return
    end if
    
    n_base_units = 0
    do i_base_unit = 1, MAX_BASE_UNITS
        if (trim(base_units(i_base_unit)) /= "") then
            n_base_units = n_base_units + 1
        end if
    end do
    
    config_out%output_file     = trim(output_file)
    config_out%type_definition = trim(type_definition)
    config_out%use_line        = trim(use_line)
    config_out%min_exponents   = min_exponents(1:n_base_units)
    config_out%max_exponents   = max_exponents(1:n_base_units)
    config_out%denominators    = denominators(1:n_base_units)
    config_out%max_n_units     = max_n_units
    config_out%max_iter        = max_iter
    config_out%base_units      = base_units(1:n_base_units)
    config_out%tests           = tests
    config_out%comparison      = comparison
    config_out%unary           = unary
    config_out%sqrt            = sqrt
    config_out%cbrt            = cbrt
    config_out%square          = square
    config_out%intrinsics      = intrinsics
    
    n_failures = 0
    
    call config_out%logger%check(n_base_units > 0, "base_units must have 1 or more members", n_failures)
    call config_out%logger%check(len(type_definition) > 0, "type_definition must have 1 or more characters", n_failures)
    call config_out%logger%check(all(denominators >= 1), "all denominators must be 1 or more", n_failures)
    
    do i_base_unit = 1, n_base_units
        call config_out%logger%check(.not. is_close(min_exponents(i_base_unit), -huge(1.0_WP)), &
                                        "min_exponents is not set properly?", n_failures)
        call config_out%logger%check(.not. is_close(max_exponents(i_base_unit), huge(1.0_WP)), &
                                        "max_exponents is not set properly?", n_failures)
    end do
    
    ! TODO: min_exponents <= max_exponents
    
    call config_out%logger%check(config_out%max_n_units > 0, "max_n_units must be 1 or greater.", n_failures)
    
    if (n_failures > 0) then
        call config_out%logger%error("config namelist input validation error(s)")
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_config_namelist

subroutine read_seed_unit_namelists(config, filename, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use checks, only: assert, is_close, all_close
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
    real(kind=WP)                :: e(MAX_BASE_UNITS)
    
    namelist /seed_unit/ label, e
    
    call assert(allocated(config%base_units))
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    
    ! First get `n_seed_units`, allocate `config`, and read everything in.
    n_seed_units = 0
    n_failures   = 0
    do
        label = ""
        e = 0.0_WP
        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call config%logger%error(trim(nml_error_message))
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        n_seed_units = n_seed_units + 1
    end do
    
    write(unit=n_seed_units_string, fmt="(i0)") n_seed_units
    write(unit=max_n_units_string, fmt="(i0)") config%max_n_units
    call config%logger%check(n_seed_units <= config%max_n_units, &
                                    "The number of seed units (" // trim(n_seed_units_string) &
                                    // ") exceed max_n_units, the maximum number of units allowed (" // trim(max_n_units_string) &
                                    // "). Either increase max_n_units or reduce the number of seed units.", &
                                    n_failures)
    
    rewind nml_unit
    allocate(config%seed_labels(n_seed_units))
    allocate(config%seed_units(n_seed_units))
    i_seed_unit = 0
    do
        label = ""
        e     = huge(1.0_WP)
        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call config%logger%error(trim(nml_error_message))
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        i_seed_unit = i_seed_unit + 1
        
        !write(unit=*, fmt=*) i_seed_unit, trim(label), e(1:size(config%base_units))
        config%seed_labels(i_seed_unit)  = trim(label)
        config%seed_units(i_seed_unit)%e = e(1:size(config%base_units))
        
        write(unit=i_seed_unit_string, fmt="(i0)") i_seed_unit
        
        call config%logger%check(len(trim(label)) /= 0, &
                                        "seed_unit #" // trim(i_seed_unit_string) // " has an empty label.", n_failures)

        do j_seed_unit = 1, i_seed_unit - 1
            write(unit=j_seed_unit_string, fmt="(i0)") j_seed_unit
            call config%logger%check(trim(label) /= config%seed_labels(j_seed_unit), &
                                            "seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same label as seed_unit #' // trim(j_seed_unit_string) // ".", &
                                                n_failures)
            call config%logger%check(.not. all_close(config%seed_units(i_seed_unit)%e, config%seed_units(j_seed_unit)%e), &
                                            "seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same exponents as seed_unit #' // trim(j_seed_unit_string) &
                                                // ' labeled "' // trim(config%seed_labels(j_seed_unit)) // '".', n_failures)
        end do
        
        do i_base_unit = 1, size(config%base_units)
            write(unit=i_base_unit_string, fmt="(i0)") i_base_unit
            call config%logger%check(.not. is_close(config%seed_units(i_seed_unit)%e(i_base_unit), huge(1.0_WP)), &
                                            "In seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                            // trim(config%seed_labels(i_seed_unit)) &
                                            // '", exponent #' // trim(i_base_unit_string) // " has not been set.", n_failures)
        end do
    end do
    close(unit=nml_unit)
    
    if (n_failures > 0) then
        call config%logger%error("input validation error(s)")
        rc = n_failures
        return
    end if
end subroutine read_seed_unit_namelists

pure function in_exponent_bounds(config, unit)
    use checks, only: assert
    use genunits_data, only: unit_type
    
    type(config_type), intent(in) :: config
    type(unit_type), intent(in)   :: unit
    
    logical :: in_exponent_bounds
    
    integer :: i_base_unit
    
    call assert(size(config%min_exponents) == size(config%max_exponents))
    
    in_exponent_bounds = .true.
    do i_base_unit = 1, size(config%min_exponents)
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
    
    real(kind=WP), intent(in) :: e
    integer, intent(in)       :: d
    
    logical :: denominator_matches
    
    real(kind=WP) :: ed
    
    ed = e * real(d, kind=WP)
    
    denominator_matches = is_close(real(nint(ed), kind=WP), ed)
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
    do i_base_unit = 1, size(config%base_units)
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
    use genunits_data, only: unit_type, unit_system_type, &
                                m_unit, d_unit, sqrt_unit, cbrt_unit, square_unit
    
    class(config_type), intent(in)       :: config
    type(unit_system_type), intent(out)  :: unit_system
    
    type(unit_type) :: units(config%max_n_units), trial_unit
    integer         :: n_units, n_units_prev, i_units, j_units, iter, rc
    
    call assert(size(config%seed_units) <= config%max_n_units)
    
    ! Add all `seed_units` to `units`
    units(1:size(config%seed_units)) = config%seed_units
    
    ! Create new units that appear from application of operators.
    n_units = size(config%seed_units)
    iter = 0
    genunit_loop: do
        write(unit=*, fmt="(a, i0, a, i0)") "iter=", iter, " n_units=", n_units
        
        iter = iter + 1
        n_units_prev = n_units
        
        do i_units = 1, n_units_prev
            ! binary operators
            do j_units = 1, n_units_prev
                ! multiplication
                trial_unit = m_unit(units(i_units), units(j_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
                
                ! division
                trial_unit = d_unit(units(i_units), units(j_units))
                call process_trial_unit(config, trial_unit, units, n_units, rc)
                if (rc /= 0) exit genunit_loop
            end do
            
            ! unary operators
            
            ! square root
            trial_unit = sqrt_unit(units(i_units))
            call process_trial_unit(config, trial_unit, units, n_units, rc)
            if (rc /= 0) exit genunit_loop
            
            ! cube root
            trial_unit = cbrt_unit(units(i_units))
            call process_trial_unit(config, trial_unit, units, n_units, rc)
            if (rc /= 0) exit genunit_loop
            
            ! square
            trial_unit = square_unit(units(i_units))
            call process_trial_unit(config, trial_unit, units, n_units, rc)
            if (rc /= 0) exit genunit_loop
        end do
        
        if ((iter > config%max_iter) .or. (n_units == n_units_prev)) then
            exit genunit_loop
        end if
    end do genunit_loop
    
    write(unit=*, fmt="(a, i0, a, i0)") "FINAL iter=", iter, " n_units=", n_units
    
    unit_system%units        = units(1:n_units)
    unit_system%base_units   = config%base_units
    unit_system%n_base_units = size(config%base_units)
end subroutine generate_system

! output

subroutine write_type(config, file_unit, i_unit, unit_system)
    use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit
    
    class(config_type), intent(in)     :: config
    integer, intent(in)                :: file_unit, i_unit
    type(unit_system_type), intent(in) :: unit_system
    
    integer         :: j_unit
    type(unit_type) :: trial_unit
    
    write(unit=file_unit, fmt="(2a)") "type, public :: ", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(2a)") "    ! ", trim(unit_system%units(i_unit)%readable(unit_system))
    write(unit=file_unit, fmt="(3a)") "    ", config%type_definition, " :: v"
    write(unit=file_unit, fmt="(a)") "contains"
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: a_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(+) => a_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: s_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(-) => s_", &
        trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(i_unit)%label())
    
    if (config%unary) then
        write(unit=file_unit, fmt="(2a)") "    procedure, private :: a_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(+) => a_", trim(unit_system%units(i_unit)%label())
        
        write(unit=file_unit, fmt="(2a)") "    procedure, private :: s_", trim(unit_system%units(i_unit)%label())
        write(unit=file_unit, fmt="(2a)") "    generic, public :: operator(-) => s_", trim(unit_system%units(i_unit)%label())
    end if
    
    do j_unit = 1, size(unit_system%units)
        ! multiply
        trial_unit = m_unit(unit_system%units(i_unit), unit_system%units(j_unit))
        if (trial_unit%is_in(unit_system%units)) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(*) => m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded: m_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        end if
        
        ! divide
        trial_unit = d_unit(unit_system%units(i_unit), unit_system%units(j_unit))
        if (trial_unit%is_in(unit_system%units)) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(/) => d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded: d_", &
                trim(unit_system%units(i_unit)%label()), "_", trim(unit_system%units(j_unit)%label())
        end if
    end do
    
    write(unit=file_unit, fmt="(3a)") "end type ", trim(unit_system%units(i_unit)%label()), new_line("a")
end subroutine write_type

subroutine write_as_operators(unit_system, file_unit, unit)
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    
    ! add
    call write_binary_operator(unit_system, file_unit, unit, unit, unit, "+")
    
    ! subtract
    call write_binary_operator(unit_system, file_unit, unit, unit, unit, "-")
end subroutine write_as_operators

subroutine write_md_operators(unit_system, file_unit, unit_left, unit_right, n_interfaces)
    use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit_left, unit_right
    integer, intent(in out)            :: n_interfaces
    
    type(unit_type) :: unit_m, unit_d
    
    ! multiply
    unit_m = m_unit(unit_left, unit_right)
    if (unit_m%is_in(unit_system%units)) then
        call write_binary_operator(unit_system, file_unit, unit_left, unit_right, unit_m, "*")
        n_interfaces = n_interfaces + 1
    end if
    
    ! divide
    unit_d = d_unit(unit_left, unit_right)
    if (unit_d%is_in(unit_system%units)) then
        call write_binary_operator(unit_system, file_unit, unit_left, unit_right, unit_d, "/")
        n_interfaces = n_interfaces + 1
    end if
end subroutine write_md_operators

subroutine write_binary_operator(unit_system, file_unit, unit_left, unit_right, unit_result, op)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit_left, unit_right, unit_result
    character(len=*), intent(in)       :: op
    
    character(len=1)              :: op_label
    character(len=:), allocatable :: binary_operator_procedure
    
    select case (op)
        case ("+")
            op_label = "a"
        case ("-")
            op_label = "s"
        case ("*")
            op_label = "m"
        case ("/")
            op_label = "d"
!        case ("**")
!            op_label = "e"
        case default
            error stop "write_binary_operator: invalid op"
    end select
    
    binary_operator_procedure = op_label // "_" // trim(unit_left%label()) // "_" &
                                    // trim(unit_right%label())
    
    call assert(len(binary_operator_procedure) <= MAX_LABEL_LEN)
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", binary_operator_procedure, "(unit_left, unit_right)"
    
    write(unit=file_unit, fmt="(2a)") "    ! left: ", trim(unit_left%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! right: ", trim(unit_right%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! result: ", trim(unit_result%readable(unit_system))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(unit_left%label()), "), intent(in) :: unit_left"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_right%label()), "), intent(in) :: unit_right"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_result%label()), ") :: ", binary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%v = unit_left%v ", op, " unit_right%v"
    
    write(unit=file_unit, fmt="(3a)") "end function ", binary_operator_procedure, new_line("a")
end subroutine write_binary_operator

subroutine write_unary_operator(unit_system, file_unit, unit, op)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type
    
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    type(unit_type), intent(in)        :: unit
    character(len=*), intent(in)       :: op
    
    character(len=1)              :: op_label
    character(len=:), allocatable :: unary_operator_procedure
    
    select case (op)
        case ("+")
            op_label = "a"
        case ("-")
            op_label = "s"
        case default
            error stop "write_unary_operator: invalid op"
    end select
    
    unary_operator_procedure = op_label // "_" // trim(unit%label())
    
    call assert(len(unary_operator_procedure) <= MAX_LABEL_LEN)
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", unary_operator_procedure, "(unit)"
    
    write(unit=file_unit, fmt="(2a)") "    ! argument: ", trim(unit%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! result: ", trim(unit%readable(unit_system))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(unit%label()), "), intent(in) :: unit"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit%label()), ") :: ", unary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", unary_operator_procedure, "%v = ", op, "unit%v"
    
    write(unit=file_unit, fmt="(3a)") "end function ", unary_operator_procedure, new_line("a")
end subroutine write_unary_operator

subroutine write_exponentiation_interfaces(config, unit_system, file_unit)
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    type(config_type), intent(in)      :: config
    type(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                :: file_unit
    
    integer         :: i_unit
    type(unit_type) :: trial_unit
    
    if (config%sqrt) then
        write(unit=file_unit, fmt="(a)") "interface sqrt"
        do i_unit = 1, size(unit_system%units)
            trial_unit = sqrt_unit(unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    module procedure sqrt_", trim(unit_system%units(i_unit)%label())
            else
                write(unit=file_unit, fmt="(2a)") "    ! excluded: sqrt_", trim(unit_system%units(i_unit)%label())
            end if
        end do
        write(unit=file_unit, fmt="(2a)") "end interface sqrt", new_line("a")
    end if
    
    if (config%cbrt) then
        write(unit=file_unit, fmt="(a)") "interface cbrt"
        do i_unit = 1, size(unit_system%units)
            trial_unit = cbrt_unit(unit_system%units(i_unit))
            if (trial_unit%is_in(unit_system%units)) then
                write(unit=file_unit, fmt="(2a)") "    module procedure cbrt_", trim(unit_system%units(i_unit)%label())
            else
                write(unit=file_unit, fmt="(2a)") "    ! excluded: cbrt_", trim(unit_system%units(i_unit)%label())
            end if
        end do
        write(unit=file_unit, fmt="(2a)") "end interface cbrt", new_line("a")
    end if
    
    if (config%square) then
        write(unit=file_unit, fmt="(a)") "interface square"
        do i_unit = 1, size(unit_system%units)
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

subroutine write_exponentiation_function(unit_system, file_unit, unit, op)
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    class(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                 :: file_unit
    type(unit_type), intent(in)         :: unit
    character(len=*), intent(in)        :: op
    
    type(unit_type)               :: unit_result
    character(len=:), allocatable :: op_pre, op_post, exponentiation_function
    
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
            op_post = ")**(1.0_WP/3.0_WP)"
            
            unit_result = cbrt_unit(unit)
        case ("square")
            op_pre = "("
            op_post = ")**2"
            
            unit_result = square_unit(unit)
        case default
            error stop "write_exponentiation_function: invalid op"
    end select
    
    exponentiation_function = op // "_" // trim(unit%label())
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", exponentiation_function, "(unit)"
    
    write(unit=file_unit, fmt="(2a)") "    ! argument: ", trim(unit%readable(unit_system))
    write(unit=file_unit, fmt="(2a)") "    ! result: ", trim(unit_result%readable(unit_system))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(unit%label()), "), intent(in) :: unit"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(unit_result%label()), ") :: ", exponentiation_function
    
    write(unit=file_unit, fmt="(6a)") "    ", exponentiation_function, "%v = ", op_pre, "unit%v", op_post
    
    write(unit=file_unit, fmt="(3a)") "end function ", exponentiation_function, new_line("a")
end subroutine write_exponentiation_function

subroutine write_module(config, unit_system, file_unit, rc)
    use checks, only: assert
    use genunits_data, only: unit_type, unit_system_type, sqrt_unit, cbrt_unit, square_unit
    
    class(config_type), intent(in)      :: config
    class(unit_system_type), intent(in) :: unit_system
    integer, intent(in)                 :: file_unit
    integer, intent(out)                :: rc
    
    integer           :: i_seed_unit, i_unit, j_unit, max_label_len, n_interfaces
    character(len=10) :: n_char
    character(len=20) :: use_format
    type(unit_type)   :: trial_unit
    
    write(unit=n_char, fmt="(i0)") size(unit_system%units)
    call config%logger%info("Generated " // trim(n_char) // " physical dimensions. Writing " // config%output_file // "...")
    
    ! Now actually write the module.
    
    write(unit=file_unit, fmt="(2a)") "module units", new_line("a")
    write(unit=file_unit, fmt="(2a)") config%use_line, new_line("a")
    write(unit=file_unit, fmt="(2a)") "implicit none"
    write(unit=file_unit, fmt="(2a)") "private", new_line("a")
    
    if (config%sqrt) then
        write(unit=file_unit, fmt="(2a)") "public :: sqrt"
    end if
    
    if (config%cbrt) then
        write(unit=file_unit, fmt="(2a)") "public :: cbrt"
    end if
    
    if (config%square) then
        write(unit=file_unit, fmt="(2a)") "public :: square"
    end if
    
    write(unit=file_unit, fmt="(a)") ""
    
    ! Write `use` lines as comments.
    if (size(config%seed_labels) > 0) then
        write(unit=file_unit, fmt="(a)", advance="no") "!use units, only: "
        
        max_label_len = 0
        do i_seed_unit = 1, size(config%seed_labels)
            max_label_len = max(max_label_len, len(trim(config%seed_labels(i_seed_unit))))
        end do
        
        do i_seed_unit = 1, size(config%seed_labels)
            if (i_seed_unit /= 1) then
                write(unit=file_unit, fmt="(a)", advance="no") "!                 "
            end if
            
            ! Align the `=>` and left-justify the labels.
            ! <https://fortran-lang.discourse.group/t/left-justification-of-strings/345>
            write(unit=use_format, fmt="(a, i0, a)") "(a, tr", max_label_len &
                                                                    - len(trim(config%seed_labels(i_seed_unit))) + 1, ", 2a)"
            write(unit=file_unit, fmt=trim(use_format), advance="no") trim(config%seed_labels(i_seed_unit)), &
                                                                        "=> ", &
                                                                        trim(config%seed_units(i_seed_unit)%label())
            if (config%sqrt .or. config%cbrt .or. config%square) then
                write(unit=file_unit, fmt="(a)") ", &"
            else
                if (i_seed_unit /= size(config%seed_labels)) then
                    write(unit=file_unit, fmt="(a)") ", &"
                else
                    write(unit=file_unit, fmt="(a)") ""
                end if
            end if
        end do
        
        if (config%sqrt .or. config%cbrt .or. config%square) then
            write(unit=file_unit, fmt="(a)", advance="no") "!                 "
        end if
        
        if (config%sqrt) then
            write(unit=file_unit, fmt="(a)", advance="no") "sqrt"
            
            if (config%cbrt .or. config%square) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end if
        
        if (config%cbrt) then
            write(unit=file_unit, fmt="(a)", advance="no") "cbrt"
            
            if (config%square) then
                write(unit=file_unit, fmt="(a)", advance="no") ", "
            end if
        end if
        
        if (config%square) then
            write(unit=file_unit, fmt="(a)", advance="no") "square"
        end if
        
        if (config%sqrt .or. config%cbrt .or. config%square) then
            write(unit=file_unit, fmt="(a)") ""
        end if
        
        write(unit=file_unit, fmt="(a)") ""
    end if
    
    do i_unit = 1, size(unit_system%units)
        call config%write_type(file_unit, i_unit, unit_system)
    end do
    
    call write_exponentiation_interfaces(config, unit_system, file_unit)
    
    write(unit=file_unit, fmt="(2a)") "contains", new_line("a")
    
    n_interfaces = 0
    
    do i_unit = 1, size(unit_system%units)
        call write_as_operators(unit_system, file_unit, unit_system%units(i_unit))
        n_interfaces = n_interfaces + 2
        
        if (config%unary) then
            call write_unary_operator(unit_system, file_unit, unit_system%units(i_unit), "+")
            call write_unary_operator(unit_system, file_unit, unit_system%units(i_unit), "-")
            n_interfaces = n_interfaces + 2
        end if
    end do
    
    do i_unit = 1, size(unit_system%units)
        do j_unit = 1, size(unit_system%units)
            call write_md_operators(unit_system, file_unit, unit_system%units(i_unit), unit_system%units(j_unit), n_interfaces)
        end do
    end do
    
    if (config%sqrt .or. config%cbrt .or. config%square) then
        do i_unit = 1, size(unit_system%units)
            if (config%sqrt) then
                trial_unit = sqrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(unit_system, file_unit, unit_system%units(i_unit), "sqrt")
                    n_interfaces = n_interfaces + 1
                end if
            end if
            
            if (config%cbrt) then
                trial_unit = cbrt_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(unit_system, file_unit, unit_system%units(i_unit), "cbrt")
                    n_interfaces = n_interfaces + 1
                end if
            end if
            
            if (config%square) then
                trial_unit = square_unit(unit_system%units(i_unit))
                if (trial_unit%is_in(unit_system%units)) then
                    call write_exponentiation_function(unit_system, file_unit, unit_system%units(i_unit), "square")
                    n_interfaces = n_interfaces + 1
                end if
            end if
        end do
    end if
    
    write(unit=file_unit, fmt="(a)") "end module units"
    
    write(unit=n_char, fmt="(i0)") n_interfaces
    call config%logger%info("Generated " // trim(n_char) // " interfaces.")
    
    rc = 0
end subroutine write_module

end module genunits_io
