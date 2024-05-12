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

public :: in_exponent_bounds, denominator_matches

character(len=*), parameter :: GENUNITS_LOG = "genunits.nml"

integer, parameter :: DEFAULT_MAX_N_UNITS = 28, & ! This is about the most the ifx will compile as of 2024-05-11.
                      DEFAULT_MAX_ITER    = 50, &
                      DEFAULT_DENOMINATOR = 1

type, public :: config_type
    character(len=:), allocatable :: output_file, type_definition, use_line
    real(kind=WP), allocatable    :: min_exponents(:), &
                                     max_exponents(:)
    integer, allocatable          :: denominators(:)
    integer                       :: max_n_units, max_iter !, max_n_interfaces
    !logical :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    type(log_type) :: logger
    
    character(len=BASE_UNIT_LEN), allocatable :: base_units(:)
    
    type(unit_type), allocatable              :: seed_units(:)
    character(len=MAX_LABEL_LEN), allocatable :: seed_labels(:)
    ! TODO: type(unit_type), allocatable              :: reject_units(:)
contains
    procedure :: read_config_namelist
    procedure :: read_seed_unit_namelists
    procedure :: generate_system
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
    !logical                      :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    namelist /config/ output_file, base_units, type_definition, use_line, min_exponents, max_exponents, denominators, max_n_units
    
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
!    tests           = .false.
!    comparison      = .false.
!    sqrt            = .false.
!    cbrt            = .false.
!    square          = .false.
!    instrinsics     = .false.
    
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
!    config_out%tests           = tests
!    config_out%comparison      = comparison
!    config_out%sqrt            = sqrt
!    config_out%cbrt            = cbrt
!    config_out%square          = square
!    config_out%instrinsics     = instrinsics
    
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
                                                // '" has the same label as pdim #' // trim(j_seed_unit_string) // ".", n_failures)
            call config%logger%check(.not. all_close(config%seed_units(i_seed_unit)%e, config%seed_units(j_seed_unit)%e), &
                                            "seed_unit #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same exponents as pdim #' // trim(j_seed_unit_string) &
                                                // ' labeled "' // trim(config%seed_labels(j_seed_unit)) // '".', n_failures)
        end do
        
        do i_base_unit = 1, size(config%base_units)
            write(unit=i_base_unit_string, fmt="(i0)") i_base_unit
            call config%logger%check(.not. is_close(config%seed_units(i_seed_unit)%e(i_base_unit), huge(1.0_WP)), &
                                            "In pdim #" // trim(i_seed_unit_string) // ' labeled "' &
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
    
    unit_system%units = units(1:n_units)
end subroutine generate_system

end module genunits_io
