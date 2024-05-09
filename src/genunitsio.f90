! module containing I/0 procedures to generate a units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module genunitsio

use prec, only: WP
use nmllog, only: log_type
use unitdata, only: MAX_LABEL_LEN, BASE_UNIT_LEN, unit_type
implicit none
private

public :: read_config_namelist, read_seed_unit_namelists

character(len=*), parameter :: GEN_UNITS_LOG = "gen_units.nml"

type, public :: config_type
    character(len=:), allocatable :: output_file, type_definition, use_line
    real(kind=WP), allocatable    :: min_exponents(:), &
                                     max_exponents(:)
    !integer :: max_n_units, max_n_interfaces
    !logical :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    type(log_type) :: logger
    
    character(len=BASE_UNIT_LEN), allocatable :: base_units(:)
    
    type(unit_type), allocatable              :: seed_units(:)
    character(len=MAX_LABEL_LEN), allocatable :: seed_labels(:)
    type(unit_type), allocatable              :: reject_units(:)
end type config_type

contains

subroutine read_config_namelist(filename, config_out, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use unitdata, only: MAX_BASE_UNITS, BASE_UNIT_LEN
    
    use prec, only: CL
    use checks, only: is_close
    use nmllog, only: INFO_LEVEL
    
    character(len=*), intent(in)   :: filename
    type(config_type), intent(out) :: config_out
    integer, intent(out)           :: rc
    
    integer           :: i_base_unit, nml_unit, rc_nml, n_failures, n_base_units
    character(len=CL) :: nml_error_message
    
    ! `config` namelist group
    character(len=CL)            :: output_file, type_definition, use_line
    character(len=BASE_UNIT_LEN) :: base_units(MAX_BASE_UNITS)
    real(kind=WP)                :: min_exponents(MAX_BASE_UNITS), max_exponents(MAX_BASE_UNITS)
    !logical                      :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    namelist /config/ output_file, base_units, type_definition, use_line, min_exponents, max_exponents
    
    call config_out%logger%open(GEN_UNITS_LOG, level=INFO_LEVEL)
    
    do i_base_unit = 1, MAX_BASE_UNITS
        base_units(i_base_unit) = ""
    end do
    
    output_file     = ""
    type_definition = "real(kind=WP)"
    use_line        = "use prec, only: WP"
    min_exponents   = -huge(1.0_WP)
    max_exponents   = huge(1.0_WP)
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
    
    do i_base_unit = 1, n_base_units
        call config_out%logger%check(.not. is_close(min_exponents(i_base_unit), -huge(1.0_WP)), &
                                        "min_exponents is not set properly?", n_failures)
        call config_out%logger%check(.not. is_close(max_exponents(i_base_unit), huge(1.0_WP)), &
                                        "max_exponents is not set properly?", n_failures)
    end do
    
    if (n_failures > 0) then
        call config_out%logger%error("config namelist input validation error(s)")
        rc = n_failures
    else
        rc = 0
    end if
end subroutine read_config_namelist

subroutine read_seed_unit_namelists(filename, config, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use checks, only: assert, is_close
    use prec, only: CL
    use unitdata, only: MAX_BASE_UNITS, MAX_LABEL_LEN
    
    character(len=*), intent(in)      :: filename
    type(config_type), intent(in out) :: config
    integer, intent(out)              :: rc
    
    integer           :: nml_unit, n_seed_units, i_seed_unit, j_seed_unit, rc_nml, n_failures, i_base_unit
    character(len=CL) :: nml_error_message
    character(len=2)  :: i_seed_unit_string, j_seed_unit_string, i_base_unit_string
    
    ! `seed_unit` namelist group
    character(len=MAX_LABEL_LEN) :: label
    real(kind=WP)                :: e(MAX_BASE_UNITS)
    
    namelist /seed_unit/ label, e
    
    call assert(allocated(config%base_units))
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    
    ! First get `n_seed_units`, allocate `config`, and read everything in.
    n_seed_units = 0
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
    
    rewind nml_unit
    allocate(config%seed_labels(n_seed_units))
    allocate(config%seed_units(n_seed_units))
    i_seed_unit = 0
    n_failures = 0
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
                                        "pdim #" // trim(i_seed_unit_string) // " has an empty label.", n_failures)

        do j_seed_unit = 1, i_seed_unit - 1
            write(unit=j_seed_unit_string, fmt="(i0)") j_seed_unit
            call config%logger%check(trim(label) /= config%seed_labels(j_seed_unit), &
                                            "pdim #" // trim(i_seed_unit_string) // ' labeled "' &
                                                // trim(config%seed_labels(i_seed_unit)) &
                                                // '" has the same label as pdim #' // trim(j_seed_unit_string) // ".", n_failures)
            ! TODO: Reenable the following.
!            call config%logger%check(.not. pdim_in_set(config, config%seed_units(i_seed_unit), &
!                                                config%seed_units(j_seed_unit:j_seed_unit)), &
!                                            "pdim #" // trim(i_seed_unit_string) // ' labeled "' &
!                                                // trim(config%seed_labels(i_seed_unit)) &
!                                                // '" has the same exponents as pdim #' // trim(j_seed_unit_string) &
!                                                // ' labeled "' // trim(config%seed_labels(j_seed_unit)) // '".', n_failures)
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

end module genunitsio
