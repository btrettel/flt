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

public :: read_config_namelist

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

!subroutine read_seed_unit_namelists(filename, config, rc)
!    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    
!    use prec, only: CL
!    use checks, only: is_close
!    use nmllog, only: INFO_LEVEL
    
!    character(len=*), intent(in)   :: filename
!    type(config_type), intent(out) :: config
!    integer, intent(out)           :: rc
    
!    integer           :: i_base_unit, nml_unit, rc_nml, n_failures, n_
!    character(len=CL) :: nml_error_message
    
!    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    
!    ! First get `n_pdims`, allocate `config`, and read everything in.
!    n_pdims = 0
!    do
!        label = ""
!        e = 0.0_WP
!        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
!        if (rc_nml == IOSTAT_END) then
!            exit
!        else if (rc_nml /= 0) then
!            call config_out%logger%error(trim(nml_error_message))
!            rc = rc_nml
!            close(unit=nml_unit)
!            return
!        end if
        
!        n_pdims = n_pdims + 1
!    end do
    
!    rewind nml_unit
!    allocate(config_out%labels(n_pdims))
!    allocate(config_out%pdims(n_pdims))
!    i_pdim = 0
!    do
!        label = ""
!        e = huge(1.0_WP)
!        read(unit=nml_unit, nml=seed_unit, iostat=rc_nml, iomsg=nml_error_message)
        
!        if (rc_nml == IOSTAT_END) then
!            exit
!        else if (rc_nml /= 0) then
!            call config_out%logger%error(trim(nml_error_message))
!            rc = rc_nml
!            close(unit=nml_unit)
!            return
!        end if
        
!        i_pdim = i_pdim + 1
        
!        !write(unit=*, fmt=*) i_pdim, trim(label), e(1:config_out%n_pdims)
!        config_out%labels(i_pdim) = trim(label)
!        config_out%pdims(i_pdim)%e = e(1:config_out%n_pdims)
        
!        write(unit=i_pdim_string, fmt="(i0)") i_pdim
        
!        call config_out%logger%check(len(trim(label)) /= 0, &
!                                        "pdim #" // trim(i_pdim_string) // " has an empty label.", n_failures)

!        do j_pdim = 1, i_pdim - 1
!            write(unit=j_pdim_string, fmt="(i0)") j_pdim
!            call config_out%logger%check(trim(label) /= config_out%labels(j_pdim), &
!                                            "pdim #" // trim(i_pdim_string) // ' labeled "' // trim(config_out%labels(i_pdim)) &
!                                                // '" has the same label as pdim #' // trim(j_pdim_string) // ".", n_failures)
!            call config_out%logger%check(.not. pdim_in_set(config_out, config_out%pdims(i_pdim), &
!                                                config_out%pdims(j_pdim:j_pdim)), &
!                                            "pdim #" // trim(i_pdim_string) // ' labeled "' // trim(config_out%labels(i_pdim)) &
!                                                // '" has the same exponents as pdim #' // trim(j_pdim_string) &
!                                                // ' labeled "' // trim(config_out%labels(j_pdim)) // '".', n_failures)
!        end do
        
!        do i_exponent = 1, config_out%n_pdims
!            write(unit=i_exponent_string, fmt="(i0)") i_exponent
!            call config_out%logger%check(.not. is_close(config_out%pdims(i_pdim)%e(i_exponent), huge(1.0_WP)), &
!                                            "In pdim #" // trim(i_pdim_string) // ' labeled "' // trim(config_out%labels(i_pdim)) &
!                                            // '", exponent #' // trim(i_exponent_string) // " has not been set.", n_failures)
!        end do
!    end do
!    close(unit=nml_unit)
    
!    if (n_failures > 0) then
!        call config_out%logger%error("input validation error(s)")
!        rc = n_failures
!        return
!    end if
!end subroutine read_seed_unit_namelists

end module genunitsio
