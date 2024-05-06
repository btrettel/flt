! module containing I/0 procedures to generate a units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module gen_units_mod

use nmllog, only: log_type
use units_types, only: MAX_LABEL_LEN, unit_type
implicit none
private

public :: read_config_namelist

character(len=*), parameter :: GEN_UNITS_LOG = "gen_units.nml"

type, public :: config_type
    character(len=:), allocatable :: output_file, type_definition, use_line
    !integer :: max_n_units, max_n_interfaces
    !logical :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    type(log_type) :: logger
    
    type(unit_type), allocatable              :: use_units(:)
    character(len=MAX_LABEL_LEN), allocatable :: use_labels(:)
end type config_type

contains

subroutine read_config_namelist(filename, config_out, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use prec, only: WP
    use units_types, only: MAX_BASE_UNITS, BASE_UNIT_LEN
    
    use prec, only: CL
    !use checks, only: is_close
    use nmllog, only: INFO_LEVEL
    
    character(len=*), intent(in)   :: filename
    type(config_type), intent(out) :: config_out
    integer, intent(out)           :: rc
    
    integer           :: i_base_unit, nml_unit, rc_nml
    character(len=CL) :: nml_error_message
    
    ! `config` namelist group
    character(len=CL)            :: output_file, type_definition, use_line
    character(len=BASE_UNIT_LEN) :: base_units(MAX_BASE_UNITS)
    real(kind=WP)                :: min_exponents(MAX_BASE_UNITS), max_exponents(MAX_BASE_UNITS)
    !logical                      :: tests, comparison, sqrt, cbrt, square, instrinsics
    
    namelist /config/ output_file, base_units, type_definition, min_exponents, max_exponents
    
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
    
    rc = 0
end subroutine read_config_namelist

end module gen_units_mod
