! module containing types for the generator of a units module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module genunits_data

use prec, only: WP
implicit none
private

public :: m_unit, d_unit, sqrt_unit, cbrt_unit, square_unit

character(len=*), parameter :: UNIT_PREFIX = "unit"
integer, public, parameter  :: MAX_BASE_UNITS = 10, &
                               MAX_LABEL_LEN  = 63, &
                               BASE_UNIT_LEN  = 10, &
                               EXPONENT_LEN   = 5

type, public :: unit_type
    real(kind=WP), allocatable :: e(:)
contains
    procedure :: label
    procedure :: readable
    procedure :: is_in
end type unit_type

type, public :: unit_system_type
    integer                                   :: n_base_units
    character(len=BASE_UNIT_LEN), allocatable :: base_units(:)
    type(unit_type), allocatable              :: units(:)
end type unit_system_type

contains

pure function label(unit)
    use checks, only: assert
    use prec, only: CL
    
    class(unit_type), intent(in) :: unit
    
    character(len=CL) :: label
    character(len=1)  :: exponent_sign, exponent_len_string
    
    integer :: i_base_unit
    
    label = UNIT_PREFIX
    write(unit=exponent_len_string, fmt="(i1)") EXPONENT_LEN
    do i_base_unit = 1, size(unit%e) ! SERIAL
        if (unit%e(i_base_unit) < 0.0_WP) then
            exponent_sign = "m"
        else
            exponent_sign = "p"
        end if
        write(unit=label, fmt="(a, a, a, i0." // exponent_len_string // ")") trim(label), "_", exponent_sign, &
                                                        nint(10.0_WP**(EXPONENT_LEN - 1) * abs(unit%e(i_base_unit)))
    end do
    
    ! Ensure that the `unit_label` won't be too long to be valid in Fortran 2003.
    call assert(len(trim(label)) <= MAX_LABEL_LEN, "genunits_data (label): label is too long")
end function label

pure function readable(unit, unit_system)
    use checks, only: assert
    use prec, only: CL
    
    class(unit_type), intent(in)        :: unit
    class(unit_system_type), intent(in) :: unit_system
    
    character(len=CL) :: readable
    
    integer          :: i_base_unit
    character(len=1) :: exponent_len_string
    
    call assert(size(unit%e) == unit_system%n_base_units, "genunits_data (readable): inconsistent number of base units")
    
    readable = ""
    write(unit=exponent_len_string, fmt="(i1)") EXPONENT_LEN - 1
    do i_base_unit = 1, unit_system%n_base_units ! SERIAL
        write(unit=readable, fmt="(4a, f0." // exponent_len_string // ")") trim(readable), " ", &
                                                    trim(unit_system%base_units(i_base_unit)), "^", unit%e(i_base_unit)
        call assert(len(trim(adjustl(readable))) < CL, "genunits_data (readable): overflow, too much to write in the string")
    end do
    readable = adjustl(readable)
end function readable

pure function is_in(unit, units)
    use checks, only: assert, all_close
    
    class(unit_type), intent(in) :: unit
    type(unit_type), intent(in)  :: units(:)
    
    logical :: is_in
    
    integer :: i_unit
    
    is_in = .false.
    do i_unit = 1, size(units) ! SERIAL
        if (all_close(unit%e, units(i_unit)%e)) then
            is_in = .true.
            exit
        end if
        call assert(.not. is_in, "genunits_data (is_in): did not exit at the right time")
    end do
end function is_in

! unit calculus

pure function m_unit(unit_left, unit_right)
    ! multiply
    
    type(unit_type), intent(in) :: unit_left, unit_right
    
    type(unit_type) :: m_unit
    
    m_unit%e = unit_left%e + unit_right%e
end function m_unit

pure function d_unit(unit_left, unit_right)
    ! divide
    
    type(unit_type), intent(in) :: unit_left, unit_right
    
    type(unit_type) :: d_unit
    
    d_unit%e = unit_left%e - unit_right%e
end function d_unit

pure function sqrt_unit(unit)
    type(unit_type), intent(in) :: unit
    
    type(unit_type) :: sqrt_unit
    
    sqrt_unit%e = 0.5_WP * unit%e
end function sqrt_unit

pure function cbrt_unit(unit)
    type(unit_type), intent(in) :: unit
    
    type(unit_type) :: cbrt_unit
    
    cbrt_unit%e = unit%e / 3.0_WP
end function cbrt_unit

pure function square_unit(unit)
    type(unit_type), intent(in) :: unit
    
    type(unit_type) :: square_unit
    
    square_unit%e = 2.0_WP * unit%e
end function square_unit

end module genunits_data
