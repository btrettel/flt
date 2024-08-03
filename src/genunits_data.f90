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

public :: m_unit, d_unit, sqrt_unit, cbrt_unit, square_unit, real_to_rational, rational_string

character(len=*), parameter :: UNIT_PREFIX    = "unit"
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
        call assert(len(trim(adjustl(label))) < CL, "genunits_data (label): overflow, too much to write in the string")
    end do
    
    ! Ensure that the `unit_label` won't be too long to be valid in Fortran 2003.
    call assert(len(trim(label)) > 0, "genunits_data (label): label has zero length")
    call assert(len(trim(label)) <= MAX_LABEL_LEN, "genunits_data (label): label is too long")
end function label

pure function readable(unit, unit_system)
    use checks, only: assert, is_close, all_close
    use prec, only: CL
    
    ! I'm not using `/` to eliminate negative unit exponents.
    ! Reason: Units like `kg.m3/2.s-2` seem more clear than `kg.m3/2/s2`.
    ! Unfortunately, UCUM assumes all exponents are integers:
    ! <https://ucum.org/ucum>: > The exponent is an integer number
    
    class(unit_type), intent(in)        :: unit
    class(unit_system_type), intent(in) :: unit_system
    
    character(len=CL) :: readable
    
    character(len=CL) :: exponent_string, positive_string, negative_string
    character(len=1)  :: operator_string
    integer           :: i_base_unit, numerator, denominator, rc
    logical           :: positive_printed_yet
    
    call assert(size(unit%e) == unit_system%n_base_units, "genunits_data (readable): inconsistent number of base units")
    
    if (all_close(unit%e, 0.0_WP)) then
        readable = "1"
        return
    end if
    
    positive_string = ""
    negative_string = ""
    positive_printed_yet = .false.
    do i_base_unit = 1, unit_system%n_base_units ! SERIAL
        if (.not. is_close(unit%e(i_base_unit), 0.0_WP)) then
            call real_to_rational(unit%e(i_base_unit), numerator, denominator, rc)
            
            call assert(rc == 0, "genunits_data (readable): real_to_rational failed")
            
            exponent_string = rational_string(numerator, denominator)
            
            if (exponent_string(1:1) == "-") then
                exponent_string = exponent_string(2:)
                operator_string = "/"
            else
                operator_string = "."
            end if
            
            if (trim(exponent_string) == "1") then
                exponent_string = ""
            end if
            
            if (unit%e(i_base_unit) > 0.0_WP) then
                if (positive_printed_yet) then
                    write(unit=positive_string, fmt="(2a)") trim(positive_string), trim(operator_string)
                end if
                
                write(unit=positive_string, fmt="(3a)") trim(positive_string), trim(unit_system%base_units(i_base_unit)), &
                                                            trim(exponent_string)
                positive_printed_yet = .true.
            else
                write(unit=negative_string, fmt="(4a)") trim(negative_string), trim(operator_string), &
                                                            trim(unit_system%base_units(i_base_unit)), &
                                                            trim(exponent_string)
            end if
            
            call assert(len(trim(adjustl(positive_string))) < CL, &
                            "genunits_data (readable): overflow, too much to write in positive_string")
            
            call assert(len(trim(adjustl(negative_string))) < CL, &
                            "genunits_data (readable): overflow, too much to write in negative_string")
        end if
    end do
    if (len(trim(adjustl(positive_string))) == 0) then
        positive_string = "1"
    end if
    
    readable = trim(adjustl(positive_string)) // trim(adjustl(negative_string))
    
    call assert(len(trim(readable)) > 0, "genunits_data (readable): readable has zero length")
end function readable

pure subroutine real_to_rational(x, numerator, denominator, rc)
    use checks, only: is_close, assert
    
    real(kind=WP), intent(in) :: x
    integer, intent(out)      :: numerator, denominator, rc
    
    integer, parameter :: MAX_ITERATIONS = 64
    
    denominator = 0
    rc          = 0
    do ! SERIAL
        denominator = denominator + 1
        
        numerator = nint(x * real(denominator, WP))
        
        if (is_close(x, real(numerator, WP) / real(denominator, WP))) then
            exit
        end if
        
        if (denominator >= MAX_ITERATIONS) then
            rc = 1
            exit
        end if
    end do
    
    call assert(denominator > 0, "genunits_data (real_to_rational): denominator is zero or negative")
end subroutine real_to_rational

pure function rational_string(numerator, denominator)
    use checks, only: assert
    use prec, only: CL
    
    integer, intent(in) :: numerator, denominator
    
    character(len=CL) :: rational_string
    
    if (denominator == 1) then
        write(unit=rational_string, fmt="(i0)") numerator
    else
        write(unit=rational_string, fmt="(a, i0, a, i0, a)") "(", numerator, "/", denominator, ")"
    end if
    
    call assert(denominator > 0, "genunits_data (rational_string): denominator is zero or negative")
end function rational_string

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
