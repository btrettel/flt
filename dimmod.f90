! # $File$
! 
! Summary: module for the generator of a dimcheck module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [dimcheck](https://github.com/btrettel/dimcheck)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module dimmod

implicit none
private

integer, public, parameter :: IP = selected_int_kind(5)
integer, public, parameter :: RP = selected_real_kind(15, 307)

integer, public, parameter :: N_DIMS = 3_IP
integer, public, parameter :: DIGITS = 4_IP
integer, public, parameter :: WIDTH  = DIGITS + 2_IP

integer, public, parameter :: OUT_UNIT = 0_IP

integer, public, parameter :: SUCCESS = 0_IP
integer, public, parameter :: EIO     = 5_IP

type, public :: config_type
    character(len=N_DIMS)               :: dims
    integer(kind=IP), dimension(N_DIMS) :: min_exp
    integer(kind=IP), dimension(N_DIMS) :: max_exp
    integer(kind=IP), dimension(N_DIMS) :: d ! denominator, for consistency with the type `rational`
end type config_type

! TODO: Switch `type_name` to use rational type for the exponents. Switch `indices` to `exp` here and in tests.f90.
type, public :: rational
    integer(kind=IP) :: n ! numerator
    integer(kind=IP) :: d ! denominator
end type

public :: type_name
public :: rational_to_real
!public :: generate_types

contains

! TODO: simplify rational
! subroutine simplify(x, rc)
! `if (mod(config%d, exps(i_dim)%d) > 0_IP) then` `config%d` is not the gcd
! `if exps(i_dim)%d > mod(config%d) then` `config%d` is not the gcd

function type_name(config, exps)
    type(config_type), intent(in)            :: config
    type(rational), dimension(*), intent(in) :: exps
    
    character(len=WIDTH*N_DIMS-1_IP) :: type_name
    
    character(len=WIDTH) :: type_name_part
    integer(kind=IP)     :: i_dim, dim_index
    character(len=1_IP)  :: digits_char
    
    write(unit=digits_char, fmt="(i1)") DIGITS
    
    ! DONE: Make this handle negative exponents.
    ! DONE: Make this handle rational exponents, not just integer exponents
    type_name = ""
    do i_dim = 1_IP, N_DIMS
        dim_index = exps(i_dim)%n * config%d(i_dim) / exps(i_dim)%d
        
        write(unit=type_name_part, fmt="(a, i" // digits_char // "." // digits_char // ",a)") &
                                        config%dims(i_dim:i_dim), abs(dim_index), "_"
        
        if (exps(i_dim)%n < 0_IP) then
            type_name_part(2_IP:2_IP) = "n"
        else
            type_name_part(2_IP:2_IP) = "p"
        end if
        
        type_name = trim(type_name) // type_name_part
    end do
    
    ! The last "_" will be cut off and removed due to the length of `type_name`.
    
    return
end function type_name

function rational_to_real(x)
    type(rational), intent(in) :: x
    
    real(kind=RP) :: rational_to_real
    
    rational_to_real = real(x%n, RP) / real(x%d, RP)
    
    return
end function rational_to_real

!subroutine generate_types(config, rc)
!    type(config_type), intent(in) :: config
!    integer(kind=IP), intent(out) :: rc
!    
!    integer(kind=IP) :: n_combos, i_dim_1, i_dim_2, i_dim_3, min_i_dim_1, min_i_dim_2, min_i_dim_3, &
!                            max_i_dim_1, max_i_dim_2, max_i_dim_3
!    logical          :: out_unit_open
!    
!    !integer(kind=IP), dimension(:), allocatable :: exps_1, exps_2, exps_3
!    
!    rc = SUCCESS
!    
!    inquire(unit=OUT_UNIT, opened=out_unit_open)
!    if (.not. out_unit_open) then
!        rc = EIO
!        return
!    end if
!    
!    n_combos = 0_IP
!    
!    min_i_dim_1 = config%min_exp(1_IP)*config%denominator(1_IP)
!    max_i_dim_1 = config%max_exp(1_IP)*config%denominator(1_IP)
!    min_i_dim_2 = config%min_exp(2_IP)*config%denominator(2_IP)
!    max_i_dim_2 = config%max_exp(2_IP)*config%denominator(2_IP)
!    min_i_dim_3 = config%min_exp(3_IP)*config%denominator(3_IP)
!    max_i_dim_3 = config%max_exp(3_IP)*config%denominator(3_IP)
!    
!    do i_dim_1 = min_i_dim_1, max_i_dim_1
!        do i_dim_2 = min_i_dim_2, max_i_dim_2
!            do i_dim_3 = min_i_dim_3, max_i_dim_3
!                n_combos = n_combos + 1_IP
!                write(unit=*, fmt=*) i_dim_1, i_dim_2, i_dim_3
!            end do
!        end do
!    end do
!
!    !allocate(dims(n_combos))
!    
!    return
!end subroutine generate_types

end module dimmod
