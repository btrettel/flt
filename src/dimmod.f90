! # $File$
! 
! Summary: module for the generator of a dimcheck module
! Standard: Fortran 2003
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module dimmod

use prec, only: I5, RP
implicit none
private

integer, public, parameter :: N_DIMS   = 3_I5
integer, public, parameter :: N_DIGITS = 4_I5
integer, public, parameter :: WIDTH    = N_DIGITS + 2_I5

integer, public, parameter :: OUT_UNIT = 0_I5

integer, public, parameter :: SUCCESS = 0_I5
integer, public, parameter :: EIO     = 5_I5

type, public :: config_type
    character(len=N_DIMS) :: dims
    integer(kind=I5)      :: min_exp(N_DIMS)
    integer(kind=I5)      :: max_exp(N_DIMS)
    integer(kind=I5)      :: d(N_DIMS) ! denominator, for consistency with the type `rational`
end type config_type

! TODO: Switch `type_name` to use rational type for the exponents. Switch `indices` to `exp` here and in tests.f90.
type, public :: rational
    integer(kind=I5) :: n ! numerator
    integer(kind=I5) :: d ! denominator
end type rational

public :: type_name
public :: rational_to_real
public :: generate_types

contains

! TODO: Later add simplification of rationals and store simplified. First have tests for type_name and modify it to
! make sure the simplified rationals make type_name return the correct indices.
! subroutine simplify(x, rc)
! `if (mod(config%d, exps(i_dim)%d) > 0_I5) then` `config%d` is not the gcd
! `if exps(i_dim)%d > mod(config%d) then` `config%d` is not the gcd

! TODO: Make `min_exp` and `max_exp` rationals so that they don't need to be integers.
! TODO: test `rational_to_real`, which itself is really only going to be used for testing purposes

function type_name(config, exps)
    type(config_type), intent(in) :: config
    type(rational), intent(in)    :: exps(:)
    
    character(len=WIDTH*N_DIMS-1_I5) :: type_name
    
    character(len=WIDTH) :: type_name_part
    integer(kind=I5)     :: i_dim, dim_index
    character(len=1_I5)  :: digits_char
    
    write(unit=digits_char, fmt="(i1)") N_DIGITS
    
    type_name = ""
    do i_dim = 1_I5, N_DIMS
        dim_index = exps(i_dim)%n * config%d(i_dim) / exps(i_dim)%d
        
        write(unit=type_name_part, fmt="(a, i" // digits_char // "." // digits_char // ",a)") &
                                        config%dims(i_dim:i_dim), abs(dim_index), "_"
        
        if (exps(i_dim)%n < 0_I5) then
            type_name_part(2_I5:2_I5) = "n"
        else
            type_name_part(2_I5:2_I5) = "p"
        end if
        
        type_name = trim(type_name) // type_name_part
    end do
    
    ! The last "_" will be cut off and removed due to the length of `type_name`.
end function type_name

function rational_to_real(x)
    type(rational), intent(in) :: x
    
    real(kind=RP) :: rational_to_real
    
    rational_to_real = real(x%n, RP) / real(x%d, RP)
end function rational_to_real

subroutine generate_types(config, rc)
    type(config_type), intent(in) :: config
    integer(kind=I5), intent(out) :: rc
    
    integer(kind=I5) :: n_combos, i_dim_1, i_dim_2, i_dim_3, min_i_dim_1, min_i_dim_2, min_i_dim_3, &
                            max_i_dim_1, max_i_dim_2, max_i_dim_3
    logical          :: out_unit_open
    
    type(rational) :: exps(N_DIMS)
    
    rc = SUCCESS
    
    inquire(unit=OUT_UNIT, opened=out_unit_open)
    if (.not. out_unit_open) then
        rc = EIO
        return
    end if
    
    n_combos = 0_I5
    
    min_i_dim_1 = config%min_exp(1_I5) * config%d(1_I5)
    max_i_dim_1 = config%max_exp(1_I5) * config%d(1_I5)
    min_i_dim_2 = config%min_exp(2_I5) * config%d(2_I5)
    max_i_dim_2 = config%max_exp(2_I5) * config%d(2_I5)
    min_i_dim_3 = config%min_exp(3_I5) * config%d(3_I5)
    max_i_dim_3 = config%max_exp(3_I5) * config%d(3_I5)
    
    do i_dim_1 = min_i_dim_1, max_i_dim_1
        exps(1_I5) = rational(i_dim_1, config%d(1_I5))
        do i_dim_2 = min_i_dim_2, max_i_dim_2
            exps(2_I5) = rational(i_dim_2, config%d(2_I5))
            do i_dim_3 = min_i_dim_3, max_i_dim_3
                exps(3_I5) = rational(i_dim_3, config%d(3_I5))
                n_combos = n_combos + 1_I5
                !write(unit=*, fmt=*) i_dim_1, i_dim_2, i_dim_3
                write(unit=*, fmt=*) n_combos, type_name(config, exps)
            end do
        end do
    end do

    !allocate(dims(n_combos))
end subroutine generate_types

end module dimmod
