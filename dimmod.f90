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

integer, public, parameter :: N_DIMS = 3_IP

integer, public, parameter :: OUT_UNIT = 0_IP

integer, public, parameter :: SUCCESS = 0_IP
integer, public, parameter :: EIO     = 5_IP

type, public :: config_type
    character(len=N_DIMS)               :: dims
    integer(kind=IP), dimension(N_DIMS) :: min_exp
    integer(kind=IP), dimension(N_DIMS) :: max_exp
    integer(kind=IP), dimension(N_DIMS) :: denominator
end type config_type

public :: type_name

contains

function type_name(config, exps)
    type(config_type), intent(in)              :: config
    integer(kind=IP), dimension(*), intent(in) :: exps
    
    character(len=4_IP*N_DIMS) :: type_name
    
    character(len=4_IP) :: type_name_part
    integer(kind=IP)    :: i_dim
    
    ! TODO: Make this handle negative exponents.
    type_name = ""
    do i_dim = 1_IP, N_DIMS
        write(unit=type_name_part, fmt="(a, i3.3)") config%dims(i_dim:i_dim), abs(exps(i_dim))
        type_name = trim(type_name) // type_name_part
    end do
    
    return
end function type_name

!subroutine generate_types(config, rc)
!    type(config_type), intent(in) :: config
!    integer(kind=IP), intent(out) :: rc
    
!    integer(kind=IP) :: n_combos, i_dim_1, i_dim_2, i_dim_3
!    logical :: out_unit_open
    
!    rc = SUCCESS
    
!    inquire(unit=OUT_UNIT, opened=out_unit_open)
!    if (.not. out_unit_open) then
!        rc = EIO
!        return
!    end if
    
!    n_combos = 0_IP

!    do m_index = min_m_index, max_m_index
!        do l_index = min_l_index, max_l_index
!            do t_index = min_t_index, max_t_index
!                n_combos = n_combos + 1_IP
!            end do
!        end do
!    end do

!    allocate(dims(n_dims))

!    i_dim = 0_IP
!    do m_index = min_m_index, max_m_index
!        do l_index = min_l_index, max_l_index
!            do t_index = min_t_index, max_t_index
!                i_dim = i_dim + 1_IP
                
!                dims(i_dim)%m = m_exponent(m_index)
!                dims(i_dim)%l = l_exponent(l_index)
!                dims(i_dim)%t = t_exponent(t_index)
!            end do
!        end do
!    end do
!end subroutine generate_types

end module dimmod
