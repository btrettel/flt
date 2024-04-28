! module for the generator of a pdim_types
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module pdim_mod

use prec, only: SP
implicit none

public :: pdim_label
public :: write_type, write_as_operators, write_md_operators, write_binary_operator
public :: linspace
public :: pdim_within_bounds

type, public :: pdim_type
    real(kind=SP), allocatable :: e(:)
end type pdim_type

type, public :: pdim_config_type
    character(len=:), allocatable :: pdim_chars, &
                                     pdim_type_defn

    integer :: n_pdims, &
               pdim_label_len, &
               pdim_human_len

    real(kind=SP), allocatable :: min_exponents(:), &
                                  max_exponents(:), &
                                  exponent_deltas(:)
end type pdim_config_type

contains

pure function pdim_label(config, pdim)
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    character(len=:), allocatable :: pdim_label
    
    integer :: i_pdim
    
    allocate(character(len=config%pdim_label_len) :: pdim_label)
    
    pdim_label = "t"
    do i_pdim = 1, config%n_pdims
        write(unit=pdim_label, fmt="(a, a, z0.8)") trim(pdim_label), "_", pdim%e(i_pdim)
    end do
end function pdim_label

pure function pdim_human_readable(config, pdim)
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    character(len=:), allocatable :: pdim_human_readable
    
    integer :: i_pdim
    
    allocate(character(len=config%pdim_human_len) :: pdim_human_readable)
    
    pdim_human_readable = "physical dimension: "
    do i_pdim = 1, config%n_pdims
        write(unit=pdim_human_readable, fmt="(a, a, a, a, f0.0)") trim(pdim_human_readable), &
                                                                    " [", config%pdim_chars(i_pdim:i_pdim), "]^", pdim%e(i_pdim)
    end do
end function pdim_human_readable

subroutine write_type(config, file_unit, i_pdim, pdims)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit, i_pdim
    type(pdim_type), intent(in)        :: pdims(:)
    
    integer         :: j_pdim
    type(pdim_type) :: pdim_m, pdim_d
    
    write(unit=file_unit, fmt="(2a)") "type, public :: ", pdim_label(config, pdims(i_pdim))
    write(unit=file_unit, fmt="(2a)") "    ! ", trim(pdim_human_readable(config, pdims(i_pdim)))
    write(unit=file_unit, fmt="(3a)") "    ", config%pdim_type_defn, " :: v"
    write(unit=file_unit, fmt="(a)") "contains"
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: a_", &
        pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(i_pdim))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(+) => a_", &
        pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(i_pdim))
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: s_", &
        pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(i_pdim))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(-) => s_", &
        pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(i_pdim))
    
    do j_pdim = 1, size(pdims)
        if (i_pdim /= j_pdim) then
            ! multiply
            pdim_m%e = pdims(i_pdim)%e + pdims(j_pdim)%e
            if (pdim_within_bounds(config, pdim_m)) then
                write(unit=file_unit, fmt="(5a)") "    procedure, private :: m_", &
                    pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(j_pdim))
                write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(*) => m_", &
                    pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(j_pdim))
            end if
            
            ! divide
            pdim_d%e = pdims(i_pdim)%e - pdims(j_pdim)%e
            if (pdim_within_bounds(config, pdim_d)) then
                write(unit=file_unit, fmt="(5a)") "    procedure, private :: d_", &
                    pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(j_pdim))
                write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(/) => d_", &
                    pdim_label(config, pdims(i_pdim)), "_", pdim_label(config, pdims(j_pdim))
            end if
        end if
    end do
    
    write(unit=file_unit, fmt="(3a)") "end type ", pdim_label(config, pdims(i_pdim)), new_line("a")
end subroutine write_type

subroutine write_as_operators(config, file_unit, pdim)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdim
    
    ! add
    call write_binary_operator(config, file_unit, pdim, pdim, pdim, "+")
    
    ! subtract
    call write_binary_operator(config, file_unit, pdim, pdim, pdim, "-")
end subroutine write_as_operators

subroutine write_md_operators(config, file_unit, pdim_left, pdim_right)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdim_left, pdim_right
    
    type(pdim_type) :: pdim_m, pdim_d
    
    ! multiply
    pdim_m%e = pdim_left%e + pdim_right%e
    if (pdim_within_bounds(config, pdim_m)) then
        call write_binary_operator(config, file_unit, pdim_left, pdim_right, pdim_d, "*")
    end if
    
    ! divide
    pdim_d%e = pdim_left%e - pdim_right%e
    if (pdim_within_bounds(config, pdim_d)) then
        call write_binary_operator(config, file_unit, pdim_left, pdim_right, pdim_d, "/")
    end if
end subroutine write_md_operators

subroutine write_binary_operator(config, file_unit, pdim_left, pdim_right, pdim_out, op)
    use checks, only: assert
    
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdim_left, pdim_right, pdim_out
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
    
    binary_operator_procedure = op_label // "_" // pdim_label(config, pdim_left) // "_" // pdim_label(config, pdim_right)
    
    call assert(len(binary_operator_procedure) <= 63)
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", binary_operator_procedure, "(pdim_left, pdim_right)"
    
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(config, pdim_left), "), intent(in) :: pdim_left"
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(config, pdim_right), "), intent(in) :: pdim_right"
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(config, pdim_out), ") :: ", binary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%e = pdim_left%e ", op, " pdim_right%e"
    
    write(unit=file_unit, fmt="(3a)") "end function ", binary_operator_procedure, new_line("a")
end subroutine write_binary_operator

pure function linspace(lower, upper, n)
    use checks, only: assert
    
    real(kind=SP), intent(in)     :: lower, upper
    integer, intent(in), optional :: n
    
    real(kind=SP), allocatable :: linspace(:)
    
    integer :: i, n_
    real(kind=SP) :: delta
    
    if (present(n)) then
        n_ = n
    else
        n_ = 50
    end if
    
    call assert(lower < upper)
    
    allocate(linspace(n_))
    delta = (upper - lower) / real(n_ - 1, SP)
    do i = 1, n_
        linspace(i) = lower + delta * real(i - 1, SP)
    end do
end function linspace

subroutine write_module(config, file_unit)
    use checks, only: assert
    
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    
    type(pdim_type), allocatable :: pdims(:)
    integer :: n_exponents(config%n_pdims), i_pdim, j_pdim, k_pdim, l_pdim
    real(kind=SP), allocatable :: exponents_1(:), exponents_2(:), exponents_3(:)
    
    ! TODO: Generalize this so that it works for an arbitrary number of physical dimensions. This only works with 3.
    call assert(config%n_pdims == 3)
    
    do i_pdim = 1, config%n_pdims
        n_exponents(i_pdim) = nint((config%max_exponents(i_pdim) - config%min_exponents(i_pdim)) &
                                    / config%exponent_deltas(i_pdim)) + 1
    end do
    
    exponents_1 = linspace(config%min_exponents(1), config%max_exponents(1), n_exponents(1))
    exponents_2 = linspace(config%min_exponents(2), config%max_exponents(2), n_exponents(2))
    exponents_3 = linspace(config%min_exponents(3), config%max_exponents(3), n_exponents(3))
    allocate(pdims(n_exponents(1) * n_exponents(2) * n_exponents(3)))
    l_pdim = 0
    do i_pdim = 1, n_exponents(1)
        do j_pdim = 1, n_exponents(2)
            do k_pdim = 1, n_exponents(3)
                l_pdim = l_pdim + 1
                allocate(pdims(l_pdim)%e(config%n_pdims))
                pdims(l_pdim)%e(1) = exponents_1(i_pdim)
                pdims(l_pdim)%e(2) = exponents_2(j_pdim)
                pdims(l_pdim)%e(3) = exponents_3(k_pdim)
            end do
        end do
    end do
    
    write(unit=file_unit, fmt=*) pdims(1)%e, size(pdims)
end subroutine write_module

pure function pdim_within_bounds(config, pdim)
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    logical :: pdim_within_bounds
    
    pdim_within_bounds = all(pdim%e < config%max_exponents) .and. all(pdim%e > config%min_exponents)
end function pdim_within_bounds

end module pdim_mod
