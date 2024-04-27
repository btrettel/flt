module pdim_mod

implicit none

public :: pdim_label
public :: write_type

integer, parameter     :: N_PDIMS        = 3, &
                          PDIM_LABEL_LEN = 1 + N_PDIMS * 9, &
                          PDIM_HUMAN_LEN = 126
character(len=N_PDIMS) :: PDIM_CHARS = "LMT"

type, public :: pdim_type
    real :: e(N_PDIMS)
end type pdim_type

contains

pure function pdim_label(pdim)
    type(pdim_type), intent(in) :: pdim
    
    character(len=PDIM_LABEL_LEN) :: pdim_label
    
    integer :: i_pdim
    
    pdim_label = "t"
    do i_pdim = 1, N_PDIMS
        write(unit=pdim_label, fmt="(a, a, z0.8)") trim(pdim_label), "_", pdim%e(i_pdim)
    end do
end function pdim_label

pure function pdim_human_readable(pdim)
    type(pdim_type), intent(in) :: pdim
    
    character(len=PDIM_HUMAN_LEN) :: pdim_human_readable
    
    integer :: i_pdim
    
    pdim_human_readable = "physical dimension: "
    do i_pdim = 1, N_PDIMS
        write(unit=pdim_human_readable, fmt="(a, a, a, a, f0.0)") trim(pdim_human_readable), &
                                                                    " [", PDIM_CHARS(i_pdim:i_pdim), "]^", pdim%e(i_pdim)
    end do
end function pdim_human_readable

subroutine write_type(file_unit, i_pdim, pdims)
    integer, intent(in)         :: file_unit, i_pdim
    type(pdim_type), intent(in) :: pdims(:)
    
    integer :: j_pdim
    
    write(unit=file_unit, fmt="(2a)") "type, public :: ", pdim_label(pdims(i_pdim))
    write(unit=file_unit, fmt="(2a)") "    ! ", trim(pdim_human_readable(pdims(i_pdim)))
    write(unit=file_unit, fmt="(a)") "contains"
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: a_", &
        pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(i_pdim))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(+) => a_", &
        pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(i_pdim))
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: s_", &
        pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(i_pdim))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(-) => s_", &
        pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(i_pdim))
    
    do j_pdim = 1, size(pdims)
        if (i_pdim /= j_pdim) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: m_", &
                pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(j_pdim))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(*) => m_", &
                pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(j_pdim))
            
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: d_", &
                pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(j_pdim))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(/) => d_", &
                pdim_label(pdims(i_pdim)), "_", pdim_label(pdims(j_pdim))
        end if
    end do
    
    write(unit=file_unit, fmt="(3a)") "end type ", pdim_label(pdims(i_pdim)), new_line("a")
    
    ! TODO: exponentiate
end subroutine write_type

subroutine write_operators(file_unit, pdim_left, pdim_right)
    integer, intent(in)         :: file_unit
    type(pdim_type), intent(in) :: pdim_left, pdim_right
    
    type(pdim_type) :: pdim_m, pdim_d
    
    ! TODO: add
    ! TODO: subtract
    
    ! TODO: multiply
    pdim_m%e = pdim_left%e + pdim_right%e
    
    ! TODO: divide
    pdim_d%e = pdim_left%e - pdim_right%e
    
    write(unit=file_unit, fmt=*) pdim_d%e
    
    ! TODO: exponentiate
end subroutine write_operators

subroutine write_binary_operator(file_unit, pdim_left, pdim_right, pdim_out, op)
    integer, intent(in)          :: file_unit
    type(pdim_type), intent(in)  :: pdim_left, pdim_right, pdim_out
    character(len=*), intent(in) :: op
    
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
        case ("**")
            op_label = "e"
        case default
            error stop "write_binary_operator: invalid op"
    end select
    
    binary_operator_procedure = op_label // "_" // pdim_label(pdim_left) // "_" // pdim_label(pdim_right)
    
    ! TODO: assert that length of `binary_operator_procedure` is 63 or less
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", binary_operator_procedure, "(pdim_left, pdim_right)"
    
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(pdim_left), "), intent(in) :: pdim_left"
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(pdim_right), "), intent(in) :: pdim_right"
    write(unit=file_unit, fmt="(4a)") "    type(", pdim_label(pdim_out), ") :: ", binary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%e = pdim_left%e ", op, " pdim_right%e"
    
    write(unit=file_unit, fmt="(2a)") "end function ", binary_operator_procedure
end subroutine write_binary_operator

end module pdim_mod

program pdim_combos

use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
use pdim_mod, only: pdim_type, write_type, write_operators, write_binary_operator
implicit none

type(pdim_type) :: pdims(2), pdim_out

! length
pdims(1)%e(1) = 1.0
pdims(1)%e(2) = 0.0
pdims(1)%e(3) = 0.0

! time
pdims(2)%e(1) = 0.0
pdims(2)%e(2) = 0.0
pdims(2)%e(3) = 1.0

! velocity
pdim_out%e(1) = 0.0
pdim_out%e(2) = 1.0
pdim_out%e(3) = -1.0

call write_type(OUTPUT_UNIT, 1, pdims)
call write_operators(OUTPUT_UNIT, pdims(1), pdims(2))
call write_binary_operator(OUTPUT_UNIT, pdims(1), pdims(2), pdim_out, "/")

end program pdim_combos
