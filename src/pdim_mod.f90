! module for the generator of a pdim_types
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module pdim_mod

use prec, only: WP
use nmllog, only: log_type
implicit none

public :: pdim_label
public :: write_type, write_as_operators, write_md_operators, write_binary_operator
public :: linspace
public :: pdim_within_bounds

real(kind=WP), parameter :: SPACING_FACTOR  = 10.0_WP
integer, parameter       :: MAX_PDIMS       = 10

type, public :: pdim_type
    real(kind=WP), allocatable :: e(:)
end type pdim_type

type, public :: pdim_config_type
    character(len=:), allocatable :: output_file, &
                                     pdim_chars, &
                                     pdim_type_defn
    integer                       :: n_pdims
    real(kind=WP), allocatable    :: min_exponents(:), &
                                     max_exponents(:)
    type(log_type)                :: logger
end type pdim_config_type

contains

pure function pdim_label(config, pdim)
    use checks, only: assert
    
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    !character(len=config%pdim_label_len) :: pdim_label
    character(len=64) :: pdim_label
    character(len=1)  :: exponent_sign
    
    integer :: i_pdim
    
    pdim_label = "pdim"
    do i_pdim = 1, config%n_pdims
        if (pdim%e(i_pdim) < 0.0_WP) then
            exponent_sign = "m"
        else
            exponent_sign = "p"
        end if
        write(unit=pdim_label, fmt="(a, a, a, i0.5)") trim(pdim_label), "_", exponent_sign, nint(10000.0_WP * abs(pdim%e(i_pdim)))
    end do
    
    ! Ensure that the `pdim_label` won't be too long to be valid in Fortran 2003.
    call assert(len(trim(pdim_label)) <= 63)
end function pdim_label

pure function pdim_human_readable(config, pdim)
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    !character(len=config%pdim_human_len) :: pdim_human_readable
    character(len=126) :: pdim_human_readable
    
    integer :: i_pdim
    
    pdim_human_readable = "physical dimension: "
    do i_pdim = 1, config%n_pdims
        write(unit=pdim_human_readable, fmt="(a, a, a, a, f0.0)") trim(pdim_human_readable), &
                                                                    " [", config%pdim_chars(i_pdim:i_pdim), "]^", pdim%e(i_pdim)
    end do
end function pdim_human_readable

pure function m_pdim(pdim_left, pdim_right)
    ! multiply
    
    type(pdim_type), intent(in) :: pdim_left, pdim_right
    
    type(pdim_type) :: m_pdim
    
    m_pdim%e = pdim_left%e + pdim_right%e
end function m_pdim

pure function d_pdim(pdim_left, pdim_right)
    ! multiply
    
    type(pdim_type), intent(in) :: pdim_left, pdim_right
    
    type(pdim_type) :: d_pdim
    
    d_pdim%e = pdim_left%e - pdim_right%e
end function d_pdim

subroutine write_type(config, file_unit, i_pdim, pdims)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit, i_pdim
    type(pdim_type), intent(in)        :: pdims(:)
    
    integer :: j_pdim
    
    write(unit=file_unit, fmt="(2a)") "type, public :: ", trim(pdim_label(config, pdims(i_pdim)))
    write(unit=file_unit, fmt="(2a)") "    ! ", trim(pdim_human_readable(config, pdims(i_pdim)))
    write(unit=file_unit, fmt="(3a)") "    ", config%pdim_type_defn, " :: v"
    write(unit=file_unit, fmt="(a)") "contains"
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: a_", &
        trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(i_pdim)))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(+) => a_", &
        trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(i_pdim)))
    
    write(unit=file_unit, fmt="(4a)") "    procedure, private :: s_", &
        trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(i_pdim)))
    write(unit=file_unit, fmt="(4a)") "    generic, public :: operator(-) => s_", &
        trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(i_pdim)))
    
    do j_pdim = 1, size(pdims)
        ! multiply
        if (pdim_within_bounds(config, m_pdim(pdims(i_pdim), pdims(j_pdim)))) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(*) => m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded due to exponent bounds: m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        end if
        
        ! divide
        if (pdim_within_bounds(config, d_pdim(pdims(i_pdim), pdims(j_pdim)))) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: d_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(/) => d_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded due to exponent bounds: d_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        end if
    end do
    
    write(unit=file_unit, fmt="(3a)") "end type ", trim(pdim_label(config, pdims(i_pdim))), new_line("a")
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
    pdim_m = m_pdim(pdim_left, pdim_right)
    if (pdim_within_bounds(config, pdim_m)) then
        call write_binary_operator(config, file_unit, pdim_left, pdim_right, pdim_m, "*")
    end if
    
    ! divide
    pdim_d = d_pdim(pdim_left, pdim_right)
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
    
    binary_operator_procedure = op_label // "_" // trim(pdim_label(config, pdim_left)) // "_" &
                                    // trim(pdim_label(config, pdim_right))
    
    call assert(len(binary_operator_procedure) <= 63)
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", binary_operator_procedure, "(pdim_left, pdim_right)"
    
    write(unit=file_unit, fmt="(2a)") "    ! left: ", trim(pdim_human_readable(config, pdim_left))
    write(unit=file_unit, fmt="(2a)") "    ! right: ", trim(pdim_human_readable(config, pdim_right))
    write(unit=file_unit, fmt="(2a)") "    ! out: ", trim(pdim_human_readable(config, pdim_out))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(pdim_label(config, pdim_left)), "), intent(in) :: pdim_left"
    write(unit=file_unit, fmt="(4a)") "    class(", trim(pdim_label(config, pdim_right)), "), intent(in) :: pdim_right"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(pdim_label(config, pdim_out)), ") :: ", binary_operator_procedure
    
    write(unit=file_unit, fmt="(5a)") "    ", binary_operator_procedure, "%v = pdim_left%v ", op, " pdim_right%v"
    
    write(unit=file_unit, fmt="(3a)") "end function ", binary_operator_procedure, new_line("a")
end subroutine write_binary_operator

pure function linspace(lower, upper, n)
    use checks, only: assert
    
    real(kind=WP), intent(in)     :: lower, upper
    integer, intent(in), optional :: n
    
    real(kind=WP), allocatable :: linspace(:)
    
    integer :: i, n_
    real(kind=WP) :: delta
    
    if (present(n)) then
        n_ = n
    else
        n_ = 50
    end if
    
    call assert(lower < upper)
    
    allocate(linspace(n_))
    delta = (upper - lower) / real(n_ - 1, WP)
    do i = 1, n_
        linspace(i) = lower + delta * real(i - 1, WP)
    end do
end function linspace

subroutine write_module(config, file_unit)
    use checks, only: assert
    
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    
    type(pdim_type), allocatable :: pdims(:)
    integer :: n_exponents(config%n_pdims), i_pdim, j_pdim, k_pdim, l_pdim
    real(kind=WP), allocatable :: exponents_1(:), exponents_2(:), exponents_3(:)
    character(len=10) :: n_pdims_char
    
    ! TODO: Generalize this so that it works for an arbitrary number of physical dimensions. This only works with 3.
    call assert(config%n_pdims == 3)
    
    do i_pdim = 1, config%n_pdims
        n_exponents(i_pdim) = nint((config%max_exponents(i_pdim) - config%min_exponents(i_pdim))) + 1
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
                
                !write(unit=*, fmt=*) pdims(l_pdim)%e, pdim_label(config, pdims(l_pdim))
            end do
        end do
    end do
    
    write(unit=n_pdims_char, fmt="(i0)") size(pdims)
    call config%logger%info("Generated " // trim(n_pdims_char) // " physical dimensions. Writing " // config%output_file // "...")
    
    ! Now actually write the module.
    
    write(unit=file_unit, fmt="(2a)") "module pdim_types", new_line("a")
    write(unit=file_unit, fmt="(2a)") "use prec, only: WP", new_line("a")
    write(unit=file_unit, fmt="(2a)") "implicit none", new_line("a")
    
    do i_pdim = 1, size(pdims)
        call write_type(config, file_unit, i_pdim, pdims)
    end do
    
    write(unit=file_unit, fmt="(2a)") "contains", new_line("a")
    
    do i_pdim = 1, size(pdims)
        call write_as_operators(config, file_unit, pdims(i_pdim))
    end do
    
    do i_pdim = 1, size(pdims)
        do j_pdim = 1, size(pdims)
            call write_md_operators(config, file_unit, pdims(i_pdim), pdims(j_pdim))
        end do
    end do
    
    write(unit=file_unit, fmt="(a)") "end module pdim_types"
end subroutine write_module

pure function pdim_within_bounds(config, pdim)
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    logical :: pdim_within_bounds
    
    pdim_within_bounds = all(pdim%e <= (config%max_exponents + SPACING_FACTOR * spacing(config%max_exponents))) &
                                .and. all(pdim%e >= (config%min_exponents - SPACING_FACTOR * spacing(config%min_exponents)))
end function pdim_within_bounds

subroutine read_config(filename, config_out, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    
    use prec, only: CL
    use checks, only: is_close
    use nmllog, only: INFO_LEVEL
    
    character(len=*), intent(in)        :: filename
    type(pdim_config_type), intent(out) :: config_out
    integer, intent(out)                :: rc
    
    integer           :: nml_unit, rc_nml, n_failures, i_pdim, n_pdims_bounds(2)
    character(len=CL) :: nml_error_message
    
    character(len=CL)        :: output_file, pdim_type_defn
    character(len=MAX_PDIMS) :: pdim_chars
    real(kind=WP)            :: min_exponents(MAX_PDIMS), max_exponents(MAX_PDIMS)
    
    namelist /config/ output_file, pdim_chars, pdim_type_defn, min_exponents, max_exponents
    
    call config_out%logger%open("pdim.nml", level=INFO_LEVEL)
    
    pdim_chars      = ""
    pdim_type_defn  = "real(kind=WP)"
    min_exponents   = 0.0_WP
    max_exponents   = 0.0_WP
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        call config_out%logger%error(trim(nml_error_message))
        return
    end if
    
    n_failures = 0
    
    call config_out%logger%check(len(pdim_chars) > 0, "pdim_chars must have 1 or more characters", n_failures)
    call config_out%logger%check(len(pdim_type_defn) > 0, "pdim_type_defn must have 1 or more characters", n_failures)
    
    n_pdims_bounds = huge(1)
    do i_pdim = 1, MAX_PDIMS
        if (is_close(min_exponents(i_pdim), 0.0_WP)) then
            n_pdims_bounds(1) = min(n_pdims_bounds(1), i_pdim - 1)
        end if
        
        if (is_close(max_exponents(i_pdim), 0.0_WP)) then
            n_pdims_bounds(2) = min(n_pdims_bounds(2), i_pdim - 1)
        end if
    end do
    
    call config_out%logger%check(all(min_exponents(1:len(trim(pdim_chars))) < max_exponents(1:len(trim(pdim_chars)))), &
        "at least one min_exponents is equal or higher than the corresponding min_exponents", n_failures)
    
    call config_out%logger%check(n_pdims_bounds(1) == len(trim(pdim_chars)), "size(min_exponents) /= len(pdim_chars).", n_failures)
    call config_out%logger%check(n_pdims_bounds(2) == len(trim(pdim_chars)), "size(max_exponents) /= len(pdim_chars).", n_failures)
    
    ! TODO: Check that `pdim_chars` has unique characters
    
    if (n_failures > 0) then
        call config_out%logger%error("input validation error(s)")
        rc = n_failures
        return
    end if
    
    config_out%output_file     = trim(output_file)
    config_out%pdim_chars      = trim(pdim_chars)
    config_out%pdim_type_defn  = trim(pdim_type_defn)
    config_out%n_pdims         = len(config_out%pdim_chars)
    config_out%min_exponents   = min_exponents(1:config_out%n_pdims)
    config_out%max_exponents   = max_exponents(1:config_out%n_pdims)
    
    call config_out%logger%info(config_out%output_file // " successfully read.")
    
    rc = 0
    
    error stop
end subroutine read_config

end module pdim_mod
