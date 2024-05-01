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
public :: pdim_in_set

real(kind=WP), parameter :: SPACING_FACTOR  = 10.0_WP
integer, parameter       :: MAX_PDIMS       = 10, &
                            MAX_LABEL_LEN   = 63, &
                            LABEL_LEN       = MAX_LABEL_LEN + 1

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
    integer, allocatable          :: denominators(:)
    
    logical :: exponentiation
    
    type(log_type) :: logger
    
    character(len=MAX_LABEL_LEN), allocatable :: labels(:)
    type(pdim_type), allocatable  :: pdims(:)
end type pdim_config_type

contains

pure function pdim_label(config, pdim)
    use checks, only: assert
    
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim
    
    !character(len=config%pdim_label_len) :: pdim_label
    character(len=LABEL_LEN) :: pdim_label
    character(len=1)         :: exponent_sign
    
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
    call assert(len(trim(pdim_label)) <= MAX_LABEL_LEN)
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
        if (pdim_in_set(config, m_pdim(pdims(i_pdim), pdims(j_pdim)), pdims)) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(*) => m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded: m_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        end if
        
        ! divide
        if (pdim_in_set(config, d_pdim(pdims(i_pdim), pdims(j_pdim)), pdims)) then
            write(unit=file_unit, fmt="(5a)") "    procedure, private :: d_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
            write(unit=file_unit, fmt="(5a)") "    generic, public :: operator(/) => d_", &
                trim(pdim_label(config, pdims(i_pdim))), "_", trim(pdim_label(config, pdims(j_pdim)))
        else
            write(unit=file_unit, fmt="(5a)") "    ! excluded: d_", &
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

subroutine write_md_operators(config, file_unit, pdim_left, pdim_right, pdims)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdim_left, pdim_right, pdims(:)
    
    type(pdim_type) :: pdim_m, pdim_d
    
    ! multiply
    pdim_m = m_pdim(pdim_left, pdim_right)
    if (pdim_in_set(config, pdim_m, pdims)) then
        call write_binary_operator(config, file_unit, pdim_left, pdim_right, pdim_m, "*")
    end if
    
    ! divide
    pdim_d = d_pdim(pdim_left, pdim_right)
    if (pdim_in_set(config, pdim_d, pdims)) then
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
    
    call assert(len(binary_operator_procedure) <= MAX_LABEL_LEN)
    
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

pure function sqrt_pdim(pdim)
    type(pdim_type), intent(in) :: pdim
    
    type(pdim_type) :: sqrt_pdim
    
    sqrt_pdim%e = 0.5_WP * pdim%e
end function sqrt_pdim

pure function cbrt_pdim(pdim)
    type(pdim_type), intent(in) :: pdim
    
    type(pdim_type) :: cbrt_pdim
    
    cbrt_pdim%e = pdim%e / 3.0_WP
end function cbrt_pdim

pure function square_pdim(pdim)
    type(pdim_type), intent(in) :: pdim
    
    type(pdim_type) :: square_pdim
    
    square_pdim%e = 2.0_WP * pdim%e
end function square_pdim

subroutine write_exponentiation_interfaces(config, file_unit, pdims)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdims(:)
    
    integer :: i_pdim
    
    write(unit=file_unit, fmt="(a)") "interface sqrt"
    do i_pdim = 1, size(pdims)
        if (pdim_in_set(config, sqrt_pdim(pdims(i_pdim)), pdims)) then
        
            write(unit=file_unit, fmt="(2a)") "    module procedure sqrt_", trim(pdim_label(config, pdims(i_pdim)))
        else
            write(unit=file_unit, fmt="(2a)") "    ! excluded: sqrt_", trim(pdim_label(config, pdims(i_pdim)))
        end if
    end do
    write(unit=file_unit, fmt="(2a)") "end interface sqrt", new_line("a")
    
    write(unit=file_unit, fmt="(a)") "interface cbrt"
    do i_pdim = 1, size(pdims)
        if (pdim_in_set(config, cbrt_pdim(pdims(i_pdim)), pdims)) then
        
            write(unit=file_unit, fmt="(2a)") "    module procedure cbrt_", trim(pdim_label(config, pdims(i_pdim)))
        else
            write(unit=file_unit, fmt="(2a)") "    ! excluded: cbrt_", trim(pdim_label(config, pdims(i_pdim)))
        end if
    end do
    write(unit=file_unit, fmt="(2a)") "end interface cbrt", new_line("a")
    
    write(unit=file_unit, fmt="(a)") "interface square"
    do i_pdim = 1, size(pdims)
        if (pdim_in_set(config, square_pdim(pdims(i_pdim)), pdims)) then
        
            write(unit=file_unit, fmt="(2a)") "    module procedure square_", trim(pdim_label(config, pdims(i_pdim)))
        else
            write(unit=file_unit, fmt="(2a)") "    ! excluded: square_", trim(pdim_label(config, pdims(i_pdim)))
        end if
    end do
    write(unit=file_unit, fmt="(2a)") "end interface square", new_line("a")
end subroutine write_exponentiation_interfaces

subroutine write_exponentiation_function(config, file_unit, pdim, op)
    type(pdim_config_type), intent(in) :: config
    integer, intent(in)                :: file_unit
    type(pdim_type), intent(in)        :: pdim
    character(len=*), intent(in)       :: op
    
    type(pdim_type)               :: pdim_out
    character(len=:), allocatable :: op_pre, op_post, exponentiation_function
    
    select case (op)
        case ("sqrt")
            op_pre = "sqrt("
            op_post = ")"
            
            pdim_out = sqrt_pdim(pdim)
        case ("cbrt")
            ! <https://community.intel.com/t5/Intel-Fortran-Compiler/Fast-cube-root/m-p/1171728>
            ! <https://www.reddit.com/r/fortran/comments/t9qkqd/cuberoot_and_my_dissent_into_madness/>
            ! <https://github.com/fortran-lang/stdlib/issues/214>
            op_pre = "("
            op_post = ")**(1.0_WP/3.0_WP)"
            
            pdim_out = cbrt_pdim(pdim)
        case ("square")
            op_pre = "("
            op_post = ")**2"
            
            pdim_out = square_pdim(pdim)
        case default
            error stop "write_exponentiation_function: invalid op"
    end select
    
    exponentiation_function = op // "_" // trim(pdim_label(config, pdim))
    
    write(unit=file_unit, fmt="(3a)") "elemental function ", exponentiation_function, "(pdim)"
    
    write(unit=file_unit, fmt="(2a)") "    ! arg: ", trim(pdim_human_readable(config, pdim))
    write(unit=file_unit, fmt="(2a)") "    ! out: ", trim(pdim_human_readable(config, pdim_out))
    
    write(unit=file_unit, fmt="(4a)") "    class(", trim(pdim_label(config, pdim)), "), intent(in) :: pdim"
    write(unit=file_unit, fmt="(4a)") "    type(", trim(pdim_label(config, pdim_out)), ") :: ", exponentiation_function
    
    write(unit=file_unit, fmt="(6a)") "    ", exponentiation_function, "%v = ", op_pre, "pdim%v", op_post
    
    write(unit=file_unit, fmt="(3a)") "end function ", exponentiation_function, new_line("a")
end subroutine write_exponentiation_function

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
        n_exponents(i_pdim) = nint(config%max_exponents(i_pdim) - config%min_exponents(i_pdim)) &
                                    * config%denominators(i_pdim) + 1
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
    
    call assert(l_pdim == size(pdims))
    
    write(unit=n_pdims_char, fmt="(i0)") size(pdims)
    call config%logger%info("Generated " // trim(n_pdims_char) // " physical dimensions. Writing " // config%output_file // "...")
    
    ! Now actually write the module.
    
    write(unit=file_unit, fmt="(2a)") "module pdim_types", new_line("a")
    write(unit=file_unit, fmt="(2a)") "use prec, only: WP", new_line("a")
    write(unit=file_unit, fmt="(2a)") "implicit none", new_line("a")
    
    ! Write `use` lines as comments.
    if (size(config%labels) > 0) then
        write(unit=file_unit, fmt="(a)", advance="no") "! use pdim_types, only: "
        
        do i_pdim = 1, size(config%labels)
            if (i_pdim /= 1) then
                write(unit=file_unit, fmt="(a)", advance="no") "!                       "
            end if
            
            write(unit=file_unit, fmt="(3a)", advance="no") trim(config%labels(i_pdim)), &
                                                                " => ", trim(pdim_label(config, config%pdims(i_pdim)))
            if (i_pdim /= size(config%labels)) then
                write(unit=file_unit, fmt="(a)") ", &"
            else
                write(unit=file_unit, fmt="(a)") ""
            end if
        end do
        write(unit=file_unit, fmt="(a)") ""
    end if
    
    do i_pdim = 1, size(pdims)
        call write_type(config, file_unit, i_pdim, pdims)
    end do
    
    if (config%exponentiation) then
        call write_exponentiation_interfaces(config, file_unit, pdims)
    end if
    
    write(unit=file_unit, fmt="(2a)") "contains", new_line("a")
    
    do i_pdim = 1, size(pdims)
        call write_as_operators(config, file_unit, pdims(i_pdim))
    end do
    
    do i_pdim = 1, size(pdims)
        do j_pdim = 1, size(pdims)
            call write_md_operators(config, file_unit, pdims(i_pdim), pdims(j_pdim), pdims)
        end do
    end do
    
    if (config%exponentiation) then
        do i_pdim = 1, size(pdims)
            if (pdim_in_set(config, sqrt_pdim(pdims(i_pdim)), pdims)) then
                call write_exponentiation_function(config, file_unit, pdims(i_pdim), "sqrt")
            end if
            
            if (pdim_in_set(config, cbrt_pdim(pdims(i_pdim)), pdims)) then
                call write_exponentiation_function(config, file_unit, pdims(i_pdim), "cbrt")
            end if
            
            if (pdim_in_set(config, square_pdim(pdims(i_pdim)), pdims)) then
                call write_exponentiation_function(config, file_unit, pdims(i_pdim), "square")
            end if
        end do
    end if
    
    write(unit=file_unit, fmt="(a)") "end module pdim_types"
end subroutine write_module

pure function pdim_in_set(config, pdim, pdims)
    use checks, only: is_close
    
    type(pdim_config_type), intent(in) :: config
    type(pdim_type), intent(in)        :: pdim, pdims(:)
    
    logical :: pdim_in_set
    
    integer :: i_pdim, j_pdim, n_match
    
    pdim_in_set = .false.
    do i_pdim = 1, size(pdims)
        n_match = 0
        do j_pdim = 1, config%n_pdims
            if (is_close(pdim%e(j_pdim), pdims(i_pdim)%e(j_pdim))) then
             n_match = n_match + 1
            end if
        end do
        
        if (n_match == config%n_pdims) then
            pdim_in_set = .true.
            exit
        end if
    end do
end function pdim_in_set

subroutine read_config(filename, config_out, rc)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    
    use prec, only: CL
    use checks, only: is_close
    use nmllog, only: INFO_LEVEL
    
    character(len=*), intent(in)        :: filename
    type(pdim_config_type), intent(out) :: config_out
    integer, intent(out)                :: rc
    
    integer           :: nml_unit, rc_nml, n_failures, i_pdim, n_pdims_bounds(3), n_pdims
    character(len=CL) :: nml_error_message
    
    character(len=CL)        :: output_file, pdim_type_defn
    character(len=MAX_PDIMS) :: pdim_chars
    real(kind=WP)            :: min_exponents(MAX_PDIMS), max_exponents(MAX_PDIMS)
    integer                  :: denominators(MAX_PDIMS)
    logical                  :: exponentiation
    
    character(len=MAX_LABEL_LEN) :: label
    real(kind=WP)                :: e(MAX_PDIMS)
    
    namelist /config/ output_file, pdim_chars, pdim_type_defn, min_exponents, max_exponents, denominators, exponentiation
    namelist /pdim/ label, e
    
    call config_out%logger%open("pdim.nml", level=INFO_LEVEL)
    
    pdim_chars      = ""
    pdim_type_defn  = "real(kind=WP)"
    min_exponents   = 0.0_WP
    max_exponents   = 0.0_WP
    denominators    = 0
    exponentiation  = .true.
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=config, iostat=rc_nml, iomsg=nml_error_message)
    close(unit=nml_unit)
    
    if ((rc_nml /= 0) .and. (rc_nml /= IOSTAT_END)) then
        call config_out%logger%error(trim(nml_error_message))
        rc = rc_nml
        close(unit=nml_unit)
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
        
        if (denominators(i_pdim) == 0) then
            n_pdims_bounds(3) = min(n_pdims_bounds(3), i_pdim - 1)
        end if
    end do
    
    call config_out%logger%check(all(min_exponents(1:len(trim(pdim_chars))) < max_exponents(1:len(trim(pdim_chars)))), &
        "at least one min_exponents is equal or higher than the corresponding min_exponents", n_failures)
    
    call config_out%logger%check(n_pdims_bounds(1) == len(trim(pdim_chars)), &
                                    "size(min_exponents) /= len(pdim_chars).", n_failures)
    call config_out%logger%check(n_pdims_bounds(2) == len(trim(pdim_chars)), &
                                    "size(max_exponents) /= len(pdim_chars).", n_failures)
    call config_out%logger%check(n_pdims_bounds(3) == len(trim(pdim_chars)), &
                                    "size(denominators) /= len(pdim_chars).", n_failures)
    call config_out%logger%check(all(denominators(1:config_out%n_pdims) > 0), &
                                    "denominators is either undefined or less than zero.", n_failures)
    
    ! TODO: Check that `pdim_chars` has unique characters
    
    if (n_failures > 0) then
        call config_out%logger%error("input validation error(s)")
        rc = n_failures
        return
    end if
    
    config_out%output_file    = trim(output_file)
    config_out%pdim_chars     = trim(pdim_chars)
    config_out%pdim_type_defn = trim(pdim_type_defn)
    config_out%n_pdims        = len(config_out%pdim_chars)
    config_out%min_exponents  = min_exponents(1:config_out%n_pdims)
    config_out%max_exponents  = max_exponents(1:config_out%n_pdims)
    config_out%denominators   = denominators(1:config_out%n_pdims)
    config_out%exponentiation = exponentiation
    
    ! `pdim` namelist groups
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    
    ! First get `n_pdims`, allocate `config`, and read everything in.
    n_pdims = 0
    do
        label = ""
        e = 0.0_WP
        read(unit=nml_unit, nml=pdim, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call config_out%logger%error(trim(nml_error_message))
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        n_pdims = n_pdims + 1
    end do
    
    rewind nml_unit
    allocate(config_out%labels(n_pdims))
    allocate(config_out%pdims(n_pdims))
    i_pdim = 0
    do
        label = ""
        e = 0.0_WP
        read(unit=nml_unit, nml=pdim, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call config_out%logger%error(trim(nml_error_message))
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        i_pdim = i_pdim + 1
        
        !write(unit=*, fmt=*) i_pdim, trim(label), e(1:config_out%n_pdims)
        config_out%labels(i_pdim) = trim(label)
        config_out%pdims(i_pdim)%e = e(1:config_out%n_pdims)
    end do
    
    close(unit=nml_unit)
    
    call config_out%logger%info(config_out%output_file // " successfully read.")
    
    rc = 0
end subroutine read_config

end module pdim_mod
