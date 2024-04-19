module autodiff

use prec, only: WP
implicit none
private

public :: rd_var, rd_const
public :: operator(+)
public :: operator(-)
public :: operator(*)
public :: operator(/)
public :: operator(**)

private :: rd_rd_add, rd_real_add, real_rd_add
private :: rd_rd_subtract, rd_real_subtract, real_rd_subtract, rd_unary_minus
private :: rd_rd_multiply, rd_real_multiply, real_rd_multiply
private :: rd_rd_divide, rd_real_divide, real_rd_divide
private :: rd_real_exponentiate, rd_integer_exponentiate

! Declare `rd` derived type
! -------------------------

! Number of design (differentiable) variables.
! The easiest Fortran 90 compliant approach has a hard-coded number.
! Making `dv` a `allocatable` is also possible, but not pursued here for simplicity.
! Fortran 2003 offers allocatable arrays in derived types and parameterized derived types, which are more flexible.
integer, public, parameter :: NDVARS = 5

! Both the dependent and independent variables need to be of type `rd`.
type, public :: rd
    real(kind=WP) :: v  ! function value
    real(kind=WP) :: dv(NDVARS) ! function derivatives value
end type rd

! Declare operator interfaces for `rd`
! ------------------------------------

interface operator(+)
    ! Overload the `+` operator so that it works for `rd`s.
    module procedure rd_rd_add, rd_real_add, real_rd_add
end interface

interface operator(-)
    ! Overload the `-` operator so that it works for `rd`s.
    module procedure rd_rd_subtract, rd_real_subtract, real_rd_subtract, rd_unary_minus
end interface

interface operator(*)
    ! Overload the `*` operator so that it works for `rd`s.
    module procedure rd_rd_multiply, rd_real_multiply, real_rd_multiply
end interface

interface operator(/)
    ! Overload the `/` operator so that it works for `rd`s.
    module procedure rd_rd_divide, rd_real_divide, real_rd_divide
end interface

interface operator(**)
    ! Overload the `**` operator so that it works for `rd`s.
    module procedure rd_real_exponentiate, rd_integer_exponentiate
end interface

contains

! Constructors for `rd`
! ---------------------

function rd_var(v, n)
    real(kind=WP), intent(in)    :: v ! value of variable to set
    integer, intent(in) :: n ! variable number represented (sets the appropriate derivative)
    type(rd) :: rd_var

    !integer :: i_dv ! loop index

!    do i_dv = 1, NDVARS
!        if (i_dv == n) then
!            rd_var%dv(i_dv) = 1.0_WP
!        else
!            rd_var%dv(i_dv) = 0.0_WP
!        end if
!    end do

    rd_var = rd_const(v)

    ! The following may be slower, though I should test that.
    ! But it will give a run-time error in debug mode if `n` is out of bounds.
    rd_var%dv(n) = 1.0_WP

    return
end function rd_var

function rd_const(v)
    real(kind=WP), intent(in) :: v ! value of constant to set
    type(rd) :: rd_const

    rd_const%v  = v
    rd_const%dv = 0.0_WP

    return
end function rd_const

! Operator procedures
! -------------------

function rd_rd_add(rd_1, rd_2)
    ! Adds two `rd`s.

    type(rd), intent(in) :: rd_1, rd_2
    type(rd)             :: rd_rd_add

    rd_rd_add%v  = rd_1%v  + rd_2%v
    rd_rd_add%dv = rd_1%dv + rd_2%dv

    return
end function rd_rd_add

function rd_real_add(rd_in, real_in)
    ! Adds a `rd` and a `real`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: rd_real_add

    rd_real_add%v  = rd_in%v + real_in
    rd_real_add%dv = rd_in%dv

    return
end function rd_real_add

function real_rd_add(real_in, rd_in)
    ! Adds a `real` and a `rd`.

    real(kind=WP), intent(in) :: real_in
    type(rd), intent(in)      :: rd_in
    type(rd)                  :: real_rd_add

    real_rd_add%v  = real_in + rd_in%v
    real_rd_add%dv = rd_in%dv

    return
end function real_rd_add

function rd_rd_subtract(rd_1, rd_2)
    ! Subtracts two `rd`s.

    type(rd), intent(in) :: rd_1, rd_2
    type(rd)             :: rd_rd_subtract

    rd_rd_subtract%v  = rd_1%v  - rd_2%v
    rd_rd_subtract%dv = rd_1%dv - rd_2%dv

    return
end function rd_rd_subtract

function rd_real_subtract(rd_in, real_in)
    ! Subtracts a `real` from a `rd`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: rd_real_subtract

    rd_real_subtract%v  = rd_in%v - real_in
    rd_real_subtract%dv = rd_in%dv

    return
end function rd_real_subtract

function real_rd_subtract(real_in, rd_in)
    ! Subtracts a `real` from a `rd`.

    real(kind=WP), intent(in) :: real_in
    type(rd), intent(in)      :: rd_in
    type(rd)                  :: real_rd_subtract

    real_rd_subtract%v  = real_in - rd_in%v
    real_rd_subtract%dv = -rd_in%dv

    return
end function real_rd_subtract

function rd_unary_minus(rd_in)
    ! Returns `-rd`.

    type(rd), intent(in) :: rd_in
    type(rd)             :: rd_unary_minus

    rd_unary_minus%v  = -rd_in%v
    rd_unary_minus%dv = -rd_in%dv

    return
end function rd_unary_minus

function rd_rd_multiply(rd_1, rd_2)
    ! Multiplies two `rd`s.

    type(rd), intent(in) :: rd_1, rd_2
    type(rd)             :: rd_rd_multiply

    rd_rd_multiply%v  = rd_1%v * rd_2%v
    rd_rd_multiply%dv = rd_1%dv * rd_2%v + rd_1%v * rd_2%dv

    return
end function rd_rd_multiply

function rd_real_multiply(rd_in, real_in)
    ! Multiplies a `rd` by a `real`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: rd_real_multiply

    rd_real_multiply%v  = rd_in%v * real_in
    rd_real_multiply%dv = rd_in%dv * real_in

    return
end function rd_real_multiply

function real_rd_multiply(real_in, rd_in)
    ! Multiplies a `real` by a `rd`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: real_rd_multiply

    real_rd_multiply%v  = real_in * rd_in%v
    real_rd_multiply%dv = real_in * rd_in%dv

    return
end function real_rd_multiply

function rd_rd_divide(rd_1, rd_2)
    ! Divides two `rd`.

    type(rd), intent(in) :: rd_1, rd_2
    type(rd)             :: rd_rd_divide

    rd_rd_divide%v  = rd_1%v / rd_2%v
    rd_rd_divide%dv = (rd_1%dv * rd_2%v - rd_1%v * rd_2%dv) / (rd_2%v**2)

    return
end function rd_rd_divide

function rd_real_divide(rd_in, real_in)
    ! Divides a `rd` by a `real`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: rd_real_divide

    rd_real_divide%v  = rd_in%v / real_in
    rd_real_divide%dv = rd_in%dv / real_in

    return
end function rd_real_divide

function real_rd_divide(real_in, rd_in)
    ! Divides a `real` by a `rd`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: real_rd_divide

    real_rd_divide%v  = real_in / rd_in%v
    real_rd_divide%dv = -real_in * rd_in%dv / (rd_in%v**2)

    return
end function real_rd_divide

function rd_real_exponentiate(rd_in, real_in)
    ! Exponentiates a `rd` by a `real`.

    type(rd), intent(in)      :: rd_in
    real(kind=WP), intent(in) :: real_in
    type(rd)                  :: rd_real_exponentiate

    rd_real_exponentiate%v  = rd_in%v**real_in
    rd_real_exponentiate%dv = real_in*(rd_in%v**(real_in - 1.0_WP))*rd_in%dv

    return
end function rd_real_exponentiate

function rd_integer_exponentiate(rd_in, integer_in)
    ! Exponentiates a `rd` by an `integer`.

    type(rd), intent(in)         :: rd_in
    integer, intent(in) :: integer_in
    type(rd)                     :: rd_integer_exponentiate

    rd_integer_exponentiate%v  = rd_in%v**integer_in
    rd_integer_exponentiate%dv = real(integer_in, WP)*(rd_in%v**(integer_in - 1))*rd_in%dv

    return
end function rd_integer_exponentiate

! No `rd**rd` as that's not likely to happen in CFD.

end module autodiff
