module autodiff

use prec, only: WP
implicit none
private

public :: operator(+)
public :: operator(-)
public :: operator(*)
public :: operator(/)
public :: operator(**)

private :: ad_ad_add, ad_real_add, real_ad_add
private :: ad_ad_subtract, ad_real_subtract, real_ad_subtract, ad_unary_minus
private :: ad_ad_multiply, ad_real_multiply, real_ad_multiply
private :: ad_ad_divide, ad_real_divide, real_ad_divide
private :: ad_real_exponentiate, ad_integer_exponentiate

! Both the dependent and independent variables need to be of type `rd`.
type, public :: ad
    integer                    :: n_dv  ! total number of differentiable variables
    real(kind=WP)              :: v     ! function value
    real(kind=WP), allocatable :: dv(:) ! function derivatives value
contains
    procedure :: init
    procedure :: init_const
end type ad

! Declare operator interfaces for `rd`
! ------------------------------------

interface operator(+)
    ! Overload the `+` operator so that it works for `rd`s.
    module procedure ad_ad_add, ad_real_add, real_ad_add
end interface

interface operator(-)
    ! Overload the `-` operator so that it works for `rd`s.
    module procedure ad_ad_subtract, ad_real_subtract, real_ad_subtract, ad_unary_minus
end interface

interface operator(*)
    ! Overload the `*` operator so that it works for `rd`s.
    module procedure ad_ad_multiply, ad_real_multiply, real_ad_multiply
end interface

interface operator(/)
    ! Overload the `/` operator so that it works for `rd`s.
    module procedure ad_ad_divide, ad_real_divide, real_ad_divide
end interface

interface operator(**)
    ! Overload the `**` operator so that it works for `rd`s.
    module procedure ad_real_exponentiate, ad_integer_exponentiate
end interface

contains

! Constructors for `rd`
! ---------------------

subroutine init(x, v, n, n_dv)
    class(ad), intent(out)    :: x
    real(kind=WP), intent(in) :: v    ! value of variable to set
    integer, intent(in)       :: n, & ! variable number represented (sets the appropriate derivative)
                                 n_dv ! total number of differentiable variables

    !integer :: i_dv ! loop index

!    do i_dv = 1, NDVARS
!        if (i_dv == n) then
!            ad_var%dv(i_dv) = 1.0_WP
!        else
!            ad_var%dv(i_dv) = 0.0_WP
!        end if
!    end do
    
    call init_const(x, v, n_dv)

    ! The following may be slower, though I should test that.
    ! But it will give a run-time error in debug mode if `n` is out of bounds.
    x%dv(n) = 1.0_WP
end subroutine init

subroutine init_const(x, v, n_dv)
    class(ad), intent(out)    :: x
    real(kind=WP), intent(in) :: v    ! value of constant to set
    integer, intent(in)       :: n_dv ! total number of differentiable variables
    
    allocate(x%dv(n_dv))

    x%n_dv = n_dv
    x%v    = v
    x%dv   = 0.0_WP
end subroutine init_const

! Operator procedures
! -------------------

function ad_ad_add(ad_1, ad_2)
    ! Adds two `rd`s.

    type(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_add

    ad_ad_add%v  = ad_1%v  + ad_2%v
    ad_ad_add%dv = ad_1%dv + ad_2%dv
end function ad_ad_add

function ad_real_add(ad_in, real_in)
    ! Adds a `rd` and a `real`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_add

    ad_real_add%v  = ad_in%v + real_in
    ad_real_add%dv = ad_in%dv
end function ad_real_add

function real_ad_add(real_in, ad_in)
    ! Adds a `real` and a `rd`.

    real(kind=WP), intent(in) :: real_in
    type(ad), intent(in)      :: ad_in
    
    type(ad) :: real_ad_add

    real_ad_add%v  = real_in + ad_in%v
    real_ad_add%dv = ad_in%dv
end function real_ad_add

function ad_ad_subtract(ad_1, ad_2)
    ! Subtracts two `rd`s.

    type(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_subtract

    ad_ad_subtract%v  = ad_1%v  - ad_2%v
    ad_ad_subtract%dv = ad_1%dv - ad_2%dv
end function ad_ad_subtract

function ad_real_subtract(ad_in, real_in)
    ! Subtracts a `real` from a `rd`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_subtract

    ad_real_subtract%v  = ad_in%v - real_in
    ad_real_subtract%dv = ad_in%dv
end function ad_real_subtract

function real_ad_subtract(real_in, ad_in)
    ! Subtracts a `real` from a `rd`.

    real(kind=WP), intent(in) :: real_in
    type(ad), intent(in)      :: ad_in
    
    type(ad) :: real_ad_subtract

    real_ad_subtract%v  = real_in - ad_in%v
    real_ad_subtract%dv = -ad_in%dv
end function real_ad_subtract

function ad_unary_minus(ad_in)
    ! Returns `-rd`.

    type(ad), intent(in) :: ad_in
    
    type(ad) :: ad_unary_minus

    ad_unary_minus%v  = -ad_in%v
    ad_unary_minus%dv = -ad_in%dv
end function ad_unary_minus

function ad_ad_multiply(ad_1, ad_2)
    ! Multiplies two `rd`s.

    type(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_multiply

    ad_ad_multiply%v  = ad_1%v * ad_2%v
    ad_ad_multiply%dv = ad_1%dv * ad_2%v + ad_1%v * ad_2%dv
end function ad_ad_multiply

function ad_real_multiply(ad_in, real_in)
    ! Multiplies a `rd` by a `real`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_multiply

    ad_real_multiply%v  = ad_in%v * real_in
    ad_real_multiply%dv = ad_in%dv * real_in
end function ad_real_multiply

function real_ad_multiply(real_in, ad_in)
    ! Multiplies a `real` by a `rd`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: real_ad_multiply

    real_ad_multiply%v  = real_in * ad_in%v
    real_ad_multiply%dv = real_in * ad_in%dv
end function real_ad_multiply

function ad_ad_divide(ad_1, ad_2)
    ! Divides two `rd`.

    type(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_divide

    ad_ad_divide%v  = ad_1%v / ad_2%v
    ad_ad_divide%dv = (ad_1%dv * ad_2%v - ad_1%v * ad_2%dv) / (ad_2%v**2)
end function ad_ad_divide

function ad_real_divide(ad_in, real_in)
    ! Divides a `rd` by a `real`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_divide

    ad_real_divide%v  = ad_in%v / real_in
    ad_real_divide%dv = ad_in%dv / real_in
end function ad_real_divide

function real_ad_divide(real_in, ad_in)
    ! Divides a `real` by a `rd`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: real_ad_divide

    real_ad_divide%v  = real_in / ad_in%v
    real_ad_divide%dv = -real_in * ad_in%dv / (ad_in%v**2)
end function real_ad_divide

function ad_real_exponentiate(ad_in, real_in)
    ! Exponentiates a `rd` by a `real`.

    type(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_exponentiate

    ad_real_exponentiate%v  = ad_in%v**real_in
    ad_real_exponentiate%dv = real_in*(ad_in%v**(real_in - 1.0_WP))*ad_in%dv
end function ad_real_exponentiate

function ad_integer_exponentiate(ad_in, integer_in)
    ! Exponentiates a `rd` by an `integer`.

    type(ad), intent(in) :: ad_in
    integer, intent(in)  :: integer_in
    
    type(ad) :: ad_integer_exponentiate

    ad_integer_exponentiate%v  = ad_in%v**integer_in
    ad_integer_exponentiate%dv = real(integer_in, WP)*(ad_in%v**(integer_in - 1))*ad_in%dv
end function ad_integer_exponentiate

! No `rd**rd` as that's not likely to happen in CFD.

end module autodiff
