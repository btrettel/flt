! Module for forward-mode automatic differentiation.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module fmad

use prec, only: WP
implicit none
private

public :: sqrt
public :: f

! Both the dependent and independent variables need to be of type `ad`.
type, public :: ad
    real(kind=WP)              :: v     ! function value
    real(kind=WP), allocatable :: dv(:) ! function derivatives value
contains
    procedure :: init
    procedure :: init_const
    procedure, private :: ad_ad_add, ad_real_add
    procedure, private, pass(ad_in) :: real_ad_add
    generic, public :: operator(+) => ad_ad_add, ad_real_add, real_ad_add
    procedure, private :: ad_ad_subtract, ad_real_subtract, ad_unary_minus
    procedure, private, pass(ad_in) :: real_ad_subtract
    generic, public :: operator(-) => ad_ad_subtract, ad_real_subtract, real_ad_subtract, ad_unary_minus
    procedure, private :: ad_ad_multiply, ad_real_multiply
    procedure, private, pass(ad_in) :: real_ad_multiply
    generic, public :: operator(*) => ad_ad_multiply, ad_real_multiply, real_ad_multiply
    procedure, private :: ad_ad_divide, ad_real_divide
    procedure, private, pass(ad_in) :: real_ad_divide
    generic, public :: operator(/) => ad_ad_divide, ad_real_divide, real_ad_divide
    procedure, private :: ad_real_exponentiate, ad_integer_exponentiate
    generic, public :: operator(**) => ad_real_exponentiate, ad_integer_exponentiate
end type ad

interface sqrt
    module procedure :: ad_sqrt
end interface sqrt

contains

! Constructors for `ad`
! ---------------------

elemental subroutine init(x, v, n, n_dv)
    use checks, only: assert, is_close
    
    class(ad), intent(in out) :: x    ! `class` can't be `intent(out)` and `pure`?!?
    real(kind=WP), intent(in) :: v    ! value of variable to set
    integer, intent(in)       :: n, & ! variable number represented (sets the appropriate derivative)
                                 n_dv ! total number of differentiable variables

    integer :: i_dv ! loop index
    
    call assert(n_dv >= 1, "autodiff (init): n_dv must be one or more")
    call assert(n >= 1, "autodiff (init): n must be 1 or more")
    call assert(n <= n_dv, "autodiff (init): n must be n_dv or less")
    
    x%v = v
    
    allocate(x%dv(n_dv))
    do concurrent (i_dv = 1:n_dv)
        x%dv(i_dv) = merge(1.0_WP, 0.0_WP, i_dv == n)
    end do
    call assert(any(is_close(x%dv, 1.0_WP)), "autodiff (init): at least one derivative set")
    call assert(any(is_close(x%dv, 0.0_WP)), "autodiff (init): at least one derivative not set")
end subroutine init

elemental subroutine init_const(x, v, n_dv)
    use checks, only: assert
    
    class(ad), intent(in out) :: x ! `class` can't be `intent(out)` and `pure`?!?
    real(kind=WP), intent(in) :: v    ! value of constant to set
    integer, intent(in)       :: n_dv ! total number of differentiable variables
    
    call assert(n_dv >= 1, "autodiff (init): n_dv must be one or more")
    
    allocate(x%dv(n_dv))

    x%v  = v
    x%dv = 0.0_WP
end subroutine init_const

! Operator procedures
! -------------------

elemental function ad_ad_add(ad_1, ad_2)
    ! Adds two `ad`s.
    
    class(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_add

    ad_ad_add%v  = ad_1%v  + ad_2%v
    ad_ad_add%dv = ad_1%dv + ad_2%dv
end function ad_ad_add

elemental function ad_real_add(ad_in, real_in)
    ! Adds an `ad` and a `real`.

    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_add

    ad_real_add%v  = ad_in%v + real_in
    ad_real_add%dv = ad_in%dv
end function ad_real_add

elemental function real_ad_add(real_in, ad_in)
    ! Adds a `real` and an `ad`.

    real(kind=WP), intent(in) :: real_in
    class(ad), intent(in)     :: ad_in
    
    type(ad) :: real_ad_add

    real_ad_add%v  = real_in + ad_in%v
    real_ad_add%dv = ad_in%dv
end function real_ad_add

elemental function ad_ad_subtract(ad_1, ad_2)
    ! Subtracts two `ad`s.

    class(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_subtract

    ad_ad_subtract%v  = ad_1%v  - ad_2%v
    ad_ad_subtract%dv = ad_1%dv - ad_2%dv
end function ad_ad_subtract

elemental function ad_real_subtract(ad_in, real_in)
    ! Subtracts a `real` from an `ad`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: ad_real_subtract

    ad_real_subtract%v  = ad_in%v - real_in
    ad_real_subtract%dv = ad_in%dv
end function ad_real_subtract

elemental function real_ad_subtract(real_in, ad_in)
    ! Subtracts a `real` from an `ad`.

    real(kind=WP), intent(in)  :: real_in
    class(ad), intent(in)      :: ad_in
    
    type(ad) :: real_ad_subtract

    real_ad_subtract%v  = real_in - ad_in%v
    real_ad_subtract%dv = -ad_in%dv
end function real_ad_subtract

elemental function ad_unary_minus(ad_in)
    ! Returns `-rd`.

    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_unary_minus

    ad_unary_minus%v  = -ad_in%v
    ad_unary_minus%dv = -ad_in%dv
end function ad_unary_minus

elemental function ad_ad_multiply(ad_1, ad_2)
    ! Multiplies two `ad`s.

    class(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_multiply

    ad_ad_multiply%v  = ad_1%v * ad_2%v
    ad_ad_multiply%dv = ad_1%dv * ad_2%v + ad_1%v * ad_2%dv
end function ad_ad_multiply

elemental function ad_real_multiply(ad_in, real_in)
    ! Multiplies an `ad` by a `real`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: ad_real_multiply

    ad_real_multiply%v  = ad_in%v * real_in
    ad_real_multiply%dv = ad_in%dv * real_in
end function ad_real_multiply

elemental function real_ad_multiply(real_in, ad_in)
    ! Multiplies a `real` by an `ad`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: real_ad_multiply

    real_ad_multiply%v  = real_in * ad_in%v
    real_ad_multiply%dv = real_in * ad_in%dv
end function real_ad_multiply

elemental function ad_ad_divide(ad_1, ad_2)
    ! Divides two `ad`.

    class(ad), intent(in) :: ad_1, ad_2
    
    type(ad) :: ad_ad_divide

    ad_ad_divide%v  = ad_1%v / ad_2%v
    ad_ad_divide%dv = (ad_1%dv * ad_2%v - ad_1%v * ad_2%dv) / (ad_2%v**2)
end function ad_ad_divide

elemental function ad_real_divide(ad_in, real_in)
    ! Divides an `ad` by a `real`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: ad_real_divide

    ad_real_divide%v  = ad_in%v / real_in
    ad_real_divide%dv = ad_in%dv / real_in
end function ad_real_divide

elemental function real_ad_divide(real_in, ad_in)
    ! Divides a `real` by an `ad`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: real_ad_divide

    real_ad_divide%v  = real_in / ad_in%v
    real_ad_divide%dv = -real_in * ad_in%dv / (ad_in%v**2)
end function real_ad_divide

elemental function ad_real_exponentiate(ad_in, real_in)
    ! Exponentiates an `ad` by a `real`.

    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: ad_real_exponentiate

    ad_real_exponentiate%v  = ad_in%v**real_in
    ad_real_exponentiate%dv = real_in*(ad_in%v**(real_in - 1.0_WP))*ad_in%dv
end function ad_real_exponentiate

elemental function ad_integer_exponentiate(ad_in, integer_in)
    ! Exponentiates an `ad` by an `integer`.

    class(ad), intent(in) :: ad_in
    integer, intent(in)   :: integer_in
    
    type(ad) :: ad_integer_exponentiate

    ad_integer_exponentiate%v  = ad_in%v**integer_in
    ad_integer_exponentiate%dv = real(integer_in, WP)*(ad_in%v**(integer_in - 1))*ad_in%dv
end function ad_integer_exponentiate

! No `rd**rd` as that's not likely to happen in CFD.

elemental function ad_sqrt(ad_in)
    ! Takes the square root of an `ad`.
    
    use checks, only: assert
    
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_sqrt
    
    call assert(ad_in%v > 0.0_WP, "autodiff (ad_sqrt): argument is zero or negative")
    
    ad_sqrt%v  = sqrt(ad_in%v)
    ad_sqrt%dv = ad_in%dv/(2.0_WP * sqrt(ad_in%v))
end function ad_sqrt

pure function f(x, y)
    ! Test function. It's here because nvfortran has a bug if it's an internal procedure in the tests.
    
    type(ad), intent(in) :: x, y
    
    type(ad) :: f
    
    f = (2.0_WP * x * y - x**2) / y + y
end function f

end module fmad
