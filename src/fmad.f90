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

public :: sqrt, tanh, log, exp, merge, max, min, abs, sin, cos, tan
public :: f

! Both the dependent and independent variables need to be of type `ad`.
type, public :: ad
    real(kind=WP)              :: v     ! function value
    real(kind=WP), allocatable :: dv(:) ! function derivatives value
contains
    procedure :: init
    procedure :: init_const
    procedure, private :: ad_ad_add, ad_real_add, ad_add_unary
    procedure, private, pass(ad_in) :: real_ad_add
    generic, public :: operator(+) => ad_ad_add, ad_real_add, real_ad_add, ad_add_unary
    procedure, private :: ad_ad_subtract, ad_real_subtract, ad_subtract_unary
    procedure, private, pass(ad_in) :: real_ad_subtract
    generic, public :: operator(-) => ad_ad_subtract, ad_real_subtract, real_ad_subtract, ad_subtract_unary
    procedure, private :: ad_ad_multiply, ad_real_multiply
    procedure, private, pass(ad_in) :: real_ad_multiply
    generic, public :: operator(*) => ad_ad_multiply, ad_real_multiply, real_ad_multiply
    procedure, private :: ad_ad_divide, ad_real_divide
    procedure, private, pass(ad_in) :: real_ad_divide
    generic, public :: operator(/) => ad_ad_divide, ad_real_divide, real_ad_divide
    procedure, private :: ad_real_exponentiate, ad_integer_exponentiate
    generic, public :: operator(**) => ad_real_exponentiate, ad_integer_exponentiate
    procedure, private :: lt_ad
    generic, public :: operator(<) => lt_ad
    procedure, private :: le_ad
    generic, public :: operator(<=) => le_ad
    procedure, private :: gt_ad
    generic, public :: operator(>) => gt_ad
    procedure, private :: ge_ad
    generic, public :: operator(>=) => ge_ad
end type ad

interface sqrt
    module procedure :: ad_sqrt
end interface sqrt

interface tanh
    module procedure :: ad_tanh
end interface tanh

interface log
    module procedure :: ad_log
end interface log

interface exp
    module procedure :: ad_exp
end interface exp

interface merge
    module procedure :: ad_ad_merge
end interface merge

interface max
    module procedure :: ad_ad_max_2
    module procedure :: real_ad_max_2
end interface max

interface min
    module procedure :: ad_ad_min_2
    module procedure :: real_ad_min_2
end interface min

interface abs
    module procedure :: ad_abs
end interface abs

interface sin
    module procedure :: ad_sin
end interface sin

interface cos
    module procedure :: ad_cos
end interface cos

interface tan
    module procedure :: ad_tan
end interface tan

contains

! Constructors for `ad`
! ---------------------

elemental subroutine init(x, v, n, n_dv)
    use checks, only: assert, is_close
    use prec, only: WP
    
    class(ad), intent(in out) :: x    ! `class` can't be `intent(out)` and `pure`?!?
    real(kind=WP), intent(in) :: v    ! value of variable to set
    integer, intent(in)       :: n, & ! variable number represented (sets the appropriate derivative)
                                 n_dv ! total number of differentiable variables

    integer :: i_dv ! loop index
    
    call assert(n_dv >= 1, "fmad (init): n_dv must be one or more")
    call assert(n >= 1, "fmad (init): n must be 1 or more")
    call assert(n <= n_dv, "fmad (init): n must be n_dv or less")
    
    x%v = v
    
    allocate(x%dv(n_dv))
    do concurrent (i_dv = 1:n_dv)
        x%dv(i_dv) = merge(1.0_WP, 0.0_WP, i_dv == n)
    end do
    call assert(any(is_close(x%dv, 1.0_WP)), "fmad (init): at least one derivative set")
    call assert(any(is_close(x%dv, 0.0_WP)), "fmad (init): at least one derivative not set")
end subroutine init

elemental subroutine init_const(x, v, n_dv)
    use checks, only: assert
    use prec, only: WP
    
    class(ad), intent(in out) :: x ! `class` can't be `intent(out)` and `pure`?!?
    real(kind=WP), intent(in) :: v    ! value of constant to set
    integer, intent(in)       :: n_dv ! total number of differentiable variables
    
    call assert(n_dv >= 1, "fmad (init): n_dv must be one or more")
    
    allocate(x%dv(n_dv))

    x%v  = v
    x%dv = 0.0_WP
end subroutine init_const

! Operator procedures
! -------------------

elemental function ad_ad_add(ad_left, ad_right)
    ! Adds two `ad`s.
    
    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_add

    ad_ad_add%v  = ad_left%v  + ad_right%v
    ad_ad_add%dv = ad_left%dv + ad_right%dv
end function ad_ad_add

elemental function ad_real_add(ad_in, real_in)
    ! Adds an `ad` and a `real`.
    
    use prec, only: WP
    
    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_add

    ad_real_add%v  = ad_in%v + real_in
    ad_real_add%dv = ad_in%dv
end function ad_real_add

elemental function real_ad_add(real_in, ad_in)
    ! Adds a `real` and an `ad`.
    
    use prec, only: WP
    
    real(kind=WP), intent(in) :: real_in
    class(ad), intent(in)     :: ad_in
    
    type(ad) :: real_ad_add

    real_ad_add%v  = real_in + ad_in%v
    real_ad_add%dv = ad_in%dv
end function real_ad_add

elemental function ad_ad_subtract(ad_left, ad_right)
    ! Subtracts two `ad`s.

    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_subtract

    ad_ad_subtract%v  = ad_left%v  - ad_right%v
    ad_ad_subtract%dv = ad_left%dv - ad_right%dv
end function ad_ad_subtract

elemental function ad_real_subtract(ad_in, real_in)
    ! Subtracts a `real` from an `ad`.
    
    use prec, only: WP
    
    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_subtract

    ad_real_subtract%v  = ad_in%v - real_in
    ad_real_subtract%dv = ad_in%dv
end function ad_real_subtract

elemental function real_ad_subtract(real_in, ad_in)
    ! Subtracts a `real` from an `ad`.

    use prec, only: WP
    
    real(kind=WP), intent(in) :: real_in
    class(ad), intent(in)     :: ad_in
    
    type(ad) :: real_ad_subtract

    real_ad_subtract%v  = real_in - ad_in%v
    real_ad_subtract%dv = -ad_in%dv
end function real_ad_subtract

elemental function ad_subtract_unary(ad_in)
    ! Returns `-rd`.

    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_subtract_unary

    ad_subtract_unary%v  = -ad_in%v
    ad_subtract_unary%dv = -ad_in%dv
end function ad_subtract_unary

elemental function ad_add_unary(ad_in)
    ! Returns `+rd`.

    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_add_unary

    ad_add_unary%v  = ad_in%v
    ad_add_unary%dv = ad_in%dv
end function ad_add_unary

elemental function ad_ad_multiply(ad_left, ad_right)
    ! Multiplies two `ad`s.

    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_multiply

    ad_ad_multiply%v  = ad_left%v * ad_right%v
    ad_ad_multiply%dv = ad_left%dv * ad_right%v + ad_left%v * ad_right%dv
end function ad_ad_multiply

elemental function ad_real_multiply(ad_in, real_in)
    ! Multiplies an `ad` by a `real`.
    
    use prec, only: WP
    
    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_multiply

    ad_real_multiply%v  = ad_in%v * real_in
    ad_real_multiply%dv = ad_in%dv * real_in
end function ad_real_multiply

elemental function real_ad_multiply(real_in, ad_in)
    ! Multiplies a `real` by an `ad`.
    
    use prec, only: WP

    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: real_ad_multiply

    real_ad_multiply%v  = real_in * ad_in%v
    real_ad_multiply%dv = real_in * ad_in%dv
end function real_ad_multiply

elemental function ad_ad_divide(ad_left, ad_right)
    ! Divides two `ad`.

    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_divide

    ad_ad_divide%v  = ad_left%v / ad_right%v
    ad_ad_divide%dv = (ad_left%dv * ad_right%v - ad_left%v * ad_right%dv) / (ad_right%v**2)
end function ad_ad_divide

elemental function ad_real_divide(ad_in, real_in)
    ! Divides an `ad` by a `real`.
    
    use prec, only: WP

    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: ad_real_divide

    ad_real_divide%v  = ad_in%v / real_in
    ad_real_divide%dv = ad_in%dv / real_in
end function ad_real_divide

elemental function real_ad_divide(real_in, ad_in)
    ! Divides a `real` by an `ad`.
    
    use prec, only: WP

    class(ad), intent(in)     :: ad_in
    real(kind=WP), intent(in) :: real_in
    
    type(ad) :: real_ad_divide

    real_ad_divide%v  = real_in / ad_in%v
    real_ad_divide%dv = -real_in * ad_in%dv / (ad_in%v**2)
end function real_ad_divide

elemental function ad_real_exponentiate(ad_in, real_in)
    ! Exponentiates an `ad` by a `real`.
    
    use checks, only: assert
    use prec, only: WP
    
    class(ad), intent(in)      :: ad_in
    real(kind=WP), intent(in)  :: real_in
    
    type(ad) :: ad_real_exponentiate
    
    call assert((abs(ad_in%v) > 0.0_WP) .and. (real_in >= 0), &
                    "fmad (ad_real_exponentiate): exponent is negative and argument is zero")

    ad_real_exponentiate%v  = ad_in%v**real_in
    ad_real_exponentiate%dv = real_in*(ad_in%v**(real_in - 1.0_WP))*ad_in%dv
end function ad_real_exponentiate

elemental function ad_integer_exponentiate(ad_in, integer_in)
    ! Exponentiates an `ad` by an `integer`.
    
    use checks, only: assert
    use prec, only: WP
    
    class(ad), intent(in) :: ad_in
    integer, intent(in)   :: integer_in
    
    type(ad) :: ad_integer_exponentiate
    
    call assert((abs(ad_in%v) > 0.0_WP) .and. (integer_in >= 0), &
                    "fmad (ad_integer_exponentiate): exponent is negative and argument is zero")

    ad_integer_exponentiate%v  = ad_in%v**integer_in
    ad_integer_exponentiate%dv = real(integer_in, WP)*(ad_in%v**(integer_in - 1))*ad_in%dv
end function ad_integer_exponentiate

! No `rd**rd` as that's not likely to happen in CFD.

elemental function lt_ad(ad_left, ad_right)
    use checks, only: assert
    
    class(ad), intent(in) :: ad_left
    type(ad), intent(in)  :: ad_right
    
    logical :: lt_ad
    
    lt_ad = ad_left%v < ad_right%v
    
    call assert(allocated(ad_left%dv), "fmad (lt_ad): ad_left%dv must be allocated")
    call assert(allocated(ad_right%dv), "fmad (lt_ad): ad_right%dv must be allocated")
end function lt_ad

elemental function le_ad(ad_left, ad_right)
    use checks, only: assert
    
    class(ad), intent(in) :: ad_left
    type(ad), intent(in)  :: ad_right
    
    logical :: le_ad
    
    le_ad = ad_left%v <= ad_right%v
    
    call assert(allocated(ad_left%dv), "fmad (le_ad): ad_left%dv must be allocated")
    call assert(allocated(ad_right%dv), "fmad (le_ad): ad_right%dv must be allocated")
end function le_ad

elemental function gt_ad(ad_left, ad_right)
    use checks, only: assert
    
    class(ad), intent(in) :: ad_left
    type(ad), intent(in)  :: ad_right
    
    logical :: gt_ad
    
    gt_ad = ad_left%v > ad_right%v
    
    call assert(allocated(ad_left%dv), "fmad (gt_ad): ad_left%dv must be allocated")
    call assert(allocated(ad_right%dv), "fmad (gt_ad): ad_right%dv must be allocated")
end function gt_ad

elemental function ge_ad(ad_left, ad_right)
    use checks, only: assert
    
    class(ad), intent(in) :: ad_left
    type(ad), intent(in)  :: ad_right
    
    logical :: ge_ad
    
    ge_ad = ad_left%v >= ad_right%v
    
    call assert(allocated(ad_left%dv), "fmad (ge_ad): ad_left%dv must be allocated")
    call assert(allocated(ad_right%dv), "fmad (ge_ad): ad_right%dv must be allocated")
end function ge_ad

elemental function ad_sqrt(ad_in)
    ! Takes the square root of an `ad`.
    
    use checks, only: assert
    use prec, only: WP
    
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_sqrt
    
    call assert(ad_in%v > 0.0_WP, "fmad (ad_sqrt): argument is zero or negative")
    
    ad_sqrt%v  = sqrt(ad_in%v)
    ad_sqrt%dv = ad_in%dv/(2.0_WP * sqrt(ad_in%v))
end function ad_sqrt

elemental function ad_tanh(ad_in)
    use prec, only: WP
    
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_tanh
    
    ad_tanh%v  = tanh(ad_in%v)
    ad_tanh%dv = ad_in%dv*(1.0_WP - tanh(ad_in%v)**2)
end function ad_tanh

elemental function ad_log(ad_in)
    use checks, only: assert
    use prec, only: WP
    
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_log
    
    call assert(ad_in%v > 0.0_WP, "fmad (ad_log): argument is zero or negative")
    
    ad_log%v  = log(ad_in%v)
    ad_log%dv = ad_in%dv/ad_in%v
end function ad_log

elemental function ad_exp(ad_in)
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_exp
    
    ad_exp%v  = exp(ad_in%v)
    ad_exp%dv = ad_in%dv*exp(ad_in%v)
end function ad_exp

pure function ad_ad_merge(ad_left, ad_right, mask)
    class(ad), intent(in) :: ad_left, ad_right
    logical, intent(in)   :: mask
    
    type(ad) :: ad_ad_merge
    
    if (mask) then
        ad_ad_merge = ad_left
    else
        ad_ad_merge = ad_right
    end if
end function ad_ad_merge

pure function ad_ad_max_2(ad_left, ad_right)
    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_max_2
    
    ad_ad_max_2 = ad_ad_merge(ad_left, ad_right, ad_left%v >= ad_right%v)
end function ad_ad_max_2

pure function real_ad_max_2(real_left, ad_right)
    ! Related: <https://en.wikipedia.org/wiki/Rectifier_(neural_networks)>
    
    use prec, only: WP
    
    real(kind=WP), intent(in) :: real_left
    class(ad), intent(in)     :: ad_right
    
    type(ad) :: real_ad_max_2
    
    type(ad) :: ad_left
    
    call ad_left%init_const(real_left, size(ad_right%dv))
    real_ad_max_2 = ad_ad_max_2(ad_left, ad_right)
end function real_ad_max_2

pure function ad_ad_min_2(ad_left, ad_right)
    class(ad), intent(in) :: ad_left, ad_right
    
    type(ad) :: ad_ad_min_2
    
    ad_ad_min_2 = ad_ad_merge(ad_left, ad_right, ad_left%v <= ad_right%v)
end function ad_ad_min_2

pure function real_ad_min_2(real_left, ad_right)
    use prec, only: WP
    
    real(kind=WP), intent(in) :: real_left
    class(ad), intent(in)     :: ad_right
    
    type(ad) :: real_ad_min_2
    
    type(ad) :: ad_left
    
    call ad_left%init_const(real_left, size(ad_right%dv))
    real_ad_min_2 = ad_ad_min_2(ad_left, ad_right)
end function real_ad_min_2

elemental function ad_abs(ad_in)
    use checks, only: assert, is_close
    use prec, only: WP
    
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_abs
    
    if (.not. is_close(ad_in%v, 0.0_WP)) then
        ad_abs%v  = abs(ad_in%v)
        ad_abs%dv = ad_in%dv*(ad_in%v/abs(ad_in%v))
    else
        call ad_abs%init_const(0.0_WP, size(ad_in%dv))
    end if
    
    call assert(ad_abs%v >= 0.0_WP, "fmad (ad_abs): value is negative")
end function ad_abs

elemental function ad_sin(ad_in)
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_sin
    
    ad_sin%v  = sin(ad_in%v)
    ad_sin%dv = ad_in%dv*cos(ad_in%v)
end function ad_sin

elemental function ad_cos(ad_in)
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_cos
    
    ad_cos%v  = cos(ad_in%v)
    ad_cos%dv = -ad_in%dv*sin(ad_in%v)
end function ad_cos

elemental function ad_tan(ad_in)
    class(ad), intent(in) :: ad_in
    
    type(ad) :: ad_tan
    
    ad_tan%v  = tan(ad_in%v)
    ad_tan%dv = ad_in%dv/(cos(ad_in%v)**2)
end function ad_tan

pure function f(x, y)
    use checks, only: assert
    use prec, only: WP
    
    ! Test function. It's here because nvfortran has a bug if it's an internal procedure in the tests.
    
    type(ad), intent(in) :: x, y
    
    type(ad) :: f
    
    call assert(y%v > 0.0_WP, "fmad (f): y is zero")
    
    f = (2.0_WP * x * y - x**2) / y + y
end function f

end module fmad
