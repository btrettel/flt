! Module for pure random number generators.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module purerng

use prec, only: I10
implicit none
private

! For arrays: One `rng_type` per `harvest`.

! TODO: `random_seed` uses spacing in lecuyer_efficient_1988 to set for arrays.

integer, public, parameter :: RNG_LECUYER       = 1
integer, public, parameter :: RNG_DETERMINISTIC = 2

integer, parameter :: SIZE_LECUYER = 2

! from lecuyer_efficient_1988, table III
integer, parameter :: L = 2
integer(kind=I10), parameter :: LECUYER_M(L) = [2147483563_I10, 2147483399_I10]
integer(kind=I10), parameter :: LECUYER_A(L) = [40014_I10, 40692_I10]
integer(kind=I10), parameter :: LECUYER_Q(L) = [53668_I10, 52774_I10]
integer(kind=I10), parameter :: LECUYER_R(L) = [12211_I10, 3791_I10]

type, public :: rng_type
    integer(kind=I10), allocatable, private :: seed(:)
    integer, private :: rng = RNG_LECUYER
contains
    procedure :: random_number => purerng_random_number
    procedure :: random_seed   => purerng_random_seed
end type rng_type

contains

elemental subroutine purerng_random_number(rng, harvest)
    use prec, only: WP
    
    class(rng_type), intent(in out) :: rng
    real(kind=WP), intent(out)      :: harvest
    
    select case (rng%rng)
        case (RNG_LECUYER)
            call lecuyer(rng, harvest)
        ! case (RNG_DETERMINISTIC)
        case default
            error stop "purerng_random_number: type of random number generator not selected."
    end select
end subroutine purerng_random_number

subroutine purerng_random_seed(rng, size, put, get)
    use prec, only: WP
    
    class(rng_type), intent(in out) :: rng
    
    integer, intent(out), optional                        :: size
    integer(kind=I10), intent(in), optional               :: put(:)
    integer(kind=I10), allocatable, intent(out), optional :: get(:)
    
    integer :: values(8)
    
    if ((.not. present(size)) .and. (.not. present(put)) .and. (.not. present(get))) then
        if (allocated(rng%seed)) then
            deallocate(rng%seed)
        end if
        
        call date_and_time(values=values)
        
        select case (rng%rng)
            case (RNG_LECUYER)
                allocate(rng%seed(SIZE_LECUYER))
                rng%seed(1) = 1_I10 + floor(real(LECUYER_M(1) - 1_I10, WP) * real(values(7), WP) / 60.0_WP, I10)
                rng%seed(2) = 1_I10 + floor(real(LECUYER_M(2) - 1_I10, WP) * real(values(8), WP) / 1000.0_WP, I10)
            ! case (RNG_DETERMINISTIC)
            case default
                error stop "purerng_random_seed(no args): type of random number generator not selected."
        end select
    end if
    
    if (present(size)) then
        select case (rng%rng)
            case (RNG_LECUYER)
                size = SIZE_LECUYER
            ! case (RNG_DETERMINISTIC)
            case default
                error stop "purerng_random_seed(size): type of random number generator not selected."
        end select
    end if
    
    if (present(put)) then
        if (allocated(rng%seed)) then
            deallocate(rng%seed)
        end if
        
        select case (rng%rng)
            case (RNG_LECUYER)
                allocate(rng%seed(SIZE_LECUYER))
            ! case (RNG_DETERMINISTIC)
            case default
                error stop "purerng_random_seed(put): type of random number generator not selected."
        end select
        
        rng%seed = put
    end if
    
    if (present(get)) then
        if (.not. allocated(rng%seed)) then
            error stop "purerng_random_seed(get): rng%seed not set."
        end if
        
        get = rng%seed
    end if
end subroutine purerng_random_seed

elemental subroutine lecuyer(rng, harvest)
    ! Random number generator from lecuyer_efficient_1988, fig. 3.
    
    use prec, only: WP
    use checks, only: assert
    
    type(rng_type), intent(in out) :: rng
    real(kind=WP), intent(out)     :: harvest
    
    integer(kind=I10) :: k(L), z
    
    call assert(allocated(rng%seed))
    call assert(size(rng%seed) == 2)
    
    ! lecuyer_efficient_1988 p. 747R
    call assert(rng%seed(1) >= 1_I10)
    call assert(rng%seed(1) <= LECUYER_M(1) - 1_I10)
    call assert(rng%seed(2) >= 1_I10)
    call assert(rng%seed(2) <= LECUYER_M(2) - 1_I10)
    
    k        = rng%seed / LECUYER_Q
    rng%seed = LECUYER_A * (rng%seed - k * LECUYER_Q) - k * LECUYER_R
    where (rng%seed < 0)
        rng%seed = rng%seed + LECUYER_M
    end where
    
    z = rng%seed(1) - rng%seed(2)
    ! TODO: Consider rewriting as a `min` to improve branch prediction? Though that would require computing both.
    if (z < 1) then
        z = z + LECUYER_M(1) - 1
    end if
    
    harvest = real(z, kind=WP) / real(LECUYER_M(1), kind=WP)
    
    ! lecuyer_efficient_1988 p. 747R:
    ! > Notice that the function will never return 0.0 or 1.0, as long as `REAL` variables have at least 23-bit mantissa (this is
    ! > the case for most 32-bit machines).
    call assert(harvest > 0.0_WP)
    call assert(harvest < 1.0_WP)
    call assert(z > 0_I10)
    call assert(z < LECUYER_M(1))
    
    ! Same `seed` bounds as before.
    call assert(rng%seed(1) >= 1_I10)
    call assert(rng%seed(1) <= LECUYER_M(1) - 1_I10)
    call assert(rng%seed(2) >= 1_I10)
    call assert(rng%seed(2) <= LECUYER_M(2) - 1_I10)
end subroutine lecuyer

end module purerng
