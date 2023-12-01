! # $File$
! 
! Summary: Module for derivative-free optimization of `real`s with a genetic algorithm.
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module ga

use prec, only: I5, RP
implicit none
private

integer, public, parameter :: MAX_N_POP   = 64_I5 ! maximum population size
integer, public, parameter :: MAX_N_GENES = 16_I5 ! maximum number of genes
integer, public, parameter :: N_FITNESS   = 8_I5  ! number of fitness/objective function values

type, public :: ga_config
    real(kind=RP)    :: p_select, p_elite, p_cross, p_mutate
    integer(kind=I5) :: n_gener, n_stall, stop_time
end type ga_config

type, public :: bounds_type
    real(kind=RP) :: lower, upper ! lower and upper bounds
end type bounds_type

type, public :: chromo_type
    real(kind=RP)    :: genes(MAX_N_GENES)
    integer(kind=I5) :: n_genes
    logical          :: set                ! whether the fitness/objective function has been set
    real(kind=RP)    :: fitness(N_FITNESS) ! fitness/objective function values
    !integer(kind=I5) :: id                 ! unique identification number
end type chromo_type

type, public :: pop_type
    type(chromo_type) :: chromo(MAX_N_POP)
    type(bounds_type) :: bounds(MAX_N_GENES)
    integer(kind=I5)  :: n_pop, n_genes
    type(chromo_type) :: best_pop_chromo, best_ever_chromo
    !integer(kind=I5) :: max_id ! maximum identification number
end type pop_type

public :: rand_int, rand_cauchy
public :: clip

contains

function rand_int(lb, ub, r)
    integer(kind=I5), intent(in) :: lb, ub
    real(kind=RP), intent(in)    :: r
    
    integer(kind=I5) :: rand_int
    
    ! The `min` function makes this not return `ub + 1_I5` when `r = 1.0_RP`.
    rand_int = min(lb + floor(real(ub + 1_I5 - lb, RP) * r), ub)
    
    return
end function rand_int

function rand_cauchy(m, b, r)
    ! Notation follows <https://mathworld.wolfram.com/CauchyDistribution.html>.
    ! TODO: Describe what `m` and `b` represent in comments.
    
    use prec, only: PI
    
    real(kind=RP), intent(in) :: m ! TODO median?
    real(kind=RP), intent(in) :: b ! TODO scale factor similar to standard deviation?
    real(kind=RP), intent(in) :: r ! random CDF value for inverse sampling
    
    real(kind=RP) :: rand_cauchy
    
    rand_cauchy = m + b * tan(PI * (r - 0.5_RP))
    
    return
end function rand_cauchy

subroutine clip(bounds, x)
    ! Clip variable `x` within upper and lower bounds.
    
    type(bounds_type), intent(in) :: bounds
    real(kind=RP), intent(in out) :: x
    
    x = min(x, bounds%upper)
    x = max(x, bounds%lower)
    
    return
end subroutine clip

!subroutine optimize(bounds, fun, best_ever_chromo, rc)
!    type(bounds_type), intent(in)  :: bounds(:)
!    type(chromo_type), intent(out) :: best_ever_chromo
!    integer(kind=I5), intent(out)  :: rc
!    
!    ! TODO: `fun`
!end subroutine optimize

end module ga
