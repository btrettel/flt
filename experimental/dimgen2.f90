program dimgen

implicit none

integer, parameter :: WP = selected_real_kind(15, 307)

integer :: n, i

integer, parameter :: min_exponent = -1, &
                      max_exponent = 1, &
                      denominator  = 6

real(kind=WP), allocatable :: exponents(:)

n = (max_exponent - min_exponent) * denominator + 1
exponents = linspace(real(min_exponent, WP), real(max_exponent, WP), n)

do i = 1, n
    print *, numerator(exponents(i), denominator), denominator
end do

contains

pure function linspace(lower, upper, n)
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
    
    ! TODO: assert that `lower < upper`
    
    allocate(linspace(n_))
    delta = (upper - lower) / real(n_ - 1, WP)
    do i = 1, n_
        linspace(i) = lower + delta * real(i - 1, WP)
    end do
end function linspace

pure function numerator(x, denominator)
    real(kind=WP), intent(in) :: x
    integer, intent(in)       :: denominator
    
    integer :: numerator
    
    numerator = nint(x * real(denominator, WP))
end function numerator

end program dimgen
