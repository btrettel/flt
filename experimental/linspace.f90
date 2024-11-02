pure function linspace(lower, upper, n)
    use checks, only: assert, is_close
    
    real(WP), intent(in)          :: lower, upper
    integer, intent(in), optional :: n
    
    real(WP), allocatable :: linspace(:)
    
    integer  :: i, n_
    real(WP) :: delta
    
    if (present(n)) then
        n_ = n
    else
        n_ = 50
    end if
    
    allocate(linspace(n_))
    
    if (n_ == 1) then
        call assert(is_close(lower, upper))
        
        linspace(1) = lower
    else
        call assert(lower < upper)
        
        delta = (upper - lower) / real(n_ - 1, WP)
        do i = 1, n_
            linspace(i) = lower + delta * real(i - 1, WP)
        end do
    end if
end function linspace
