module udio_mod

implicit none

type meter
    real :: v
contains
    procedure :: meter_wf
    generic   :: write(formatted) => meter_wf
end type meter

contains

subroutine meter_wf(dtv, unit, iotype, vlist, iostat, iomsg)
    class(meter), intent(in)         :: dtv
    integer, intent(in)              :: unit
    character(len=*), intent(in)     :: iotype
    integer, intent(in)              :: vlist(:)
    integer, intent(out)             :: iostat
    character(len=*), intent(in out) :: iomsg
    
    character(len=16) :: pfmt
    
    if (iotype == "LISTDIRECTED" .or. iotype == "DTg0") then
        pfmt = "(g0, a)"
    else
        write(pfmt, "(2a, i0, a, i0, a)") "(", iotype(3:), vlist(1), ".", vlist(2), ", a)"
    end if
    write(unit, fmt=trim(pfmt), iostat=iostat) dtv%v, " m"
end subroutine meter_wf

end module udio_mod

program udio

use udio_mod
implicit none

type(meter) :: x

x%v = 10.123

! TODO: try `iotype`

! TODO: Check latest gfortran to see if it can handle a zero width case.
! 0 trident$ gfortran udio.f90 
! udio.f90:56:22:
! 
!    56 | write(*, fmt="(dt'g'(0,3))") x
!       |                      1
! Error: Positive width required in format string at (1)

write(*, fmt="(dt'f'(6,3))") x
write(*, fmt="(dt'en'(10,3))") x
write(*, fmt="(dt'g0')") x ! Workaround for gfortran not accepting widths of 0.
write(*, fmt=*) x

end program udio
