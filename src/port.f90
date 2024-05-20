! Module for cross-platform portability.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module port

implicit none
private

integer, public, parameter :: PLATFORM_UNKNOWN  = -1
integer, public, parameter :: PLATFORM_UNIXLIKE = 0
integer, public, parameter :: PLATFORM_WINDOWS  = 1

public :: platform

contains

function platform()
    ! Similar to sys.platform in Python, but returns an integer instead of a string.
    ! <https://docs.python.org/3/library/sys.html#sys.platform>
    
    ! Based on the following:
    ! <https://fortran-lang.discourse.group/t/how-to-access-a-subdirectory-in-a-win-unix-compatible-way/6630/4>
    
    ! Also see:
    ! <https://en.wikipedia.org/wiki/Path_(computing)#Representations_of_paths_by_operating_system_and_shell>
    
    use prec, only: CL
    use checks, only: assert
    
    integer :: platform
    
    character(len=CL) :: path
    integer           :: forward_slash_index, backslash_index
    
    call get_environment_variable("PATH", path)
    
    forward_slash_index = index(path, "/") ! Unix-like
    backslash_index     = index(path, achar(92)) ! Windows (nvfortran thinks `\` is trying to escape something, so I need to use `achar`.
    
    if ((forward_slash_index > 0) .and. (backslash_index == 0)) then
        platform = PLATFORM_UNIXLIKE
    else if ((forward_slash_index == 0) .and. (backslash_index > 0)) then
        platform = PLATFORM_WINDOWS
    else
        platform = PLATFORM_UNKNOWN
    end if
    
    call assert(platform >= PLATFORM_UNKNOWN)
    call assert(platform <= PLATFORM_WINDOWS)
end function platform

end module port
