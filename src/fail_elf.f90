! # $File$
! 
! Summary: procedure to terminate the program with an error
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module fail

! Set modules and other boilerplate
! ---------------------------------

implicit none
private

! Declare public and private procedures
! -------------------------------------

public :: failure

contains

! Procedures
! ----------

subroutine failure()
    stop
end subroutine failure

end module fail
