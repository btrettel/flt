! Module to make changes for different builds. Release version.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module build

implicit none
private

logical, public, parameter :: DEBUG = .false.
logical, public, parameter :: FUZZ  = .false.

end module build
