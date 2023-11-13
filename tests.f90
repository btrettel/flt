! # $File$
! 
! Summary: generator of a dimcheck module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [dimcheck](https://github.com/btrettel/dimcheck)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program tests

use dimmod, only: IP, WIDTH, N_DIMS, config_type, type_name
implicit none

type(config_type)                 :: config
character(len=WIDTH*N_DIMS)       :: blah
integer(kind=IP), dimension(3_IP) :: indices

!integer(kind=IP)  :: rc_types

config%dims        = "mlt"
config%denominator = 6_IP

indices = (/0_IP, 0_IP, 0_IP/)
blah = type_name(config, indices)
write(unit=*, fmt=*) blah

indices = (/-1_IP, -1_IP, -1_IP/)
blah = type_name(config, indices)
write(unit=*, fmt=*) blah

stop

end program tests
