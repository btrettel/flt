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

use dimmod, only: IP, config_type, type_name
implicit none

type(config_type)                 :: config
character(len=12_IP)              :: blah
integer(kind=IP), dimension(3_IP) :: exps

!integer(kind=IP)  :: rc_types

config%dims = "mlt"
exps        = (/0_IP, 0_IP, 0_IP/)

blah = type_name(config, exps)

write(unit=*, fmt=*) blah

stop

end program tests
