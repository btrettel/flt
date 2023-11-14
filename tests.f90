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

use dimmod, only: IP, WIDTH, N_DIMS, config_type, rational, type_name
implicit none

type(config_type)               :: config
character(len=WIDTH*N_DIMS)     :: blah
type(rational), dimension(3_IP) :: exps

!integer(kind=IP)  :: rc_types

config%dims        = "mlt"
config%denominator = 6_IP

!indices = (/0_IP, 0_IP, 0_IP/)
exps(1_IP)%n = 0_IP
exps(2_IP)%n = 0_IP
exps(3_IP)%n = 0_IP
exps(1_IP)%d = config%denominator(1_IP)
exps(2_IP)%d = config%denominator(2_IP)
exps(3_IP)%d = config%denominator(3_IP)
blah = type_name(config, exps)
write(unit=*, fmt=*) blah

!indices = (/-1_IP, -1_IP, -1_IP/)
exps(1_IP)%n = -1_IP
exps(2_IP)%n = -1_IP
exps(3_IP)%n = -1_IP
exps(1_IP)%d = config%denominator(1_IP)
exps(2_IP)%d = config%denominator(2_IP)
exps(3_IP)%d = config%denominator(3_IP)
blah = type_name(config, exps)
write(unit=*, fmt=*) blah

stop

end program tests
