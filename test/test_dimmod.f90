! # $File$
! 
! Summary: tests for the generator of a dimcheck module
! Standard: Fortran 90, ELF90 subset
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_dimmod

use prec, only: I5
use dimmod, only: WIDTH, N_DIMS, config_type, rational, type_name, generate_types
implicit none

type(config_type)               :: config
character(len=WIDTH*N_DIMS)     :: type_name_output
type(rational), dimension(3_I5) :: exps
integer(kind=I5)                :: rc_types

config%dims    = "mlt"
config%min_exp = -1_I5
config%max_exp = 1_I5
config%d       = 6_I5

!indices = (/0_I5, 0_I5, 0_I5/)
exps(1_I5)%n = 0_I5
exps(2_I5)%n = 0_I5
exps(3_I5)%n = 0_I5
exps(1_I5)%d = 2_I5
exps(2_I5)%d = 2_I5
exps(3_I5)%d = 2_I5
type_name_output = type_name(config, exps)
write(unit=*, fmt=*) type_name_output

!indices = (/-1_I5, -1_I5, -1_I5/)
exps(1_I5)%n = -1_I5
exps(2_I5)%n = -1_I5
exps(3_I5)%n = -1_I5
exps(1_I5)%d = 2_I5
exps(2_I5)%d = 2_I5
exps(3_I5)%d = 2_I5
type_name_output = type_name(config, exps)
write(unit=*, fmt=*) type_name_output

call generate_types(config, rc_types)

write(unit=*, fmt=*) rc_types

stop

end program test_dimmod
