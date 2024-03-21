! tests for the generator of a dimcheck module
! Standard: Fortran 2008
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_dimmod

use dimmod, only: WIDTH, N_DIMS, config_type, rational, type_name, generate_types
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)               :: logger
type(config_type)            :: config
character(len=WIDTH*N_DIMS)  :: type_name_output
type(rational), dimension(3) :: exps
integer                      :: rc_types
type(test_results_type)      :: tests

call logger%open("dimmod.nml")
call tests%start_tests(logger)

config%dims    = "mlt"
config%min_exp = -1
config%max_exp = 1
config%d       = 6

!indices = (/0, 0, 0/)
exps(1)%n = 0
exps(2)%n = 0
exps(3)%n = 0
exps(1)%d = 2
exps(2)%d = 2
exps(3)%d = 2
type_name_output = type_name(config, exps)
write(unit=*, fmt=*) type_name_output
call tests%character_eq(type_name_output, "mp000_lp000_tp000", "type_name, zeros")

!indices = (/-1, -1, -1/)
exps(1)%n = -1
exps(2)%n = -1
exps(3)%n = -1
exps(1)%d = 2
exps(2)%d = 2
exps(3)%d = 2
type_name_output = type_name(config, exps)
write(unit=*, fmt=*) type_name_output

call generate_types(config, rc_types)

write(unit=*, fmt=*) rc_types

call tests%end_tests()
call logger%close()

end program test_dimmod
