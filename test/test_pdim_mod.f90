! tests for the generator of a pdim_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_pdim_mod

use pdim_mod, only: pdim_config_type, pdim_type, write_module, pdim_within_bounds
! write_type, write_as_operators, write_md_operators, linspace
use pdim_types, only: length   => pdim_p10000_p00000_p00000, &
                      time     => pdim_p00000_p00000_p10000, &
                      velocity => pdim_p10000_p00000_m10000
use prec, only: SP, WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(pdim_config_type)  :: config
type(pdim_type)         :: pdim

type(length)   :: x, y, z
type(time)     :: t
type(velocity) :: v
integer        :: rc_fail, failure_unit

character(len=*), parameter :: FAILURE_OUTPUT = "test_pdim_types_fail.txt"

call logger%open("pdim_mod.nml")
call tests%start_tests(logger)

config%pdim_chars     = "LMT"
config%pdim_type_defn = "real(kind=WP)"
config%n_pdims        = len(config%pdim_chars)

allocate(config%min_exponents(config%n_pdims))
allocate(config%max_exponents(config%n_pdims))
allocate(config%exponent_deltas(config%n_pdims))
config%min_exponents   = [-1.0_SP, -1.0_SP, -1.0_SP]
config%max_exponents   = [1.0_SP, 1.0_SP, 1.0_SP]
config%exponent_deltas = [1.0_SP, 1.0_SP, 1.0_SP]

! `pdim_within_bounds`

allocate(pdim%e(config%n_pdims))

pdim%e(1) = 0.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_true(pdim_within_bounds(config, pdim), "pdim_within_bounds, .true.")

pdim%e(1) = 1.0_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_true(pdim_within_bounds(config, pdim), "pdim_within_bounds, upper boundary, .true.")

pdim%e(1) = -1.0_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_true(pdim_within_bounds(config, pdim), "pdim_within_bounds, lower boundary, .true.")

pdim%e(1) = 1.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_false(pdim_within_bounds(config, pdim), "pdim_within_bounds, .false., above")

pdim%e(1) = -1.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_false(pdim_within_bounds(config, pdim), "pdim_within_bounds, .false., below")

! TODO: `write_type`

! Generated pdim_types.f90.

x%v = 1.0_WP
y%v = -1.0_WP
t%v = 1.0_WP

v = x / t
z = x + y

call tests%real_eq(v%v, 1.0_WP, "pdim_types, 1")
call tests%real_eq(z%v, 0.0_WP, "pdim_types, 2")

! TODO: This will need to be updated for Windows.
call execute_command_line("make test_pdim_types_fail 2> " // FAILURE_OUTPUT, exitstat=rc_fail)
call tests%integer_ne(rc_fail, 0, "check if there is a compile-time error if the physical dimensions don't match")

! Delete output file if test succeeded, otherwise keep it for examination.
if (rc_fail == 0) then
    open(newunit=failure_unit, file=FAILURE_OUTPUT, status="old", action="read")
    close(unit=failure_unit, status="delete")
end if

call tests%end_tests()
call logger%close()

end program test_pdim_mod
