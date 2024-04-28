! tests for the generator of a pdim_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_pdim_mod

!use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
use pdim_mod, only: pdim_config_type, pdim_type, write_module, pdim_within_bounds
! write_type, write_as_operators, write_md_operators, linspace
use prec, only: SP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(pdim_config_type)  :: config
type(pdim_type)         :: pdim
integer                 :: out_unit

call logger%open("pdim_mod.nml")
call tests%start_tests(logger)

!! length
!pdims(1)%e(1) = 1.0_SP
!pdims(1)%e(2) = 0.0_SP
!pdims(1)%e(3) = 0.0_SP

!! time
!pdims(2)%e(1) = 0.0_SP
!pdims(2)%e(2) = 0.0_SP
!pdims(2)%e(3) = 1.0_SP

!! velocity
!pdim_out%e(1) = 0.0_SP
!pdim_out%e(2) = 1.0_SP
!pdim_out%e(3) = -1.0_SP

!call write_type(OUTPUT_UNIT, 1, pdims)
!call write_as_operators(OUTPUT_UNIT, pdims(1))
!call write_md_operators(OUTPUT_UNIT, pdims(1), pdims(2))
!!call write_binary_operator(OUTPUT_UNIT, pdims(1), pdims(2), pdim_out, "/")
!!print *, linspace(-1.0_SP, 1.0_SP, 5)

config%pdim_chars     = "LMT"
config%pdim_type_defn = "real(kind=WP)"
!config%pdim_type_defn = "type(ad)"
config%n_pdims        = len(config%pdim_chars)
config%pdim_label_len = 1 + config%n_pdims * 9
config%pdim_human_len = 126

allocate(config%min_exponents(config%n_pdims))
allocate(config%max_exponents(config%n_pdims))
allocate(config%exponent_deltas(config%n_pdims))
config%min_exponents   = [-1.0_SP, -1.0_SP, -1.0_SP]
config%max_exponents   = [1.0_SP, 1.0_SP, 1.0_SP]
config%exponent_deltas = [1.0_SP, 1.0_SP, 1.0_SP]

!call write_module(config, OUTPUT_UNIT)

open(newunit=out_unit, &
        action="write", &
        status="replace", &
        position="rewind", &
        file="pdim_types.f90")
call write_module(config, out_unit)
close(unit=out_unit)

allocate(pdim%e(config%n_pdims))

pdim%e(1) = 0.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_true(pdim_within_bounds(config, pdim), "pdim_within_bounds, .true.")

pdim%e(1) = 1.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_false(pdim_within_bounds(config, pdim), "pdim_within_bounds, .false., above")

pdim%e(1) = -1.5_SP
pdim%e(2) = 0.0_SP
pdim%e(3) = 0.0_SP
call tests%logical_false(pdim_within_bounds(config, pdim), "pdim_within_bounds, .false., below")

call tests%end_tests()
call logger%close()

end program test_pdim_mod
