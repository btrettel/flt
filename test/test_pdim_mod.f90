! tests for the generator of a pdim_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_pdim_mod

use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
use pdim_mod, only: pdim_type, write_type, write_operators, write_binary_operator
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(pdim_type)         :: pdims(2), pdim_out

call logger%open("pdim_mod.nml")
call tests%start_tests(logger)

! length
pdims(1)%e(1) = 1.0
pdims(1)%e(2) = 0.0
pdims(1)%e(3) = 0.0

! time
pdims(2)%e(1) = 0.0
pdims(2)%e(2) = 0.0
pdims(2)%e(3) = 1.0

! velocity
pdim_out%e(1) = 0.0
pdim_out%e(2) = 1.0
pdim_out%e(3) = -1.0

call write_type(OUTPUT_UNIT, 1, pdims)
call write_operators(OUTPUT_UNIT, pdims(1), pdims(2))
call write_binary_operator(OUTPUT_UNIT, pdims(1), pdims(2), pdim_out, "/")

! TODO: eliminate this test
call tests%integer_eq(1, 1, "blah")

call tests%end_tests()
call logger%close()

end program test_pdim_mod
