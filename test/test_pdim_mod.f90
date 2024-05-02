! tests for the generator of a pdim_types module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_pdim_mod

use pdim_mod, only: pdim_config_type, pdim_type, write_module, pdim_in_set
! write_type, write_as_operators, write_md_operators, linspace
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(pdim_config_type)  :: config
type(pdim_type)         :: pdim, pdims(2)

call logger%open("pdim_mod.nml")
call tests%start_tests(logger)

config%pdim_chars     = "LMT"
config%pdim_type_defn = "real(kind=WP)"
config%n_pdims        = len(config%pdim_chars)

allocate(config%min_exponents(config%n_pdims))
allocate(config%max_exponents(config%n_pdims))
allocate(config%denominators(config%n_pdims))
config%min_exponents = [-1.0_WP, -1.0_WP, -1.0_WP]
config%max_exponents = [1.0_WP, 1.0_WP, 1.0_WP]
config%denominators  = [1, 1, 1]

! `pdim_within_bounds`

allocate(pdim%e(config%n_pdims))
allocate(pdims(1)%e(config%n_pdims))
allocate(pdims(2)%e(config%n_pdims))

pdims(1)%e(1) = 0.5_WP
pdims(1)%e(2) = 0.0_WP
pdims(1)%e(3) = 0.0_WP

pdims(2)%e(1) = 0.0_WP
pdims(2)%e(2) = -0.25_WP
pdims(2)%e(3) = 0.0_WP

pdim = pdims(1)
call tests%logical_true(pdim_in_set(config, pdim, pdims), "pdim_in_set, .true.")

pdim%e(1) = 1.5_WP
pdim%e(2) = 0.0_WP
pdim%e(3) = 0.0_WP
call tests%logical_false(pdim_in_set(config, pdim, pdims), "pdim_in_set, .false.")

! TODO: `write_type`

call tests%end_tests()
call logger%close()

end program test_pdim_mod
