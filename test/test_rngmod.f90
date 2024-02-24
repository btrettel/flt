! tests for the rngmod module
! Standard: Fortran 2004
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_rngmod

use prec, only: RP
use rngmod, only: set_determ, determ, pseudo
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: test_data
real(kind=RP)   :: output

character(len=*), parameter :: LOG_FILENAME = "rngmod.nml"

call logger%open(LOG_FILENAME)
call test_data%start_tests(logger)

! TODO: add direct tests using `determ` and `select_rng` directly
! TODO: `set_determ`
! TODO: make `pseudo` work with 2D arrays
! TODO: actually test `pseudo`

! `determ`

call set_determ([0.4_RP])
output = select_rng(rng=determ)
call test_data%real_eq(output, 0.4_RP, "set_determ (size=1, #1)")
call test_data%real_eq(output, 0.4_RP, "set_determ (size=1, #2)")

call set_determ([0.1_RP, 0.7_RP])
output = select_rng(rng=determ)
call test_data%real_eq(output, 0.1_RP, "set_determ (size=2, #1)")
output = select_rng(rng=determ)
call test_data%real_eq(output, 0.7_RP, "set_determ (size=2, #2)")
output = select_rng(rng=determ)
call test_data%real_eq(output, 0.1_RP, "set_determ (size=2, #3)")

! `pseudo`

output = select_rng(rng=pseudo)
write(unit=*, fmt=*) output

call test_data%end_tests()
call logger%close()

contains

function select_rng(rng)
    ! As an example of how to write a procedure that takes a RNG as an argument.
    ! Works in gfortran, Oracle, and FTN95.
    
    real(kind=RP) :: select_rng
    
    interface
        subroutine rng(x)
            use prec, only: RP
            real(kind=RP), intent(out) :: x
        end subroutine rng
    end interface
    
    call rng(select_rng)
end function select_rng

end program test_rngmod
