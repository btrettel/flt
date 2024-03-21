! checks whether a test namelist file had passing tests
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program passed

use nmllog, only: log_type, NML_RECL
use prec, only: RP
implicit none

character(len=128) :: filename
integer :: nml_unit

integer       :: n_tests, n_failures
real(kind=RP) :: duration ! in seconds

namelist /tests_summary/ n_tests, n_failures, duration

if (command_argument_count() > 0) then
    call get_command_argument(1, filename)
else
    write(unit=*, fmt="(a)") "Usage: passed FILENAME"
    stop 0
end if

open(newunit=nml_unit, file=trim(filename), status="old", action="read", delim="quote", recl=NML_RECL)
read(unit=nml_unit, nml=tests_summary)
close(unit=nml_unit)

! `error stop` does not always have a non-zero exit code. Specifically, crayftn has a zero exit code for `error stop`.
! So I am going to use `stop 1` here. It's not an *internal* error, so I suppose I could fashion a reason saying that `error stop`
! is for internal failures only.

write(unit=*, fmt="(a, i0, a, i0, a, f10.3, a)") "n_tests=", n_tests, " n_failures=", n_failures, " duration=", duration, " s"

if (n_failures > 0) then
    stop 1
else
    stop 0
end if

end program passed
