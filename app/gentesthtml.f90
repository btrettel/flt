! Program to generate a HTML page listing test results.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program gentesthtml

use, intrinsic :: iso_fortran_env, only: IOSTAT_END, ERROR_UNIT
use prec, only: CL, WP
use stopcodes, only: EX_OK
implicit none

integer           :: i, total_tests, total_failures, out_unit, rc
character(len=CL) :: output_filename, input_filename, error_message

call get_command_argument(1, value=output_filename)
if ((len(trim(output_filename)) == 0) .or. &
        (trim(output_filename) == "--help") .or. &
        (trim(output_filename) == "-help") .or. &
        (trim(output_filename) == "-h")) then
    write(unit=*, fmt="(a)") "Usage: gentesthtml OUTPUT_FILENAME INPUT_FILENAME(S)"
    stop EX_OK, quiet=.true.
end if

open(newunit=out_unit, action="write", status="replace", position="rewind", file=trim(output_filename), &
        iostat=rc, iomsg=error_message)
if (rc /= 0) then
    write(unit=ERROR_UNIT, fmt="(a)") trim(error_message)
    stop 1
end if

write(unit=out_unit, fmt="(a)") "<!doctype html>"
write(unit=out_unit, fmt="(a)") '<html lang="">'
write(unit=out_unit, fmt="(a)") "<head>"
write(unit=out_unit, fmt="(a)") '<meta charset="utf-8">'
write(unit=out_unit, fmt="(a)") '<meta name="viewport" content="width=device-width, initial-scale=1">'
write(unit=out_unit, fmt="(a)") "<title>Test results</title>"
write(unit=out_unit, fmt="(a)") "</head>"
write(unit=out_unit, fmt="(a)") "<body>"

i = 0
total_tests    = 0
total_failures = 0
do ! SERIAL
    i = i + 1
    call get_command_argument(i + 1, value=input_filename)
    if (len(trim(input_filename)) == 0) then
        exit
    end if
    write(unit=*, fmt="(3a)") "Reading ", trim(input_filename), " (first pass)..."
    
    call get_totals(trim(input_filename), total_tests, total_failures)
    if (rc /= 0) stop 1
end do
print "(a, i0)", "Total tests: ", total_tests
print "(a, i0)", "Total failures: ", total_failures

write(unit=out_unit, fmt="(a, i0, a, i0, a, i0, a)") "<p>Totals: ", total_tests, " tests run, ", total_tests - total_failures, &
                                                        " tests passing, ", total_failures, " tests failing.</p>"

i = 0
do ! SERIAL
    i = i + 1
    call get_command_argument(i + 1, value=input_filename)
    if (len(trim(input_filename)) == 0) then
        exit
    end if
    write(unit=*, fmt="(3a)") "Reading ", trim(input_filename), " (second pass)..."
    
    call write_test_html(trim(input_filename), out_unit, rc)
    if (rc /= 0) stop 1
end do

write(unit=out_unit, fmt="(a)") "</body>"
write(unit=out_unit, fmt="(a)") "</html>"

close(unit=out_unit)

contains

subroutine get_totals(filename, total_tests, total_failures)
    character(len=*), intent(in) :: filename
    integer, intent(in out)      :: total_tests, total_failures
    
    integer :: nml_unit
    
    integer  :: n_tests, n_failures
    real(WP) :: duration ! in seconds
    
    namelist /tests_summary/ n_tests, n_failures, duration
    
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    read(unit=nml_unit, nml=tests_summary)
    
    total_tests    = total_tests    + n_tests
    total_failures = total_failures + n_failures
    
    close(unit=nml_unit)
end subroutine get_totals

subroutine write_test_html(filename, out_unit, rc)
    use timer, only: TIMESTAMP_LEN
    use checks, only: assert
    
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: out_unit
    integer, intent(out)         :: rc
    
    integer           :: nml_unit, rc_nml, dot_index
    character(len=CL) :: basename, nml_error_message
    character(len=5)  :: file_ext
    logical           :: out_unit_open
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=9)             :: variable_type
    character(len=2)             :: test_operator
    logical                      :: test_passes, returned_logical, expected_logical
    character(len=CL)            :: message, returned_character, expected_character
    real(WP)                     :: returned_real, expected_real, tolerance, difference
    integer                      :: returned_integer, expected_integer
    
    namelist /test_result/ timestamp, variable_type, test_operator, test_passes, message, &
                            returned_logical, expected_logical, &
                            returned_real, expected_real, tolerance, difference, &
                            returned_integer, expected_integer, &
                            returned_character, expected_character
    
    integer  :: n_tests, n_failures
    real(WP) :: duration ! in seconds
    
    namelist /tests_summary/ n_tests, n_failures, duration
    
    inquire(unit=out_unit, opened=out_unit_open)
    call assert(out_unit_open, "gentesthtml (write_test_html): out_unit must be open")
    
    rc = 0
    
    dot_index = index(filename, ".")
    file_ext  = filename(dot_index:)
    basename  = filename(1:dot_index-1)
    
    if (file_ext /= ".nml") then
        rc = 1
        return 
    end if
    
    write(unit=out_unit, fmt="(a)") "<hr/>"
    write(unit=out_unit, fmt="(3a)") "<p>Test set: ", basename, "</p>"
    write(unit=out_unit, fmt="(a)") "<table border=1>"
    write(unit=out_unit, fmt="(a)") "<tr>"
    write(unit=out_unit, fmt="(a)") "<td>status</td>"
    write(unit=out_unit, fmt="(a)") "<td>message</td>"
    write(unit=out_unit, fmt="(a)") "<td>timestamp</td>"
    write(unit=out_unit, fmt="(a)") "</tr>"
    open(newunit=nml_unit, file=filename, status="old", action="read", delim="quote")
    do ! SERIAL
        timestamp          = ""
        variable_type      = ""
        test_operator      = ""
        test_passes        = .false.
        message            = ""
        returned_logical   = .false.
        expected_logical   = .false.
        returned_real      = 0.0_WP
        expected_real      = 0.0_WP
        tolerance          = 0.0_WP
        difference         = 0.0_WP
        returned_integer   = 0
        expected_integer   = 0
        returned_character = ""
        expected_character = ""
        read(unit=nml_unit, nml=test_result, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            write(unit=ERROR_UNIT, fmt="(a)") trim(nml_error_message)
            rc = rc_nml
            close(unit=nml_unit)
            return
        end if
        
        select case (trim(variable_type))
            case("logical")
                !print *, test_passes, trim(variable_type), returned_logical, expected_logical, trim(message)
                call write_row(out_unit, test_passes, timestamp, message)
            case("real")
                !print *, test_passes, trim(variable_type), returned_real, trim(test_operator), expected_real, tolerance, &
                            !difference, trim(message)
                call write_row(out_unit, test_passes, timestamp, message)
            case("integer")
                !print *, test_passes, trim(variable_type), returned_integer, trim(test_operator), expected_integer, trim(message)
                call write_row(out_unit, test_passes, timestamp, message)
            case("character")
                !print *, test_passes, trim(variable_type), returned_character, expected_character, trim(message)
                call write_row(out_unit, test_passes, timestamp, message)
        end select
    end do
    write(unit=out_unit, fmt="(a)") "</table>"
    
    rewind nml_unit
    read(unit=nml_unit, nml=tests_summary)
    write(unit=out_unit, fmt="(a, i0, a, i0, a, i0, a)") "<p>", n_tests, " tests run, ", n_tests - n_failures, &
                                                        " tests passing, ", n_failures, " tests failing.</p>"
    
    close(unit=nml_unit)
end subroutine write_test_html

subroutine write_row(out_unit, test_passes, timestamp, message)
    use checks, only: assert
    
    integer, intent(in)          :: out_unit
    logical, intent(in)          :: test_passes
    character(len=*), intent(in) :: timestamp, message
    
    logical          :: out_unit_open
    character(len=8) :: bgcolor, test_passes_string
    
    inquire(unit=out_unit, opened=out_unit_open)
    call assert(out_unit_open, "gentesthtml (write_test_html): out_unit must be open")
    
    write(unit=out_unit, fmt="(a)") "<tr>"
    
    if (test_passes) then
        bgcolor = "green"
        test_passes_string = "passing"
    else
        bgcolor = "red"
        test_passes_string = "failing"
    end if
    write(unit=out_unit, fmt="(5a)") "<td bgcolor=", trim(bgcolor), ">", trim(test_passes_string), "</td>"
    
    write(unit=out_unit, fmt="(3a)") "<td>", trim(message), "</td>"
    
    write(unit=out_unit, fmt="(3a)") "<td>", trim(timestamp), "</td>"
    
    write(unit=out_unit, fmt="(a)") "</tr>"
end subroutine write_row

end program gentesthtml
