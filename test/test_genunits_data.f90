! tests for the genunits_data module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_genunits_data

use genunits_data, only: unit_type, unit_system_type, m_unit, d_unit, sqrt_unit, cbrt_unit, square_unit
use prec, only: WP
use nmllog, only: log_type
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests
type(unit_type)         :: unit1, unit2, unit_out
type(unit_system_type)  :: unit_system

call logger%open("genunits_data.nml")
call tests%start_tests(logger)

unit_system%base_units   = ["kg", "m ", "s "]
unit_system%n_base_units = size(unit_system%base_units)

allocate(unit_system%units(2))
allocate(unit_system%units(1)%e(unit_system%n_base_units))
allocate(unit_system%units(2)%e(unit_system%n_base_units))

unit_system%units(1)%e(1) = 0.5_WP
unit_system%units(1)%e(2) = 0.0_WP
unit_system%units(1)%e(3) = 0.0_WP

unit_system%units(2)%e(1) = 0.0_WP
unit_system%units(2)%e(2) = -0.25_WP
unit_system%units(2)%e(3) = 0.0_WP

call tests%integer_eq(unit_system%n_base_units, 3, "unit_system%n_base_units == 3")

allocate(unit1%e(unit_system%n_base_units))
unit1%e(1) = 1.0_WP
unit1%e(2) = 1.5_WP
unit1%e(3) = -2.0_WP

call tests%character_eq(unit1%label(), "unit_p10000_p15000_m20000", "unit%label")
call tests%character_eq(unit1%readable(unit_system), "kg.m3/2.s-2", "unit%readable")

! `is_in`

unit1 = unit_system%units(1)
call tests%logical_true(unit1%is_in(unit_system%units), "unit1%is_in(unit_system%units), .true.")

unit1%e(1) = 1.5_WP
unit1%e(2) = 0.0_WP
unit1%e(3) = 0.0_WP
call tests%logical_false(unit1%is_in(unit_system%units), "unit1%is_in(unit_system%units), .false.")

! unit calculus

unit1%e(1) = 1.0_WP
unit1%e(2) = 1.5_WP
unit1%e(3) = -2.0_WP

allocate(unit2%e(unit_system%n_base_units))
unit2%e(1) = 2.0_WP
unit2%e(2) = -4.0_WP
unit2%e(3) = 1.25_WP

unit_out = m_unit(unit1, unit2)
call tests%real_eq(unit_out%e(1), 3.0_WP, "m_unit, index 1")
call tests%real_eq(unit_out%e(2), -2.5_WP, "m_unit, index 2")
call tests%real_eq(unit_out%e(3), -0.75_WP, "m_unit, index 3")

unit_out = d_unit(unit1, unit2)
call tests%real_eq(unit_out%e(1), -1.0_WP, "d_unit, index 1")
call tests%real_eq(unit_out%e(2), 5.5_WP, "d_unit, index 2")
call tests%real_eq(unit_out%e(3), -3.25_WP, "d_unit, index 3")

unit_out = sqrt_unit(unit1)
call tests%real_eq(unit_out%e(1), 0.5_WP, "sqrt_unit, index 1")
call tests%real_eq(unit_out%e(2), 0.75_WP, "sqrt_unit, index 2")
call tests%real_eq(unit_out%e(3), -1.0_WP, "sqrt_unit, index 3")

unit_out = cbrt_unit(unit1)
call tests%real_eq(unit_out%e(1), 1.0_WP/3.0_WP, "cbrt_unit, index 1")
call tests%real_eq(unit_out%e(2), 0.5_WP, "cbrt_unit, index 2")
call tests%real_eq(unit_out%e(3), -2.0_WP/3.0_WP, "cbrt_unit, index 3")

unit_out = square_unit(unit1)
call tests%real_eq(unit_out%e(1), 2.0_WP, "square_unit, index 1")
call tests%real_eq(unit_out%e(2), 3.0_WP, "square_unit, index 2")
call tests%real_eq(unit_out%e(3), -4.0_WP, "square_unit, index 3")

call test_real_to_rational(tests)
call test_rational_string(tests)

call tests%end_tests()
call logger%close()

contains

subroutine test_real_to_rational(tests)
    use genunits_data, only: real_to_rational
    use prec, only: PI
    
    type(test_results_type), intent(in out) :: tests
    
    real(kind=WP) :: x
    integer       :: numerator, denominator, rc
    
    x = 1.0_WP
    call real_to_rational(x, numerator, denominator, rc)
    call tests%integer_eq(numerator, 1, "real_to_rational, 1.0, numerator")
    call tests%integer_eq(denominator, 1, "real_to_rational, 1.0, denominator")
    call tests%integer_eq(rc, 0, "real_to_rational, 1.0, rc")
    
    x = 1.5_WP
    call real_to_rational(x, numerator, denominator, rc)
    call tests%integer_eq(numerator, 3, "real_to_rational, 1.5, numerator")
    call tests%integer_eq(denominator, 2, "real_to_rational, 1.5, denominator")
    call tests%integer_eq(rc, 0, "real_to_rational, 1.5, rc")
    
    x = PI
    call real_to_rational(x, numerator, denominator, rc)
    call tests%integer_eq(rc, 1, "real_to_rational, PI, rc")
end subroutine test_real_to_rational

subroutine test_rational_string(tests)
    use genunits_data, only: rational_string
    use prec, only: CL
    
    type(test_results_type), intent(in out) :: tests
    
    character(len=CL) :: string
    
    string = rational_string(1, 1)
    call tests%character_eq(string, "1", "rational_string, 1")
    
    string = rational_string(1, 2)
    call tests%character_eq(string, "1/2", "rational_string, 1/2")
end subroutine test_rational_string

end program test_genunits_data
