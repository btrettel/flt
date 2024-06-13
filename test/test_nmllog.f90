! tests for the logging module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_nmllog

use nmllog, only: log_type, UNIT_CLOSED, WARNING_LEVEL, TIMESTAMP_LEN
use unittest, only: test_results_type
implicit none

type(log_type)          :: logger
type(test_results_type) :: tests

character(len=*), parameter :: TEST_FILENAME     = "test.nml"
character(len=*), parameter :: DEBUG_MESSAGE     = "Debug level test"
character(len=*), parameter :: INFO_MESSAGE      = "Info level test"
character(len=*), parameter :: WARNING_MESSAGE   = "Warning level test"
character(len=*), parameter :: ERROR_MESSAGE     = "Error level test"
character(len=*), parameter :: CRITICAL_MESSAGE  = "Critical level test"
character(len=*), parameter :: NO_PREFIX_MESSAGE = "No prefix test"

call logger%open("nmllog.nml")
call tests%start_tests(logger)

call test_now(tests)
call test_log_subroutines(tests)
call test_log_debug_info(tests)
call test_pure_log(tests)
call test_check(tests)
call test_stdout_prefix()

call tests%end_tests()
call logger%close()

contains

subroutine test_now(tests)
    use nmllog, only: now
    use unittest, only: validate_timestamp
    
    type(test_results_type), intent(in out) :: tests
    
    character(len=TIMESTAMP_LEN) :: timestamp
    
    timestamp = now()
    
    call validate_timestamp(tests, timestamp, "now")
end subroutine test_now

subroutine test_log_subroutines(tests)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use prec, only: CL
    use nmllog, only: DEBUG_LEVEL, DEBUG_STRING, INFO_STRING, WARNING_STRING, ERROR_STRING, CRITICAL_STRING, NML_RECL
    use unittest, only: validate_timestamp
    
    type(test_results_type), intent(in out) :: tests
    
    type(log_type) :: test_logger
    integer        :: nml_unit, rc_nml, num_nml_groups, n_debug, n_info, n_warning, n_error, n_critical, n_not_set
    
    character(len=CL) :: nml_error_message
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=8)             :: level
    character(len=CL)            :: message
    character(len=1)             :: nml_group_str
    
    namelist /log/ timestamp, level, message
    
    call tests%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed before opening")
    call test_logger%open(TEST_FILENAME)
    
    call tests%character_eq(test_logger%filename, TEST_FILENAME, "logger, filename")
    
    ! Test the default levels.
    call tests%integer_eq(test_logger%stdout_level, WARNING_LEVEL, "logger, stdout_level before opening")
    call tests%integer_eq(test_logger%file_level, WARNING_LEVEL, "logger, file_level before opening")
    
    call tests%integer_ne(test_logger%unit, UNIT_CLOSED, "logger, unit open after opening")
    
    test_logger%file_level = DEBUG_LEVEL ! Set so that the next lines pass.
    call test_logger%debug(DEBUG_MESSAGE)
    call test_logger%info(INFO_MESSAGE)
    call test_logger%warning(WARNING_MESSAGE)
    call test_logger%error(ERROR_MESSAGE)
    call test_logger%critical(CRITICAL_MESSAGE)

    call test_logger%debug_info()

    call test_logger%close()
    
    call tests%integer_eq(test_logger%unit, UNIT_CLOSED, "logger, unit closed after closing")
    
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    num_nml_groups = 0
    n_debug        = 0
    n_info         = 0
    n_warning      = 0
    n_error        = 0
    n_critical     = 0
    n_not_set      = 0
    do
        read(unit=nml_unit, nml=log, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call tests%integer_eq(rc_nml, 0, "reading nmllog, error")
            call tests%logger%error(trim(nml_error_message))
            exit
        end if
        
        num_nml_groups = num_nml_groups + 1
        
        write(unit=nml_group_str, fmt="(i1)") num_nml_groups
        
        call validate_timestamp(tests, timestamp, trim(message))
        
        call tests%integer_ge(len(trim(message)), 1, "Namelist group number " // nml_group_str // " message absent")
        call tests%integer_ge(len(trim(level)), 1, "Namelist group number " // nml_group_str // " level absent")
        
        select case (trim(level))
            case (DEBUG_STRING)
                n_debug = n_debug + 1
                call tests%character_eq(trim(message), DEBUG_MESSAGE, "nmllog, debug message")
            case (INFO_STRING)
                n_info = n_info + 1
                call tests%character_eq(trim(message), INFO_MESSAGE, "nmllog, info message")
            case (WARNING_STRING)
                n_warning = n_warning + 1
                call tests%character_eq(trim(message), WARNING_MESSAGE, "nmllog, warning message")
            case (ERROR_STRING)
                n_error = n_error + 1
                call tests%character_eq(trim(message), ERROR_MESSAGE, "nmllog, error message")
            case (CRITICAL_STRING)
                n_critical = n_critical + 1
                call tests%character_eq(trim(message), CRITICAL_MESSAGE, "nmllog, critical message")
            case default
                n_not_set = n_not_set + 1
        end select
        
        !write(unit=*, fmt=*) rc_nml, timestamp, trim(level), trim(message)
    end do
    
    call tests%integer_eq(rc_nml, IOSTAT_END, "nmllog, iostat at end of file")
    
    if (rc_nml == IOSTAT_END) then
        close(unit=nml_unit, status="delete")
    else
        ! Don't delete if there was an error.
        close(unit=nml_unit)
    end if
    
    call tests%integer_eq(n_debug, 1, "nmllog, number of debug levels")
    call tests%integer_eq(n_info, 1, "nmllog, number of info levels")
    call tests%integer_eq(n_warning, 1, "nmllog, number of warning levels")
    call tests%integer_eq(n_error, 1, "nmllog, number of error levels")
    call tests%integer_eq(n_critical, 1, "nmllog, number of critical levels")
    call tests%integer_eq(n_not_set, 0, "nmllog, no unknown levels")
end subroutine test_log_subroutines

subroutine test_log_debug_info(tests)
    use prec, only: CL
    use nmllog, only: DEBUG_STRING, NML_RECL
    use unittest, only: validate_timestamp
    
    type(test_results_type), intent(in out) :: tests
    
    type(log_type) :: test_logger
    integer        :: nml_unit
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=8)             :: level
    character(len=CL)            :: compiler_options, compiler_version
    
    ! Search nmllog.f90 for `real_huge` to see why this is commented out.
    !real(kind=WP) :: real_huge
    integer       :: real_kind_code, real_precision, real_range, real_radix, &
                        real_min_exponent, real_max_exponent, &
                        integer_kind_code, integer_range, integer_huge
    logical       :: real_support_datatype, real_support_denormal, real_support_divide, &
                        real_support_inf, real_support_nan, real_support_sqrt, real_support_standard
    
    namelist /debug_info/ timestamp, level, compiler_options, compiler_version, &
                            real_kind_code, real_precision, real_range, real_radix, & !real_huge, &
                            real_min_exponent, real_max_exponent, &
                            real_support_datatype, real_support_denormal, real_support_divide, &
                            real_support_inf, real_support_nan, real_support_sqrt, real_support_standard, &
                            integer_kind_code, integer_range, integer_huge
    
    call test_logger%open(TEST_FILENAME)
    call test_logger%debug_info()
    call test_logger%close()
    
    timestamp             = ""
    level                 = ""
    compiler_options      = ""
    compiler_version      = ""
    real_kind_code        = -1
    real_precision        = -1
    real_range            = -1
    real_radix            = -1
    !real_huge             = 0.0_WP
    real_min_exponent     = -1
    real_max_exponent     = -1
    real_support_datatype = .false.
    real_support_denormal = .false.
    real_support_divide   = .false.
    real_support_inf      = .false.
    real_support_nan      = .false.
    real_support_sqrt     = .false.
    real_support_standard = .false.
    integer_kind_code     = -1
    integer_range         = -1
    integer_huge          = 0
    
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=debug_info)
    close(unit=nml_unit, status="delete")
    
    call validate_timestamp(tests, timestamp, "test_log_debug_info, timestamp")
    call tests%character_eq(level, DEBUG_STRING, "test_log_debug_info, level")
    call tests%integer_ge(len(trim(compiler_options)), 1, "test_log_debug_info, compiler_options")
    call tests%integer_ge(len(trim(compiler_version)), 1, "test_log_debug_info, compiler_version")
    call tests%integer_ge(real_kind_code, 1, "test_log_debug_info, real_kind_code")
    call tests%integer_eq(real_precision, 15, "test_log_debug_info, real_precision")
    call tests%integer_eq(real_range, 307, "test_log_debug_info, real_range")
    call tests%integer_eq(real_radix, 2, "test_log_debug_info, real_radix")
    ! LATER: real_huge
    call tests%integer_eq(real_min_exponent, -1021, "test_log_debug_info, real_min_exponent")
    call tests%integer_eq(real_max_exponent, 1024, "test_log_debug_info, real_max_exponent")
    call tests%logical_true(real_support_datatype, "test_log_debug_info, real_support_datatype")
    
    ! LATER: flang-7 does not support denormals. Check if nvfortran does later.
    ! call tests%logical_true(real_support_denormal, "test_log_debug_info, real_support_denormal")
    
    call tests%logical_true(real_support_divide, "test_log_debug_info, real_support_divide")
    call tests%logical_true(real_support_inf, "test_log_debug_info, real_support_inf")
    call tests%logical_true(real_support_nan, "test_log_debug_info, real_support_nan")
    call tests%logical_true(real_support_sqrt, "test_log_debug_info, real_support_sqrt")
    
    ! LATER: Commented out as crayftn fails this.
    !call tests%logical_true(real_support_standard, "test_log_debug_info, real_support_standard")
    
    call tests%integer_ge(integer_kind_code, 1, "test_log_debug_info, integer_kind_code")
    call tests%integer_eq(integer_range, 9, "test_log_debug_info, integer_range")
    call tests%integer_eq(integer_huge, 2147483647, "test_log_debug_info, integer_huge")
end subroutine test_log_debug_info

subroutine test_pure_log(tests)
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use nmllog, only: pure_log_type, log_type, DEBUG_LEVEL, DEBUG_STRING, INFO_STRING, WARNING_STRING, ERROR_STRING, &
                        CRITICAL_STRING, NML_RECL
    use prec, only: CL
    use unittest, only: validate_timestamp
    
    type(test_results_type), intent(in out) :: tests
    
    type(log_type)      :: test_logger
    type(pure_log_type) :: pure_logger
    
    integer           :: nml_unit, rc_nml, num_nml_groups, n_debug, n_info, n_warning, n_error, n_critical, n_not_set
    character(len=CL) :: nml_error_message
    
    character(len=TIMESTAMP_LEN) :: timestamp
    character(len=8)             :: level
    character(len=CL)            :: message
    character(len=1)             :: nml_group_str
    
    namelist /log/ timestamp, level, message
    
    call test_logger%open(TEST_FILENAME)
    test_logger%file_level = DEBUG_LEVEL
    
    call pure_logger%open(test_logger)
    
    call pure_logging_subroutine(pure_logger)
    
    call pure_logger%close()
    call test_logger%close()
    
    open(newunit=nml_unit, file=TEST_FILENAME, status="old", action="read", delim="quote", recl=NML_RECL)
    read(unit=nml_unit, nml=log, iostat=rc_nml, iomsg=nml_error_message)
    num_nml_groups = 0
    n_debug        = 1
    n_info         = 0
    n_warning      = 0
    n_error        = 0
    n_critical     = 0
    n_not_set      = 0
    do
        read(unit=nml_unit, nml=log, iostat=rc_nml, iomsg=nml_error_message)
        
        if (rc_nml == IOSTAT_END) then
            exit
        else if (rc_nml /= 0) then
            call tests%integer_eq(rc_nml, 0, "reading nmllog, error")
            call tests%logger%error(trim(nml_error_message))
            exit
        end if
        
        num_nml_groups = num_nml_groups + 1
        
        write(unit=nml_group_str, fmt="(i1)") num_nml_groups
        
        call validate_timestamp(tests, timestamp, message)
        
        call tests%integer_ge(len(trim(message)), 1, "Namelist group number " // nml_group_str // " message absent")
        call tests%integer_ge(len(trim(level)), 1, "Namelist group number " // nml_group_str // " level absent")
        
        select case (trim(level))
            case (DEBUG_STRING)
                n_debug = n_debug + 1
                
                if (n_debug == 2) then
                    call tests%character_eq(message, DEBUG_MESSAGE // " [pure]", "nmllog, debug message")
                end if
            case (INFO_STRING)
                n_info = n_info + 1
                call tests%character_eq(message, INFO_MESSAGE // " [pure]", "nmllog, info message")
            case (WARNING_STRING)
                n_warning = n_warning + 1
                call tests%character_eq(message, WARNING_MESSAGE // " [pure]", "nmllog, warning message")
            case (ERROR_STRING)
                n_error = n_error + 1
                call tests%character_eq(message, ERROR_MESSAGE // " [pure]", "nmllog, error message")
            case (CRITICAL_STRING)
                n_critical = n_critical + 1
                call tests%character_eq(message, CRITICAL_MESSAGE // " [pure]", "nmllog, critical message")
            case default
                n_not_set = n_not_set + 1
        end select
        
        !write(unit=*, fmt=*) rc_nml, timestamp, trim(level), trim(message)
    end do
    
    call tests%integer_eq(rc_nml, IOSTAT_END, "nmllog, iostat at end of file")
    
    if (rc_nml == IOSTAT_END) then
        close(unit=nml_unit, status="delete")
    else
        ! Don't delete if there was an error.
        close(unit=nml_unit)
    end if
    
    call tests%integer_eq(n_debug, 3, "nmllog, number of debug levels")
    call tests%integer_eq(n_info, 1, "nmllog, number of info levels")
    call tests%integer_eq(n_warning, 1, "nmllog, number of warning levels")
    call tests%integer_eq(n_error, 1, "nmllog, number of error levels")
    call tests%integer_eq(n_critical, 1, "nmllog, number of critical levels")
    call tests%integer_eq(n_not_set, 0, "nmllog, no unknown levels")
end subroutine test_pure_log

pure subroutine pure_logging_subroutine(pure_logger)
    use nmllog, only: pure_log_type
    
    type(pure_log_type), intent(in out) :: pure_logger
    
    call pure_logger%debug(DEBUG_MESSAGE)
    call pure_logger%info(INFO_MESSAGE)
    call pure_logger%warning(WARNING_MESSAGE)
    call pure_logger%error(ERROR_MESSAGE)
    call pure_logger%critical(CRITICAL_MESSAGE)
end subroutine pure_logging_subroutine

subroutine test_check(tests)
    use nmllog, only: pure_log_type, log_type
    
    type(test_results_type), intent(in out) :: tests
    
    type(log_type)      :: impure_logger
    type(pure_log_type) :: pure_logger
    
    integer :: rc_check
    
    call impure_logger%open(TEST_FILENAME)
    call pure_logger%open(impure_logger)
    
    ! TODO: Check the log itself for these messages too.
    
    rc_check = 0
    call impure_logger%check(.true., "impure check, .true.", rc_check)
    call tests%integer_eq(rc_check, 0, "impure check, .true.")

    rc_check = 0
    call impure_logger%check(.false., "impure check, .false.", rc_check)
    call tests%integer_eq(rc_check, 1, "impure check, .false.")
    
    rc_check = 0
    call pure_logger%check(.true., "pure check, .true.", rc_check)
    call tests%integer_eq(rc_check, 0, "impure check, .true.")

    rc_check = 0
    call pure_logger%check(.false., "pure check, .false.", rc_check)
    call tests%integer_eq(rc_check, 1, "impure check, .false.")
    
    call pure_logger%close()
    call impure_logger%close()
end subroutine test_check

subroutine test_stdout_prefix()
    ! LATER: Check that stdout doesn't have the prefix. Right now I do that manually.
    
    use nmllog, only: log_type
    
    type(log_type) :: test_logger
    
    call test_logger%open(TEST_FILENAME)

    test_logger%stdout_prefix = .false.
    call test_logger%warning(NO_PREFIX_MESSAGE)
    
    call test_logger%close()
end subroutine test_stdout_prefix

end program test_nmllog
