! Grammar fuzzer for Fortran code that uses the geninput namelist code generation module.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program nmlfuzz

use prec, only: CL
use cli, only: get_input_file_name_from_cli
use geninput_io, only: config_type, input_variable_type, read_config_namelist, read_input_variable_namelists, &
                        sort_input_variables
use stopcodes, only: EX_OK
implicit none

integer, parameter :: NMLFUZZ_UNGUIDED_MODE = 1
integer, parameter :: NMLFUZZ_GUIDED_MODE   = 2

character(len=CL) :: input_file
type(config_type) :: config
integer           :: rc_config, rc_input_variables

type(input_variable_type), allocatable :: input_variables(:)
integer, allocatable :: input_variable_indexes(:)

! Read all namelists and exit if any have issues.
call get_input_file_name_from_cli("nmlfuzz", input_file)

! Read all namelists and exit if any have issues.
call read_config_namelist(input_file, config, rc_config)
if (rc_config /= 0) then
    error stop
end if

call read_input_variable_namelists(input_file, input_variables, rc_input_variables)
if (rc_input_variables /= 0) then
    error stop
end if

call sort_input_variables(input_variables)

select case (config%nmlfuzz_mode)
    case (NMLFUZZ_UNGUIDED_MODE)
        call unguided_fuzzer()
    case (NMLFUZZ_GUIDED_MODE)
        call guided_fuzzer()
    case default
        error stop "Invalid nmlfuzz_mode."
end select

stop EX_OK, quiet=.true.

contains

subroutine get_nml_filename(nml_file)
    use prec, only: WP
    character(len=CL), intent(out) :: nml_file
    
    integer  :: i
    real(WP) :: x
    logical  :: file_exists
    
    do i = 1, 999
        call random_number(x)
        write(unit=nml_file, fmt="(2a, z0, a)") trim(config%namelist_group), "_", x, ".nml"
        inquire(file=trim(nml_file), exist=file_exists)
        if (.not. file_exists) exit
    end do
    
    if (file_exists) error stop "nmlfuzz (get_nml_filename): could not create file"
end subroutine get_nml_filename

subroutine unguided_fuzzer()
    use prec, only: WP
    use purerng, only: rng_type
    use checks, only: is_close
    use timer, only: timer_type
    use ga, only: STOP_NOW_FILE
    
    type(rng_type)    :: rng
    character(len=CL) :: nml_file
    integer           :: out_unit, i_var, x_integer, exit_code, i_exit_code
    character(4)      :: type4
    real(WP)          :: x, x_real
    logical           :: run_time_exceeded, bad_exit_code, stop_now_detected, out_file_exists
    type(timer_type)  :: wtime
    
    call rng%random_seed()

    fuzzer_loop: do
        call get_nml_filename(nml_file)
        
        open(newunit=out_unit, action="write", status="replace", position="rewind", &
                file=trim(nml_file))
        write(unit=out_unit, fmt="(2a)") "&", trim(config%namelist_group)
        var_loop: do i_var = 1, size(input_variables)
            if (input_variables(i_var)%fuzz) then
                call rng%random_number(x)
                
                type4 = input_variables(i_var)%type_definition(1:4)
                select case (type4)
                    case ("inte")
                        x_integer = nint(input_variables(i_var)%fuzz_range(1) &
                                    + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1)))
                        
                        write(unit=out_unit, fmt="(2a, i0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                                    " ! ", trim(input_variables(i_var)%txt_unit)
                    case ("real", "type")
                        x_real = input_variables(i_var)%fuzz_range(1) &
                                    + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1))
                        
                        write(unit=out_unit, fmt="(2a, g0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                                    " ! ", trim(input_variables(i_var)%txt_unit)
                    case ("char")
                        error stop "nmlfuzz can't handle char variables at present: " !&
                                        !// trim(input_variables(i)%variable_name)
                    case ("logi")
                        x_integer = nint(input_variables(i_var)%fuzz_range(1) &
                                    + x*(input_variables(i_var)%fuzz_range(2) - input_variables(i_var)%fuzz_range(1)))
                        
                        if (x_integer == 0) then
                            write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .false.", &
                                                                    " ! ", trim(input_variables(i_var)%txt_unit)
                        else if (x_integer == 1) then
                            write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .true.", &
                                                                    " ! ", trim(input_variables(i_var)%txt_unit)
                        else
                            stop "Any element of fuzz_range for logicals must be 0.0 for false or 1.0 for true: " &
                                            // trim(input_variables(i_var)%variable_name)
                        end if
                    case default
                        error stop "Invalid type definition: " // trim(input_variables(i_var)%type_definition)
                end select
            end if
        end do var_loop
        write(unit=out_unit, fmt="(a)") "/"
        close(unit=out_unit)
        
        call wtime%start()
        call execute_command_line(config%executable // " " // nml_file, exitstat=exit_code)
        call wtime%stop()
        
        inquire(file=trim(nml_file)//".out", exist=out_file_exists)
        if (out_file_exists) then
            open(newunit=out_unit, status="old", file=trim(nml_file)//".out")
            close(unit=out_unit, status="delete")
        end if
        
        run_time_exceeded = wtime%read() > config%run_time_threshold
        call wtime%reset()
        
        bad_exit_code = .true.
        ! Check exit code to see if it's acceptable.
        exit_code_loop: do i_exit_code = 1, size(config%acceptable_exit_codes)
            if (exit_code == config%acceptable_exit_codes(i_exit_code)) then
                bad_exit_code = .false.
                exit exit_code_loop
            end if
        end do exit_code_loop
        
        if (run_time_exceeded .or. bad_exit_code) then
            print "(2a)", "Failure detected, file kept: ", trim(nml_file)
        else
            open(newunit=out_unit, status="old", file=trim(nml_file))
            close(unit=out_unit, status="delete")
        end if
        
        ! detect `stop_now` file and quit if found
        inquire(file=STOP_NOW_FILE, exist=stop_now_detected)
        if (stop_now_detected) then
            open(newunit=out_unit, status="old", file=STOP_NOW_FILE)
            close(unit=out_unit, status="delete")
            write(unit=*, fmt="(2a)") STOP_NOW_FILE, " detected, terminating."
            exit
        end if
    end do fuzzer_loop
end subroutine unguided_fuzzer

subroutine guided_fuzzer()
    use ga, only: ga_config_type, pop_type, standard_ga_config, init_pop, optimize_ga
    use purerng, only: rng_type
    
    type(ga_config_type) :: ga_config
    type(rng_type)       :: rng
    type(pop_type)       :: pop
    integer              :: i_var, n_genes, i_gene, rc

    ! get n_genes
    n_genes = 0
    do i_var = 1, size(input_variables)
        if (input_variables(i_var)%fuzz) n_genes = n_genes + 1
    end do
    
    if (n_genes == 0) error stop "No variables to fuzz?"
    
    ! Make conversion table between `chromo` and `input_variables`.
    allocate(input_variable_indexes(n_genes))
    i_gene = 0
    do i_var = 1, size(input_variables)
        if (input_variables(i_var)%fuzz) then
            i_gene = i_gene + 1
            input_variable_indexes(i_gene) = i_var
        end if
    end do
    
    call standard_ga_config(n_genes, ga_config)
    allocate(ga_config%lb(ga_config%n_genes))
    allocate(ga_config%ub(ga_config%n_genes))
    ga_config%n_gener  = 100000
    ga_config%progress = .false.
    ga_config%stop_if_all_unfeasible = .false.
    
    var_loop: do i_gene = 1, n_genes
        i_var = input_variable_indexes(i_gene)
        
        ga_config%lb(i_gene) = input_variables(i_var)%fuzz_range(1)
        ga_config%ub(i_gene) = input_variables(i_var)%fuzz_range(2)
    end do var_loop
    
    call rng%random_seed()
    call init_pop(ga_config, rng, pop)
    call optimize_ga(ga_config, rng, guided_fuzzer_objfun, pop, rc)
end subroutine guided_fuzzer

subroutine guided_fuzzer_objfun(chromo, f, sum_g)
    use prec, only: WP
    use timer, only: timer_type
    use checks, only: assert
    
    real(WP), intent(in)  :: chromo(:)
    real(WP), intent(out) :: f
    real(WP), intent(out) :: sum_g
    
    integer           :: n_genes, out_unit, i_gene, x_integer, exit_code, in_unit, i_var, i_exit_code
    character(len=CL) :: nml_file
    real(WP)          :: x_real
    type(timer_type)  :: wtime
    logical           :: run_time_exceeded, bad_exit_code, out_file_exists
    character(4)      :: type4
    
    n_genes = size(chromo)
    
    ! Write .nml file.
    call get_nml_filename(nml_file)
    
    open(newunit=out_unit, action="write", status="replace", position="rewind", &
            file=trim(nml_file))
    write(unit=out_unit, fmt="(2a)") "&", trim(config%namelist_group)
    var_loop: do i_gene = 1, n_genes
        i_var = input_variable_indexes(i_gene)
        call assert(input_variables(i_var)%fuzz, &
                        "nmlfuzz (unguided_fuzzer_objfun): Variable not marked as fuzzable?", print_integer=[i_gene, i_var])
        
        type4 = input_variables(i_var)%type_definition(1:4)
        select case (type4)
            case ("inte")
                x_integer = nint(chromo(i_gene))
                
                write(unit=out_unit, fmt="(2a, i0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                            " ! ", trim(input_variables(i_var)%txt_unit)
            case ("real", "type")
                x_real = chromo(i_gene)
                
                write(unit=out_unit, fmt="(2a, g0, 2a)") trim(input_variables(i_var)%variable_name), " = ", x_real, &
                                                            " ! ", trim(input_variables(i_var)%txt_unit)
            case ("char")
                error stop "nmlfuzz can't handle char variables at present."
            case ("logi")
                x_integer = nint(chromo(i_gene))
                
                if (x_integer == 0) then
                    write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .false.", &
                                                            " ! ", trim(input_variables(i_var)%txt_unit)
                else if (x_integer == 1) then
                    write(unit=out_unit, fmt="(4a)") trim(input_variables(i_var)%variable_name), " = .true.", &
                                                            " ! ", trim(input_variables(i_var)%txt_unit)
                else
                    error stop "Any element of fuzz_range for logicals must be 0.0 for false or 1.0 for true."
                end if
            case default
                error stop "Invalid type definition."
        end select
    end do var_loop
    write(unit=out_unit, fmt="(a)") "/"
    close(unit=out_unit)
    
    ! Run the program.
    
    call wtime%start()
    call execute_command_line(config%executable // " " // nml_file, exitstat=exit_code)
    call wtime%stop()
    
    ! Evaluate the output.
    
    run_time_exceeded = wtime%read() > config%run_time_threshold
    !print *, 1, run_time_exceeded, wtime%read(), config%run_time_threshold
    
    bad_exit_code = .true.
    ! Check exit code to see if it's acceptable.
    exit_code_loop: do i_exit_code = 1, size(config%acceptable_exit_codes)
        if (exit_code == config%acceptable_exit_codes(i_exit_code)) then
            bad_exit_code = .false.
            exit exit_code_loop
        end if
    end do exit_code_loop
    !print *, 2, bad_exit_code, exit_code
    
    inquire(file=trim(nml_file)//".out", exist=out_file_exists)
    if (out_file_exists) then
        open(newunit=in_unit, file=trim(nml_file)//".out", status="old", action="read")
        read(unit=in_unit, fmt="(es24.17, 1x, es24.17)") f, sum_g
        close(unit=in_unit)
        open(newunit=out_unit, status="old", file=trim(nml_file)//".out")
        close(unit=out_unit, status="delete")
        !print *, 3, f, sum_g
    else
        ! TODO: For input validation errors, output a .out file listing the amount of the violation.
        
        f     = 0.0_WP
        sum_g = 10.0_WP
    end if
    
    ! One component of the objective function is run time. Longer run times are more likely bad.
    f = f - wtime%read()
    
    if (run_time_exceeded .or. bad_exit_code) then
        print "(2a)", "Failure detected, file kept: ", trim(nml_file)
    else
        open(newunit=out_unit, status="old", file=trim(nml_file))
        close(unit=out_unit, status="delete")
    end if
    
    !print *, 4, f, sum_g
    call wtime%reset()
end subroutine guided_fuzzer_objfun

end program nmlfuzz
