program pdim_gen

use pdim_mod, only: pdim_config_type, write_module, read_config
implicit none

type(pdim_config_type) :: config
integer                :: out_unit, rc

call read_config("test/pdim_test.nml", config, rc)

if (rc /= 0) then
    error stop
end if

open(newunit=out_unit, action="write", status="replace", position="rewind", file=config%output_file)
call write_module(config, out_unit)
close(unit=out_unit)

call config%logger%close()

end program pdim_gen
