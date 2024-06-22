program test

implicit none

type :: config_type
    real :: x = 1.0
end type config_type

integer :: nml_unit

type(config_type), target :: config
real, pointer :: x

namelist /list/ x

! I can't put this on the line defining `x` as nvfortran doesn't like that.
x => config%x

print *, config%x

x = 3.0

open(newunit=nml_unit, file="out.nml", status="replace", action="write", delim="quote")
write(unit=nml_unit, nml=list)
close(nml_unit)

open(newunit=nml_unit, file="namelist_derived_type.nml", status="old", action="read")
read(unit=nml_unit, nml=list)
close(nml_unit)

print *, config%x

end program test

! TODO: Figure out how (if possible) to make `include`d file work for both derived type and subroutines. I could make something generate `config_type` and the namelist lines.
