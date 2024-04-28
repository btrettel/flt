program pdim_gen

use pdim_mod, only: pdim_config_type, pdim_type, write_module, pdim_within_bounds
use prec, only: SP
implicit none

type(pdim_config_type) :: config
integer                :: out_unit

config%pdim_chars     = "LMT"
config%pdim_type_defn = "real(kind=WP)"
!config%pdim_type_defn = "type(ad)"
config%n_pdims        = len(config%pdim_chars)

allocate(config%min_exponents(config%n_pdims))
allocate(config%max_exponents(config%n_pdims))
allocate(config%exponent_deltas(config%n_pdims))
config%min_exponents   = [-1.0_SP, -1.0_SP, -1.0_SP]
config%max_exponents   = [1.0_SP, 1.0_SP, 1.0_SP]
config%exponent_deltas = [1.0_SP, 1.0_SP, 1.0_SP]

open(newunit=out_unit, &
        action="write", &
        status="replace", &
        position="rewind", &
        file="src/pdim_types.f90")
call write_module(config, out_unit)
close(unit=out_unit)

end program pdim_gen
