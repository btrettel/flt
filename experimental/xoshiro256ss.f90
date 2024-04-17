use, intrinsic :: iso_fortran_env, only: INT64, REAL64

elemental subroutine xoshiro256ss(rng, harvest)
    type(rng_type), intent(in out) :: rng
    real(kind=REAL64), intent(out) :: harvest
    
    integer(kind=INT64)  :: iharvest, & ! `harvest` in integer form
                            t
    
    ! This subroutine mostly follows <https://prng.di.unimi.it/xoshiro256starstar.c>, but I added code to convert to `real`.
    
    ! Some notes on conversion from C to Fortran here:
    ! C: `s[i]` ==> Fortran: `rng%seed(i + 1)`
    ! C: `rotl(...)` ==> Fortran: `ishftc(...)`
    ! C: `x << y` ==> Fortran: `shiftl(x, y)`
    ! C: `x ^= y` ==> Fortran: `x = ieor(x, y)`
    
    ! C: `const uint64_t result = rotl(s[1] * 5, 7) * 9;`
    iharvest = ishftc(rng%seed(2) * 5, 7) * 9
    
    ! Update seed for next iteration.
    
    ! C: `const uint64_t t = s[1] << 17;`
    t = shiftl(rng%seed(2), 17)
    
    ! C: `s[2] ^= s[0];`
    rng%seed(3) = ieor(rng%seed(3), rng%seed(1))
    
    ! C: `s[3] ^= s[1];`
    rng%seed(4) = ieor(rng%seed(4), rng%seed(2))
    
    ! C: `s[1] ^= s[2];`
    rng%seed(2) = ieor(rng%seed(2), rng%seed(3))
    
    ! C: `s[0] ^= s[3];`
    rng%seed(1) = ieor(rng%seed(1), rng%seed(4))
    
    ! C: `s[2] ^= t;`
    rng%seed(3) = ieor(rng%seed(3), t)
    
    ! C: `s[3] = rotl(s[3], 45);`
    rng%seed(4) = ishftc(rng%seed(4), 45)
    
    ! Convert `iharvest` to a `real`. (Not included in the original code.)
    ! Some identical code elsewhere:
    ! <https://github.com/DSCF-1224/xoshiro-fortran/blob/main/src/imp_transform_to_unit_interval.f90>
    ! <https://github.com/jannisteunissen/xoroshiro128plus_fortran/blob/master/m_xoroshiro128plus.f90> (`function U01(self)`)
    ! Some explanations of how this works for other languages:
    ! <https://dotat.at/@/2023-06-23-random-double.html>
    ! <https://stackoverflow.com/a/75991248/1124489>
    iharvest = ior(shiftl(1023_INT64, 52), shiftr(iharvest, 12))
    harvest  = transfer(iharvest, harvest) - 1.0_REAL64
    
    ! Also see:
    ! <https://gcc.gnu.org/wiki/GFortranSource>: Check random.c. `random_r8` converts the integer to a real.
end subroutine xoshiro256ss
