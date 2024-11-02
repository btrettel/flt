use, intrinsic :: iso_fortran_env, only: INT64, REAL64

elemental subroutine xoshiro256ss_next(rng, harvest)
    type(rng_type), intent(in out) :: rng
    real(REAL64), intent(out)      :: harvest
    
    integer(INT64) :: iharvest, & ! `harvest` in integer form
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
    ! <https://dotat.at/@/2023-06-23-random-double.html#bithacking-solution>
    ! <https://stackoverflow.com/a/75991248/1124489>
    iharvest = ior(shiftl(1023_INT64, 52), shiftr(iharvest, 12))
    harvest  = transfer(iharvest, harvest) - 1.0_REAL64
    
    ! Also see:
    ! <https://gcc.gnu.org/wiki/GFortranSource>: Check random.c. `random_r8` converts the integer to a real.
end subroutine xoshiro256ss_next

pure subroutine xoshiro256ss_jump(rng)
    type(rng_type), intent(in out) :: rng
    
    ! Similar to:
    ! <https://github.com/DSCF-1224/xoshiro-fortran/blob/main/src/imp_jump_state_core.f90#L56>
    ! <https://github.com/jannisteunissen/xoroshiro128plus_fortran/blob/master/m_xoroshiro128plus.f90#L110>
    
    integer :: i, b
    
    ! C: `static const uint64_t JUMP[] = { 0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c };
    integer(INT64), parameter :: JUMP(4) = [1733541517147835066_INT64, &
                                            -3051731464161248980_INT64, &
                                            -6244198995065845334_INT64, &
                                            4155657270789760540_INT64]
    
    integer(INT64) :: s(4)
    real(REAL64)   :: harvest
    
    ! C: `uint64_t s0 = 0;`
    ! C: `uint64_t s1 = 0;`
    ! C: `uint64_t s2 = 0;`
    ! C: `uint64_t s3 = 0;`
    s = 0_INT64
    
    ! C: `for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)`
    do i = 0, 4
        ! C: `for(int b = 0; b < 64; b++) {`
        do b = 0, 63
            ! C: `if (JUMP[i] & UINT64_C(1) << b) {`
            if (btest(JUMP(i), b)) then
                ! C: `s0 ^= s[0];`
                ! C: `s1 ^= s[1];`
                ! C: `s2 ^= s[2];`
                ! C: `s3 ^= s[3];`
                s = ieor(s, rng%seed)
            end if
            ! C: `}`
            
            ! C: `next();`
            call xoshiro256ss_next(rng, harvest)
            
            ! C: `}`
        end do
    end do
    
    ! C: `s[0] = s0;`
    ! C: `s[1] = s1;`
    ! C: `s[2] = s2;`
    ! C: `s[3] = s3;`
    rng%seed = s
end subroutine xoshiro256ss_jump

! TODO
! <https://prng.di.unimi.it/>
! <https://prng.di.unimi.it/splitmix64.c>
! <https://github.com/fortran-lang/stdlib/blob/master/src/stdlib_random.fypp#L91>
! <https://fortranwiki.org/fortran/show/ieor>
! <https://www.geeksforgeeks.org/bitwise-operators-in-c-cpp/>
! <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_XOR_assignment>
!pure subroutine splitmix64_next()
    
!end subroutine splitmix64_next
