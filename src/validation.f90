! Module for validation testing procedures.
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

module validation

implicit none
private

public :: t_tail

contains

function t_tail(t, dof)
    use prec, only: WP
    use checks, only: assert
    
    ! ALGORITHM AS 27  APPL. STATIST. VOL.19, NO.1
    ! Calculate the upper tail area under Student's t-distribution.
    ! Translated from Algol by Alan Miller.
    
    ! Modified from <https://jblevins.org/mirror/amiller/as27.f90>.
    ! The constants are single precision, but this is fine for my purposes.
    
    ! Copyright:
    ! Arguably ineligible as it doesn't contain any creative expression.
    ! This is fair use otherwise.
    
    real(WP), intent(in) :: t, dof
    
    real(WP) :: t_tail

    real(WP) :: v, tt
    real(WP), parameter  :: A1 = 0.09979441_WP, A2 = -0.581821_WP, A3 = 1.390993_WP, &
                                    A4 = -1.222452_WP, A5 = 2.151185_WP
    real(WP), parameter  :: B1 = 5.537409_WP, B2 = 11.42343_WP
    real(WP), parameter  :: C1 = 0.04431742_WP, C2 = -0.2206018_WP, C3 = -0.03317253_WP, &
                                    C4 = 5.679969_WP, C5 = -12.96519_WP
    real(WP), parameter  :: D1 = 5.166733_WP, D2 = 13.49862_WP
    real(WP), parameter  :: E1 = 0.009694901_WP, E2 = -0.1408854_WP, E3 = 1.88993_WP, &
                                    E4 = -12.75532_WP, E5 = 25.77532_WP
    real(WP), parameter  :: F1 = 4.233736_WP, F2 = 14.3963_WP
    real(WP), parameter  :: G1 = -9.187228e-5_WP, G2 = 0.03789901_WP, G3 = -1.280346_WP, &
                                    G4 = 9.249528_WP, G5 = -19.08115_WP
    real(WP), parameter  :: H1 = 2.777816_WP, H2 = 16.46132_WP
    real(WP), parameter  :: I1 = 5.79602e-4_WP, I2 = -0.02763334_WP, I3 = 0.4517029_WP, &
                                    I4 = -2.657697_WP, I5 = 5.127212_WP
    real(WP), parameter  :: J1 = 0.5657187_WP, J2 = 21.83269_WP

    call assert(dof > 4.0_WP, "test_validation (t_tail): dof > 4 violated")

    v = 1.0_WP / dof
    tt = abs(t)
    t_tail = 0.5_WP*(1.0_WP + &
                tt*(((A1 + v*(A2 + v*(A3 + v*(A4 + v*A5)))) / (1.0_WP - v*(B1 - v*B2))) + &
                tt*(((C1 + v*(C2 + v*(C3 + v*(C4 + v*C5)))) / (1.0_WP - v*(D1 - v*D2))) + &
                tt*(((E1 + v*(E2 + v*(E3 + v*(E4 + v*E5)))) / (1.0_WP - v*(F1 - v*F2))) + &
                tt*(((G1 + v*(G2 + v*(G3 + v*(G4 + v*G5)))) / (1.0_WP - v*(H1 - v*H2))) + &
                tt*((I1 + v*(I2 + v*(I3 + v*(I4 + v*I5)))) / (1.0_WP - v*(J1 - v*J2))) )))))**(-8)
    
    if (t < 0.0_WP) then
        t_tail = 1.0_WP - t_tail
    end if
end function t_tail

end module validation
