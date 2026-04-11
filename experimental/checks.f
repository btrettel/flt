      PROGRAM CHECKS
      EXTERNAL ASSERT
      CALL ASSERT(.TRUE., 'PASSING')
      CALL ASSERT(.FALSE., 'FAILING')
      END
      
      SUBROUTINE ASSERT(CONDIT, MESSAG)
C     Condition being asserted.
      LOGICAL CONDIT
C     Message to output if CONDIT is .FALSE.
      CHARACTER*(*) MESSAG
      
      IF (.NOT. CONDIT) THEN
          WRITE(*, *) MESSAG
          STOP 1
      END IF
      END
