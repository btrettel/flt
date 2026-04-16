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
C     Change to .FALSE. to disable assertions.
      LOGICAL DEBUG
      PARAMETER (DEBUG = .TRUE.)
      IF (DEBUG) THEN
          IF (.NOT. CONDIT) THEN
              WRITE(*, *) 'ASSERTION FAILED.'
              WRITE(*, *) MESSAG
              STOP 1
          END IF
      END IF
      END
