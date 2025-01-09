# returncodes.f90 to-do

- Make a table of `iostat` values in different Fortran compilers so that you know which values to pick to not conflict with any compiler.
    - <https://fortranwiki.org/fortran/show/iso_fortran_env>
        - `iostat_end`, `iostat_eor`
    - <https://www.scivision.dev/oneapi-fortran-iostat-codes/>
    - <https://www.ibm.com/docs/en/xl-fortran-linux/16.1.1?topic=inputoutput-conditions-iostat-values>
    - <https://groups.google.com/g/comp.lang.fortran/c/l8UJoI-x9PM>
- instability detected
- failed to converge
    - progress stalling
    - max iterations reached

***

FDS, cons.f90 (simplified a bit):

    INTEGER, PARAMETER :: NO_STOP=0
    INTEGER, PARAMETER :: INSTABILITY_STOP=1
    INTEGER, PARAMETER :: USER_STOP=2
    INTEGER, PARAMETER :: SETUP_STOP=3
    INTEGER, PARAMETER :: SETUP_ONLY_STOP=4
    INTEGER, PARAMETER :: CTRL_STOP=5
    INTEGER, PARAMETER :: TGA_ANALYSIS_STOP=6
    INTEGER, PARAMETER :: LEVELSET_STOP=7
    INTEGER, PARAMETER :: REALIZABILITY_STOP=8
    INTEGER, PARAMETER :: VERSION_STOP=10
    INTEGER, PARAMETER :: MPI_TIMEOUT_STOP=11

FDS, main.f90:

    SELECT CASE(STOP_STATUS)
      CASE(NO_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: FDS completed successfully'
         IF (STATUS_FILES) CLOSE(LU_NOTREADY,STATUS='DELETE')
      CASE(INSTABILITY_STOP)
         WRITE(MESSAGE,'(A)') 'ERROR: Numerical Instability - FDS stopped'
      CASE(USER_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: FDS stopped by user'
      CASE(SETUP_STOP)
         WRITE(MESSAGE,'(A)') 'ERROR: FDS was improperly set-up - FDS stopped'
      CASE(SETUP_ONLY_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: Set-up only'
      CASE(CTRL_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: FDS was stopped by KILL control function and completed successfully'
      CASE(TGA_ANALYSIS_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: FDS performed a TGA analysis only and finished successfully'
      CASE(LEVELSET_STOP)
         WRITE(MESSAGE,'(A)') 'STOP: FDS performed a level set analysis only and finished successfully'
      CASE(REALIZABILITY_STOP)
         WRITE(MESSAGE,'(A)') 'ERROR: Unrealizable mass density - FDS stopped'
      CASE(MPI_TIMEOUT_STOP)
         WRITE(MESSAGE,'(A)') 'ERROR: An MPI exchange timed out - FDS stopped'
      CASE DEFAULT
         WRITE(MESSAGE,'(A)') 'null'
    END SELECT
