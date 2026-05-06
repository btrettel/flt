      program checks
      external assert
      call assert(.true., 'PASSING')
      call assert(.false., 'FAILING')
      end

      subroutine assert(condit, messag)
c     Condition being asserted.
      logical condit
c     Message to output if `condit` is `.false.`
      character*(*) messag
c     Change to `.false.` to disable assertions.
      logical debug
      parameter (debug = .true.)
      if (debug) then
          if ( .not. condit) then
              write (*, *) 'ASSERTION FAILED.'
              write (*, *) messag
              stop 1
          endif
      endif
      end
