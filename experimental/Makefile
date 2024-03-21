src/dimmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

src/ga$(DBGOBJEXT): src/prec$(DBGOBJEXT) src/rngmod$(DBGOBJEXT)

src/rngmod$(DBGOBJEXT): src/prec$(DBGOBJEXT)

############
# dimcheck #
############

test_dimmod$(BINEXT): src/dimmod$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_dimmod.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_dimmod.f90

dimmod.nml: test_dimmod$(BINEXT)
	$(RUN)test_dimmod$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

######
# ga #
######

test_ga$(BINEXT): src/ga$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_ga.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_ga.f90

ga.nml: test_ga$(BINEXT)
	$(RUN)test_ga$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*

##########
# rngmod #
##########

test_rngmod$(BINEXT): src/rngmod$(DBGOBJEXT) src/unittest$(DBGOBJEXT) test/test_rngmod.f90
	$(FC) $(OFLAG) $@ $(FFLAGS) $(DBGFLAGS) src/*$(DBGOBJEXT) test/test_rngmod.f90

rngmod.nml: test_rngmod$(BINEXT)
	$(RUN)test_rngmod$(BINEXT)
	#python3 test/passed.py $@
	test ! -e fort.*
