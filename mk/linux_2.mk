# Why run `make valgrind` here? As it uses gfortran and is more strict.
# LATER: Enable FC=lfortran for `make all`, particularly for the style suggestions
.PHONY: all
all:
	$(MAKE) lint
	$(MAKE) valgrind # gfortran
	$(MAKE) clean
	$(MAKE) FC=ifx
	$(MAKE) clean
	$(MAKE) FC=ifort
	$(MAKE) clean
	$(MAKE) FC=nvfortran
	$(MAKE) clean
	$(MAKE) lfortran
	$(MAKE) clean
	@echo =======================================
	@echo = All tests passed for all compilers. =
	@echo =======================================

.PHONY: valgrind
valgrind:
	$(MAKE) test RUN='valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=1 --show-reachable=no ./'

# TODO: <https://github.com/camfort/camfort/wiki/Sanity-Checks>
.PHONY: lint
lint:
	$(RUN)f90lint.py app/*.f90 src/*.f90 test/*.f90
	$(RUN)lint-wrapper.py app/*.f90 src/*.f90 test/*.f90

# TODO: Make depend on *.gcda files?
html-cov/index.html: $(TESTNML)
	lcov --directory . --directory src/ --capture --output-file $(COV)
	genhtml -t "flt" -o html-cov $(COV)

.PHONY: coverage
coverage: html-cov/index.html

.PHONY: depends
depends:
	$(RUN)depends.py app/*.f90 src/*.f90 test/*.f90

# Most of these won't compile due to gaps in lfortran, but some will.
.PHONY: lfortran
lfortran:
	$(MAKE) FC=lfortran src/autodiff.o
	#$(MAKE) FC=lfortran src/checks.o
	$(MAKE) FC=lfortran src/debug.o
	#$(MAKE) FC=lfortran src/genunits_data.o
	#$(MAKE) FC=lfortran src/genunits_io.o
	#$(MAKE) FC=lfortran src/nmllog.o
	$(MAKE) FC=lfortran src/prec.o
	#$(MAKE) FC=lfortran src/purerng.o
	$(MAKE) FC=lfortran src/release.o
	#$(MAKE) FC=lfortran src/timer.o
	#$(MAKE) FC=lfortran src/unittest.o
	#$(MAKE) FC=lfortran genunits
