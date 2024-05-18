# Why run `make valgrind` here? As it uses gfortran and is more strict.
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
	@echo =======================================
	@echo = All tests passed for all compilers. =
	@echo =======================================

.PHONY: valgrind
valgrind:
	$(MAKE) test RUN='valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=1 --show-reachable=no ./'

# TODO: <https://github.com/camfort/camfort/wiki/Sanity-Checks>
lint:
	$(RUN)lint-wrapper.py src/*.f90 test/*.f90

# TODO: Make depend on *.gcda files?
html-cov/index.html: $(TESTNML)
	lcov --directory . --directory src/ --capture --output-file $(COV)
	genhtml -t "flt" -o html-cov $(COV)

coverage: html-cov/index.html
