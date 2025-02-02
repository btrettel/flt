# ga.f90

If a constraint being unsatisfied can be determined without running an expensive solver, it may be advantageous to put the code for the constraint before the solver is run and only run the solver conditional on the constraint being satisfied.

Treat failures (for example, file system errors) differently from constraints. So a sum of constraint violations does not replace a return code.
