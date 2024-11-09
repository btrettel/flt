# ga.f90 to-do

- Subroutine to optimize genetic algorithm parameters with a genetic algorithm. Objective function is wall-clock time.
- Make simulation fully specified by the chromosome so that no additional `config` type needs to be passed in beyond the genetic algorithm configuration.
- Won't initially for simplicity: `chromo%out(:)` (for non-objective function outputs that may be of interest). Make type a `class(*)` variable so that I can put any output I want in there.
- Hybridize: Nelder-Mead method for local search when genetic algorithm has stalled.
- Procedure to round to values in an array, and related procedure to get array from text file.
