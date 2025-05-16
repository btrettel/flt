# ga.f90 to-do

- Subroutine to optimize genetic algorithm parameters with a genetic algorithm. Objective function is wall-clock time.
- Won't initially for simplicity: `chromo%out(:)` (for non-objective function outputs that may be of interest). Make type a `class(*)` variable so that I can put any output I want in there.
- Hybridize: Nelder-Mead method for local search when genetic algorithm has stalled.
- Procedure to round to values in an array, and related procedure to get array from text file.
- `integer` chromosome in addition?
- Maybe change `rel_b` to something like `rel_mut_size`.
- Low priority: To improve convergence to the global optimum better:
    - luke_essentials_2013 pp. 128--130: Fitness sharing (I don't like how it makes the objective function values dependent on the population. That makes comparing objective function values against the best observed value not meaningful.)
    - luke_essentials_2013 p. 103: Island model (Multi-start should perform similarly as-is. The advantage of the island model I think is if I need MPI-type parallelism.)
- Skip mutation for constants. Clip if samples exceeded.
