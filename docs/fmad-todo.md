# fmad.f90 to-do

- Make `init` and `init_const` have optional argument to avoid allocation for `dv` and instead check size of `dv` and zero it.
- Addition and subtraction for `real`s (with tests). (What did I mean by this? fmad.f90 already handles `real`s for addition and subtraction.)
- Assert that `dv` of the output is allocated (via `assert`) and has correct size (via `assert_dimension`).
- comparison operators for `real`s, with tests
- `fmad` variable "pruner", which takes a big list of variables, only some of which are active, and translates that into derivative array indices, and vice-versa (or something like this, to minimize the number of variables to track derivatives of)
    - `init`, `init_const`, and (new) `init_input`
    - `init_input` will work with the pruner to enable or disable AD as needed.
- Better constructors for AD. I should be able to get a constant by using `ad` directly. Make `dv` unallocated at first and if any operator uses it, then allocate? That'll be expensive.
- <https://en.wikipedia.org/wiki/Tornado_diagram>
- Modify your AD to use SIMD vectorization. Use `do concurrent` with OpenMP or OpenACC directives? See personal notes on automatic differentiation for other speed ideas too.
- Can declare certain derivatives as "active" or "inactive to easily enable or disable (respectively) differentiation with respect to particular variables at compile or run time for speed. Not yet sure how to pick `dv` indices in this case. With allocatable `dv`, this can be done at run time.
- Add description array for each differentiable variable?
- Make sure that all operations are branchless.
- Test fmad against numerical differentiation. I'm afraid that my tests might be looking too closely at the implementation, so something independent could be useful.
