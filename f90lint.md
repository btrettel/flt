# f90lint.py

This script does some static analysis not done by other static analyzers I have access to. The cutoffs listed below are somewhat arbitrary and are not the result of detailed analysis.

- Assertion density > 2.0% per file and globally[^1].
- Ratio of source lines of test code to source lines of code > 0.5 per file and globally.
- In test files, all subroutines starting with `test_` must be `call`ed.

[^1]: <https://www.microsoft.com/en-us/research/publication/assessing-the-relationship-between-software-assertions-and-code-qualityan-empirical-investigation/>, <http://spinroot.com/p10/rule5.html>
