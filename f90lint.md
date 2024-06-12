# f90lint.py

This script does some static analysis not done by other static analyzers I have access to. The cutoffs listed below are somewhat arbitrary and are not the result of detailed analysis.

- Assertion density ≥ 4.0% per file and globally[^1]. `call assert`, `error stop` and `call [...]%check` count as assertions.
- Ratio of source lines of test code to source lines of code ≥ 0.5 per file and globally.
- In test files, all subroutines starting with `test_` must be `call`ed.

[^1]: <https://spinroot.com/gerard/pdf/Assertive_Testing.pdf> ("As a rule of thumb, you should aim for an average assertion density across all of your code of two percent or more."), <https://youtu.be/wZpDV-1P9uM?t=2234> ("I can say I just mentioned [there are] only 31 rules for level of compliance 4. If we had to reduce that to 2, this would be a rule that I would keep, assertion density high [...]"), <https://www.microsoft.com/en-us/research/publication/assessing-the-relationship-between-software-assertions-and-code-qualityan-empirical-investigation/>, <http://spinroot.com/p10/rule5.html>
