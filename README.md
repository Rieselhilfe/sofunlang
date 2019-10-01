# ((λ)) Sofun: a Stack-Oriented FUNctional language
An Interpreter for Sofun (Stack-Oriented FUNctional programming language) written in haskell.
Sofun is not a programming language meant for daily usage, but to test a rather exotic combination of two paradigms:
* Stacks are used not only to store data, no, a whole sofun program consists of only **one nested stack**
* Functional concepts: everything is a function, functions can be composed, functions can be passed as arguments, there is some kind of partial application

This is the new haskell interpreter and REPL for sofun,
improvements over the c++ version include:
* More than 50 times faster
* Now with a basic type system
* More and better error messages, less edge cases
* far more elegant code ;)
* several tools to make working with the repl easier (a reworked debug mode, command to print out function definitions of loaded files)
* better naming of built-in functions
