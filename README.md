# Flame.Loyc
Flame.Loyc is an experimental [Loyc](https://github.com/qwertie/loyc) front-end for the [Flame compiler framework](https://github.com/jonathanvdc/Flame).

However, Flame.Loyc is very much a work in progress.
Everyday constructs like the import statement (`using` in C#)
and the member access (dot, `.`) operator have not been implemented yet.

The Flame.Loyc library itself is written in F#, and makes use of Flame.Functional.

## `fecs`
`fecs` is short for the Flame Enhanced C# compiler.
It uses Loyc's EC# parser and Lexical Macro Processor (LeMP) to 
parse and process EC# code, which is then transformed into
a Flame assembly by Flame.Loyc. A Flame back-end then
transforms this assembly into some output format.

Available back-ends (sorted in descending order of stability):
 * .NET assembly: `fecs <source files> -platform clr`
 * C++ source files: `fecs <source files> -platform c++`
 * Python source files: `fecs <source files> -platform python`
 
`fecs` interprets command-line arguments exactly like Flame's D# compiler, `dsc`.

For example, these commands are equivalent (iff `Test.ecs` and `Test.ds` are equivalent):

    fecs Test.ecs -platform clr
    dsc  Test.ds  -platform clr
