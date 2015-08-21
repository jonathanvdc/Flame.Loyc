# Flame.Loyc and `fecs`

## Flame.Loyc
Flame.Loyc is an experimental [Loyc](https://github.com/qwertie/loyc) front-end for the [Flame compiler framework](https://github.com/jonathanvdc/Flame).

Simple constructs such as classes and namespaces have already been implemented,
but more complex features, such as generics, have not.

The Flame.Loyc library itself is written in F#, and makes use of Flame.Functional, which is also an F# library.

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

## Building Flame.Loyc and `fecs`
Opening `Flame.Loyc.sln` in Visual Studio and hitting the "Build Solution" button should work fine if you have both a C# and an F# compiler installed.
