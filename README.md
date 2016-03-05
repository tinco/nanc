NANC
==============
Not ANSI C Compiler (pronounce: Nancy)

Nanc is a two phase project.

Phase one is to create a C compiler in Haskell that
conforms to specs in such a way that it compiles most well written C projects.

Phase two will be a fork of phase one. In it we will diverge from the C
spec in such a way as to create a 'better' dialect of C called Nancy C that is not backwards
compatible.

What Nancy C will actually look like is not really fully defined yet since
phase one is already a pretty large project.

Some ideas I hope Nancy C will have:

  - Sane namespaces (*no* insane namespaces)
  - Modules
  - No header files
  - Some sort of hygienic macro to replace preprocessor
  - Pointer asterisk on the type side
  - No dangling if/while/for body
  - Explicit switch fall-through with `continue`
  - Optional semicolons on ends of statements

Languages to take lessons from:

  - C#
  - Haskell
  - Rust
  - C++14+ (only the good parts)

Note that it is explicitly a goal for this compiler to *not* compile C++.

# Installation

You will need to have the specific version of the LLVM dev libraries installed
that is mentioned in `stack.yml`.

You will need to have Stack installed as found at: [https://www.stackage.org](Stackage)

When all dependencies are installed and this repository cloned locally, cd to
it and run `stack build`. It should run succesfully or give you instructions on
how to make it run succesfully. If it gives you any trouble please file an issue
here and I'll help you find out what's going wrong.

# Testing

Run `stack test --test-arguments --fail-fast`

To run the tests and fail at the first exception.

# FAQ

## Why?

Because C is horrible, and it's crazy that after so many years it is still the best
language for many use cases. There should be a modern language that is usable for
*every* current use case of C that has none of the weird 70's baggage that C has.

## Why Haskell?

Someone made a fully compliant C parser in Haskell already, and someone made a very
nice LLVM library for Haskell too. Writing the glue in between was almost too easy.

Also, Haskell is an excellent language for building compilers besides being an excellent
language in general.

