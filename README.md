# Lox Tree-Walk Interpreter

This project is an implementation of the Lox tree-walk interpreter from [Crafting Interpreters](https://craftinginterpreters.com/). The original book uses Java for the first part, but what is a sane developer's Java? Haskell, of course! It would be nice to implement the second part, which is originally in C, in Rust. As we all know, a good developer's C is Rust.

## Special Thanks

Special thanks to [Serhii](https://github.com/lsap) for all the support during this project.

## Running the Project

This project uses Cabal, a build system for Haskell. To run the project, follow these steps:

1. Install Cabal if you haven't already.
2. Navigate to the project directory.
3. Run `cabal build` to build the project.
4. Run `cabal run` to execute the project.

## Current Progress

These are the chapters from the book, I'll be using this to track progress:

- [x] [Scanning](https://craftinginterpreters.com/scanning.html)
- [x] [Representing Code](https://craftinginterpreters.com/representing-code.html)
- [x] [Parsing Expressions](https://craftinginterpreters.com/parsing-expressions.html)
- [x] [Evaluating Expressions](https://craftinginterpreters.com/evaluating-expressions.html)
- [x] [Statements and State](https://craftinginterpreters.com/statements-and-state.html)
- [x] [Control Flow](https://craftinginterpreters.com/control-flow.html)
- [x] [Functions](https://craftinginterpreters.com/functions.html)
- [x] [Resolving and Binding](https://craftinginterpreters.com/resolving-and-binding.html) (Won't do resolving, since my implementation works properly already)
- [ ] ~~[Classes](https://craftinginterpreters.com/classes.html)~~ The book says I can skip it if I don't like writing classes, and I don't!!!
- [ ] ~~[Inheritance](https://craftinginterpreters.com/inheritance.html)~~

Moving on to the JIT part in Rust!
