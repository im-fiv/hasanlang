# Hasanlang
![Build Status](https://github.com/greenbush5/hasanlang/actions/workflows/build.yml/badge.svg)
![Language](https://img.shields.io/badge/Language-Rust-orange)
![Contributions](https://img.shields.io/badge/Contributions-Welcome-brightgreen)

Hasanlang is a work-in-progress programming language, made for fun, that aims to be self-hosted. The goal of this project is to extend my knowledge about compilers, programming languages, and how they are made. Currently, this is my primary project.

## Project Status
This project is in the early stages. The language specifications, parser, and basic compiler are under active development. Plans to become self-hosted are underway. I'm not certain whether I will have the will to finish this, but as it seeems, the likelihood of giving up is relatively low.

## Roadmap
- [x] Pest Parser
- [x] Corrective Parser
- [ ] Semantic Analysis
- [ ] Compiler
- [ ] Standard Library
- [ ] Language Server
- [ ] Self-hosting

## Getting Started
To set up Hasanlang, you will need to have [Rust](https://www.rust-lang.org/tools/install) and [LLVM 15.0](https://releases.llvm.org/download.html) on your system. Then you can clone this repository and build the project using `cargo`, the Rust package manager.

```bash
git clone https://github.com/greenbush5/hasanlang.git
cd hasanlang
cargo build
```

Additionally, you can install [Hasanlang syntax highlighting extension](https://github.com/greenbush5/hasanlang-syntax-extension) for VS Code. This is my first time making a syntax highlighting extension, so feel free to contribute to this extension as well!

## Contributing
I would love to have your help in making Hasanlang better, contributions are very welcome.

Here are ways you can contribute:

- by reporting bugs
- by suggesting new features
- by writing or editing documentation
- by writing specifications
- by writing code and submitting pull requests

## Bugs
As I said before, this is my first time making a project as complex as this, so my code is particularly prone to errors. Due to the complexity of this project, your help in finding and/or fixing bugs would significantly contribute to its advancement. With your help, this entire project may become possible to complete!

## License
Hasanlang is [MIT licensed](https://en.wikipedia.org/wiki/MIT_License).

## My current TODO list

### Compiler:

- [ ] Exhaustive expression compiling
- [ ] Exhaustive statement compiling
- [ ] Bug fixes
  - [ ] Nested functions not working
  - [ ] Constant string variables causing the compiler to crash
- [ ] `if` statements
- [ ] `for` and `while` loops
- [ ] Automatic `return` termination

### Semantic Analysis:

- [ ] Modifiers validation
- [ ] Type resolving
- [ ] Interfaces validation
  - [ ] Implementation validation
  - [ ] Type's implemented interfaces map
- [ ] Function return type validation
- [ ] Function call argument types validation
- [ ] Generics validation
  - [ ] Definitions
  - [ ] Calls
- [ ] Imports resolving
- [ ] Name collision checking
  - [ ] Modules
  - [ ] Classes and variables
  - [ ] Types
  - [ ] Function arguments
  - [ ] Generic parameters
- [ ] Undefined identifiers checking
- [ ] Type alias substitution
- [ ] Circular references checking
- [ ] Circular imports checking
- [ ] `return` statement checking
- [ ] `break` statement checking

### Corrective Parser:

- [x] Module system
  - [x] Imports *(entire module, specific items, wildcard imports, renamed imports)*
  - [x] Declarations
- [x] Interfaces *(definitions, implementations, and type annotations)*
- [x] Constants
- [x] Implement `for` statements
- [x] Separate `public`, `private`, and `static` from class function attributes
- [x] Rename function definition arguments into function_arguments
- [x] Rewrite `parse_function_header` to use `parse_function_definition_arguments`
- [x] Add support for floats
- [x] Implement anonymous functions
- [x] Add more unary operators
- [x] Implement error reporting
- [x] Implement `if` and `while` statements
- [x] Implement booleans
- [x] Implement enums
- [x] Make tests check the generated AST
- [x] ~~Add raw types *(direct LLVM types, should not have any methods on them, intended for compiler usage only)* that can be denoted with an exclamation mark: `AType!`, `ArrayType![]`, `GenericType!<...>`, `GenericArrType!<...>[]`~~
- [x] Add type cast operator to *recursive* expressions
- [x] Fix *recursive* expressions
- [x] Remove `inputX.hsl` code samples from root directory
- [x] Move this into `README.md`

## Side tasks

### Corrective Parser:
- [ ] Overhaul the CLI
- [ ] Add spans for better semantic analysis error reporting
- [x] Allow for generics in function type annotation
- [x] Rewrite `parse_type` function along with the grammar of types