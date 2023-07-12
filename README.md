# Hasanlang
![Build Status](https://github.com/greenbush5/hasanlang/actions/workflows/build.yml/badge.svg)
![Language](https://img.shields.io/badge/Language-Rust-orange)
![Contributions](https://img.shields.io/badge/Contributions-Welcome-brightgreen)

Hasanlang is a work-in-progress programming language that aims to be a self-hosted, made for fun programming language. The goal of this project is to extend my knowledge about compilers, programming languages, and how they are made. This is nothing more than a side project *(perhaps only for now?)*

## Project Status
This project is in the early stages. The language specifications, parser, and basic compiler are under active development. Plans to become self-hosted are underway. Please take into account that this project may not get finished at all.

## Roadmap
- [x] Lexer
- [ ] Basic Parser
- [ ] Compiler
- [ ] Standard Library
- [ ] Self-hosting

## Getting Started
To set up Hasanlang, you will need to have [Rust](https://www.rust-lang.org/tools/install) and [LLVM 15.0](https://releases.llvm.org/download.html) on your system. Then you can clone this repository and build the project using `cargo`, the Rust package manager.

```bash
git clone https://github.com/greenbush5/hasanlang.git
cd hasanlang
cargo build
```

Additionally, you can install [Hasanlang syntax highlighting extension](https://github.com/greenbush5/hasanlang-syntax-extension) for VS Code. This is my first time making a syntax highlighting extension, so feel free to contribute to it as well!

## Contributing
I would love to have your help in making Hasanlang better. Contributions are very welcome.

Here are ways you can contribute:

- by reporting bugs
- by suggesting new features
- by writing or editing documentation
- by writing specifications
- by writing code and submitting pull requests

## License
Hasanlang is [MIT licensed](https://en.wikipedia.org/wiki/MIT_License).

## My current TODO list
- [ ] Remove `inputX.hsl` code samples from root directory
- [ ] Fix recursive expressions
- [ ] Add precedence to type cast operator
- [ ] Add enums
- [ ] Store a span in every AST node for error reporting
- [ ] Clarify `parse_..._expression` function calls *(call more specific functions instead)*
- [x] Move this into `README.md`