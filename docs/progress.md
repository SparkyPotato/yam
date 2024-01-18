# Progress and quality of the compiler infrastructure

## Compiler

- [x] Lexer
- [x] Parser
  - [x] Recovery
- [ ] Package and library support
  - [ ] Resolve names to paths from other packages.
  - [ ] Use both source-trees and library files to access other packages.
- [ ] HIR and lowering
  - [x] Basic lowering
  - [ ] Resilience from CST
  - [ ] Proper name resolution
- [ ] Type inference
  - [x] Basic inference
  - [ ] Full operator support
  - [ ] Good error reporting
- [ ] Code generation
  - [x] Terrible Cranelift codegen.
  - [ ] Support for all features and types and control flow.
  - [ ] Compile to library files with metadata for importing.

## LSP
- [ ] Nothing :)
