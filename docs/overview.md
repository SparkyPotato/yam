# Overview of the yam compiler infrastructure

The yam compiler is built around the needs of both a batch compiler and a language server, allowing for fully-featured and up-to-date IDE support at all time, and also allowing the compiler to use the LSP's refactoring support for diagnostics and fix suggestions.

## The pipeline

The compiler is heavily focused around incrementality, reducing the recomputation required on edit, keeping IDE responsiveness.

### Parsing

Source code is parsed into a concrete syntax tree (CST), with the entire file being reparsed on every edit. This may seem wasteful, but parsing even huge files is fast enough that the added headache of maintaining an incremental parser is not worth it.

### Indexing

To constrain change propogation through the incremental system, on every reparse, a public and private *index* is generated for each file. An index contains the symbols declared in the file for name resolution. 

The public index is used for name resolution ib modules outside the current one, while the private index is used for name resolution in the current module and its submodules. This allows us to save on re-running name resolution for *every* file whenever a new symbol is added.

### HIR

Next, the indices are used to generate HIR nodes for each item in the changed module. If visible symbols in the indices have changed, HIR is also generated for other modules affected by this change. 

HIR is a name-resolved and desugared tree-like IR. It is here that our IR transitions from being file-based to item based. Each item becomes it's own HIR node, and the workspace becomes a flat list of HIR nodes. This allows us to be incremental over each HIR node, instead of over each file. At the moment, we *do not* have more fine-grained incrementality than an HIR node.
