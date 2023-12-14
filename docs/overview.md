# Overview of the yam compiler infrastructure

The yam compiler is built around the needs of both a batch compiler and a language server, allowing for fully-featured and up-to-date IDE support at all time, and also allowing the compiler to use the LSP's refactoring support for diagnostics and fix suggestions.

## The pipeline

The compiler is heavily focused around incrementality, reducing the recomputation required on edit, keeping IDE responsiveness.

### Parsing

Source code is parsed into a concrete syntax tree (CST), with the entire file being reparsed on every edit. This may seem wasteful, but parsing even huge files is fast enough that the added headache of maintaining an incremental parser is not worth it.

### Indexing

To constrain change propogation through the incremental system, on every reparse, a public and private *index* is generated for each file. An index contains the symbols declared in the file for name resolution. 

The public index is used for name resolution in modules outside the current one, while the private index is used for name resolution in the current module and its submodules. This allows us to save on re-running name resolution for *every* file whenever a new symbol is added.

Imports and reimports are left as-is in the indices because we only have local knowledge at the moment.

### Canonicalization

After indexing, a module tree is generated, storing each index pair (public and private).

This module tree is then used to canonicalize all indices and generate a new tree, where each index now knows the absolute paths of everything visible.

Module trees are recursively tracked trees, which gives us an interesting benefit for free - since IDs are stable, and only IDs used are tracked, we only build a dependency on parts of the tree we search through.

### HIR

Next, the canonicalized tree is used to generate HIR nodes for each item in the changed module. If visible symbols in the indices have changed, HIR is also generated for other modules affected by this change. 

HIR is a name-resolved and desugared tree-like IR. It is here that our IR transitions from being file-based to item based. Each item becomes it's own HIR node, and the workspace becomes a flat list of HIR nodes. This allows us to be incremental over each HIR node, instead of over each file. At the moment, we *do not* have more fine-grained incrementality than an HIR node.

### THIR
Once HIR for each item has been generated, we type check in two stages:
1. Declaration - go through each item's 'signature' and generate relevant (interned) types.
2. Definition - go through each item's 'body' and infer types.

#### Declaration
Structs generate a type describing themselves, as well as the type of each field. 
Enums generate the integer lang item representation of themselves. 
Functions generate a type describing themselves, as well as the type of each parameter and the return type.
Static generate their own type, and so do type aliases.

#### Definition
Using the type declarations of each item, function bodies and static initializers are type checked with constraint-based type inference.
While going through the whole expression tree, constraints are generated between partially-resolved types, and then solved at the end to generate a fully-resolved type.

Constraints are solved in a loop unless a whole pass goes through without any constraints being solved. 
Then, constraints are 'finalized', where they can default to a type (for example, integer literals default to `i32`).
After that, the constraint solve loop is repeated until no more constraints are solved.
After this loop, any unsolved constraints are reported as errors.

## Avoiding span invalidation

If we were to store spans directly in HIR nodes, we would force a recomputation every time a span changes.

Instead, we build a stable side-channel that stores CST nodes, and store these IDs in HIR. We also base diagnostics on these IDs so even the diagnostics generated are always valid.
