# BSPModel

A typed model of bulk-synchronous parallel (BSP) computation with a staged compiler for parallel stateful workloads, implemented in Scala.

This is the implementation behind:

> Zilu Tian, Dan Olteanu, and Christoph Koch. *"Using process calculus for optimizing data and computation sharing in complex stateful parallel computations."* Proc. ACM Manag. Data (SIGMOD), 2025.

The compiler formalism statically analyzes and transforms parallel stateful workloads, eliminating redundant synchronization — in the paper, a 10x speedup in distributed execution.

## Layout

- `core/` — the typed core model: graphs, partitions, scopes, compute methods, and partition messages.
- `opt/` — staged expressions and optimization passes (double buffering, data & computation sharing), with worked examples (Game of Life, SIR epidemic model) and tests.

## Build

sbt project; see `build.sbt` for the module layout.
