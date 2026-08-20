graph-executor
==============

A Typed Racket library for modeling, executing, visualizing, and model-checking directed graphs.

## Key Features

- **Unified Model**: Define a model once to enable execution, visualization, and model checking.
- **Modular Composition**: Build independent graphs with different state types, then connect them into a single model.
- **Reproducible Execution**: Every run is deterministic and can be replayed from journals.
- **Deterministic I/O**: Use `prompt` (input) and `message` (output) inside transitions; replaying with a journal remains a pure function.
- **Graph Visualization**: Render models to Graphviz (DOT) and highlight execution paths from journals.
- **Model Checking**: Detects deadlocks, livelocks, and invariant violations across states and `prompt` branches, outputting counterexamples as journals.

## Under the Hood

- **Zero Third-Party Dependencies**: Built only with standard Typed Racket. It requires no external packages.
- **Powered by Delimited Continuations**: Uses Racket's delimited continuations (`call-with-composable-continuation`) to implement internal effects like state, event logging (`emitter`), and nondeterministic computation (`amb`).
- **Dual Code Representation**: The `code` macro captures both executable functions and their S-expressions, keeping execution logic and DOT visualization in sync.
- **Journal-Driven Design**: Journals are core data structures, not just logs. They power deterministic replay, visualization, and counterexample generation.
