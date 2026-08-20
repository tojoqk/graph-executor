graph-executor
==============

A Typed Racket library for modeling, executing, visualizing, and model-checking directed graphs.

## Key Features

- **Unified Model**: Define a model once to enable execution, visualization, and model checking.
- **Reproducible Execution**: Every run is deterministic and can be replayed from journals.
- **Deterministic I/O**: Use `prompt` (input) and `message` (output) inside transitions; replaying with a journal remains a pure function.
- **Graph Visualization**: Render models to Graphviz (DOT) and highlight execution paths from journals.
- **Model Checking**: Detects deadlocks, livelocks, and invariant violations, outputting counterexamples as journals.
