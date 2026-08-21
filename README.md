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

## Installation

Install the library directly from GitHub using `raco`:

```shell
raco pkg install https://github.com/tojoqk/graph-executor.git
```

## Quick Start: The Water Jug Puzzle

![Water Jug Graph](examples/simple-water-jug.svg)

Here is a complete example of the classic Water Jug Puzzle (measuring exactly 4 gallons using 3-gallon and 5-gallon jugs).

This single file shows how to:
- Define states and transitions.
- Draw the graph with Graphviz (DOT).
- Play the game in your terminal.
- Test the model and find the shortest path automatically.

```racket
#lang typed/racket

(require graph-executor)

(struct jug-state ([left : Integer] [right : Integer])
  #:type-name Jug-State
  #:transparent)

(: jug-graph (-> String Positive-Integer Positive-Integer Positive-Integer
                 (Values (Graph Jug-State) (Node Jug-State))))
(define (jug-graph g left-cap right-cap target)
  (when (= left-cap right-cap)
    (error 'jug-graph "must not be same caps (~a, ~a)" left-cap right-cap))

  (: show-cleared (-> Jug-State Jug-State))
  (define (show-cleared st)
    (message (format "Congratulations! You made exactly ~a gallons!" target))
    st)

  (: fill-left (-> Jug-State Jug-State))
  (define (fill-left st)
    (message (format "Filled the ~a-gallon jug." left-cap))
    (struct-copy jug-state st [left left-cap]))

  (: fill-right (-> Jug-State Jug-State))
  (define (fill-right st)
    (message (format "Filled the ~a-gallon jug." right-cap))
    (struct-copy jug-state st [right right-cap]))

  (: empty-left (-> Jug-State Jug-State))
  (define (empty-left st)
    (message (format "Emptied the ~a-gallon jug." left-cap))
    (struct-copy jug-state st [left 0]))

  (: empty-right (-> Jug-State Jug-State))
  (define (empty-right st)
    (message (format "Emptied the ~a-gallon jug." right-cap))
    (struct-copy jug-state st [right 0]))

  (: pour-left-to-right (-> Jug-State Jug-State))
  (define (pour-left-to-right st)
    (let* ([amount (min (jug-state-left st) (- right-cap (jug-state-right st)))]
           [new-left (- (jug-state-left st) amount)]
           [new-right (+ (jug-state-right st) amount)])
      (message (format "Poured ~a gallons from ~aG to ~aG." amount left-cap right-cap))
      (struct-copy jug-state st [left new-left] [right new-right])))

  (: pour-right-to-left (-> Jug-State Jug-State))
  (define (pour-right-to-left st)
    (let* ([amount (min (jug-state-right st) (- left-cap (jug-state-left st)))]
           [new-right (- (jug-state-right st) amount)]
           [new-left (+ (jug-state-left st) amount)])
      (message (format "Poured ~a gallons from ~aG to ~aG." amount right-cap left-cap))
      (struct-copy jug-state st [right new-right] [left new-left])))

  (: can-fill-right? (-> Jug-State Boolean))
  (define (can-fill-right? st) (< (jug-state-right st) right-cap))

  (: can-fill-left? (-> Jug-State Boolean))
  (define (can-fill-left? st) (< (jug-state-left st) left-cap))

  (: can-empty-right? (-> Jug-State Boolean))
  (define (can-empty-right? st) (> (jug-state-right st) 0))

  (: can-empty-left? (-> Jug-State Boolean))
  (define (can-empty-left? st) (> (jug-state-left st) 0))

  (: can-pour-right-to-left? (-> Jug-State Boolean))
  (define (can-pour-right-to-left? st)
    (and (> (jug-state-right st) 0) (< (jug-state-left st) left-cap)))

  (: can-pour-left-to-right? (-> Jug-State Boolean))
  (define (can-pour-left-to-right? st)
    (and (> (jug-state-left st) 0) (< (jug-state-right st) right-cap)))

  (: is-cleared? (-> Jug-State Boolean))
  (define (is-cleared? st)
    (or (= (jug-state-left st) target)
        (= (jug-state-right st) target)))

  (: prompt-playing (-> Jug-State String))
  (define (prompt-playing st)
    (format "Goal: Make exactly ~aG\n  Current Status:\n    [ ~aG Jug: ~a/~a | ~aG Jug: ~a/~a ]\n  What will you do?"
            target
            left-cap (jug-state-left st) left-cap
            right-cap (jug-state-right st) right-cap))

  (define j-node (inst (node g) Jug-State (U 'puzzle 'check 'terminal)))
  (define j-edge (inst dot-edge Jug-State))
  (define j-graph (inst graph Jug-State))

  (define playing (j-node "Playing" #:type 'puzzle #:prompt (code prompt-playing)))
  (define check   (j-node "Check Clear" #:type 'check))
  (define cleared (j-node "Cleared!" #:type 'terminal #:trans (code show-cleared)))

  (values
   (j-graph g
            #:edges
            (list
             (j-edge (format "Fill ~aG" left-cap) #:from playing #:to check #:when (code can-fill-left?) #:trans (code fill-left))
             (j-edge (format "Fill ~aG" right-cap) #:from playing #:to check #:when (code can-fill-right?) #:trans (code fill-right))

             (j-edge (format "Empty ~aG" left-cap) #:from playing #:to check #:when (code can-empty-left?) #:trans (code empty-left))
             (j-edge (format "Empty ~aG" right-cap) #:from playing #:to check #:when (code can-empty-right?) #:trans (code empty-right))

             (j-edge (format "Pour ~aG -> ~aG" left-cap right-cap) #:from playing #:to check #:when (code can-pour-left-to-right?) #:trans (code pour-left-to-right))
             (j-edge (format "Pour ~aG -> ~aG" right-cap left-cap) #:from playing #:to check #:when (code can-pour-right-to-left?) #:trans (code pour-right-to-left))
             (j-edge "Not yet" #:mode 'auto #:from check #:to playing #:when (code (negate is-cleared?)))
             (j-edge "Clear!" #:mode 'auto #:from check #:to cleared #:when (code is-cleared?))))
   playing))

(module+ model
  (provide make-model)
  (: make-model (-> Positive-Integer Positive-Integer Positive-Integer (Model Jug-State)))
  (define (make-model left-cap right-cap target)
    (model
     (thunk
      (define-values (graph node-init) (jug-graph "Water Jug Puzzle" left-cap right-cap target))
      (values (list graph) node-init (jug-state 0 0))))))

(module+ main
  (require racket/cmdline
           (submod ".." model))

  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))

  (define program-name "water-jug")
  (command-line
   #:program program-name
   #:once-any
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   [("--console") "Run console" (set-box! mode 'console)]
   #:args ()
   (define m (make-model 3 5 4))
   (case (unbox mode)
     [(dot) (render-dot (dot-renderer m))]
     [(console)
      (parameterize ([current-console-commands (list (list 'quit 'q "Quit"))]
                     [current-console-trace-display 'hide])
        (writeln (console-run m)))])))

(module+ test
  (require typed/rackunit (submod ".." model))

  (define m (make-model 3 5 4))

  (: terminal-node? (-> (Node Jug-State) Boolean))
  (define (terminal-node? x) (eq? (node-type x) 'terminal))

  (check-false (find-livelock m))
  (check-false (find-deadlock m terminal-node?))
  (check-false (find-false-terminal m terminal-node?))
  (check-false (find-auto-conflict m))

  (check-false (find-counterexample m (lambda (_n [st : Jug-State])
                                        (<= 0 (jug-state-left st) 3))))
  (check-false (find-counterexample m (lambda (_n [st : Jug-State])
                                        (<= 0 (jug-state-right st) 5))))
  (check-false (find-counterexample m (lambda (_n [st : Jug-State])
                                        (<= (+ (jug-state-left st) (jug-state-right st))
                                            (+ 3 5)))))

  (: shortest-path (-> (Model Jug-State) (Option Journal)))
  (define (shortest-path m)
    (let loop : (Option Journal) ([depth : Natural 0])
      (find-deadlock m (negate terminal-node?)
                     #:bound depth
                     #:bounded (thunk (loop (add1 depth))))))

  (check-equal? (shortest-path m)
                '((auto ("Clear!"))
                  (choose ("Pour 5G -> 3G"))
                  (auto ("Not yet"))
                  (choose ("Fill 5G"))
                  (auto ("Not yet"))
                  (choose ("Pour 5G -> 3G"))
                  (auto ("Not yet"))
                  (choose ("Empty 3G"))
                  (auto ("Not yet"))
                  (choose ("Pour 5G -> 3G"))
                  (auto ("Not yet"))
                  (choose ("Fill 5G")))))
```

Draw the graph with Graphviz:

```shell
racket examples/simple-water-jug.rkt --dot | dot -Tpng -o water-jug.png
```


Play the game in your terminal:

```shell
racket examples/simple-water-jug.rkt --console
```

<details><summary>See console output: Interactive gameplay and the final Journal</summary>

```

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 0/3 | 5G Jug: 0/5 ]
  What will you do?
  - [1] Fill 3G
  - [2] Fill 5G
  - [q] Quit
? 1

Filled the 3-gallon jug.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 3/3 | 5G Jug: 0/5 ]
  What will you do?
  - [1] Fill 5G
  - [2] Empty 3G
  - [3] Pour 3G -> 5G
  - [q] Quit
? 3

Poured 3 gallons from 3G to 5G.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 0/3 | 5G Jug: 3/5 ]
  What will you do?
  - [1] Fill 3G
  - [2] Fill 5G
  - [3] Empty 5G
  - [4] Pour 5G -> 3G
  - [q] Quit
? 1

Filled the 3-gallon jug.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 3/3 | 5G Jug: 3/5 ]
  What will you do?
  - [1] Fill 5G
  - [2] Empty 3G
  - [3] Empty 5G
  - [4] Pour 3G -> 5G
  - [q] Quit
? 4

Poured 2 gallons from 3G to 5G.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 1/3 | 5G Jug: 5/5 ]
  What will you do?
  - [1] Fill 3G
  - [2] Empty 3G
  - [3] Empty 5G
  - [4] Pour 5G -> 3G
  - [q] Quit
? 3

Emptied the 5-gallon jug.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 1/3 | 5G Jug: 0/5 ]
  What will you do?
  - [1] Fill 3G
  - [2] Fill 5G
  - [3] Empty 3G
  - [4] Pour 3G -> 5G
  - [q] Quit
? 4

Poured 1 gallons from 3G to 5G.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 0/3 | 5G Jug: 1/5 ]
  What will you do?
  - [1] Fill 3G
  - [2] Fill 5G
  - [3] Empty 5G
  - [4] Pour 5G -> 3G
  - [q] Quit
? 1

Filled the 3-gallon jug.

* Goal: Make exactly 4G
  Current Status:
    [ 3G Jug: 3/3 | 5G Jug: 1/5 ]
  What will you do?
  - [1] Fill 5G
  - [2] Empty 3G
  - [3] Empty 5G
  - [4] Pour 3G -> 5G
  - [q] Quit
? 4

Poured 3 gallons from 3G to 5G.

Congratulations! You made exactly 4 gallons!

* Choose:
  - [q] Quit
? q
((auto ("Clear!")) (choose ("Pour 3G -> 5G")) (auto ("Not yet")) (choose ("Fill 3G")) (auto ("Not yet")) (choose ("Pour 3G -> 5G")) (auto ("Not yet")) (choose ("Empty 5G")) (auto ("Not yet")) (choose ("Pour 3G -> 5G")) (auto ("Not yet")) (choose ("Fill 3G")) (auto ("Not yet")) (choose ("Pour 3G -> 5G")) (auto ("Not yet")) (choose ("Fill 3G")))
```

</details>

Run tests and model checking:

```shell
raco test examples/simple-water-jug.rkt
```

## License

Apache License 2.0. See [LICENSE](LICENSE) for details.
