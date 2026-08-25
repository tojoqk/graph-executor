#lang typed/racket

(require graph-executor)

(define-type Jug-State (Immutable-HashTable Positive-Integer Integer))

(: caps->jug-state (-> (Listof Positive-Integer) Jug-State))
(define (caps->jug-state caps)
  (for/hash : (Immutable-HashTable Positive-Integer Integer) ([cap (in-list caps)])
    (values cap 0)))

(: dup (-> (Listof Positive-Integer) (Option Positive-Integer)))
(define (dup np)
  (cond [(null? np) #f]
        [(member (car np) (cdr np)) => car]
        [else (dup (cdr np))]))

(: uniq (All (A) (-> (Listof A) (Listof A))))
(define (uniq xs)
  (cond [(null? xs) '()]
        [(member (car xs) (cdr xs)) (uniq (cdr xs))]
        [else (cons (car xs) (uniq (cdr xs)))]))

(: jug-graph (-> String (Listof Positive-Integer) Natural
                 (Values (Graph Jug-State) (Node Jug-State))))
(define (jug-graph g caps target)
  (: show-cleared (-> Jug-State Jug-State))
  (define (show-cleared st)
    (message (format "Congratulations! You made exactly ~a gallons!" target))
    st)

  (: fill (-> Positive-Integer (-> Jug-State Jug-State)))
  (define ((fill cap) st)
    (message (format "Filled the ~a-gallon jug." cap))
    (hash-set st cap cap))

  (: empty (-> Positive-Integer (-> Jug-State Jug-State)))
  (define ((empty cap) st)
    (message (format "Emptied the ~a-gallon jug." cap))
    (hash-set st cap 0))

  (: pour (-> Positive-Integer Positive-Integer (-> Jug-State Jug-State)))
  (define ((pour src dst) st)
    (let* ([amount (min (hash-ref st src) (- dst (hash-ref st dst)))]
           [new-src (- (hash-ref st src) amount)]
           [new-dst (+ (hash-ref st dst) amount)])
      (message (format "Poured ~a gallons from ~aG to ~aG." amount src dst))
      (hash-set (hash-set st src new-src) dst new-dst)))

  (: can-fill? (-> Positive-Integer (-> Jug-State Boolean)))
  (define ((can-fill? cap) st) (< (hash-ref st cap) cap))

  (: can-empty? (-> Positive-Integer (-> Jug-State Boolean)))
  (define ((can-empty? cap) st) (> (hash-ref st cap) 0))

  (: can-pour? (-> Positive-Integer Positive-Integer (-> Jug-State Boolean)))
  (define ((can-pour? src dst) st)
    (and (not (= src dst))
         (< 0 (hash-ref st src))
         (< (hash-ref st dst) dst)))

  (: is-cleared? (-> Jug-State Boolean))
  (define (is-cleared? st)
    (and (memf (lambda ([cap : Positive-Integer])
                 (= (hash-ref st cap) target))
               caps)
         #t))

  (: prompt-playing (-> Jug-State String))
  (define (prompt-playing st)
    (format "Goal: Make exactly ~aG\n  Current Status:\n~a\n  What will you do?"
            target
            (string-join (map (lambda ([cap : Positive-Integer])
                                (format "    - ~aG Jug: ~a/~a"
                                        cap (hash-ref st cap) cap))
                              caps)
                         "\n")))

  (define j-node (inst (node g) Jug-State (U 'puzzle 'check 'terminal)))
  (define j-edge (inst edge Jug-State))
  (define j-graph (inst graph Jug-State))

  (cond [(dup caps) => (lambda ([dup-cap : Positive-Integer])
                         (error 'jug-graph "must not be same caps (~a, ~a)" dup-cap dup-cap))])

  (define playing (j-node "Playing" #:type 'puzzle #:prompt (code prompt-playing)))
  (define check   (j-node "Check Clear" #:type 'check))
  (define cleared (j-node "Cleared!" #:type 'terminal #:trans (code show-cleared)))

  (values
   (j-graph g
            #:edges
            `(,@(for/list : (Listof (Edge Jug-State))
                          ([cap (in-list caps)])
                  (j-edge (format "Fill ~aG" cap)
                          #:from playing
                          #:to check
                          #:when (code (can-fill? cap))
                          #:trans (code (fill cap))))
              ,@(for/list : (Listof (Edge Jug-State))
                          ([cap (in-list caps)])
                  (j-edge (format "Empty ~aG" cap)
                          #:from playing
                          #:to check
                          #:when (code (can-empty? cap))
                          #:trans (code (empty cap))))
              ,@(for*/list : (Listof (Edge Jug-State))
                           ([cap1 (in-list caps)]
                            [cap2 (in-list caps)]
                            #:unless (= cap1 cap2))
                  (j-edge (format "Pour ~aG -> ~aG" cap1 cap2)
                          #:from playing
                          #:to check
                          #:when (code (can-pour? cap1 cap2))
                          #:trans (code (pour cap1 cap2))))
              ,(j-edge "Not yet" #:mode 'auto #:from check #:to playing #:when (code (negate is-cleared?)))
              ,(j-edge "Clear!" #:mode 'auto #:from check #:to cleared #:when (code is-cleared?))))
   playing))

(module+ model
  (provide make-model)
  (: make-model (-> (Listof Positive-Integer) Positive-Integer (Model Jug-State)))
  (define (make-model caps target)
    (model
     (thunk
      (define-values (graph node-init) (jug-graph "Water Jug Puzzle" caps target))
      (values (list graph) node-init (caps->jug-state caps))))))

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
   (define m (make-model '(3 5 7) 1))
   (case (unbox mode)
     [(dot) (render-dot m #:config (dot-config #:global
                                               (dot-global-config #:rankdir 'LR)))]
     [(console)
      (writeln (console-run m #:config (console-config
                                        #:commands (list (list 'quit 'q "Quit"))
                                        #:trace-display 'hide)))])))

(module+ test
  (require typed/rackunit (submod ".." model))

  (define m (make-model '(3 5 7) 1))

  (: terminal-node? (-> Node-Info Boolean))
  (define (terminal-node? x) (eq? (node-info-type x) 'terminal))

  (check-false (find-livelock m))
  (check-false (find-deadlock m terminal-node?))
  (check-false (find-false-terminal m terminal-node?))
  (check-false (find-auto-conflict m))

  (: shortest-path (-> (Model Jug-State) (Option Journal)))
  (define (shortest-path m)
    (let loop : (Option Journal) ([depth : Natural 0])
      (find-deadlock m (negate terminal-node?)
                     #:bound depth
                     #:bounded (thunk (loop (add1 depth))))))

  (check-equal? (shortest-path (make-model '(3 5 7) 1))
                '((auto ("Clear!"))
                  (choose ("Pour 5G -> 7G"))
                  (auto ("Not yet"))
                  (choose ("Pour 3G -> 7G"))
                  (auto ("Not yet"))
                  (choose ("Fill 5G"))
                  (auto ("Not yet"))
                  (choose ("Fill 3G")))))
