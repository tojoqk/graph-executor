#lang typed/racket

(require graph-executor
         graph-executor/effect/amb
         graph-executor/effect/emitter)

(: list->amb (All (A) (-> (-> (-> A) * A) (Listof A) A)))
(define (list->amb amb lst)
  (let loop : A ([lst lst])
    (if (null? lst)
        (amb)
        (amb (thunk (car lst))
             (thunk (loop (cdr lst)))))))

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

(define-type Command (U (List 'Fill Positive-Integer)
                        (List 'Empty Positive-Integer)
                        (List 'Pour Positive-Integer Positive-Integer)))
(define-predicate command-pour? (List 'Pour Positive-Integer Positive-Integer))

(define-type Command-Type (U 'Fill 'Empty 'Pour))

(define-predicate command-type? Command-Type)

(: command-type (-> Command (U 'Fill 'Empty 'Pour)))
(define (command-type cmd) (first cmd))

(: cap->symbol (-> Positive-Integer Symbol))
(define (cap->symbol c) (string->symbol (format "~aG" c)))

(: symbol->cap (-> Symbol Positive-Integer))
(define (symbol->cap c)
  (assert (string->number (car (string-split (symbol->string c) "G")))
          exact-positive-integer?))

(: command-arg1 (-> Command Positive-Integer))
(define (command-arg1 cmd) (second cmd))

(: command-arg2 (-> (List 'Pour Positive-Integer Positive-Integer) Positive-Integer))
(define (command-arg2 cmd) (third cmd))

(: jug-graph (-> String (Listof Positive-Integer) Natural
                 (Values (Graph Jug-State) (Node Jug-State))))
(define (jug-graph g caps target)
  (cond [(dup caps) => (lambda ([dup-cap : Positive-Integer])
                         (error 'jug-graph "must not be same caps (~a, ~a)" dup-cap dup-cap))])

  (define j-node (inst (node g) Jug-State (U 'puzzle 'check 'terminal)))
  (define j-edge (inst dot-edge Jug-State))
  (define j-graph (inst graph Jug-State))

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

  (: commands (-> Jug-State (Listof (U (List 'Fill Positive-Integer)
                                       (List 'Empty Positive-Integer)
                                       (List 'Pour Positive-Integer Positive-Integer)))))
  (define (commands st)
    (define-values (call-with-emitter emit)
      ((inst make-emitter (U (List 'Fill Positive-Integer)
                             (List 'Empty Positive-Integer)
                             (List 'Pour Positive-Integer Positive-Integer)))))
    (define-values (call-with-amb amb amb-fail)
      ((inst make-amb (U 'Fill 'Empty 'Pour Positive-Integer))))
    (car
     (call-with-emitter
      (thunk
       (call-with-amb
        (thunk
         (let ([cmd (amb (thunk 'Fill) (thunk 'Empty) (thunk 'Pour))])
           (assert cmd symbol?)
           (case cmd
             [(Fill) (let ([cap (assert (list->amb amb caps) number?)])
                       (when ((can-fill? cap) st)
                         (emit (list cmd cap))))]
             [(Empty) (let ([cap (assert (list->amb amb caps) number?)])
                        (when ((can-empty? cap) st)
                          (emit (list cmd cap))))]
             [(Pour) (let ([cap1 (assert (list->amb amb caps) number?)]
                           [cap2 (assert (list->amb amb caps) number?)])
                       (when ((can-pour? cap1 cap2) st)
                         (emit (list 'Pour cap1 cap2))))])
           (amb-fail))))))))

  (: update (-> Jug-State Jug-State))
  (define (update st)
    (let ([cmds (commands st)])
      (let ([type (prompt "Select Action:"
                          `(choose ,command-type? ,(uniq (map command-type cmds))))])
        (case type
          [(Fill) (let* ([fill-cmds (filter (lambda ([x : Command]) (eq? (car x) 'Fill)) cmds)]
                         [cap (symbol->cap
                               (prompt "Fill which jug?"
                                       `(choose ,(uniq (map (compose cap->symbol command-arg1) fill-cmds)))))])
                    ((fill cap) st))]
          [(Empty) (let* ([empty-cmds (filter (lambda ([x : Command]) (eq? (car x) 'Empty)) cmds)]
                          [cap (symbol->cap
                                (prompt "Empty which jug?"
                                        `(choose ,(uniq (map (compose cap->symbol command-arg1) empty-cmds)))))])
                     ((empty cap) st))]
          [(Pour) (let* ([pour-cmds (filter command-pour? cmds)]
                         [cap1 (symbol->cap
                                (prompt "Pour FROM which jug?"
                                        `(choose ,(uniq (map (compose cap->symbol command-arg1) pour-cmds)))))]
                         [pour-cap1-cmds (filter (lambda ([cmd : (List 'Pour Positive-Integer Positive-Integer)])
                                                   (= (command-arg1 cmd) cap1))
                                                 pour-cmds)]
                         [cap2 (symbol->cap
                                (prompt "Pour TO which jug?"
                                        `(choose ,(uniq (map (compose cap->symbol command-arg2) pour-cap1-cmds)))))])
                    ((pour cap1 cap2) st))]))))

  (define playing (j-node "Playing" #:type 'puzzle #:prompt (code prompt-playing)))
  (define check   (j-node "Check Clear" #:type 'check))
  (define cleared (j-node "Cleared!" #:type 'terminal #:trans (code show-cleared)))


  (values
   (j-graph g
            #:edges
            (list
             (j-edge "Make a Move"
                     #:from playing #:to check #:trans (code update))
             (j-edge "Not yet" #:mode 'auto #:from check #:to playing #:when (code (negate is-cleared?)))
             (j-edge "Clear!" #:mode 'auto #:from check #:to cleared #:when (code is-cleared?))))
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
   (define m (make-model '(3 5 7) 4))
   (case (unbox mode)
     [(dot) (render-dot (dot-renderer m))]
     [(console)
      (parameterize ([current-console-commands (list (list 'quit 'q "Quit"))]
                     [current-console-trace-display 'hide])
        (writeln (console-run m)))])))

(module+ test
  (require typed/rackunit (submod ".." model))

  (define m (make-model '(3 5 7) 1))

  (: terminal-node? (-> (Node Jug-State) Boolean))
  (define (terminal-node? x) (eq? (node-type x) 'terminal))

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

  (check-equal? (shortest-path m)
                '((auto ("Clear!"))
                  (choose ("Make a Move") (Pour) (5G) (7G))
                  (auto ("Not yet"))
                  (choose ("Make a Move") (Pour) (3G) (7G))
                  (auto ("Not yet"))
                  (choose ("Make a Move") (Fill) (5G))
                  (auto ("Not yet"))
                  (choose ("Make a Move") (Fill) (3G)))))
