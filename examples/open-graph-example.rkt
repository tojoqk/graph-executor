#lang typed/racket

(require graph-executor)

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:transparent)

(: insert-money (-> Vending-State Vending-State))
(define (insert-money st)
  (let ([amount (prompt "How much(n*100)?" `(range 1 ,(quotient (v-state-wallet st) 100)))])
    (struct-copy v-state st
                 [wallet (- (v-state-wallet st) (* 100 amount))]
                 [inserted (+ (v-state-inserted st) (* 100 amount))])))

(: purchase (-> Integer (-> Vending-State Vending-State)))
(define ((purchase amount) st)
  (struct-copy v-state st
               [inserted (- (v-state-inserted st) amount)]))

(: reset-money (-> Vending-State Vending-State))
(define (reset-money st)
  (struct-copy v-state st
               [wallet (+ (v-state-wallet st) (v-state-inserted st))]
               [inserted 0]))

(: price-met? (-> Integer (-> Vending-State Boolean)))
(define ((price-met? price) st)
  (>= (v-state-inserted st) price))

(: can-insert? (-> Integer (-> Vending-State Boolean)))
(define ((can-insert? price) st)
  (<= price (v-state-wallet st)))

(: inserted? (-> Vending-State Boolean))
(define (inserted? st)
  (< 0 (v-state-inserted st)))

(: vending-graph (-> String
                     (Values (-> AnyNode (Code (-> Vending-State Any))
                                 (OpenGraph Vending-State))
                             (Node Vending-State))))
(define (vending-graph g)
  (define v-node ((inst node Vending-State (U 'start 'normal)) g))
  (define v-edge (inst edge Vending-State))
  (define v-bridge (inst dot-bridge Vending-State))
  (define v-graph (inst open-graph Vending-State))

  (define idle       (v-node "Idle (Accepting Coins)" #:type 'start))
  (define has-coins  (v-node "Selecting Item"         #:type 'normal))
  (define dispensing (v-node "Dispensing Item"        #:type 'normal))
  (define ret-change (v-node "Returning Change"       #:type 'normal))

  (values
   (lambda (output output-edge)
     (v-graph
      g
      #:edges
      (list
       (v-edge "Insert Money" #:from idle #:to has-coins
               #:when (code (can-insert? 100))
               #:trans (code insert-money))
       (v-edge "Insert More" #:from has-coins #:to has-coins
               #:when (code (can-insert? 100))
               #:trans (code insert-money))
       (v-edge "Purchase Drink (150 Yen)" #:from has-coins #:to dispensing
               #:when (code (price-met? 150))
               #:trans (code (purchase 150)))
       (v-edge "Dispense Done (Remaining Inserted)" #:mode 'auto #:from dispensing #:to has-coins
               #:when (code inserted?))
       (v-edge "Dispense Done (Just Zero)" #:mode 'auto #:from dispensing #:to idle
               #:when (code (negate inserted?)))
       (v-edge "Press Return Lever" #:from has-coins #:to ret-change
               #:when (code inserted?)
               #:trans (code reset-money))
       (v-edge "Change Dispatched" #:mode 'auto #:from ret-change #:to idle))
      #:bridges
      (list
       (v-bridge "Walk Away" #:from idle #:to output
                 #:trans output-edge
                 #:dot-minlen 3))))
   idle))

(struct terminal ([wallet : Integer])
  #:type-name Terminal
  #:transparent)

(: terminal-graph (-> String
                      (Values (-> (OpenGraph Terminal))
                              (Node Terminal))))
(define (terminal-graph g)
  (define t-node ((inst node Terminal (U 'entry 'terminal)) g))
  (define t-graph (inst open-graph Terminal))
  (define t-edge (inst edge Terminal))
  (define entry (t-node "Terminal Entry" #:type 'entry))
  (define terminal (t-node "Terminal" #:type 'terminal))

  (values
   (lambda ()
     (t-graph g
              #:edges
              (list
               (t-edge "Terminate" #:mode 'auto #:from entry #:to terminal))))
   entry))

(: vending-graph->terminal-graph (-> Vending-State Terminal))
(define (vending-graph->terminal-graph x)
  (terminal (v-state-wallet x)))

(: wire (-> (Values (Listof AnyGraph) AnyNode)))
(define (wire)
  (define v-any-graph (any-graph v-state?))
  (define t-any-graph (any-graph terminal?))
  (define t-any-node (any-node terminal?))
  (define v-any-node (any-node v-state?))
  (define-values (gen-t-graph t-entry)
    (terminal-graph "Terminal"))
  (define-values (gen-v-graph v-entry)
    (vending-graph "Vending Machine Model"))

  (values (list (v-any-graph (gen-v-graph (t-any-node t-entry)
                                          (code vending-graph->terminal-graph)))
                (t-any-graph (gen-t-graph)))
          (v-any-node v-entry)))

(module+ model
  (provide make-model)

  (: make-model (-> (Model Any)))
  (define (make-model)
    (define-values (graphs node-init) (wire))
    (define state-init (v-state 400 0))
    (model graphs node-init state-init)))

(module+ main
  (require racket/cmdline
           (submod ".." model))
  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))
  (define program-name "open-graph-example")
  (command-line
   #:program program-name
   #:once-any
   [("--console") "Run console" (set-box! mode 'console)]
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   #:args ()
   (define m (make-model))
   (case (unbox mode)
     [(dot) (render-dot (dot-renderer m))]
     [(console) (writeln (console-run m))])))

(module+ test
  (require typed/rackunit)
  (require (submod ".." model))

  (define m (make-model))
  (check-false (find-deadlock m (lambda ([n : (Node Any)])
                                  (symbol=? (node-type n) 'terminal))))
  (check-false (find-counterexample m
                                    (lambda (_n st)
                                      (or (not (v-state? st))
                                          (not (negative? (v-state-wallet st)))))))

  (check-equal? (find-counterexample m
                                     (lambda (_n st)
                                       (or (not (v-state? st))
                                           (not (zero? (v-state-wallet st))))))
                '((choose ("Insert More") (1)) (choose ("Insert More") (1)) (choose ("Insert More") (1)) (choose ("Insert Money") (1))))
  (check-equal? (find-counterexample m
                                     (lambda (_n st)
                                       (or (not (v-state? st))
                                           (not (zero? (v-state-wallet st)))))
                                     #:max-depth 1)
                '((choose ("Insert Money") (4))))

  (check-false (find-livelock m)))
