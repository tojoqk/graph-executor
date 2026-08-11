#lang typed/racket

(require graph-executor)

(provide vending-graph
         Vending-State
         v-state?
         v-state-wallet)

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:transparent)

(: insert-money (-> Integer (-> Vending-State Vending-State)))
(define ((insert-money amount) st)
  (struct-copy v-state st
               [wallet (- (v-state-wallet st) amount)]
               [inserted (+ (v-state-inserted st) amount)]))

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
                     (Values (Graph Vending-State)  (Node Vending-State))))
(define (vending-graph graph-name)
  (define v-node ((inst node Vending-State (U 'start 'normal 'terminal)) graph-name))
  (define v-edge (inst dot-edge Vending-State))
  (define v-graph (inst graph Vending-State))

  (define idle       (v-node "Idle (Accepting Coins)" #:type 'start))
  (define has-coins  (v-node "Selecting Item"         #:type 'normal))
  (define dispensing (v-node "Dispensing Item"        #:type 'normal))
  (define ret-change (v-node "Returning Change"       #:type 'normal))
  (define terminal   (v-node "Terminal"               #:type 'terminal))

  (values
   (v-graph
    graph-name
    #:edges
    (list
     (v-edge "Insert 100 Yen" #:from idle #:to has-coins
             #:when (code (can-insert? 100))
             #:trans (code (insert-money 100)))
     (v-edge "Insert More" #:from has-coins #:to has-coins
             #:when (code (can-insert? 100))
             #:trans (code (insert-money 100)))
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
     (v-edge "Change Dispatched" #:mode 'auto #:from ret-change #:to idle)
     (v-edge "Walk Away" #:from idle #:to terminal #:dot-minlen 2)))
   idle))

(module+ system
  (provide make-system)

  (define-values (v-graph node-init) (vending-graph "Vending Machine Model"))
  (define graphs (list v-graph))
  (define state-init (v-state 400 0))
  (define-type S Vending-State)

  (: make-system (-> (Values (->* () (Journal) Journal)
                             (->* () (Journal) DotRenderer)
                             (-> (-> Status (Node S) S Any) [#:max-depth Natural] (Option Journal)))))
  (define (make-system)
    (: renderer (->* () (Journal) DotRenderer))
    (define (renderer [j '()])
      (let-values ([(_node _state h) (replay graphs node-init state-init j)])
        (dot-renderer graphs node-init #:history h)))
    (: run (->* () (Journal) Journal))
    (define (run [j '()])
      (console-run graphs node-init state-init #:journal j))
    (: find-cex (-> (-> Status (Node S) S Any) [#:max-depth Natural] (Option Journal)))
    (define (find-cex invariant #:max-depth [max-depth #f])
      (find-counterexample graphs node-init state-init invariant #:max-depth max-depth))
    (values run renderer find-cex)))

(module+ main
  (require racket/cmdline)
  (require (submod ".." system))
  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))
  (define program-name "graph-example")
  (command-line
   #:program program-name
   #:once-any
   [("--console") "Run console" (set-box! mode 'console)]
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   #:args ()
   (define-values (run renderer _find-cex) (make-system))
   (case (unbox mode)
     [(dot) (render-dot (renderer))]
     [(console) (writeln (run))])))

(module+ test
  (require typed/rackunit)
  (require (submod ".." system))

  (define-values (_run _renderer find-cex) (make-system))
  (check-false (find-cex (lambda (_s _n [st : Vending-State])
                           (not (negative? (v-state-wallet st))))))
  (check-equal? (find-cex (lambda (_s _n [st : Vending-State])
                            (< 100 (v-state-wallet st))))
                '((choose ("Insert More")) (choose ("Insert More")) (choose ("Insert 100 Yen")))))
