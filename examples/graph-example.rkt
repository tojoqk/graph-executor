#lang typed/racket

(require "../graph.rkt")
(provide vending-graph
         Vending-State
         v-state?
         v-state-wallet)

(define-type Vending-Node-Type (U 'start 'normal 'terminal))

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:transparent)

(: initial-state (-> Integer Vending-State))
(define (initial-state w)
  (v-state w 0))

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
                     (Values (Graph Vending-Node-Type Vending-State)
                             (-> Natural Vending-State)
                             (Node Vending-Node-Type Vending-State))))
(define (vending-graph graph-name)
  (define v-node ((inst node-maker Vending-Node-Type Vending-State) graph-name))
  (define v-edge (inst make-edge Vending-Node-Type Vending-State))
  (define v-graph (inst make-graph Vending-Node-Type Vending-State))

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
     (v-edge "Insert 100 Yen" #:mode 'choose #:dom idle #:cod has-coins
             #:when (can-insert? 100)
             #:trans (insert-money 100))
     (v-edge "Insert More" #:mode 'choose #:dom has-coins #:cod has-coins
             #:when (can-insert? 100)
             #:trans (insert-money 100))
     (v-edge "Purchase Drink (150 Yen)" #:mode 'choose #:dom has-coins #:cod dispensing
             #:when (price-met? 150)
             #:trans (purchase 150))
     (v-edge "Dispense Done (Remaining Inserted)" #:mode 'auto #:dom dispensing #:cod has-coins
             #:when inserted?)
     (v-edge "Dispense Done (Just Zero)" #:mode 'auto #:dom dispensing #:cod idle
             #:when (negate inserted?))
     (v-edge "Press Return Lever" #:mode 'choose #:dom has-coins #:cod ret-change
             #:when inserted?
             #:trans reset-money)
     (v-edge "Change Dispatched" #:mode 'auto #:dom ret-change #:cod idle)
     (v-edge "Walk Away" #:mode 'choose #:dom idle #:cod terminal)))
   initial-state
   idle))

(module+ main
  (require "../executor.rkt")
  (define-values (v-graph initial-state v-entry) (vending-graph "Vending Machine Model"))
  (repl-run (list v-graph) (initial-state 400) v-entry))
