#lang typed/racket

(require graph-executor)

(provide vending-graph
         Vending-State
         v-state?
         v-state-wallet)

(define-type Vending-Node-Type (U 'start 'normal 'terminal))

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:prefab)

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
                     (Values (OpenGraph Vending-Node-Type Vending-State)
                             (Node Vending-Node-Type Vending-State))))
(define (vending-graph graph-name)
  (define v-node ((inst node-maker Vending-Node-Type Vending-State) graph-name))
  (define v-edge (inst make-dot-edge Vending-Node-Type Vending-State))
  (define v-graph (inst make-open-graph Vending-Node-Type Vending-State))

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
     (v-edge "Insert 100 Yen" #:dom idle #:cod has-coins
             #:when (can-insert? 100)
             #:trans (insert-money 100))
     (v-edge "Insert More" #:dom has-coins #:cod has-coins
             #:when (can-insert? 100)
             #:trans (insert-money 100))
     (v-edge "Purchase Drink (150 Yen)" #:dom has-coins #:cod dispensing
             #:when (price-met? 150)
             #:trans (purchase 150))
     (v-edge "Dispense Done (Remaining Inserted)" #:mode 'auto #:dom dispensing #:cod has-coins
             #:when inserted?)
     (v-edge "Dispense Done (Just Zero)" #:mode 'auto #:dom dispensing #:cod idle
             #:when (negate inserted?))
     (v-edge "Press Return Lever" #:dom has-coins #:cod ret-change
             #:when inserted?
             #:trans reset-money)
     (v-edge "Change Dispatched" #:mode 'auto #:dom ret-change #:cod idle)
     (v-edge "Walk Away" #:dom idle #:cod terminal #:dot-minlen 2)))
   idle))

(module+ main
  (require graph-executor
           racket/cmdline)
  (: console-mode (Boxof Boolean))
  (define console-mode (box #f))
  (command-line
   #:program "graph-example"
   #:once-each
   [("--console") "Run console" (set-box! console-mode #t)]
   #:args ()
   (define-values (v-graph node-init) (vending-graph "Vending Machine Model"))
   (define graphs (list (graph-close v-graph)))
   (if (unbox console-mode)
       (let ([state-init (v-state 400 0)])
         (let-values ([(node-current state-current journal)
                       (console-run graphs node-init state-init)])
           (pretty-write `((init (graph ,(node-graph-name node-init))
                                 (node ,(node-name node-init))
                                 (state ,state-init))
                           (current (graph ,(node-graph-name node-current))
                                    (node ,(node-name node-current))
                                    (state ,state-current))
                           (journal ,journal)))))
       (write-dot graphs node-init))))
