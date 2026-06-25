#lang racket

(module vending-machine-example typed/racket
  (require "../graph.rkt")
  (provide vending-graph
           Vending-State
           (struct-out v-state))

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
  
  (: vending-graph (All (T S)
                        (-> String
                            (Node T S)
                            (-> Vending-State S)
                            (Values (OpenGraph Vending-Node-Type Vending-State T S)
                                    (Node Vending-Node-Type Vending-State)))))
  (define (vending-graph g output output-edge)
    (define v-node ((inst node-maker Vending-Node-Type Vending-State) g))
    (define v-edge (inst make-edge Vending-Node-Type Vending-State))
    (define v-bridge (inst make-bridge Vending-Node-Type Vending-State T S))
    (define v-graph (inst make-open-graph Vending-Node-Type Vending-State T S))

    (define idle       (v-node "Idle (Accepting Coins)" #:type 'start))
    (define has-coins  (v-node "Selecting Item"         #:type 'normal))
    (define dispensing (v-node "Dispensing Item"        #:type 'normal))
    (define ret-change (v-node "Returning Change"       #:type 'normal))

    (values
     (v-graph
      g
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
       (v-edge "Change Dispatched" #:mode 'auto #:dom ret-change #:cod idle))
      #:bridges
      (list
       (v-bridge "Walk Away" #:dom idle #:cod output
                 #:trans output-edge)))
     idle)))

(require (submod "." vending-machine-example))
(provide (all-from-out (submod "." vending-machine-example)))

(module terminal typed/racket
  (require "../graph.rkt")
  (provide terminal-graph
           Terminal terminal)

  (define-type Terminal-Node-Type 'terminal)

  (struct terminal ([wallet : Integer])
    #:type-name Terminal
    #:transparent)

  (: terminal-graph (-> String
                        (Values (Graph Terminal-Node-Type Terminal)
                                (Node Terminal-Node-Type Terminal))))
  (define (terminal-graph g)
    (define t-node ((inst node-maker Terminal-Node-Type Terminal) g))
    (define t-graph (inst make-graph Terminal-Node-Type Terminal))
    (define terminal (t-node "Terminated" #:type 'terminal))

    (values
     (t-graph g)
     terminal)))

(module vending-to-terminal typed/racket
  (require (submod ".." vending-machine-example))
  (require (submod ".." terminal))
  (provide vending-graph->terminal-graph)

  (: vending-graph->terminal-graph (-> Vending-State Terminal))
  (define (vending-graph->terminal-graph x)
    (terminal (v-state-wallet x))))

(require (submod "." terminal))
(require (submod "." vending-to-terminal))

(module+ main
  (require "../executor/repl.rkt")
  (require "../visualizer/dot.rkt")
  (require racket/cmdline)
  (define repl-mode (box #f))
  (command-line
   #:program "graph-example"
   #:once-each
   [("--repl") "Run repl" (set-box! repl-mode #t)]
   #:args ()
   (define-values (t-graph t-entry)
     (terminal-graph "Terminal"))
   (define-values (v-graph v-entry)
     (vending-graph "Vending Machine Model" t-entry vending-graph->terminal-graph))
   (if (unbox repl-mode)
       (let-values ([(state _)
                     (repl-run (list v-graph t-graph) (v-state 400 0) v-entry)])
         state)
       (write-dot (list t-graph v-graph) v-entry))))
