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
                     (Values (Graph Vending-Node-Type Vending-State)
                             (Node Vending-Node-Type Vending-State))))
(define (vending-graph graph-name)
  (define v-node ((inst node Vending-Node-Type Vending-State) graph-name))
  (define v-edge (inst dot-edge Vending-Node-Type Vending-State))
  (define v-graph (inst graph Vending-Node-Type Vending-State))

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
             #:when (code (can-insert? 100))
             #:trans (code (insert-money 100)))
     (v-edge "Insert More" #:dom has-coins #:cod has-coins
             #:when (code (can-insert? 100))
             #:trans (code (insert-money 100)))
     (v-edge "Purchase Drink (150 Yen)" #:dom has-coins #:cod dispensing
             #:when (code (price-met? 150))
             #:trans (code (purchase 150)))
     (v-edge "Dispense Done (Remaining Inserted)" #:mode 'auto #:dom dispensing #:cod has-coins
             #:when (code inserted?))
     (v-edge "Dispense Done (Just Zero)" #:mode 'auto #:dom dispensing #:cod idle
             #:when (code (negate inserted?)))
     (v-edge "Press Return Lever" #:dom has-coins #:cod ret-change
             #:when (code inserted?)
             #:trans (code reset-money))
     (v-edge "Change Dispatched" #:mode 'auto #:dom ret-change #:cod idle)
     (v-edge "Walk Away" #:dom idle #:cod terminal #:dot-minlen 2)))
   idle))

(module+ console
  (require typed/racket/gui typed/pict)
  (provide make-system)

  (define-values (v-graph node-init) (vending-graph "Vending Machine Model"))
  (define graphs (list v-graph))
  (define state-init (v-state 400 0))

  (: make-system (-> (Values (->* () (Journal) Journal)
                             (->* () (Journal) (-> Output-Port Void)))))
  (define (make-system)
    (: writer (->* () (Journal) (-> Output-Port Void)))
    (define (writer [j '()])
      (let-values ([(_node _state h) (replay graphs node-init state-init j)])
        (make-dot-writer graphs node-init #:history h)))
    (: show (-> Journal Void))
    (define (show j)
      (show-pict (dot-writer->pict (writer j)) #:frame-style '() #:frame-x 0 #:frame-y 0))
    (: run (->* () (Journal) Journal))
    (define (run [j '()])
      (parameterize ([current-console-commands (list* (list 'action 'r "Render Graph" show)
                                                      (current-console-commands))]
                     [current-eventspace (make-eventspace)])
        (let-values ([(_node _state j-result)
                      (console-run graphs node-init state-init #:journal j)])
          j-result)))
    (values run writer)))

(module+ main
  (require racket/cmdline)
  (require (submod ".." console))
  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))
  (define program-name "graph-example")
  (command-line
   #:program program-name
   #:once-any
   [("--console") "Run console" (set-box! mode 'console)]
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   #:args ()
   (define-values (run writer) (make-system))
   (case (unbox mode)
     [(dot) ((writer) (current-output-port))]
     [(console) (writeln (run))])))
