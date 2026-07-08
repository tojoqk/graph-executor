#lang typed/racket

(require graph-executor)

(define-type Vending-Node-Type (U 'start 'normal 'terminal))

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:transparent)

(: insert-money (-> Vending-State Vending-State))
(define (insert-money st)
  (let ([amount (prompt "How much?" `(range 1 ,(v-state-wallet st)))])
    (struct-copy v-state st
                 [wallet (- (v-state-wallet st) amount)]
                 [inserted (+ (v-state-inserted st) amount)])))

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
                     (Node Any Any)
                     (-> Vending-State Any)
                     (Values (OpenGraph Vending-Node-Type Vending-State)
                             (Node Vending-Node-Type Vending-State))))
(define (vending-graph g output output-edge)
  (define v-node ((inst node-maker Vending-Node-Type Vending-State) g))
  (define v-edge (inst make-edge Vending-Node-Type Vending-State))
  (define v-bridge (inst make-dot-bridge Vending-Node-Type Vending-State))
  (define v-graph (inst make-open-graph Vending-Node-Type Vending-State))

  (define idle       (v-node "Idle (Accepting Coins)" #:type 'start))
  (define has-coins  (v-node "Selecting Item"         #:type 'normal))
  (define dispensing (v-node "Dispensing Item"        #:type 'normal))
  (define ret-change (v-node "Returning Change"       #:type 'normal))

  (values
   (v-graph
    g
    #:edges
    (list
     (v-edge "Insert Money" #:dom idle #:cod has-coins
             #:when (can-insert? 100)
             #:trans insert-money)
     (v-edge "Insert More" #:dom has-coins #:cod has-coins
             #:when (can-insert? 100)
             #:trans insert-money)
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
               #:trans output-edge
               #:dot-minlen 3)))
   idle))

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
  (define t-edge (inst make-edge Terminal-Node-Type Terminal))
  (define entry (t-node "Terminal Entry" #:type 'terminal))
  (define terminal (t-node "Terminal" #:type 'terminal))

  (values
   (t-graph g
            #:edges
            (list
             (t-edge "Terminate" #:mode 'auto #:dom entry #:cod terminal)))
   entry))

(: vending-graph->terminal-graph (-> Vending-State Terminal))
(define (vending-graph->terminal-graph x)
  (terminal (v-state-wallet x)))

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
   (define-values (t-graph t-entry)
     (terminal-graph "Terminal"))
   (define-values (v-graph node-init)
     (vending-graph "Vending Machine Model"
                    (node->any-node t-entry terminal?)
                    vending-graph->terminal-graph))
   (: graphs (Listof (Graph Any Any)))
   (define graphs (list (open-graph->any-graph v-graph v-state?)
                        (graph->any-graph t-graph terminal?)))
   (if (unbox console-mode)
       (let ([state-init (v-state 400 0)])
         (let-values ([(node-current state-current history)
                       (console-run graphs
                                    (node->any-node node-init v-state?)
                                    state-init)])
           (pretty-write `((init (graph ,(node-graph-name node-init))
                                 (node ,(node-name node-init))
                                 (state ,state-init))
                           (current (graph ,(node-graph-name node-current))
                                    (node ,(node-name node-current))
                                    (state ,state-current))
                           (journal ,@(history->journal history))))))
       (write-dot graphs (node->any-node node-init v-state?)))))
