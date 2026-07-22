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
                     (Values (-> AnyNode (Code (-> Vending-State Any))
                                 (OpenGraph Vending-Node-Type Vending-State))
                             (Node Vending-Node-Type Vending-State))))
(define (vending-graph g)
  (define v-node ((inst node Vending-Node-Type Vending-State) g))
  (define v-edge (inst edge Vending-Node-Type Vending-State))
  (define v-bridge (inst dot-bridge Vending-Node-Type Vending-State))
  (define v-graph (inst open-graph Vending-Node-Type Vending-State))

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
       (v-edge "Insert Money" #:dom idle #:cod has-coins
               #:when (code (can-insert? 100))
               #:trans (code insert-money))
       (v-edge "Insert More" #:dom has-coins #:cod has-coins
               #:when (code (can-insert? 100))
               #:trans (code insert-money))
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
       (v-edge "Change Dispatched" #:mode 'auto #:dom ret-change #:cod idle))
      #:bridges
      (list
       (v-bridge "Walk Away" #:dom idle #:cod output
                 #:trans output-edge
                 #:dot-minlen 3))))
   idle))

(define-type Terminal-Node-Type 'terminal)

(struct terminal ([wallet : Integer])
  #:type-name Terminal
  #:transparent)

(: terminal-graph (-> String
                      (Values (-> (OpenGraph Terminal-Node-Type Terminal))
                              (Node Terminal-Node-Type Terminal))))
(define (terminal-graph g)
  (define t-node ((inst node Terminal-Node-Type Terminal) g))
  (define t-graph (inst open-graph Terminal-Node-Type Terminal))
  (define t-edge (inst edge Terminal-Node-Type Terminal))
  (define entry (t-node "Terminal Entry" #:type 'terminal))
  (define terminal (t-node "Terminal" #:type 'terminal))

  (values
   (lambda ()
     (t-graph g
              #:edges
              (list
               (t-edge "Terminate" #:mode 'auto #:dom entry #:cod terminal))))
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

(module+ console
  (require typed/pict typed/racket/gui)
  (provide make-system)

  (define-values (graphs node-init) (wire))
  (define state-init (v-state 400 0))

  (: make-system (-> (Values (->* () (Journal) Journal)
                             (->* () (Journal) DotWriter))))
  (define (make-system)
    (: writer (->* () (Journal) DotWriter))
    (define (writer [j '()])
      (let-values ([(_node _state h) (replay graphs node-init state-init j)])
        (dot-writer graphs node-init #:history h)))
    (: show (-> Journal Void))
    (define (show j)
      (let ([bmp (make-bitmap 1 1)])
        (render-dot (writer j) bmp)
        (show-pict (bitmap bmp) #:frame-style '() #:frame-x 0 #:frame-y 0)))
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
  (require racket/cmdline
           (submod ".." console))
  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))
  (define program-name "open-graph-example")
  (command-line
   #:program program-name
   #:once-any
   [("--console") "Run console" (set-box! mode 'console)]
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   #:args ()
   (define-values (run writer) (make-system))
   (case (unbox mode)
     [(dot) (write-dot (writer))]
     [(console) (writeln (run))])))
