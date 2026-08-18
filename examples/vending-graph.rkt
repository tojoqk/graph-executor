#lang typed/racket

(require graph-executor)

(struct v-state ([wallet : Integer]
                 [inserted : Integer])
  #:type-name Vending-State
  #:transparent)

(: vending-graph (-> String
                     (Values (-> AnyNode (Code (-> Vending-State Any))
                                 (OpenGraph Vending-State))
                             (Node Vending-State))))
(define (vending-graph g)
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

  (define v-node (inst (node g) Vending-State (U 'start 'normal)))
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
               #:when (code (can-insert? 1))
               #:trans (code insert-money))
       (v-edge "Insert More" #:from has-coins #:to has-coins
               #:when (code (can-insert? 1))
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

(struct street ([wallet : Integer])
  #:type-name Street-State
  #:transparent)

(: street-graph (-> String
                    (Values
                     (-> AnyNode (Code (-> Street-State Any)) (OpenGraph Street-State))
                     (Node Street-State))))
(define (street-graph g)
  (define s-node (inst (node g) Street-State (U 'street 'terminal)))
  (define s-graph (inst open-graph Street-State))
  (define s-edge (inst edge Street-State))
  (define s-bridge (inst bridge Street-State))

  (define on-street (s-node "Street Entry" #:type 'street))
  (define bench (s-node "Bench (Rest)" #:type 'terminal))

  (values
   (lambda (output output-edge)
     (s-graph g
              #:edges
              (list
               (s-edge "Sit on Bench" #:mode 'choose #:from on-street #:to bench
                       #:priority -1))
              #:bridges
              (list
               (s-bridge "Go to Vending Machine" #:from on-street #:to output
                         #:trans output-edge))))
   on-street))

(: vending-graph->street-graph (-> Vending-State Street-State))
(define (vending-graph->street-graph x)
  (street (v-state-wallet x)))

(: street-graph->vending-graph (-> Street-State Vending-State))
(define (street-graph->vending-graph x)
  (v-state (street-wallet x) 0))

(: wire (-> (Values (Listof AnyGraph) AnyNode)))
(define (wire)
  (define v-any-graph (any-graph v-state?))
  (define t-any-graph (any-graph street?))
  (define t-any-node (any-node street?))
  (define v-any-node (any-node v-state?))
  (define-values (gen-t-graph t-entry)
    (street-graph "Street"))
  (define-values (gen-v-graph v-entry)
    (vending-graph "Vending Machine Model"))

  (values (list (v-any-graph (gen-v-graph (t-any-node t-entry)
                                          (code vending-graph->street-graph)))
                (t-any-graph (gen-t-graph (v-any-node v-entry)
                                          (code street-graph->vending-graph))))
          (t-any-node t-entry)))

(module+ model
  (provide make-model)

  (: make-model (-> (Model Any)))
  (define (make-model)
    (model
     (thunk
      (define-values (graphs node-init) (wire))
      (define state-init (street 300))
      (values graphs node-init state-init)))))

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

  (: terminal-node? (-> (Node Any) Any))
  (define (terminal-node? n)
    (symbol=? (node-type n) 'terminal))
  
  (define m (make-model))
  (check-false (find-livelock m))
  (check-false (find-deadlock m terminal-node?))
  (check-false (find-false-terminal m terminal-node?))
  (check-false (find-auto-conflict m))
  (check-false (find-counterexample m
                                    (negate (lambda (_n st)
                                              (and (v-state? st)
                                                   (negative? (v-state-wallet st)))))))

  (check-equal? (find-counterexample m (negate (lambda ([n : (Node Any)] st)
                                                 (and (terminal-node? n)
                                                      (street? st)
                                                      (= (street-wallet st) 0)))))
                `((choose ("Sit on Bench"))
                  (choose ("Walk Away"))
                  (auto ("Dispense Done (Just Zero)"))
                  (choose ("Purchase Drink (150 Yen)"))
                  (auto ("Dispense Done (Remaining Inserted)"))
                  (choose ("Purchase Drink (150 Yen)"))
                  ,@(make-list 299 '(choose ("Insert More") (1)))
                  (choose ("Insert Money") (1))
                  (choose ("Go to Vending Machine")))))
