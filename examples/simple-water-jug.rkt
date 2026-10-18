#lang typed/racket

(require graph-executor)

(struct jug-state ([left : Integer] [right : Integer])
  #:type-name Jug-State
  #:transparent)

(: jug-graph (-> String Positive-Integer Positive-Integer Positive-Integer
                 (Values (Graph Jug-State) (Node Jug-State))))
(define (jug-graph g left-cap right-cap target)
  (when (= left-cap right-cap)
    (error 'jug-graph "must not be same caps (~a, ~a)" left-cap right-cap))

  (: pour-left-to-right/message (-> Jug-State Any))
  (define (pour-left-to-right/message st)
    (let ([amount (min (jug-state-left st) (- right-cap (jug-state-right st)))])
      (message (format "Poured ~a gallons from ~aG to ~aG." amount left-cap right-cap))))

  (: pour-right-to-left/message (-> Jug-State Any))
  (define (pour-right-to-left/message st)
    (let ([amount (min (jug-state-right st) (- left-cap (jug-state-left st)))])
      (message (format "Poured ~a gallons from ~aG to ~aG." amount right-cap left-cap))))

  (: prompt-playing (-> Jug-State String))
  (define (prompt-playing st)
    (format "Goal: Make exactly ~aG\n  Current Status:\n    [ ~aG Jug: ~a/~a | ~aG Jug: ~a/~a ]\n  What will you do?"
            target
            left-cap (jug-state-left st) left-cap
            right-cap (jug-state-right st) right-cap))

  (: can-fill-left-jug? (-> Jug-State Boolean))
  (define (can-fill-left-jug? st) (< (jug-state-left st) left-cap))

  (: can-fill-right-jug? (-> Jug-State Boolean))
  (define (can-fill-right-jug? st) (< (jug-state-right st) right-cap))

  (: can-empty-left-jug? (-> Jug-State Boolean))
  (define (can-empty-left-jug? st) (> (jug-state-left st) 0))

  (: can-empty-right-jug? (-> Jug-State Boolean))
  (define (can-empty-right-jug? st) (> (jug-state-right st) 0))

  (: can-pour-left-to-right? (-> Jug-State Boolean))
  (define (can-pour-left-to-right? st)
    (and (> (jug-state-left st) 0) (< (jug-state-right st) right-cap)))

  (: can-pour-right-to-left? (-> Jug-State Boolean))
  (define (can-pour-right-to-left? st)
    (and (> (jug-state-right st) 0) (< (jug-state-left st) left-cap)))

  (: goal-not-reached? (-> Jug-State Boolean))
  (define (goal-not-reached? st)
    (and (not (= (jug-state-left st) target))
         (not (= (jug-state-right st) target))))

  (: goal-reached? (-> Jug-State Boolean))
  (define (goal-reached? st)
    (or (= (jug-state-left st) target)
        (= (jug-state-right st) target)))

  (: fill-left-jug (-> Jug-State Jug-State))
  (define (fill-left-jug st) (jug-state left-cap (jug-state-right st)))

  (: fill-right-jug (-> Jug-State Jug-State))
  (define (fill-right-jug st) (jug-state (jug-state-left st) right-cap))

  (: empty-left-jug (-> Jug-State Jug-State))
  (define (empty-left-jug st) (jug-state 0 (jug-state-right st)))

  (: empty-right-jug (-> Jug-State Jug-State))
  (define (empty-right-jug st) (jug-state (jug-state-left st) 0))

  (: pour-left-to-right (-> Jug-State Jug-State))
  (define (pour-left-to-right st)
    (let ([a (min (jug-state-left st) (- right-cap (jug-state-right st)))])
      (jug-state (- (jug-state-left st) a) (+ (jug-state-right st) a))))

  (: pour-right-to-left (-> Jug-State Jug-State))
  (define (pour-right-to-left st)
    (let ([a (min (- left-cap (jug-state-left st)) (jug-state-right st))])
      (jug-state (+ (jug-state-left st) a) (- (jug-state-right st) a))))

  (define-values (make-node make-graph) (graph-maker g))
  (define node (inst make-node Jug-State (U 'puzzle 'check 'terminal)))
  (define edge (inst make-edge Jug-State))
  (define graph (inst make-graph Jug-State))

  (define playing (node "Playing" #:type 'puzzle #:prompt (code prompt-playing)))
  (define check   (node "Check Clear" #:type 'check))
  (define cleared (node "Cleared!"
                        #:type 'terminal
                        #:tags '(cleared)
                        #:before
                        (code
                         (lambda (_)
                           (message
                            (format "Congratulations! You made exactly ~a gallons!" target))))))

  (values
   (graph #:edges
          (list
           (edge (format "Fill ~aG" left-cap) #:from playing #:to check
                 #:when (code can-fill-left-jug?)
                 #:trans (code fill-left-jug)
                 #:before (code (lambda (_)
                                  (message (format "Filled the ~a-gallon jug." left-cap)))))
           (edge (format "Fill ~aG" right-cap) #:from playing #:to check
                 #:when (code can-fill-right-jug?)
                 #:trans (code fill-right-jug)
                 #:before (code (lambda (_)
                                  (message (format "Filled the ~a-gallon jug." right-cap)))))
           (edge (format "Empty ~aG" left-cap) #:from playing #:to check
                 #:when (code can-empty-left-jug?)
                 #:trans (code empty-left-jug)
                 #:before (code (lambda (_)
                                  (message (format "Emptied the ~a-gallon jug." left-cap)))))
           (edge (format "Empty ~aG" right-cap) #:from playing #:to check
                 #:when (code can-empty-right-jug?)
                 #:trans (code empty-right-jug)
                 #:before (code (lambda (_)
                                  (message (format "Emptied the ~a-gallon jug." right-cap)))))
           (edge (format "Pour ~aG -> ~aG" left-cap right-cap) #:from playing #:to check
                 #:when (code can-pour-left-to-right?)
                 #:trans (code pour-left-to-right)
                 #:before (code pour-left-to-right/message))
           (edge (format "Pour ~aG -> ~aG" right-cap left-cap) #:from playing #:to check
                 #:when (code can-pour-right-to-left?)
                 #:trans (code pour-right-to-left)
                 #:before (code pour-right-to-left/message))
           (edge "Not yet" #:mode 'auto #:from check #:to playing
                 #:when (code goal-not-reached?))
           (edge "Clear!" #:mode 'auto #:from check #:to cleared
                 #:when (code goal-reached?))))
   playing))

(module+ model
  (provide make-model)

  (: terminal-node? (-> Node-Info Boolean))
  (define (terminal-node? x) (eq? (node-info-type x) 'terminal))

  (: make-model (-> Positive-Integer Positive-Integer Positive-Integer (Model Jug-State)))
  (define (make-model left-cap right-cap target)
    (model
     (thunk
      (define-values (graph node-init) (jug-graph "Water Jug Puzzle" left-cap right-cap target))
      (values (list graph) node-init (jug-state 0 0)))
     terminal-node?)))

(module+ main
  (require racket/cmdline
           (submod ".." model))

  (: mode (Boxof (U 'dot 'console 'random)))
  (define mode (box 'dot))

  (define program-name "water-jug")
  (command-line
   #:program program-name
   #:once-any
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   [("--console") "Run console" (set-box! mode 'console)]
   [("--random") "Run random" (set-box! mode 'random)]
   #:args ()
   (define m (make-model 3 5 4))
   (case (unbox mode)
     [(dot) (render-dot m
                        #:config (dot-config #:global
                                             (dot-global-config #:rankdir 'LR)))]
     [(console)
      (writeln (console-run m #:config (console-config
                                        #:commands (list (quit-console-command 'q "Quit"))
                                        #:trace-display 'hide)))]
     [(random)
      (define j (console-run m #:config (console-config
                                         #:chooser (lambda (_) 'random)
                                         #:trace-display 'hide)))
      (displayln "---------")
      (printf "Solved in ~a steps!\n"
              (for/sum ([e j])
                (case (journal-entry-edge-mode e)
                  [(choice) 1]
                  [else 0])))])))

(module+ test
  (require typed/rackunit (submod ".." model))

  (define m (make-model 3 5 4))

  (check-false (find-unsafety m
                              #:invariants
                              (list
                               (invariant "bound"
                                          (match-λ* [(list _ (jug-state l r))
                                                      (and (<= 0 l 3)
                                                           (<= 0 r 5))])))))
  (check-false (find-livelock m))

  (: shortest-path (-> (Model Jug-State) (Option Journal)))
  (define (shortest-path m)
    (let loop : (Option Journal) ([depth : Natural 0])
      (find-witness m (lambda ([n : Node-Info] _st) (member 'cleared (node-info-tags n)))
                    #:bound depth
                    #:bounded (thunk (loop (add1 depth))))))

  (check-equal? (shortest-path m)
                (list (auto "Clear!")
                      (choice "Pour 5G -> 3G")
                      (auto "Not yet")
                      (choice "Fill 5G")
                      (auto "Not yet")
                      (choice "Pour 5G -> 3G")
                      (auto "Not yet")
                      (choice "Empty 3G")
                      (auto  "Not yet")
                      (choice "Pour 5G -> 3G")
                      (auto "Not yet")
                      (choice "Fill 5G"))))
