#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../prompt/model-checker.rkt")
(require "../executor.rkt")
(require "../journal.rkt")
(require "../effect/amb.rkt")
(require "../effect/emitter.rkt")

(provide model-checker-run
         current-model-checker-counterexample-display
         current-model-checker-trace-display)

(: current-model-checker-counterexample-display (Parameterof (U 'show 'hide)))
(define current-model-checker-counterexample-display (make-parameter 'show))

(: current-model-checker-counterexample-display? (-> Boolean))
(define (current-model-checker-counterexample-display?)
  (case (current-model-checker-counterexample-display)
    [(show) #t]
    [(hide) #f]))

(: current-model-checker-trace-display (Parameterof (U 'show 'hide)))
(define current-model-checker-trace-display (make-parameter 'hide))

(: current-model-checker-trace-display? (-> Boolean))
(define (current-model-checker-trace-display?)
  (case (current-model-checker-trace-display)
    [(show) #t]
    [(hide) #f]))

(define-type Next-Mode (U 'terminated 'auto 'choose))

(: model-checker-run (All (S) (case-> (-> (Listof (Graph S)) (Node S) S
                                          #:mode 'first
                                          #:invariant (-> Next-Mode Symbol S Any)
                                          [#:max-depth (Option Natural)]
                                          (Option Journal))
                                      (-> (Listof (Graph S)) (Node S) S
                                          #:mode 'all
                                          #:invariant (-> Next-Mode Symbol S Any)
                                          [#:max-depth (Option Natural)]
                                          (Listof Journal)))))
(define (model-checker-run gs entry initial-state
                           #:mode mode
                           #:invariant invariant
                           #:max-depth [max-depth #f])
  (define-values (call-with-journal-emitter journal-emit)
    ((inst make-emitter Journal 'done)))
  (define-values (call-with-prompt-value-emitter prompt-value-emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (call-with-amb amb amb-fail)
    ((inst make-amb Prompt-Value)))
  (: prompt Prompt-Implementation)
  (define result
    (car
     (call-with-journal-emitter
      (thunk
       (let/ec return : 'done
         (call-with-amb
          (thunk
           (let loop : 'done ([n entry]
                              [st initial-state]
                              [j : Journal '()]
                              [depth 0]
                              [seen : (Setof (Pairof Symbol S)) (set)])
             (define seen-key `(,(node-id n) . ,st))
             (when (set-member? seen seen-key)
               (amb-fail))
             (define ne (next-edges gs st n))
             (unless (invariant (car ne) (node-type n) st)
               (journal-emit j)
               (when (current-model-checker-counterexample-display?)
                 (printf "Counterexample: ~s\n" j))
               (case mode
                 [(first) (return 'done)]
                 [(all) (amb-fail)]))
             (case (car ne)
               [(terminated) (amb-fail)]
               [(auto choose)
                (define-values (name _)
                  ((model-checker-prompt amb) "choose"
                                              `(choose ,(map (inst edge-name S) (second ne)))))
                (define chosen-edge (find-edge (second ne) name))
                (when (current-model-checker-trace-display?)
                  (printf "Current Edge: ~a (Graph: ~a)\n" (edge-name chosen-edge) (node-graph-name n)))
                (match-define (cons ps next-st)
                  (call-with-prompt-value-emitter
                   (thunk (step st chosen-edge amb prompt-value-emit))))
                (if (and max-depth (= max-depth depth))
                    (amb-fail)
                    (loop (edge-to chosen-edge)
                          next-st
                          (cons (choose-journal-entry (edge-name chosen-edge) '() ps) j)
                          (add1 depth)
                          (set-add seen seen-key)))])))
          (thunk 'done)))))))
  (case mode
    [(first) (if (null? result) #f (car result))]
    [(all) result]))

(: step (All (S) (-> S
                     (Edge S)
                     (-> (-> Prompt-Value) * Prompt-Value)
                     (-> (Pairof Prompt-Value Prompt-Attributes) Void)
                     S)))
(define (step st e amb emit)
  (define (void-message val)
    (void))
  (parameterize ([current-prompt (prompt/log amb emit)]
                 [current-message void-message])
    ((node-trans (edge-to e))
     (begin0 ((edge-trans e) st)
       (when (current-model-checker-trace-display?)
         (let ([n (edge-to e)])
           (printf "Current Node: ~a (Graph: ~a)\n" (node-name n) (node-graph-name n))))))))

(: prompt/log (All (S) (-> (-> (-> Prompt-Value) * Prompt-Value)
                           (-> (Pairof Prompt-Value Prompt-Attributes) Void)
                           Prompt-Implementation)))
(define ((prompt/log amb emit) title op)
  (define-values (val attrs) ((model-checker-prompt amb) title op))
  (emit (cons val attrs))
  (values val attrs))
