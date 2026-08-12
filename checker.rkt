#lang typed/racket

(require "graph.rkt")
(require "model.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "prompt/checker.rkt")
(require "executor.rkt")
(require "journal.rkt")
(require "effect/amb.rkt")
(require "effect/emitter.rkt")
(require "effect/state.rkt")

(provide find-counterexample
         current-model-checker-trace-display)

(: current-model-checker-trace-display (Parameterof (U 'show 'hide)))
(define current-model-checker-trace-display (make-parameter 'hide))

(: current-model-checker-trace-display? (-> Boolean))
(define (current-model-checker-trace-display?)
  (case (current-model-checker-trace-display)
    [(show) #t]
    [(hide) #f]))

(: find-counterexample (All (S) (-> (Model S) (-> Status (Node S) S Any)
                                    [#:journal Journal]
                                    [#:max-depth (Option Natural)]
                                    (Option Journal))))
(define (find-counterexample m invariant
                             #:journal [j '()]
                             #:max-depth [max-depth #f])
  (define-values (call-with-seen-state seen-get seen-set)
    ((inst make-state (Setof (Pairof Symbol S)) False)))
  (define-values (call-with-prompt-value-emitter prompt-value-emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (call-with-amb amb amb-fail)
    ((inst make-amb Prompt-Value)))
  (define gs (model-graphs m))
  (define-values (n st _h) (replay gs (model-node m) (model-state m) j))
  (let/ec return : Journal
    (cdr
     (call-with-seen-state
      (thunk
       (call-with-amb
        (thunk
         (let loop : #f ([n n] [st st] [j j] [depth 0])
           (define seen-key `(,(node-id n) . ,st))
           (when (set-member? (seen-get) seen-key)
             (amb-fail))
           (define ne (next-edges gs st n))
           (define ne-type (car ne))
           (unless (invariant (car ne) n st)
             (return j))
           (case ne-type
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
                  (begin
                    (seen-set (set-add (seen-get) seen-key))
                    (loop (edge-to chosen-edge)
                          next-st
                          (cons (case ne-type
                                  [(auto) (auto-journal-entry (edge-name chosen-edge) ps)]
                                  [(choose) (choose-journal-entry (edge-name chosen-edge) '() ps)]) j)
                          (add1 depth))))])))
        (thunk #f)))
      (set)))))

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
