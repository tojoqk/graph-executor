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

(provide find-counterexample find-deadlock find-false-terminal find-auto-conflict find-livelock
         Checker-Config checker-config
         current-checker-trace-display)

(: current-checker-trace-display (Parameterof (U 'show 'hide)))
(define current-checker-trace-display (make-parameter 'hide))

(: current-checker-trace-display? (-> Boolean))
(define (current-checker-trace-display?)
  (case (current-checker-trace-display)
    [(show) #t]
    [(hide) #f]))

(define-type (Next-Edge S)
  (U (List 'auto (Pairof (Edge S) (Listof (Edge S))))
     (List 'choose (Pairof (Edge S) (Listof (Edge S))))
     (List 'terminated)
     (List 'auto-conflicted (Pairof String (Listof String)))))

(struct %checker-config ([prompt : Checker-Prompt-Config])
  #:type-name Checker-Config)

(: checker-config (-> [#:prompt Checker-Prompt-Config] Checker-Config))
(define (checker-config #:prompt [prompt-config (checker-prompt-config)])
  (%checker-config prompt-config))

(: find-counterexample (All (S) (-> (Model S) (-> Node-Meta S Any)
                                    [#:journal Journal]
                                    [#:bound (Option Natural)]
                                    [#:bounded (-> (Option Journal))]
                                    [#:config Checker-Config]
                                    (Option Journal))))
(define (find-counterexample m invariant
                             #:journal [j '()]
                             #:bound [bound #f]
                             #:bounded [bounded (const #f)]
                             #:config [config (checker-config)])
  (%find-counterexample m
                        (lambda (_ne [n : Node-Meta] [st : S])
                          (invariant n st))
                        #:journal j
                        #:bound bound
                        #:bounded bounded
                        #:config config))

(: find-deadlock (All (S) (-> (Model S) (-> Node-Meta Any)
                              [#:bound (Option Natural)]
                              [#:bounded (-> (Option Journal))]
                              [#:config Checker-Config]
                              (Option Journal))))
(define (find-deadlock m terminal-node? #:bound [bound #f] #:bounded [bounded (const #f)] #:config [config (checker-config)])
  (%find-counterexample m
                        (lambda ([ne : (Next-Edge S)] [n : Node-Meta] _st)
                          (case (car ne)
                            [(terminated) (terminal-node? n)]
                            [else #t]))
                        #:bound bound
                        #:bounded bounded
                        #:config config))

(: find-false-terminal (All (S) (-> (Model S) (-> Node-Meta Any)
                                    [#:bound (Option Natural)]
                                    [#:bounded (-> (Option Journal))]
                                    [#:config Checker-Config]
                                    (Option Journal))))
(define (find-false-terminal m terminal-node? #:bound [bound #f] #:bounded [bounded (const #f)] #:config [config (checker-config)])
  (%find-counterexample m
                        (lambda ([ne : (Next-Edge S)] [n : Node-Meta] _st)
                          (case (car ne)
                            [(terminated) #t]
                            [else (not (terminal-node? n))]))
                        #:bound bound
                        #:bounded bounded
                        #:config config))

(: find-auto-conflict (All (S) (-> (Model S)
                                   [#:bound (Option Natural)]
                                   [#:bounded (-> (Option Journal))]
                                   [#:config Checker-Config]
                                   (Option Journal))))
(define (find-auto-conflict m #:bound [bound #f] #:bounded [bounded (const #f)] #:config [config (checker-config)])
  (%find-counterexample m
                        (lambda ([ne : (Next-Edge S)] _n _st)
                          (case (car ne)
                            [(auto-conflicted) #f]
                            [else #t]))
                        #:bound bound
                        #:bounded bounded
                        #:config config))

(define pmt-meta (prompt-meta "choose"))

(: %find-counterexample (All (S) (-> (Model S) (-> (Next-Edge S) Node-Meta S Any)
                                     [#:journal Journal]
                                     [#:bound (Option Natural)]
                                     [#:bounded (-> (Option Journal))]
                                     #:config Checker-Config
                                     (Option Journal))))
(define (%find-counterexample m invariant
                              #:journal [j '()]
                              #:bound [bound #f]
                              #:bounded [bounded (const #f)]
                              #:config config)
  (define-values (call-with-bounded-state _bounded-get bounded-set)
    ((inst make-state Boolean (Pairof (Immutable-HashTable (Pairof Symbol S) Natural)
                                      (Option Journal)))))
  (define-values (call-with-seen-state seen-get seen-set)
    ((inst make-state (Immutable-HashTable (Pairof Symbol S) Natural) (Option Journal))))
  (define-values (call-with-prompt-value-emitter prompt-value-emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (call-with-amb amb amb-fail)
    ((inst make-amb Prompt-Value)))
  (define gs (model-graphs m))
  (define-values (n st _h) (replay m j))
  (match-define (list* bounded? _ result)
    (call-with-bounded-state
     #f
     (thunk
      (call-with-seen-state
       (hash)
       (thunk
        (let/ec return : Journal
          (call-with-amb
           (thunk
            (let loop : #f ([n n] [st st] [j j] [depth : Natural 0])
              (define seen-key `(,(node-id n) . ,st))
              (let ([seen-depth (hash-ref (seen-get) seen-key #f)])
                (when seen-depth
                  (if bound
                      (if (< depth seen-depth)
                          (void)
                          (amb-fail))
                      (amb-fail))))
              (define ne (next-edges gs st n))
              (define ne-type (car ne))
              (unless (invariant ne (node-node-meta n) st)
                (return j))
              (case ne-type
                [(terminated auto-conflicted) (amb-fail)]
                [(auto choose) (when (and bound (= bound depth))
                                 (begin (bounded-set #t)
                                        (amb-fail)))
                               (define name
                                 (choose amb (map (inst edge-name S) (second ne))))
                               (define chosen-edge (find-edge (second ne) name))
                               (when (current-checker-trace-display?)
                                 (printf "Current Edge: ~a (Graph: ~a)\n" (edge-name chosen-edge) (node-graph-name n)))
                               (match-define (cons ps next-st)
                                 (call-with-prompt-value-emitter
                                  (thunk (step st chosen-edge amb config prompt-value-emit))))
                               (seen-set (hash-set (seen-get) seen-key depth))
                               (loop (edge-to chosen-edge)
                                     next-st
                                     (cons (case ne-type
                                             [(auto) (auto-journal-entry (edge-name chosen-edge) ps)]
                                             [(choose) (choose-journal-entry (edge-name chosen-edge) '() ps)]) j)
                                     (add1 depth))])))
           (thunk #f))))))))
  (if result
      result
      (if bounded?
          (bounded)
          #f)))

(: step (All (S) (-> S
                     (Edge S)
                     (-> (-> Prompt-Value) * Prompt-Value)
                     Checker-Config
                     (-> (Pairof Prompt-Value Prompt-Attributes) Void)
                     S)))
(define (step st e amb config emit)
  (define (void-message val)
    (void))
  (parameterize ([current-prompt (prompt/log amb (%checker-config-prompt config) emit)]
                 [current-message void-message])
    ((node-trans (edge-to e))
     (begin0 ((edge-trans e) st)
       (when (current-checker-trace-display?)
         (let ([n (edge-to e)])
           (printf "Current Node: ~a (Graph: ~a)\n" (node-name n) (node-graph-name n))))))))

(: prompt/log (All (S) (-> (-> (-> Prompt-Value) * Prompt-Value)
                           Checker-Prompt-Config
                           (-> (Pairof Prompt-Value Prompt-Attributes) Void)
                           Prompt-Implementation)))
(define ((prompt/log amb config emit) meta op)
  (define-values (val attrs) ((checker-prompt amb config) meta op))
  (emit (cons val attrs))
  (values val attrs))

(: find-livelock (All (S) (-> (Model S) [#:config Checker-Config] (Option Journal))))
(define (find-livelock m #:config [config (checker-config)])
  (define-values (call-with-reachable-state reachable-get reachable-set)
    ((inst make-state (Setof (Pairof Symbol S)) False)))
  (define-values (call-with-prompt-value-emitter prompt-value-emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (call-with-amb amb amb-fail)
    ((inst make-amb Prompt-Value)))
  (define gs (model-graphs m))
  (let/ec return : Journal
    (cdr
     (call-with-reachable-state
      (set)
      (thunk
       (call-with-amb
        (thunk
         (let loop : #f ([n (model-node m)] [st (model-state m)] [j : Journal '()] [breadcrumbs : (Setof (Pairof Symbol S)) (set)])
           (define key `(,(node-id n) . ,st))
           (when (set-member? (reachable-get) key)
             (amb-fail))
           (when (set-member? breadcrumbs key)
             (when (current-checker-trace-display?)
               (displayln "--- Start find-terminal ---"))
             (cond [(find-terminal config gs n st (reachable-get))
                    => (lambda ([next-reachable : (Setof (Pairof Symbol S))])
                         (reachable-set (set-union next-reachable breadcrumbs))
                         (when (current-checker-trace-display?)
                           (displayln "--- End find-terminal ---"))
                         (amb-fail))]
                   [else (return j)]))
           (define ne (next-edges gs st n))
           (define ne-type (car ne))
           (case ne-type
             [(terminated auto-conflicted) (reachable-set (set-union (reachable-get) breadcrumbs))
                                           (amb-fail)]
             [(auto choose) (define name
                              (choose amb (map (inst edge-name S) (second ne))))
                            (define chosen-edge (find-edge (second ne) name))
                            (when (current-checker-trace-display?)
                              (printf "Current Edge: ~a (Graph: ~a)\n" (edge-name chosen-edge) (node-graph-name n)))
                            (match-define (cons ps next-st)
                              (call-with-prompt-value-emitter
                               (thunk (step st chosen-edge amb config prompt-value-emit))))
                            (loop (edge-to chosen-edge)
                                  next-st
                                  (cons (case ne-type
                                          [(auto) (auto-journal-entry (edge-name chosen-edge) ps)]
                                          [(choose) (choose-journal-entry (edge-name chosen-edge) '() ps)]) j)
                                  (set-add breadcrumbs key))])))
        (thunk #f)))))))

(: find-terminal (All (S) (-> Checker-Config
                              (Listof (Graph S)) (Node S) S
                              (Setof (Pairof Symbol S))
                              (Option (Setof (Pairof Symbol S))))))
(define (find-terminal config gs n st reachable)
  (define-values (call-with-seen-state seen-get seen-set)
    ((inst make-state (Setof (Pairof Symbol S)) False)))
  (define-values (call-with-amb amb amb-fail)
    ((inst make-amb Prompt-Value)))
  (let/ec return : (Setof (Pairof Symbol S))
    (cdr
     (call-with-seen-state
      (set)      
      (thunk
       (call-with-amb
        (thunk
         (let loop : #f ([n n] [st st] [breadcrumbs : (Listof (Pairof Symbol S)) '()])
           (define key `(,(node-id n) . ,st))
           (when (set-member? reachable key)
             (return (set-union reachable (list->set breadcrumbs))))
           (when (set-member? (seen-get) key)
             (amb-fail))
           (define ne (next-edges gs st n))
           (define ne-type (car ne))
           (case ne-type
             [(terminated) (return (set-union reachable (list->set breadcrumbs)))]
             [(auto-conflicted) (seen-set (set-add (seen-get) key))
                                (amb-fail)]
             [(auto choose) (define name
                              (choose amb (map (inst edge-name S) (second ne))))
                            (define chosen-edge (find-edge (second ne) name))
                            (when (current-checker-trace-display?)
                              (printf "Current Edge: ~a (Graph: ~a)\n" (edge-name chosen-edge) (node-graph-name n)))
                            (seen-set (set-add (seen-get) key))
                            (loop (edge-to chosen-edge)
                                  (step/no-log st chosen-edge amb config)
                                  (cons key breadcrumbs))])))
        (thunk #f)))))))

(: step/no-log (All (S) (-> S
                            (Edge S)
                            (-> (-> Prompt-Value) * Prompt-Value)
                            Checker-Config
                            S)))
(define (step/no-log st e amb config)
  (define (void-message _val) (void))
  (parameterize ([current-prompt (checker-prompt amb (%checker-config-prompt config))]
                 [current-message void-message])
    ((node-trans (edge-to e))
     (begin0 ((edge-trans e) st)
       (when (current-checker-trace-display?)
         (let ([n (edge-to e)])
           (printf "Current Node: ~a (Graph: ~a)\n" (node-name n) (node-graph-name n))))))))

(: choose (-> (-> (-> Prompt-Value) * Prompt-Value) (Listof String) String))
(define (choose amb lst)
  (assert (let loop : Prompt-Value ([lst lst])
            (if (null? lst)
                (amb)
                (amb (thunk (car lst))
                     (thunk (loop (cdr lst))))))
          string?))
