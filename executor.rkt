#lang typed/racket

(require "graph.rkt")
(require "model.rkt")
(require "journal.rkt")
(require "history.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "effect/consumer.rkt")
(require "effect/emitter.rkt")

(provide replay apply-journal
         auto-choose
         find-graph next-edges
         find-edge
         Command Transform-Command Action-Command Restore-Command Quit-Command
         transform-command? transform-command transform-command-proc
         action-command? action-command action-command-proc
         restore-command? restore-command restore-command-proc
         quit-command? quit-command)

(struct transform-command ([proc : (-> Journal Journal)])
  #:type-name Transform-Command)
(struct action-command ([proc : (-> Journal Void)])
  #:type-name Action-Command)
(struct restore-command ([proc : (-> (Option Journal))])
  #:type-name Restore-Command)
(struct quit-command ()
  #:type-name Quit-Command)

(define-type Command (U Transform-Command Action-Command Restore-Command Quit-Command))

(define-type Event (U Prompt-Result Message-Result))
(define-type Pmt (Pairof Prompt-Value Prompt-Attributes))

(: replay (All (S) (-> (Model S) Journal (Values (Node S) S (History S)))))
(define (replay m j)
  (define-values (call-with-consumer consume) ((inst make-consumer Pmt S)))
  (define-values (call-with-emitter emit) ((inst make-emitter Event (Pairof (Listof Pmt) S))))
  (define gs (model-graphs m))
  (let loop ([n (model-node m)] [st (model-state m)] [j (reverse j)] [h : (History S) '()])
    (let ([ne (next-edges gs st n)])
      (if (null? j)
          (values n st h)
          (case (car ne)
            [(choose auto)
             (let* ([edges (cadr ne)]
                    [j-rec (car j)]
                    [name (caadr j-rec)]
                    [attrs (cdadr j-rec)]
                    [ps-init (cddr j-rec)])
               (cond [(findf (lambda ([e : (Edge S)]) (string=? name (edge-name e))) edges)
                      => (lambda ([e : (Edge S)])
                           (match-define (list* edge-evs pvs-1 st-1)
                             (call-with-emitter
                              (thunk
                               (call-with-consumer
                                ps-init
                                (thunk
                                 (parameterize ([current-message (emit-message emit)]
                                                [current-prompt (pop-prompt consume emit)])
                                   ((edge-trans e) st)))))))
                           (match-define (list* node-evs _ next-st)
                             (call-with-emitter
                              (thunk
                               (call-with-consumer
                                pvs-1
                                (thunk
                                 (parameterize ([current-message (emit-message emit)]
                                                [current-prompt (pop-prompt consume emit)])
                                   ((node-trans (edge-to e)) st-1)))))))
                           (loop (edge-to e)
                                 next-st
                                 (cdr j)
                                 (list*
                                  (node-record node-evs (edge-to e))
                                  (case (edge-mode e)
                                    [(auto) (auto-edge-record edge-evs e)]
                                    [(choose) (choose-edge-record edge-evs e ((node-prompt n) st) edges attrs)]
                                    [(annotation) (error 'replay "invalid edge mode")])
                                  h)))]
                     [else (error 'replay "edge not found")]))]
            [(terminated) (error 'replay "unexpected termination")]
            [(auto-conflicted) (error 'replay "unexpected auto-conflicted error: ~s" (second ne))])))))

(: apply-journal (All (S) (-> (Model S) Journal (Values Node-Info S))))
(define (apply-journal m j)
  (define-values (n st _h) (replay m j))
  (values (node-node-info n) st))

(: find-graph (All (S) (-> (Listof (Graph S)) Symbol (Graph S))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error 'find-graph "not found" g-id)]))

(: next-edges (All (S)
                   (-> (Listof (Graph S))
                       S
                       (Node S)
                       (U (List 'auto (List (Edge S)))
                          (List 'choose (Pairof (Edge S) (Listof (Edge S))))
                          (List 'terminated)
                          (List 'auto-conflicted (Pairof String (Listof String)))))))
(define (next-edges gs st n)
  (let* ([g (find-graph gs (node-graph-id n))]
         [es (edge-sort (filter-state st (remove-annotation (filter-node n (graph-edges g)))))]
         [aes (auto-edges es)])
    (if (null? aes)
        (if (null? es)
            (list 'terminated)
            (list 'choose es))
        (if (null? (cdr aes))
            (list 'auto aes)
            (list 'auto-conflicted (cons (edge-name (car aes))
                                         (map (inst edge-name S) (cdr aes))))))))

(: auto-choose (All (S) (-> (List 'auto (List (Edge S))) (Edge S))))
(define (auto-choose ne) (caadr ne))

;; --- private ---
(: edge-sort (All (S) (-> (Listof (Edge S)) (Listof (Edge S)))))
(define (edge-sort es)
  ((inst sort (Edge S) Integer) es > #:key edge-priority))

(: group-by-priority (All (S) (-> (Listof (Edge S)) (Listof (Listof (Edge S))))))
(define (group-by-priority es)
  ((inst group-by (Edge S) Integer) edge-priority es))

(: filter-state (All (S) (-> S (Listof (Edge S)) (Listof (Edge S)))))
(define (filter-state st es)
  (filter (lambda ([e : (Edge S)])
            ((edge-when e) st))
          es))

(: remove-annotation (All (S) (-> (Listof (Edge S)) (Listof (Edge S)))))
(define (remove-annotation es)
  (filter (lambda ([e : (Edge S)]) (not (eq? (edge-mode e) 'annotation)))
          es))

(: filter-auto (All (S) (-> (Listof (Edge S)) (Listof (Edge S)))))
(define (filter-auto es)
  (filter (lambda ([e : (Edge S)]) (eq? (edge-mode e) 'auto))
          es))

(: auto-edges (All (S) (-> (Listof (Edge S)) (Listof (Edge S)))))
(define (auto-edges es)
  (let loop ([ess : (Listof (Listof (Edge S))) (group-by-priority es)])
    (if (null? ess)
        '()
        (let ([auto-es (filter-auto (car ess))])
          (if (null? auto-es)
              (loop (cdr ess))
              auto-es)))))

(: filter-node (All (S) (-> (Node S) (Listof (Edge S)) (Listof (Edge S)))))
(define (filter-node n es)
  (filter (lambda ([e : (Edge S)])
            (eq? (node-id n) (node-id (edge-from e))))
          es))

(: find-edge (All (S) (-> (Pairof (Edge S) (Listof (Edge S))) String (Edge S))))
(define (find-edge es name)
  (cond
    [(findf (lambda ([e : (Edge S)]) (string=? name (edge-name e))) es) => identity]
    [else (error 'find-edge "not found")]))

(: emit-message (-> (-> Event Void) (-> Any Void)))
(define ((emit-message emit) msg)
  (emit (message-result msg)))

(: pop-prompt (-> (-> (-> Nothing) Pmt)
                  (-> Event Void)
                  Prompt-Implementation))
(define ((pop-prompt consume emit) info op)
  (: push-event! (-> Prompt-Value Prompt-Attributes Void))
  (define (push-event! val attrs)
    (emit (prompt-result op info (cons val attrs))))
  (: fail (-> Nothing))
  (define (fail)
    (error 'replay "unexpected end of prompt values"))
  (let* ([val+attrs (consume fail)]
         [val (car val+attrs)]
         [attrs (cdr val+attrs)])
    (case (car op)
      [(choose) (assert val symbol?)
                (push-event! val attrs)
                (values val attrs)]
      [(string) (assert val string?)
                (push-event! val attrs)
                (values val attrs)]
      [(integer) (assert (assert val exact?) integer?)
                 (push-event! val attrs)
                 (values val attrs)]
      [(natural) (assert (assert val exact?) natural?)
                 (push-event! val attrs)
                 (values val attrs)]
      [(positive-integer) (assert (assert val exact?) positive-integer?)
                          (push-event! val attrs)
                          (values val attrs)]
      [(between) (assert val exact?)
               (assert val integer?)
               (let ([min (second op)] [max : Integer (third op)])
                 (cond [(and (<= min val) (<= val max)) (push-event! val attrs)
                                                        (values val attrs)]
                       [else (error 'replay "between error" val)]))]
      [(random) (assert val natural?)
                (push-event! val attrs)
                (values val attrs)])))
