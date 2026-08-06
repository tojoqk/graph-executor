#lang typed/racket

(require "graph.rkt")
(require "journal.rkt")
(require "history.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "effect/consumer.rkt")
(require "effect/emitter.rkt")

(provide replay
         find-graph Status next-edges auto-choose
         current-auto-conflict-policy current-single-choose-policy
         current-node-id current-node?
         current-edge-id current-edge?
         find-edge
         Command)

(define-type Status (U 'terminated 'auto 'choose))

(define-type Command (U (List 'transform (-> Journal Journal))
                        (List 'action (-> Journal Void))
                        (List 'restore (-> (Option Journal)))
                        (List 'quit)))

(: current-node-id (Parameterof (Option Symbol)))
(define current-node-id (make-parameter #f))

(: current-node? (All (S) (-> (Node S) Boolean)))
(define (current-node? n)
  (cond [(current-node-id) => (curry symbol=? (node-id n))]
        [else #f]))

(: current-edge-id (Parameterof (Option Symbol)))
(define current-edge-id (make-parameter #f))

(: current-edge? (All (S) (-> (Edge S) Boolean)))
(define (current-edge? e)
  (cond [(current-edge-id) => (curry symbol=? (edge-id e))]
        [else #f]))

(define-type Event (U Prompt-Info Message-Info))
(define-type Pmt (Pairof Prompt-Value Prompt-Attributes))

(: replay (All (S) (-> (Listof (Graph S)) (Node S) S Journal
                       (Values (Node S) S (History S)))))
(define (replay gs n st j)
  (define-values (call-with-consumer consume) ((inst make-consumer Pmt S)))
  (define-values (call-with-emitter emit) ((inst make-emitter Event (Pairof (Listof Pmt) S))))
  (let loop ([n n] [st st] [j (reverse j)] [h : (History S) '()])
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
                                (thunk
                                 (parameterize ([current-message (emit-message emit)]
                                                [current-prompt (pop-prompt consume emit)])
                                   ((edge-trans e) st)))
                                ps-init))))
                           (match-define (list* node-evs _ next-st)
                             (call-with-emitter
                              (thunk
                               (call-with-consumer
                                (thunk
                                 (parameterize ([current-message (emit-message emit)]
                                                [current-prompt (pop-prompt consume emit)])
                                   ((node-trans (edge-to e)) st-1)))
                                pvs-1))))
                           (loop (edge-to e)
                                 next-st
                                 (cdr j)
                                 (list*
                                  (node-record node-evs n)
                                  (case (edge-mode e)
                                    [(auto) (auto-edge-record edge-evs e)]
                                    [(choose) (choose-edge-record edge-evs e ((node-prompt n) st) edges attrs)]
                                    [(annotation) (error 'replay "invalid edge mode")])
                                  h)))]
                     [else (error 'replay "edge not found")]))]
            [(terminated) (error 'replay "unexpected termination")])))))

(: current-auto-conflict-policy (Parameterof (U 'random 'choose)))
(define current-auto-conflict-policy (make-parameter 'random))

(: current-single-choose-policy (Parameterof (U 'skip 'choose)))
(define current-single-choose-policy (make-parameter 'choose))

(: find-graph (All (S) (-> (Listof (Graph S)) Symbol (Graph S))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error 'find-graph "not found" g-id)]))

(: next-edges (All (S)
                   (-> (Listof (Graph S))
                       S
                       (Node S)
                       (U (List 'auto (Pairof (Edge S) (Listof (Edge S))))
                          (List 'choose (Pairof (Edge S) (Listof (Edge S))))
                          (List 'terminated)))))
(define (next-edges gs st n)
  (let* ([g (find-graph gs (node-graph-id n))]
         [es (edge-sort (filter-state st (remove-annotation (filter-node n (graph-edges g)))))]
         [aes (auto-edges es)])
    (if (null? aes)
        (if (null? es)
            (list 'terminated)
            (if (null? (cdr es))
                (let ([policy (current-single-choose-policy)])
                  (cond [(eq? policy 'skip) (list 'auto es)]
                        [(eq? policy 'choose) (list 'choose es)]))
                (list 'choose es)))
        (if (null? (cdr aes))
            (list 'auto aes)
            (let ([policy (current-auto-conflict-policy)])
              (cond [(eq? policy 'random) (list 'auto aes)]
                    [(eq? policy 'choose) (list 'choose aes)]))))))

(: auto-choose (All (S)
                    (-> (List 'auto (Pairof (Edge S) (Listof (Edge S)))) (Edge S))))
(define (auto-choose ne)
  (let* ([edges (cadr ne)]
         [s (sum-weight edges)]
         [r (random s)])
    (let loop ([edges edges]
               [r r])
      (let ([fst (car edges)]
            [rst (cdr edges)])
        (cond [(< r (edge-weight fst)) fst]
              [(null? rst) (error "auto-choose: unreachble")]
              [else (loop rst (- r (edge-weight fst)))])))))

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

(: sum-weight (All (S) (-> (Pairof (Edge S) (Listof (Edge S))) Positive-Integer)))
(define (sum-weight es)
  (define any-edge-foldl (inst foldl (Edge S) Exact-Positive-Integer))
  (any-edge-foldl (lambda ([e1 : (Edge S)] [acc : Exact-Positive-Integer])
                    (+ (edge-weight e1)
                       acc))
                  (edge-weight (car es))
                  (cdr es)))

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
  (emit (message-info msg)))

(: pop-prompt (-> (-> (-> Nothing) Pmt)
                  (-> Event Void)
                  Prompt-Implementation))
(define ((pop-prompt consume emit) title op)
  (: push-event! (-> String Prompt-Op Prompt-Value Prompt-Attributes Void))
  (define (push-event! title op val attrs)
    (emit (prompt-info op title val attrs)))
  (: fail (-> Nothing))
  (define (fail)
    (error 'replay "unexpected end of prompt values"))
  (let* ([val+attrs (consume fail)]
         [val (car val+attrs)]
         [attrs (cdr val+attrs)])
    (case (car op)
      [(choose) (assert val string?)
                (push-event! title op val attrs)
                (values val attrs)]
      [(string) (assert val string?)
                (push-event! title op val attrs)
                (values val attrs)]
      [(integer) (assert (assert val exact?) integer?)
                 (push-event! title op val attrs)
                 (values val attrs)]
      [(natural) (assert (assert val exact?) natural?)
                 (push-event! title op val attrs)
                 (values val attrs)]
      [(positive-integer) (assert (assert val exact?) positive-integer?)
                          (push-event! title op val attrs)
                          (values val attrs)]
      [(range) (assert val exact?)
               (assert val integer?)
               (let ([min (second op)] [max : Integer (third op)])
                 (cond [(and (<= min val) (<= val max)) (push-event! title op val attrs)
                                                        (values val attrs)]
                       [else (error 'replay "range error" val)]))]
      [(random) (assert val natural?)
                (push-event! title op val attrs)
                (values val attrs)])))
