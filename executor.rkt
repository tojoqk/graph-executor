#lang typed/racket

(require "graph.rkt")
(require "journal.rkt")
(require "history.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "event-logger.rkt")

(provide replay
         find-graph next-edges auto-choose
         current-auto-conflict-policy current-single-choose-policy
         current-node-id current-node?
         current-edge-id current-edge?)

(: current-node-id (Parameterof (Option Symbol)))
(define current-node-id (make-parameter #f))

(: current-node? (All (T S) (-> (Node T S) Boolean)))
(define (current-node? n)
  (cond [(current-node-id) => (curry symbol=? (node-id n))]
        [else #f]))

(: current-edge-id (Parameterof (Option Symbol)))
(define current-edge-id (make-parameter #f))

(: current-edge? (All (T S) (-> (Edge T S) Boolean)))
(define (current-edge? e)
  (cond [(current-edge-id) => (curry symbol=? (edge-id e))]
        [else #f]))

(: replay (All (T S) (-> (Listof (Graph T S)) (Node T S) S Journal
                         (Values (Node T S) S (History T S)))))
(define (replay gs n st j)
  (let loop ([n n] [st st] [j (reverse j)] [h : (History T S) '()])
    (let ([ne (next-edges gs st n)])
      (if (null? j)
          (values n st h)
          (case (car ne)
            [(choose auto)
             (let* ([edges (cadr ne)]
                    [j-rec (car j)]
                    [name (caadr j-rec)]
                    [attrs (cdadr j-rec)]
                    [ps-init (reverse (cddr j-rec))])
               (cond [(findf (lambda ([e : (Edge T S)]) (string=? name (edge-name e))) edges)
                      => (lambda ([e : (Edge T S)])
                           (let* ([cod (edge-cod e)]
                                  [mode (edge-mode e)]
                                  [logger  (if (eq? mode 'auto)
                                               (make-event-logger e cod)
                                               (let ([pmt ((node-prompt n) st)])
                                                 (make-event-logger e pmt edges attrs cod)))]
                                  [bps : (Boxof (Listof (Pairof Prompt-Value Prompt-Attributes))) (box ps-init)])
                             (: pop-bps (-> (U 'edge 'node) Prompt-Implementation))
                             (define ((pop-bps type) title op)
                               (: push-event! (-> Prompt-Info Void))
                               (define (push-event! x)
                                 (event-logger-prompt-log! logger type x))
                               (let ([ps (unbox bps)])
                                 (set-box! bps (cdr ps))
                                 (if (null? ps)
                                     (error 'replay "unexpected end of prompt values")
                                     (let ([val (caar ps)]
                                           [attrs (cdar ps)])
                                       (case (car op)
                                         [(choose)
                                          (assert val string?)
                                          (let ([info (prompt-info-choose title attrs val (third op))])
                                            (push-event! info)
                                            info)]
                                         [(string)
                                          (assert val string?)
                                          (let ([info (prompt-info-string title attrs val)])
                                            (push-event! info)
                                            info)]
                                         [(integer)
                                          (assert (assert val exact?) integer?)
                                          (let ([info (prompt-info-integer title attrs val)])
                                            (push-event! info)
                                            info)]
                                         [(natural)
                                          (assert (assert val exact?) natural?)
                                          (let ([info (prompt-info-natural title attrs val)])
                                            (push-event! info)
                                            info)]
                                         [(positive-integer)
                                          (assert (assert val exact?) positive-integer?)
                                          (let ([info (prompt-info-positive-integer title attrs val)])
                                            (push-event! info)
                                            info)]
                                         [(range)
                                          (assert val exact?)
                                          (assert val integer?)
                                          (let ([min (second op)] [max : Integer (third op)])
                                            (cond
                                              [(and (<= min val) (<= val max))
                                               (let ([info (prompt-info-range title attrs val min max)])
                                                 (push-event! info)
                                                 info)]
                                              [else
                                               (error 'retry "range error" val)]))]
                                         [(random)
                                          (assert val natural?)
                                          (let ([info (prompt-info-random title attrs val (second op))])
                                            (push-event! info)
                                            info)])))))
                             (: message-to-log (-> (U 'edge 'node) (-> Any Void)))
                             (define ((message-to-log type) msg)
                               (event-logger-message-log! logger type msg))
                             (let ([next-st
                                    (parameterize ([current-message (message-to-log 'node)]
                                                   [current-prompt (pop-bps 'node)])
                                      ((node-trans cod)
                                       (parameterize ([current-message (message-to-log 'edge)]
                                                      [current-prompt (pop-bps 'edge)])
                                         ((edge-trans e) st))))])
                               (loop cod
                                     next-st
                                     (cdr j)
                                     (list* (event-logger->history-node logger)
                                            (event-logger->history-edge logger)
                                            h)))))]
                     [else (error 'replay "edge not found")]))]
            [(terminated) (error 'replay "unexpected termination")])))))

(: current-auto-conflict-policy (Parameterof (U 'random 'choose)))
(define current-auto-conflict-policy (make-parameter 'random))

(: current-single-choose-policy (Parameterof (U 'skip 'choose)))
(define current-single-choose-policy (make-parameter 'choose))

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Graph T S))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error 'find-graph "not found" g-id)]))

(: next-edges (All (T S)
                   (-> (Listof (Graph T S))
                       S
                       (Node T S)
                       (U (List 'auto (Pairof (Edge T S) (Listof (Edge T S))))
                          (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
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

(: auto-choose (All (T S)
                    (-> (List 'auto (Pairof (Edge T S) (Listof (Edge T S)))) (Edge T S))))
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
(: edge-sort (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (edge-sort es)
  ((inst sort (Edge T S) Integer) es > #:key edge-priority))

(: group-by-priority (All (T S) (-> (Listof (Edge T S)) (Listof (Listof (Edge T S))))))
(define (group-by-priority es)
  ((inst group-by (Edge T S) Integer) edge-priority es))

(: filter-state (All (T S) (-> S (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-state st es)
  (filter (lambda ([e : (Edge T S)])
            ((edge-when e) st))
          es))

(: remove-annotation (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (remove-annotation es)
  (filter (lambda ([e : (Edge T S)]) (not (eq? (edge-mode e) 'annotation)))
          es))

(: filter-auto (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-auto es)
  (filter (lambda ([e : (Edge T S)]) (eq? (edge-mode e) 'auto))
          es))

(: auto-edges (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (auto-edges es)
  (let loop ([ess : (Listof (Listof (Edge T S))) (group-by-priority es)])
    (if (null? ess)
        '()
        (let ([auto-es (filter-auto (car ess))])
          (if (null? auto-es)
              (loop (cdr ess))
              auto-es)))))

(: sum-weight (All (T S) (-> (Pairof (Edge T S) (Listof (Edge T S))) Positive-Integer)))
(define (sum-weight es)
  (define any-edge-foldl (inst foldl (Edge T S) Exact-Positive-Integer))
  (any-edge-foldl (lambda ([e1 : (Edge T S)] [acc : Exact-Positive-Integer])
                    (+ (edge-weight e1)
                       acc))
                  (edge-weight (car es))
                  (cdr es)))

(: filter-node (All (T S) (-> (Node T S) (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-node n es)
  (filter (lambda ([e : (Edge T S)])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))
