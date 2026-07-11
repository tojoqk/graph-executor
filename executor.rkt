#lang typed/racket

(require "graph.rkt")
(require "journal.rkt")
(require "history.rkt")
(require "prompt.rkt")
(require "message.rkt")

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
                    [name (caar j-rec)]
                    [attrs (cdar j-rec)]
                    [ps-init (reverse (cdr j-rec))]
                    [edge-events : (Boxof (Listof (U Message-Info Prompt-Info))) (box '())]
                    [node-events : (Boxof (Listof (U Message-Info Prompt-Info))) (box '())])
               (cond [(findf (lambda ([e : (Edge T S)]) (string=? name (edge-name e))) edges)
                      => (lambda ([e : (Edge T S)])
                           (let ([cod (edge-cod e)]
                                 [bps : (Boxof (Listof (Pairof Prompt-Value Prompt-Attributes))) (box ps-init)])
                             (: pop-bps (-> (U 'edge 'node) (Prompt Any)))
                             (define ((pop-bps type) title op)
                               (define evs-box
                                 (case type
                                   [(node) node-events]
                                   [(edge) edge-events]))
                               (: push-event! (-> Prompt-Info Void))
                               (define (push-event! x)
                                 (set-box! evs-box (cons x (unbox evs-box))))
                               (let ([ps (unbox bps)])
                                 (set-box! bps (cdr ps))
                                 (if (null? ps)
                                     (error 'replay "unexpected end of prompt values")
                                     (let ([val (caar ps)]
                                           [attrs (cdar ps)])
                                       (case (car op)
                                         [(choose)
                                          (assert val string?)
                                          (push-event! (prompt-info-choose title
                                                                           attrs
                                                                           val
                                                                           (third op)))
                                          val]
                                         [(string)
                                          (assert val string?)
                                          (push-event! (prompt-info-string title attrs val))
                                          val]
                                         [(integer)
                                          (assert (assert val exact?) integer?)
                                          (push-event! (prompt-info-integer title attrs val))
                                          val]
                                         [(natural)
                                          (assert (assert val exact?) natural?)
                                          (push-event! (prompt-info-natural title attrs val))
                                          val]
                                         [(positive)
                                          (assert (assert val exact?) positive-integer?)
                                          (push-event! (prompt-info-positive title attrs val))
                                          val]
                                         [(range) (assert val exact?)
                                                  (assert val integer?)
                                                  (let ([min (second op)] [max : Integer (third op)])
                                                    (cond
                                                      [(and (positive? min) (<= min val) (<= val max))
                                                       (push-event! (prompt-info-range-positive title attrs val min max))
                                                       val]
                                                      [(and (natural? min)
                                                            (<= min val) (<= val max))
                                                       (push-event! (prompt-info-range-natural title attrs val min max))
                                                       val]
                                                      [(and (<= min val) (<= val max))
                                                       (push-event! (prompt-info-range-integer title attrs val min max))
                                                       val]
                                                      [else
                                                       (error 'retry "range error" val)]))]
                                         [(random)
                                          (assert val natural?)
                                          (push-event! (prompt-info-random title attrs val (second op)))
                                          val])))))
                             (: message-to-log (-> (U 'edge 'node) (-> Any Void)))
                             (define ((message-to-log type) msg)
                               (define evs-box
                                 (case type
                                   [(node) node-events]
                                   [(edge) edge-events]))
                               (: push-event! (-> Message-Info Void))
                               (define (push-event! x)
                                 (set-box! evs-box (cons x (unbox evs-box))))
                               (push-event! (message-info msg)))
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
                                     (list* (cons 'node
                                                  (history-node
                                                   (find-graph gs (node-graph-id cod))
                                                   (unbox node-events)
                                                   cod))
                                            (if (eq? (edge-mode e) 'auto)
                                                (cons 'auto
                                                      (history-auto
                                                       (find-graph gs (node-graph-id n))
                                                       (unbox edge-events)
                                                       e))
                                                (cons 'choose
                                                      (history-choose
                                                       (find-graph gs (node-graph-id n))
                                                       (unbox edge-events)
                                                       e
                                                       (second ne)
                                                       attrs)))
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
         [es (edge-sort (filter-state st (remove-annotation (filter-node n (graph-all-edges g)))))]
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

(: graph-all-edges (All (T S) (-> (Graph T S) (Listof (Edge T S)))))
(define (graph-all-edges g)
  ((inst append (Edge T S)) (graph-edges g) (graph-bridges g)))

(: filter-node (All (T S) (-> (Node T S) (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-node n es)
  (filter (lambda ([e : (Edge T S)])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))
